const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const cc = @import("capi.zig");
const sema = cy.sema;
const types = cy.types;
const bt = types.BuiltinTypes;
const fmt = cy.fmt;
const v = fmt.v;
const vmc = cy.vmc;
const log = cy.log.scoped(.module);

pub const ModFuncKey = cy.hash.KeyU96;

/// A module contains symbols declared in Cyber source code and through the embedded API.
/// Overloaded functions are linked together so that type signatures can be checked sequentially.
pub const Module = struct {
    /// Name to sym.
    symMap: std.StringHashMapUnmanaged(*cy.Sym),

    /// Sym+funcSig to func sym. This is used to check whether an overloaded function is unique.
    /// This should only be populated for a function sym once it's overloaded.
    overloadedFuncMap: std.HashMapUnmanaged(ModFuncKey, *cy.Func, cy.hash.KeyU96Context, 80),

    /// Tracks which binded vars are retained.
    retainedVars: std.ArrayListUnmanaged(*cy.Sym),

    /// Chunk that contains the module declaration.
    chunk: *cy.Chunk,

    pub fn init(chunk: *cy.Chunk) Module {
        return .{
            .symMap = .{},
            .overloadedFuncMap = .{},
            .retainedVars = .{},
            .chunk = chunk,
        };
    }

    pub fn deinitRetained(self: *Module, vm: *cy.VM) void {
        if (cy.Trace and self.retainedVars.items.len > 0) {
            log.tracev("deinit retained: {}", .{self.retainedVars.items.len});
        }
        for (self.retainedVars.items) |sym| {
            switch (sym.type) {
                .hostVar => {
                    log.tracev("release {s}", .{sym.name()});
                    const hostVar = sym.cast(.hostVar);
                    cy.arc.release(vm, hostVar.val);
                },
                else => {}
            }
        }
        self.retainedVars.clearRetainingCapacity();
    }

    pub fn deinit(self: *Module, alloc: std.mem.Allocator) void {
        self.retainedVars.deinit(alloc);
        self.symMap.deinit(alloc);
        self.overloadedFuncMap.deinit(alloc);
    }

    pub fn dump(self: *const Module, c: *cy.VMcompiler) void {
        std.debug.print("Module spec={s} ({} syms):\n", .{self.absSpec, self.syms.size});
        var iter = self.syms.iterator();
        while (iter.next()) |e| {
            const sym = e.value_ptr.*;
            const key = e.key_ptr.*.moduleSymKey;
            const name = sema.getName(c, key.nameId);
            std.debug.print("{s}: {}\n", .{name, sym.symT});
        }
    }

    pub fn getFirstFunc(m: *const Module, name: []const u8) ?*cy.Func {
        const sym = m.symMap.get(name) orelse return null;
        return sym.cast(.func).first;
    }

    pub fn getFuncById(m: *const Module, id: cy.FuncId) *cy.Func {
        return m.chunk.funcs.items[id];
    }

    pub fn getSym(m: *Module, name: []const u8) ?*cy.Sym {
        return m.symMap.get(name);
    }

    fn isFuncUnique(m: *Module, func: *cy.sym.FuncSym, func_sig: sema.FuncSigId) bool {
        if (func.firstFuncSig == func_sig) {
            return false;
        }
        if (func.numFuncs == 1) {
            return true;
        }
        
        const key = ModFuncKey.initModFuncKey(func, func_sig);
        return !m.overloadedFuncMap.contains(key);
    }

    pub fn updateParentRefs(m: *Module, parent: *cy.Sym) void {
        var iter = m.symMap.valueIterator();
        while (iter.next()) |val_ptr| {
            val_ptr.*.parent = parent;
            if (val_ptr.*.type == .func) {
                const func_sym = val_ptr.*.cast(.func);
                var opt_func: ?*cy.Func = func_sym.first;
                while (opt_func) |func| {
                    func.parent = parent;
                    opt_func = func.next;
                }
            }
        }
    }
};

pub fn reportDupSym(c: *cy.Chunk, sym: *cy.Sym, name: []const u8, declId: cy.NodeId) !void {
    return c.reportErrorFmt("`{}` has already been declared as a `{}`.", &.{v(name), v(sym.type)}, declId);
}

/// `is_top_decl`: If true, gets appended to the chunk's `syms`. Would be false when adding field and enum members symbols.
fn addUniqueSym(c: *cy.Chunk, mod: *Module, name: []const u8, sym: *cy.Sym, is_top_decl: bool, decl: cy.NodeId) !void {
    errdefer sym.destroy(c.compiler.vm, c.alloc);
    const res = try mod.symMap.getOrPut(c.alloc, name);
    if (res.found_existing) {
        const existing = res.value_ptr.*;
        const sym_mod = sym.getMod() orelse {
            return c.reportErrorFmt("`{}` can not contain symbols.", &.{v(name)}, decl);
        };
        if (existing.type != .placeholder) {
            try reportDupSym(c, existing, name, decl);
        }

        // Replace placeholder sym. Copy module.
        const placeholder = existing.cast(.placeholder);
        sym_mod.* = placeholder.getMod().*;
        sym_mod.updateParentRefs(sym);
        placeholder.resolved = true;
    }
    if (is_top_decl) {
        try c.syms.append(c.alloc, sym);
    }
    res.value_ptr.* = sym;
}

fn addSym(c: *cy.Chunk, mod: *Module, name: []const u8, sym: *cy.Sym) !cy.chunk.SymId {
    const id: cy.chunk.SymId = @intCast(c.syms.items.len);
    try c.syms.append(c.alloc, sym);
    try mod.symMap.putNoClobber(c.alloc, name, sym);
    return id;
}

fn prepareFuncSym(c: *cy.Chunk, parent: *cy.Sym, mod: *Module, name: []const u8, declId: cy.NodeId) !*cy.sym.FuncSym {
    if (mod.symMap.get(name)) |sym| {
        if (sym.type == .func) {
            return sym.cast(.func);
        } else {
            try reportDupSym(c, sym, name, declId);
        }
    }
    // Create func sym.
    const func = try cy.sym.createSym(c.alloc, .func, undefined);
    func.* = .{
        .head = cy.Sym.init(.func, parent, name),
        .numFuncs = 0,
        .firstFuncSig = cy.NullId,
        .first = undefined,
        .last = undefined,
    };
    _ = try addSym(c, mod, name, @ptrCast(func));
    return func;
}

fn createFunc(c: *cy.Chunk, ftype: cy.sym.FuncType, parent: *cy.Sym, sym: ?*cy.sym.FuncSym, nodeId: cy.NodeId, isMethod: bool) !*cy.Func {
    const func = try c.alloc.create(cy.Func);
    func.* = .{
        .type = ftype,
        .funcSigId = cy.NullId,
        .retType = cy.NullId,
        .reqCallTypeCheck = undefined,
        .sym = sym,
        .throws = false,
        .parent = parent,
        .isMethod = isMethod,
        .numParams = undefined,
        .declId = nodeId,
        .next = null,
        .data = undefined,
    };
    return func;
}

fn addFuncToSym(c: *cy.Chunk, sym: *cy.sym.FuncSym, func: *cy.Func) !void {
    if (sym.numFuncs == 0) {
        // First func for sym.
        sym.numFuncs = 1;
        sym.first = func;
        sym.last = func;
    } else {
        // Attach to end.
        sym.last.next = func;
        sym.last = func;
        sym.numFuncs += 1;
    }
    try c.funcs.append(c.alloc, func);
}

pub const ChunkExt = struct {
    
    pub fn createChunkSym(c: *cy.Chunk, name: []const u8) !*cy.sym.Chunk {
        const sym = try cy.sym.createSym(c.alloc, .chunk, .{
            .head = cy.Sym.init(.chunk, null, name),
            .mod = undefined,
        });
        @as(*Module, @ptrCast(&sym.mod)).* = Module.init(c);
        try c.syms.append(c.alloc, @ptrCast(sym));
        return sym;
    }

    pub fn declareModuleAlias(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, importedSym: *cy.Sym, declId: cy.NodeId) !*cy.sym.Import {
        const sym = try cy.sym.createSym(c.alloc, .module_alias, .{
            .head = cy.Sym.init(.import, parent, name),
            .declId = declId,
            .sym = importedSym,
        });
        const mod = parent.getMod().?;
        _ = try addSym(c, mod, name, @ptrCast(sym), declId);
        return sym;
    }

    pub fn reserveUseAlias(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, decl: cy.NodeId) !*cy.sym.UseAlias {
        const sym = try cy.sym.createSym(c.alloc, .use_alias, .{
            .head = cy.Sym.init(.use_alias, parent, name),
            .decl = decl,
            .sym = undefined,
            .resolved = false,
        });
        const mod = parent.getMod().?;
        _ = try addUniqueSym(c, mod, name, @ptrCast(sym), true, decl);
        return sym;
    } 

    pub fn reserveTypeAlias(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, declId: cy.NodeId) !*cy.sym.TypeAlias {
        const sym = try cy.sym.createSym(c.alloc, .typeAlias, .{
            .head = cy.Sym.init(.typeAlias, parent, name),
            .declId = declId,
            .type = cy.NullId,  // Null indicates it needs to be resolved later on.
            .sym = undefined,
            .mod = undefined,
            .resolved = false,
        });
        const mod = parent.getMod().?;
        @as(*Module, @ptrCast(&sym.mod)).* = Module.init(c);
        _ = try addUniqueSym(c, mod, name, @ptrCast(sym), true, declId);
        return sym;
    } 

    pub fn reserveDistinctType(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, decl_id: cy.NodeId, opt_type_id: ?cy.TypeId) !*cy.sym.DistinctType {
        const type_id = opt_type_id orelse try c.sema.pushType();
        const sym = try cy.sym.createSym(c.alloc, .distinct_t, .{
            .head = cy.Sym.init(.distinct_t, parent, name),
            .decl = decl_id,
            .type = type_id,
            .mod = undefined,
            .resolved = false,
        });
        const mod = parent.getMod().?;
        sym.getMod().* = Module.init(mod.chunk);
        _ = try addUniqueSym(c, mod, name, @ptrCast(sym), true, decl_id);

        c.compiler.sema.types.items[type_id] = .{
            .sym = @ptrCast(sym),
            .kind = .null,
            .data = undefined,
        };
        return sym;
    }

    pub fn declareTypeTemplate(c: *cy.Chunk, parent: *cy.Sym, name: []const u8,
        sigId: cy.sema.FuncSigId, params: []const cy.sym.TemplateParam, kind: cy.sym.TemplateKind,
        ctNodes: []const cy.NodeId, declId: cy.NodeId) !void {

        const sym = try cy.sym.createSym(c.alloc, .typeTemplate, .{
            .head = cy.Sym.init(.typeTemplate, parent, name),
            .kind = kind,
            .declId = declId,
            .params = params,
            .sigId = sigId,
            .variants = .{},
            .variantCache = .{},
            .ctNodes = ctNodes,
        });
        const mod = parent.getMod().?;
        _ = try addUniqueSym(c, mod, name, @ptrCast(sym), true, declId);
        return sym;
    }

    pub fn reserveEnumType(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, isChoiceType: bool, declId: cy.NodeId) !*cy.sym.EnumType {
        const typeId = try c.sema.pushType();
        const sym = try cy.sym.createSym(c.alloc, .enum_t, .{
            .head = cy.Sym.init(.enum_t, parent, name),
            .type = typeId,
            .decl = declId,
            .members = undefined,
            .numMembers = 0,
            .isChoiceType = isChoiceType,
            .variantId = cy.NullId,
            .mod = undefined,
        });
        const mod = parent.getMod().?;
        @as(*Module, @ptrCast(&sym.mod)).* = Module.init(mod.chunk);
        _ = try addUniqueSym(c, mod, name, @ptrCast(sym), true, declId);
        c.compiler.sema.types.items[typeId] = .{
            .sym = @ptrCast(sym),
            .kind = if (isChoiceType) .choice else .@"enum",
            .data = undefined,
        };
        return sym;
    }

    pub fn declareEnumMember(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, typeId: types.TypeId, is_choice_type: bool, val: u32, payloadType: cy.TypeId, declId: cy.NodeId) !*cy.sym.EnumMember {
        const sym = try cy.sym.createSym(c.alloc, .enumMember, .{
            .head = cy.Sym.init(.enumMember, parent, name),
            .type = typeId,
            .val = val,
            .payloadType = payloadType,
            .is_choice_type = is_choice_type,
        });
        const mod = parent.getMod().?;
        _ = try addUniqueSym(c, mod, name, @ptrCast(sym), false, declId);
        return sym;
    }

    pub fn reserveHostVar(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, declId: cy.NodeId) !*cy.sym.HostVar {
        const sym = try cy.sym.createSym(c.alloc, .hostVar, .{
            .head = cy.Sym.init(.hostVar, parent, name),
            .retainedIdx = cy.NullU16,
            .val = cy.Value.Void,
            .declId = declId,
            .type = cy.NullId,
        });

        const mod = parent.getMod().?;
        _ = try addUniqueSym(c, mod, name, @ptrCast(sym), true, declId);
        return sym;
    }

    pub fn resolveHostVar(c: *cy.Chunk, sym: *cy.sym.HostVar, type_id: cy.TypeId, value: cy.Value) !void {
        const mod = sym.head.parent.?.getMod().?;
        if (value.isPointer()) {
            const retainIdx = mod.retainedVars.items.len;
            cy.arc.retain(c.compiler.vm, value);
            try mod.retainedVars.append(c.alloc, @ptrCast(sym));
            sym.retainedIdx = @intCast(retainIdx);
        }
        sym.type = type_id;
        sym.val = value;
    } 

    pub fn reserveUserVar(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, declId: cy.NodeId) !*cy.sym.UserVar {
        const sym = try cy.sym.createSym(c.alloc, .userVar, .{
            .head = cy.Sym.init(.userVar, parent, name),
            .declId = declId,
            .type = cy.NullId,
        });
        const mod = parent.getMod().?;
        _ = try addUniqueSym(c, mod, name, @ptrCast(sym), true, declId);
        return sym;
    }

    pub fn resolveUserVar(c: *cy.Chunk, sym: *cy.sym.UserVar, type_id: cy.TypeId) void {
        _ = c;
        sym.type = type_id;
    }

    /// Can assume sym name is unique.
    pub fn addPlaceholder(c: *cy.Chunk, parent: *cy.Sym, name: []const u8) !*cy.sym.Placeholder {
        const sym = try cy.sym.createSym(c.alloc, .placeholder, .{
            .head = cy.Sym.init(.placeholder, parent, name),
            .mod = undefined,
            .resolved = false,
        });
        const mod = parent.getMod().?;
        @as(*Module, @ptrCast(&sym.mod)).* = Module.init(mod.chunk);
        try mod.chunk.syms.append(c.alloc, @ptrCast(sym));
        try mod.symMap.putNoClobber(c.alloc, name, @ptrCast(sym));
        return sym;
    }

    pub fn declareField(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, idx: u32, typeId: types.TypeId, declId: cy.NodeId) !*cy.sym.Field {
        const sym = try cy.sym.createSym(c.alloc, .field, .{
            .head = cy.Sym.init(.field, parent, name),
            .idx = idx,
            .type = typeId,
        });
        const mod = parent.getMod().?;
        _ = try addUniqueSym(c, mod, name, @ptrCast(sym), false, declId);
        return sym;
    }

    pub fn declareObjectVariantType(c: *cy.Chunk, parent: *cy.sym.TypeTemplate, variantId: u32) !*cy.sym.ObjectType {
        const name = parent.head.name();
        const typeId = try c.sema.pushType();
        const sym = try cy.sym.createSym(c.alloc, .object_t, .{
            .head = cy.Sym.init(.object_t, @ptrCast(parent), name),
            .declId = parent.declId,
            .type = typeId,
            .fields = undefined,
            .variantId = variantId,
            .rt_size = cy.NullId,
            .numFields = cy.NullId,
            .mod = undefined,
        });

        const mod = parent.head.parent.?.getMod().?;
        @as(*Module, @ptrCast(&sym.mod)).* = Module.init(mod.chunk);
        c.compiler.sema.types.items[typeId] = .{
            .sym = @ptrCast(sym),
            .kind = .object,
            .data = .{ .object = .{
                .numFields = cy.NullU16,
            }},
        };
        return sym;
    }

    pub fn declareEnumVariantType(c: *cy.Chunk, parent: *cy.sym.TypeTemplate, isChoiceType: bool, variantId: u32) !*cy.sym.EnumType {
        const name = parent.head.name();
        const typeId = try c.sema.pushType();
        const sym = try cy.sym.createSym(c.alloc, .enum_t, .{
            .head = cy.Sym.init(.enum_t, @ptrCast(parent), name),
            .type = typeId,
            .members = undefined,
            .numMembers = 0,
            .isChoiceType = isChoiceType,
            .variantId = variantId,
            .mod = undefined,
        });

        const mod = parent.head.parent.?.getMod().?;
        @as(*Module, @ptrCast(&sym.mod)).* = Module.init(mod.chunk);

        var kind: cy.types.TypeKind = undefined;
        if (parent == c.sema.option_tmpl) {
            kind = .option;
        } else if (isChoiceType) {
            kind = .choice;
        } else {
            kind = .@"enum";
        }
        c.compiler.sema.types.items[typeId] = .{
            .sym = @ptrCast(sym),
            .kind = kind,
            .data = undefined,
        };
        return sym;
    }

    pub fn reserveObjectType(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, declId: cy.NodeId, opt_type_id: ?cy.TypeId) !*cy.sym.ObjectType {
        const mod = parent.getMod().?;
        const type_id = opt_type_id orelse try c.sema.pushType();
        const sym = try cy.sym.createSym(c.alloc, .object_t,
            cy.sym.ObjectType.init(parent, mod.chunk, name, declId, type_id)
        );
        _ = try addUniqueSym(c, mod, name, @ptrCast(sym), true, declId);

        c.compiler.sema.types.items[type_id] = .{
            .sym = @ptrCast(sym),
            .kind = .object,
            .data = .{ .object = .{
                .numFields = cy.NullU16,
            }},
        };
        return sym;
    }

    pub fn reserveStructType(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, declId: cy.NodeId, opt_type_id: ?cy.TypeId) !*cy.sym.ObjectType {
        const mod = parent.getMod().?;

        const typeId = opt_type_id orelse try c.sema.pushType();
        const sym = try cy.sym.createSym(c.alloc, .struct_t, .{
            .head = cy.Sym.init(.struct_t, parent, name),
            .declId = declId,
            .type = typeId,
            .fields = undefined,
            .variantId = cy.NullId,
            .numFields = cy.NullId,
            .rt_size = cy.NullId,
            .mod = undefined,
        });

        @as(*Module, @ptrCast(&sym.mod)).* = Module.init(mod.chunk);

        _ = try addUniqueSym(c, mod, name, @ptrCast(sym), true, declId);
        c.compiler.sema.types.items[typeId] = .{
            .sym = @ptrCast(sym),
            .kind = .@"struct",
            .data = .{ .@"struct" = .{
                .numFields = cy.NullU16,
            }},
        };
        return sym;
    }

    /// Declared at the chunk level.
    /// TODO: Hash object members to avoid duplicate types at the chunk level.
    ///       Actually, it might not even be worth it since it means the usual case requires
    ///       iterating the members to create the hash.
    pub fn reserveUnnamedObjectType(c: *cy.Chunk, parent: *cy.Sym, declId: cy.NodeId) !*cy.sym.ObjectType {
        const mod = parent.getMod().?;
        var buf: [16]u8 = undefined;
        const name = mod.chunk.getNextUniqUnnamedIdent(&buf);

        const nameDup = try c.alloc.dupe(u8, name);
        try c.parser.ast.strs.append(c.alloc, nameDup);

        const typeId = try c.sema.pushType();
        const sym = try cy.sym.createSym(c.alloc, .object_t, .{
            .head = cy.Sym.init(.object_t, parent, nameDup),
            .declId = declId,
            .type = typeId,
            .fields = undefined,
            .variantId = cy.NullId,
            .numFields = cy.NullId,
            .rt_size = cy.NullId,
            .mod = undefined,
        });

        @as(*Module, @ptrCast(&sym.mod)).* = Module.init(mod.chunk);

        const symId = try addSym(c, mod, name, @ptrCast(sym));

        // Update node's `name` so it can do a lookup during resolving.
        const node = mod.chunk.ast.node(declId);
        mod.chunk.parser.ast.nodePtr(node.data.objectDecl.header).data.objectHeader.name = @intCast(symId);

        c.compiler.sema.types.items[typeId] = .{
            .sym = @ptrCast(sym),
            .kind = .object,
            .data = .{ .object = .{
                .numFields = cy.NullU16,
            }},
        };
        return sym;
    }

    pub fn declareUnnamedStructType(c: *cy.Chunk, parent: *cy.Sym, declId: cy.NodeId) !*cy.sym.ObjectType {
        const mod = parent.getMod().?;
        var buf: [16]u8 = undefined;
        const name = mod.chunk.getNextUniqUnnamedIdent(&buf);

        const nameDup = try c.alloc.dupe(u8, name);
        try c.parser.ast.strs.append(c.alloc, nameDup);

        const typeId = try c.sema.pushType();
        const sym = try cy.sym.createSym(c.alloc, .struct_t, .{
            .head = cy.Sym.init(.struct_t, parent, nameDup),
            .declId = declId,
            .type = typeId,
            .fields = undefined,
            .variantId = cy.NullId,
            .numFields = cy.NullId,
            .rt_size = cy.NullId,
            .mod = undefined,
        });

        @as(*Module, @ptrCast(&sym.mod)).* = Module.init(mod.chunk);

        const symId = try addSym(c, mod, name, @ptrCast(sym));

        // Update node's `name` so it can do a lookup during resolving.
        const node = mod.chunk.ast.node(declId);
        mod.chunk.parser.ast.nodePtr(node.data.objectDecl.header).data.objectHeader.name = @intCast(symId);

        c.compiler.sema.types.items[typeId] = .{
            .sym = @ptrCast(sym),
            .kind = .@"struct",
            .data = .{ .@"struct" = .{
                .numFields = cy.NullU16,
            }},
        };
        return sym;
    }

    pub fn declareCustomObjectType(
        c: *cy.Chunk, parent: *cy.Sym, name: []const u8, declId: cy.NodeId,
        getChildrenFn: cc.ObjectGetChildrenFn, finalizerFn: cc.ObjectFinalizerFn, opt_type_id: ?cy.TypeId,
    ) !*cy.sym.CustomObjectType {
        const mod = parent.getMod().?;

        const type_id = opt_type_id orelse try c.sema.pushType();

        const sym = try cy.sym.createSym(c.alloc, .custom_object_t, .{
            .head = cy.Sym.init(.custom_object_t, parent, name),
            .declId = declId,
            .type = type_id,
            .getChildrenFn = getChildrenFn,
            .finalizerFn = finalizerFn,
            .mod = undefined,
        });
        @as(*Module, @ptrCast(&sym.mod)).* = Module.init(mod.chunk);

        _ = try addUniqueSym(c, mod, name, @ptrCast(sym), true, declId);
        c.compiler.sema.types.items[type_id] = .{
            .sym = @ptrCast(sym),
            .kind = .custom_object,
            .data = .{ .custom_object = .{
                .getChildrenFn = getChildrenFn,
                .finalizerFn = finalizerFn,
            }},
        };
        return sym;
    }

    pub fn declareFloatType(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, bits: u8, opt_type_id: ?types.TypeId, node_id: cy.NodeId) !*cy.sym.FloatType {
        const mod = parent.getMod().?;

        const type_id = opt_type_id orelse try c.sema.pushType();
        const sym = try cy.sym.createSym(c.alloc, .float_t, .{
            .head = cy.Sym.init(.float_t, parent, name),
            .type = type_id,
            .mod = undefined,
            .bits = bits,
        });
        @as(*Module, @ptrCast(&sym.mod)).* = Module.init(mod.chunk);

        _ = try addUniqueSym(c, mod, name, @ptrCast(sym), true, node_id);
        c.compiler.sema.types.items[type_id] = .{
            .sym = @ptrCast(sym),
            .kind = .float,
            .data = undefined,
        };
        return sym;
    }

    pub fn declareIntType(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, bits: u8, opt_type_id: ?types.TypeId, node_id: cy.NodeId) !*cy.sym.IntType {
        const mod = parent.getMod().?;

        const type_id = opt_type_id orelse try c.sema.pushType();
        const sym = try cy.sym.createSym(c.alloc, .int_t, .{
            .head = cy.Sym.init(.int_t, parent, name),
            .type = type_id,
            .mod = undefined,
            .bits = bits,
        });
        @as(*Module, @ptrCast(&sym.mod)).* = Module.init(mod.chunk);
        _ = try addUniqueSym(c, mod, name, @ptrCast(sym), true, node_id);
        c.compiler.sema.types.items[type_id] = .{
            .sym = @ptrCast(sym),
            .kind = .int,
            .data = undefined,
        };
        return sym;
    }

    pub fn declareBoolType(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, opt_type_id: ?types.TypeId, node_id: cy.NodeId) !*cy.sym.BoolType {
        const mod = parent.getMod().?;

        const type_id = opt_type_id orelse try c.sema.pushType();
        const sym = try cy.sym.createSym(c.alloc, .bool_t, .{
            .head = cy.Sym.init(.bool_t, parent, name),
            .type = type_id,
            .mod = undefined,
        });
        @as(*Module, @ptrCast(&sym.mod)).* = Module.init(mod.chunk);
        _ = try addUniqueSym(c, mod, name, @ptrCast(sym), true, node_id);

        c.compiler.sema.types.items[type_id] = .{
            .sym = @ptrCast(sym),
            .kind = .bool,
            .data = undefined,
        };
        return sym;
    }

    pub fn addUserLambda(c: *cy.Chunk, parent: *cy.Sym, funcSigId: sema.FuncSigId, declId: cy.NodeId) !*cy.Func {
        const func = try createFunc(c, .userLambda, parent, null, declId, false);
        const func_sig = c.compiler.sema.getFuncSig(funcSigId);
        func.funcSigId = funcSigId;
        func.retType = func_sig.getRetType();
        func.reqCallTypeCheck = func_sig.reqCallTypeCheck;
        func.numParams = @intCast(func_sig.paramLen);
        try c.funcs.append(c.alloc, func);
        return func;
    }

    pub fn reserveHostFunc(
        c: *cy.Chunk, parent: *cy.Sym, name: []const u8, node: cy.NodeId, is_method: bool,
    ) !*cy.Func {
        const mod = parent.getMod().?;
        const sym = try prepareFuncSym(c, parent, mod, name, node);
        const func = try createFunc(c, .hostFunc, parent, sym, node, is_method);
        func.data = .{ .hostFunc = .{
            .ptr = undefined,
        }};
        try addFuncToSym(c, sym, func);
        return func;
    }

    pub fn resolveHostFunc(c: *cy.Chunk, func: *cy.Func, func_sig: sema.FuncSigId, func_ptr: cy.ZHostFuncFn) !void {
        try resolveFunc(c, func, func_sig);
        func.data.hostFunc.ptr = func_ptr;
    }

    pub fn reserveUserFunc(
        c: *cy.Chunk, parent: *cy.Sym, name: []const u8, node: cy.NodeId, is_method: bool,
    ) !*cy.Func {
        const mod = parent.getMod().?;
        const sym = try prepareFuncSym(c, parent, mod, name, node);
        const func = try createFunc(c, .userFunc, parent, sym, node, is_method);
        try addFuncToSym(c, sym, func);
        return func;
    }

    pub fn resolveUserFunc(c: *cy.Chunk, func: *cy.Func, func_sig: sema.FuncSigId) !void {
        try resolveFunc(c, func, func_sig);
    }

    fn resolveFunc(c: *cy.Chunk, func: *cy.Func, func_sig_id: sema.FuncSigId) !void {
        const mod = func.parent.getMod().?;
        const sym = func.sym.?;
        if (!mod.isFuncUnique(sym, func_sig_id)) {
            return c.reportErrorFmt("`{}` has already been declared with the same function signature.", &.{v(func.name())}, func.declId);
        }
        if (sym.first == func) {
            sym.firstFuncSig = func_sig_id;
        }
        if (sym.numFuncs > 1) {
            const key = ModFuncKey.initModFuncKey(sym, func_sig_id);
            try mod.overloadedFuncMap.putNoClobber(c.alloc, key, func);
        }
        const func_sig = c.compiler.sema.getFuncSig(func_sig_id);
        func.funcSigId = func_sig_id;
        func.retType = func_sig.getRetType();
        func.reqCallTypeCheck = func_sig.reqCallTypeCheck;
        func.numParams = @intCast(func_sig.paramLen);
    }

    pub fn getOptResolvedSym(_: *cy.Chunk, modSym: *cy.Sym, name: []const u8) !?*cy.Sym {
        const mod = modSym.getMod() orelse {
            return null;
        };
        const sym = mod.getSym(name) orelse {
            return null;
        };
        switch (sym.type) {
            .module_alias => {
                return sym.cast(.module_alias).sym;
            },
            .use_alias => {
                const alias = sym.cast(.use_alias);
                if (!alias.resolved) {
                    try sema.resolveUseAlias(mod.chunk, alias);
                }
                return alias.sym;
            },
            .typeAlias => {
                const alias = sym.cast(.typeAlias);
                if (!alias.resolved) {
                    const src_chunk = alias.getMod().chunk;
                    try sema.resolveTypeAlias(src_chunk, alias);
                }
                return alias.sym;
            },
            else => {
                return sym;
            }
        }
    }

    pub fn mustFindSym(c: *cy.Chunk, modSym: *cy.Sym, name: []const u8, nodeId: cy.NodeId) !*cy.Sym {
        const mod = modSym.getMod() orelse {
            const symPath = try modSym.formatAbsPath(&cy.tempBuf);
            return c.reportErrorFmt("Can not access `{}` from parent `{}`. Parent is not a module.", &.{v(name), v(symPath)}, nodeId);
        };
        const sym = mod.getSym(name) orelse {
            const symPath = try modSym.resolved().formatAbsPath(&cy.tempBuf);
            return c.reportErrorFmt("Can not find the symbol `{}` in `{}`.", &.{v(name), v(symPath)}, nodeId);
        };
        return sym;
    }

    pub fn getResolvedDistinctSym(
        c: *cy.Chunk, modSym: *cy.Sym, name: []const u8, nodeId: cy.NodeId, comptime must: bool,
    ) anyerror!(if (must) *cy.Sym else ?*cy.Sym) {
        const mod = modSym.getMod() orelse {
            const symPath = try modSym.formatAbsPath(&cy.tempBuf);
            return c.reportErrorFmt("Can not access `{}` from parent `{}`. Parent is not a module.", &.{v(name), v(symPath)}, nodeId);
        };
        const sym = mod.symMap.get(name) orelse {
            if (must) {
                return c.reportErrorFmt(
                    \\Can not find the symbol `{}` in `{}`.
                , &.{v(name), v(modSym.name())}, nodeId);
            } else {
                return null;
            }
        };
        switch (sym.type) {
            .userVar,
            .hostVar,
            .struct_t,
            .object_t,
            .custom_object_t,
            .enum_t,
            .chunk,
            .bool_t,
            .int_t,
            .float_t,
            .distinct_t,
            .typeTemplate,
            .placeholder,
            .enumMember => {
                return sym;
            },
            .module_alias => {
                return sym.cast(.module_alias).sym;
            },
            .use_alias => {
                const alias = sym.cast(.use_alias);
                if (!alias.resolved) {
                    try sema.resolveUseAlias(mod.chunk, alias);
                }
                return alias.sym;
            },
            .typeAlias => {
                const alias = sym.cast(.typeAlias);
                if (!alias.resolved) {
                    const src_chunk = alias.getMod().chunk;
                    try sema.resolveTypeAlias(src_chunk, alias);
                }
                return alias.sym;
            },
            .func => {
                const func = sym.cast(.func);
                if (func.numFuncs > 1) {
                    if (must) {
                        // More than one func for sym.
                        return c.reportErrorFmt("Symbol `{}` is ambiguous. There are multiple functions with the same name.", &.{v(name)}, nodeId);
                    } else {
                        return null;
                    }
                } else {
                    return sym;
                }
            },
            .field => {
                if (must) {
                    return c.reportErrorFmt("Can not reference `{}` as a symbol.", &.{v(name)}, nodeId);
                } else {
                    return null;
                }
            },
            .null => cy.unexpected(),
        }
    }

    pub fn getDistinctSym(
        c: *cy.Chunk, modSym: *cy.Sym, name: []const u8, nodeId: cy.NodeId, comptime must: bool,
    ) anyerror!(if (must) *cy.Sym else ?*cy.Sym) {
        const mod = modSym.getMod() orelse {
            const symPath = try modSym.formatAbsPath(&cy.tempBuf);
            return c.reportErrorFmt("Can not access `{}` from parent `{}`. Parent is not a module.", &.{v(name), v(symPath)}, nodeId);
        };
        const sym = mod.symMap.get(name) orelse {
            if (must) {
                return c.reportErrorFmt(
                    \\Can not find the symbol `{}` in `{}`.
                , &.{v(name), v(modSym.name())}, nodeId);
            } else {
                return null;
            }
        };

        switch (sym.type) {
            .userVar,
            .hostVar,
            .struct_t,
            .object_t,
            .custom_object_t,
            .enum_t,
            .chunk,
            .bool_t,
            .int_t,
            .float_t,
            .distinct_t,
            .typeTemplate,
            .placeholder,
            .use_alias,
            .module_alias,
            .typeAlias,
            .enumMember => {
                return sym;
            },
            .func => {
                const func = sym.cast(.func);
                if (func.numFuncs > 1) {
                    if (must) {
                        // More than one func for sym.
                        return c.reportErrorFmt("Symbol `{}` is ambiguous. There are multiple functions with the same name.", &.{v(name)}, nodeId);
                    } else {
                        return null;
                    }
                } else {
                    return sym;
                }
            },
            .field => {
                if (must) {
                    return c.reportErrorFmt("Can not reference `{}` as a symbol.", &.{v(name)}, nodeId);
                } else {
                    return null;
                }
            },
            .null => cy.unexpected(),
        }
    }
};

test "module internals" {
    try t.eq(@offsetOf(Module, "symMap"), @offsetOf(vmc.Module, "symMap"));
    try t.eq(@offsetOf(Module, "overloadedFuncMap"), @offsetOf(vmc.Module, "overloadedFuncMap"));
    try t.eq(@offsetOf(Module, "retainedVars"), @offsetOf(vmc.Module, "retainedVars"));
    try t.eq(@offsetOf(Module, "chunk"), @offsetOf(vmc.Module, "chunk"));
}