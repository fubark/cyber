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

pub const ModFuncKey = cy.hash.KeyU64;

pub const ModuleSymId = u32;
pub const ModuleFuncId = u32;

/// A module contains symbols declared in Cyber source code and through the embedded API.
/// Overloaded functions are linked together so that type signatures can be checked sequentially.
pub const Module = struct {
    /// Syms.
    syms: std.ArrayListUnmanaged(*cy.Sym),

    /// Name to sym.
    symMap: std.StringHashMapUnmanaged(ModuleSymId),

    /// Functions. Includes lambdas which are not linked from a named sym.
    funcs: std.ArrayListUnmanaged(*cy.Func),

    /// Sym+funcSig to func sym. This is used to check whether an overloaded function is unique.
    /// This should only be populated for a function sym once it's overloaded.
    overloadedFuncMap: std.HashMapUnmanaged(ModFuncKey, *cy.Func, cy.hash.KeyU64Context, 80),

    /// Tracks which binded vars are retained.
    retainedVars: std.ArrayListUnmanaged(*cy.Sym),

    /// Chunk that contains the module declaration.
    chunk: *cy.Chunk,

    pub fn init(chunk: *cy.Chunk) Module {
        return .{
            .syms = .{},
            .symMap = .{},
            .funcs = .{},
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

        for (self.syms.items) |sym| {
            sym.deinitRetained(vm);
        }
    }

    pub fn deinit(self: *Module, alloc: std.mem.Allocator) void {
        self.retainedVars.deinit(alloc);

        for (self.syms.items) |sym| {
            sym.destroy(self.chunk.vm, alloc);
        }
        self.syms.deinit(alloc);

        for (self.funcs.items) |func| {
            alloc.destroy(func);
        }
        self.funcs.deinit(alloc);

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

    pub fn getFunc(m: *const Module, id: ModuleFuncId) *cy.Func {
        return m.funcs.items[id];
    }

    pub fn getSymById(m: *const Module, id: ModuleSymId) *cy.Sym {
        return m.syms.items[id];
    }

    pub fn getSym(m: *Module, name: []const u8) ?*cy.Sym {
        const symId = m.symMap.get(name) orelse return null;
        return m.syms.items[symId];
    }

    fn isFuncUnique(m: *Module, func: *cy.sym.FuncSym, symId: ModuleSymId, funcSigId: sema.FuncSigId) bool {
        if (func.firstFuncSig == funcSigId) {
            return false;
        }
        if (func.numFuncs == 1) {
            return true;
        }
        
        const key = ModFuncKey.initModFuncKey(symId, funcSigId);
        return !m.overloadedFuncMap.contains(key);
    }
};

fn checkUniqueSym(c: *cy.Chunk, mod: *Module, name: []const u8, declId: cy.NodeId) !void {
    if (mod.symMap.get(name)) |symId| {
        const sym = mod.syms.items[symId];
        return reportDupSym(c, sym, name, declId);
    }
}

fn reportDupSym(c: *cy.Chunk, sym: *cy.Sym, name: []const u8, declId: cy.NodeId) !void {
    return c.reportErrorAt("`{}` has already been declared as a `{}`.", &.{v(name), v(sym.type)}, declId);
}

fn addSym(c: *cy.Chunk, mod: *Module, name: []const u8, sym: *cy.Sym) !ModuleSymId {
    const id = mod.syms.items.len;
    try mod.syms.append(c.alloc, sym);
    try mod.symMap.putNoClobber(c.alloc, name, @intCast(id));
    return @intCast(id);
}

const PrepareFuncSymResult = struct {
    sym: *cy.sym.FuncSym,
    symId: ModuleSymId,
};

fn prepareFuncSym(c: *cy.Chunk, parent: *cy.Sym, mod: *Module, name: []const u8, funcSigId: sema.FuncSigId, declId: cy.NodeId) !PrepareFuncSymResult {
    if (mod.symMap.get(name)) |symId| {
        const sym = mod.syms.items[symId];
        if (sym.type == .func) {
            const func = sym.cast(.func);
            if (!mod.isFuncUnique(func, symId, funcSigId)) {
                return c.reportErrorAt("`{}` has already been declared with the same function signature.", &.{v(name)}, declId);
            }
            return .{ .sym = func, .symId = symId };
        } else {
            try reportDupSym(c, sym, name, declId);
        }
    }
    // Create func sym.
    const func = try cy.sym.createSym(c.alloc, .func, undefined);
    func.head = cy.Sym.init(.func, parent, name);
    func.numFuncs = 0;
    const id = try addSym(c, mod, name, @ptrCast(func));
    return .{ .sym = func, .symId = id };
}

fn createFunc(c: *cy.Chunk, ftype: cy.sym.FuncType, parent: *cy.Sym, sym: ?*cy.sym.FuncSym, funcSigId: sema.FuncSigId, nodeId: cy.NodeId, isMethod: bool) !*cy.Func {
    const funcSig = c.compiler.sema.getFuncSig(funcSigId);
    const func = try c.alloc.create(cy.Func);
    func.* = .{
        .type = ftype,
        .funcSigId = funcSigId,
        .retType = funcSig.getRetType(),
        .reqCallTypeCheck = funcSig.reqCallTypeCheck,
        .sym = sym,
        .parent = parent,
        .isMethod = isMethod,
        .numParams = @intCast(funcSig.paramLen),
        .declId = nodeId,
        .next = null,
        .data = undefined,
    };
    return func;
}

fn addFuncToSym(c: *cy.Chunk, mod: *Module, symId: ModuleSymId, sym: *cy.sym.FuncSym, func: *cy.Func) !void {
    if (sym.numFuncs == 0) {
        // First func for sym.
        sym.numFuncs = 1;
        sym.first = func;
        sym.last = func;
        sym.firstFuncSig = func.funcSigId;
    } else {
        // Attach to end.
        sym.last.next = func;
        sym.last = func;
        sym.numFuncs += 1;
    }

    if (sym.numFuncs > 1) {
        if (sym.numFuncs == 2) {
            // Insert first func into overloaded map.
            const key = ModFuncKey.initModFuncKey(symId, sym.firstFuncSig);
            try mod.overloadedFuncMap.putNoClobber(c.alloc, key, sym.first);
        }
        // Insert new func into overloaded map.
        const key = ModFuncKey.initModFuncKey(symId, func.funcSigId);
        try mod.overloadedFuncMap.putNoClobber(c.alloc, key, func);
    }

    try mod.funcs.append(c.alloc, func);
}

pub const ChunkExt = struct {
    
    pub fn createChunkSym(c: *cy.Chunk, name: []const u8) !*cy.sym.Chunk {
        const sym = try cy.sym.createSym(c.alloc, .chunk, .{
            .head = cy.Sym.init(.chunk, null, name),
            .mod = undefined,
        });
        @as(*Module, @ptrCast(&sym.mod)).* = Module.init(c);
        try c.modSyms.append(c.alloc, @ptrCast(sym));
        return sym;
    }

    pub fn declareImport(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, importedSym: *cy.Sym, declId: cy.NodeId) !*cy.sym.Import {
        const mod = parent.getMod().?;
        try checkUniqueSym(c, mod, name, declId);

        const sym = try cy.sym.createSym(c.alloc, .import, .{
            .head = cy.Sym.init(.import, parent, name),
            .declId = declId,
            .sym = importedSym,
        });
        _ = try addSym(c, mod, name, @ptrCast(sym));
        return sym;
    }

    pub fn declareTypeAlias(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, declId: cy.NodeId) !void {
        const mod = parent.getMod().?;
        try checkUniqueSym(c, mod, name, declId);

        const sym = try cy.sym.createSym(c.alloc, .typeAlias, .{
            .head = cy.Sym.init(.typeAlias, parent, name),
            .declId = declId,
            .type = cy.NullId,  // Null indicates it needs to be resolved later on.
            .sym = undefined,
        });
        _ = try addSym(c, mod, name, @ptrCast(sym));
    }

    pub fn declareTypeTemplate(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, sigId: cy.sema.FuncSigId, params: []const cy.sym.TemplateParam, ctNodes: []const cy.NodeId, declId: cy.NodeId) !void {
        const mod = parent.getMod().?;
        try checkUniqueSym(c, mod, name, declId);

        const sym = try cy.sym.createSym(c.alloc, .typeTemplate, .{
            .head = cy.Sym.init(.typeTemplate, parent, name),
            .declId = declId,
            .params = params,
            .sigId = sigId,
            .variants = .{},
            .variantCache = .{},
            .ctNodes = ctNodes,
        });
        _ = try addSym(c, mod, name, @ptrCast(sym));
    }

    pub fn declareEnumType(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, isChoiceType: bool, declId: cy.NodeId) !*cy.sym.EnumType {
        const mod = parent.getMod().?;
        try checkUniqueSym(c, mod, name, declId);

        const typeId = try c.sema.pushType();
        const sym = try cy.sym.createSym(c.alloc, .enumType, .{
            .head = cy.Sym.init(.enumType, parent, name),
            .type = typeId,
            .members = @as([*]const ModuleSymId, @ptrCast(@alignCast(&.{}))),
            .numMembers = 0,
            .isChoiceType = isChoiceType,
            .mod = undefined,
        });
        @as(*Module, @ptrCast(&sym.mod)).* = Module.init(mod.chunk);
        try mod.chunk.modSyms.append(c.alloc, @ptrCast(sym));

        _ = try addSym(c, mod, name, @ptrCast(sym));
        c.compiler.sema.types.items[typeId] = .{
            .sym = @ptrCast(sym),
            .kind = if (isChoiceType) .choice else .@"enum",
            .data = undefined,
        };
        return sym;
    }

    pub fn declareEnumMember(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, typeId: types.TypeId, val: u32, payloadType: cy.TypeId, declId: cy.NodeId) !ModuleSymId {
        const mod = parent.getMod().?;
        try checkUniqueSym(c, mod, name, declId);

        const sym = try cy.sym.createSym(c.alloc, .enumMember, .{
            .head = cy.Sym.init(.enumMember, parent, name),
            .type = typeId,
            .val = val,
            .payloadType = payloadType,
        });
        const id = try addSym(c, mod, name, @ptrCast(sym));
        return id;
    }

    /// Once declared, the value is retained.
    pub fn declareHostVar(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, declId: cy.NodeId, typeId: types.TypeId, value: cy.Value) !*cy.sym.HostVar {
        const mod = parent.getMod().?;
        try checkUniqueSym(c, mod, name, declId);

        var retainIdx: u16 = cy.NullU16;
        if (value.isPointer()) {
            retainIdx = @intCast(mod.retainedVars.items.len);
            cy.arc.retain(c.compiler.vm, value);
        }

        const sym = try cy.sym.createSym(c.alloc, .hostVar, .{
            .head = cy.Sym.init(.hostVar, parent, name),
            .retainedIdx = retainIdx,
            .val = value,
            .declId = declId,
            .type = typeId,
        });

        if (value.isPointer()) {
            try mod.retainedVars.append(c.alloc, @ptrCast(sym));
        }
        _ = try addSym(c, mod, name, @ptrCast(sym));
        return sym;
    }

    pub fn declareUserVar(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, declId: cy.NodeId, typeId: types.TypeId) !*cy.sym.UserVar {
        const mod = parent.getMod().?;
        try checkUniqueSym(c, mod, name, declId);
        const sym = try cy.sym.createSym(c.alloc, .userVar, .{
            .head = cy.Sym.init(.userVar, parent, name),
            .declId = declId,
            .type = typeId,
        });
        _ = try addSym(c, mod, name, @ptrCast(sym));
        return sym;
    }

    pub fn declareField(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, idx: u32, typeId: types.TypeId, declId: cy.NodeId) !ModuleSymId {
        const mod = parent.getMod().?;
        try checkUniqueSym(c, mod, name, declId);

        const sym = try cy.sym.createSym(c.alloc, .field, .{
            .head = cy.Sym.init(.field, parent, name),
            .idx = idx,
            .type = typeId,
        });
        const id = try addSym(c, mod, name, @ptrCast(sym));
        return id;
    }

    pub fn declareObjectVariantType(c: *cy.Chunk, parent: *cy.sym.TypeTemplate, variantId: u32) !*cy.sym.ObjectType {
        const mod = parent.head.parent.?.getMod().?;

        const name = parent.head.name();
        const typeId = try c.sema.pushType();
        const sym = try cy.sym.createSym(c.alloc, .object, .{
            .head = cy.Sym.init(.object, @ptrCast(parent), name),
            .declId = parent.declId,
            .type = typeId,
            .fields = undefined,
            .variantId = variantId,
            .numFields = 0,
            .mod = undefined,
        });

        @as(*Module, @ptrCast(&sym.mod)).* = Module.init(mod.chunk);
        try mod.chunk.modSyms.append(c.alloc, @ptrCast(sym));

        c.compiler.sema.types.items[typeId] = .{
            .sym = @ptrCast(sym),
            .kind = .object,
            .data = .{ .object = .{
                .numFields = 0,
            }},
        };
        return sym;
    }

    pub fn declareObjectType(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, declId: cy.NodeId) !*cy.sym.ObjectType {
        const mod = parent.getMod().?;
        try checkUniqueSym(c, mod, name, declId);

        const typeId = try c.sema.pushType();
        const sym = try cy.sym.createSym(c.alloc, .object, .{
            .head = cy.Sym.init(.object, parent, name),
            .declId = declId,
            .type = typeId,
            .fields = undefined,
            .variantId = cy.NullId,
            .numFields = 0,
            .mod = undefined,
        });

        @as(*Module, @ptrCast(&sym.mod)).* = Module.init(mod.chunk);
        try mod.chunk.modSyms.append(c.alloc, @ptrCast(sym));

        _ = try addSym(c, mod, name, @ptrCast(sym));
        c.compiler.sema.types.items[typeId] = .{
            .sym = @ptrCast(sym),
            .kind = .object,
            .data = .{ .object = .{
                .numFields = 0,
            }},
        };
        return sym;
    }

    /// Declared at the chunk level.
    /// TODO: Hash object members to avoid duplicate types at the chunk level.
    ///       Actually, it might not even be worth it since it means the usual case requires
    ///       iterating the members to create the hash.
    pub fn declareUnnamedObjectType(c: *cy.Chunk, parent: *cy.Sym, declId: cy.NodeId) !*cy.sym.ObjectType {
        const mod = parent.getMod().?;
        var buf: [16]u8 = undefined;
        const name = mod.chunk.getNextUniqUnnamedIdent(&buf);

        const nameDup = try c.alloc.dupe(u8, name);
        try c.parser.ast.strs.append(c.alloc, nameDup);

        const typeId = try c.sema.pushType();
        const sym = try cy.sym.createSym(c.alloc, .object, .{
            .head = cy.Sym.init(.object, parent, nameDup),
            .declId = declId,
            .type = typeId,
            .fields = undefined,
            .variantId = cy.NullId,
            .numFields = 0,
            .mod = undefined,
        });

        @as(*Module, @ptrCast(&sym.mod)).* = Module.init(mod.chunk);
        try mod.chunk.modSyms.append(c.alloc, @ptrCast(sym));

        const symId = try addSym(c, mod, name, @ptrCast(sym));

        // Update node's `name` so it can do a lookup during resolving.
        const node = mod.chunk.ast.node(declId);
        mod.chunk.parser.ast.nodePtr(node.data.objectDecl.header).data.objectHeader.name = @intCast(symId);

        c.compiler.sema.types.items[typeId] = .{
            .sym = @ptrCast(sym),
            .kind = .object,
            .data = .{ .object = .{
                .numFields = 0,
            }},
        };
        return sym;
    }

    pub fn declareHostObjectType(
        c: *cy.Chunk, parent: *cy.Sym, name: []const u8, declId: cy.NodeId,
        getChildrenFn: cc.ObjectGetChildrenFn, finalizerFn: cc.ObjectFinalizerFn,
    ) !*cy.sym.HostObjectType {
        const mod = parent.getMod().?;
        try checkUniqueSym(c, mod, name, declId);

        const typeId = try c.sema.pushType();

        const sym = try cy.sym.createSym(c.alloc, .hostObjectType, .{
            .head = cy.Sym.init(.hostObjectType, parent, name),
            .declId = declId,
            .type = typeId,
            .getChildrenFn = getChildrenFn,
            .finalizerFn = finalizerFn,
            .mod = undefined,
        });
        @as(*Module, @ptrCast(&sym.mod)).* = Module.init(mod.chunk);
        try mod.chunk.modSyms.append(c.alloc, @ptrCast(sym));

        _ = try addSym(c, mod, name, @ptrCast(sym));
        // c.vm.types.buf[rtTypeId].isHostObject = true;
        c.compiler.sema.types.items[typeId] = .{
            .sym = @ptrCast(sym),
            .kind = .hostObject,
            .data = .{ .hostObject = .{
                .getChildrenFn = getChildrenFn,
                .finalizerFn = finalizerFn,
            }},
        };
        return sym;
    }

    pub fn declarePredefinedType(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, typeId: types.TypeId) !*cy.sym.PredefinedType {
        const mod = parent.getMod().?;
        try checkUniqueSym(c, mod, name, cy.NullId);

        const sym = try cy.sym.createSym(c.alloc, .predefinedType, .{
            .head = cy.Sym.init(.predefinedType, parent, name),
            .type = typeId,
            .mod = undefined,
        });
        @as(*Module, @ptrCast(&sym.mod)).* = Module.init(mod.chunk);
        try mod.chunk.modSyms.append(c.alloc, @ptrCast(sym));

        _ = try addSym(c, mod, name, @ptrCast(sym));
        c.compiler.sema.types.items[typeId] = .{
            .sym = @ptrCast(sym),
            .kind = .predefined,
            .data = undefined,
        };
        return sym;
    }

    pub fn declareUserFunc(
        c: *cy.Chunk, parent: *cy.Sym, name: []const u8,
        funcSigId: sema.FuncSigId, declId: cy.NodeId, isMethod: bool,
    ) !*cy.Func {
        const mod = parent.getMod().?;
        const res = try prepareFuncSym(c, parent, mod, name, funcSigId, declId);
        const func = try createFunc(c, .userFunc, parent, res.sym, funcSigId, declId, isMethod);
        try addFuncToSym(c, mod, res.symId, res.sym, func);
        return func;
    }

    pub fn addUserLambda(c: *cy.Chunk, parent: *cy.Sym, funcSigId: sema.FuncSigId, declId: cy.NodeId) !*cy.Func {
        const mod = parent.getMod().?;
        const func = try createFunc(c, .userLambda, parent, null, funcSigId, declId, false);
        try mod.funcs.append(c.alloc, func);
        return func;
    }

    pub fn declareHostInlineFunc(
        c: *cy.Chunk, parent: *cy.Sym, name: []const u8, funcSigId: sema.FuncSigId,
        nodeId: cy.NodeId, funcPtr: cy.ZHostFuncFn, isMethod: bool,
    ) !*cy.Func {
        const mod = parent.getMod().?;
        const res = try prepareFuncSym(c, parent, mod, name, funcSigId, nodeId);
        const func = try createFunc(c, .hostInlineFunc, parent, res.sym, funcSigId, nodeId, isMethod);
        func.data = .{ .hostInlineFunc = .{
            .ptr = funcPtr,
        }};
        try addFuncToSym(c, mod, res.symId, res.sym, func);
        return func;
    }

    pub fn declareHostFunc(
        c: *cy.Chunk, parent: *cy.Sym, name: []const u8, funcSigId: sema.FuncSigId,
        nodeId: cy.NodeId, funcPtr: cy.ZHostFuncFn, isMethod: bool,
    ) !*cy.Func {
        const mod = parent.getMod().?;
        const res = try prepareFuncSym(c, parent, mod, name, funcSigId, nodeId);
        const func = try createFunc(c, .hostFunc, parent, res.sym, funcSigId, nodeId, isMethod);
        func.data = .{ .hostFunc = .{
            .ptr = @ptrCast(funcPtr),
        }};
        try addFuncToSym(c, mod, res.symId, res.sym, func);
        return func;
    }

    pub fn declareHostFuncSig(
        c: *cy.Chunk, parent: *cy.Sym, name: []const u8, params: []const types.TypeId, ret: types.TypeId,
        nodeId: cy.NodeId, funcPtr: cy.ZHostFuncFn, isMethod: bool,
    ) !*cy.Func {
        const funcSigId = try c.sema.ensureFuncSig(params, ret);
        return declareHostFunc(c, parent, name, funcSigId, nodeId, funcPtr, isMethod);
    }

    pub fn getOptResolvedSym(_: *cy.Chunk, modSym: *cy.Sym, name: []const u8) !?*cy.Sym {
        const mod = modSym.getMod() orelse {
            return null;
        };
        const sym = mod.getSym(name) orelse {
            return null;
        };
        switch (sym.type) {
            .import => {
                return sym.cast(.import).sym;
            },
            .typeAlias => {
                const alias = sym.cast(.typeAlias);
                try ensureTypeAliasIsResolved(mod, alias);
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
            return c.reportErrorAt("Can not access `{}` from parent `{}`. Parent is not a module.", &.{v(name), v(symPath)}, nodeId);
        };
        const sym = mod.getSym(name) orelse {
            const symPath = try modSym.resolved().formatAbsPath(&cy.tempBuf);
            return c.reportErrorAt("Can not find the symbol `{}` in `{}`.", &.{v(name), v(symPath)}, nodeId);
        };
        switch (sym.type) {
            .typeAlias => {
                const alias = sym.cast(.typeAlias);
                try ensureTypeAliasIsResolved(mod, alias);
                return alias.sym;
            },
            else => {
                return sym;
            }
        }
    }

    pub fn findDistinctSym(
        c: *cy.Chunk, modSym: *cy.Sym, name: []const u8, nodeId: cy.NodeId, comptime must: bool,
    ) anyerror!(if (must) *cy.Sym else ?*cy.Sym) {
        const mod = modSym.getMod() orelse {
            const symPath = try modSym.formatAbsPath(&cy.tempBuf);
            return c.reportErrorAt("Can not access `{}` from parent `{}`. Parent is not a module.", &.{v(name), v(symPath)}, nodeId);
        };
        const symId = mod.symMap.get(name) orelse {
            if (must) {
                return c.reportErrorAt(
                    \\Can not find the symbol `{}` in `{}`.
                , &.{v(name), v(modSym.name())}, nodeId);
            } else {
                return null;
            }
        };

        const sym = mod.syms.items[symId];
        switch (sym.type) {
            .userVar,
            .hostVar,
            .object,
            .hostObjectType,
            .enumType,
            .chunk,
            .predefinedType,
            .typeTemplate,
            .enumMember => {
                return sym;
            },
            .import => {
                return sym.cast(.import).sym;
            },
            .typeAlias => {
                const alias = sym.cast(.typeAlias);
                try ensureTypeAliasIsResolved(mod, alias);
                return alias.sym;
            },
            .func => {
                const func = sym.cast(.func);
                if (func.numFuncs > 1) {
                    if (must) {
                        // More than one func for sym.
                        return c.reportErrorAt("Symbol `{}` is ambiguous. There are multiple functions with the same name.", &.{v(name)}, nodeId);
                    } else {
                        return null;
                    }
                } else {
                    return sym;
                }
            },
            .field => {
                if (must) {
                    return c.reportErrorAt("Can not reference `{}` as a symbol.", &.{v(name)}, nodeId);
                } else {
                    return null;
                }
            },
            .uninit => cy.unexpected(),
        }
    }
};

fn ensureTypeAliasIsResolved(mod: *Module, alias: *cy.sym.TypeAlias) !void {
    if (alias.type == cy.NullId) {
        const srcChunk = mod.chunk;
        const node = srcChunk.ast.node(alias.declId);
        alias.type = try cy.sema.resolveTypeSpecNode(srcChunk, node.data.typeAliasDecl.typeSpec);
        alias.sym = mod.chunk.compiler.sema.types.items[alias.type].sym;
    }
}

test "module internals" {
    try t.eq(@offsetOf(Module, "syms"), @offsetOf(vmc.Module, "syms"));
    try t.eq(@offsetOf(Module, "symMap"), @offsetOf(vmc.Module, "symMap"));
    try t.eq(@offsetOf(Module, "funcs"), @offsetOf(vmc.Module, "funcs"));
    try t.eq(@offsetOf(Module, "overloadedFuncMap"), @offsetOf(vmc.Module, "overloadedFuncMap"));
    try t.eq(@offsetOf(Module, "retainedVars"), @offsetOf(vmc.Module, "retainedVars"));
    try t.eq(@offsetOf(Module, "chunk"), @offsetOf(vmc.Module, "chunk"));
}