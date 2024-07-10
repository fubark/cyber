const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const C = @import("capi.zig");
const sema = cy.sema;
const types = cy.types;
const bt = types.BuiltinTypes;
const fmt = cy.fmt;
const v = fmt.v;
const vmc = cy.vmc;
const log = cy.log.scoped(.module);
const ast = cy.ast;

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

    pub fn getTraitMethodImpl(m: *Module, c: *cy.Compiler, member: cy.sym.TraitMember) ?*cy.Func {
        const sym = m.getSym(member.func.name()) orelse return null;
        if (sym.type != .func) {
            return null;
        }

        const func = sym.cast(.func).first;
        if (!func.isMethod()) {
            return null;
        }
        const trait_sig = c.sema.getFuncSig(member.func.funcSigId);
        const func_sig = c.sema.getFuncSig(func.funcSigId);

        const trait_params = trait_sig.params()[1..];
        const func_params = func_sig.params()[1..];
        for (trait_params, 0..) |trait_param, i| {
            if (trait_param.type != func_params[i].type) {
                return null;
            }
        }
        if (trait_sig.ret != func_sig.ret) {
            return null;
        }
        return func;
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

    pub fn addSym(m: *Module, alloc: std.mem.Allocator, name: []const u8, sym: *cy.Sym) !void {
        try m.symMap.putNoClobber(alloc, name, sym);
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

pub fn reportDupSym(c: *cy.Chunk, sym: *cy.Sym, name: []const u8, decl: ?*ast.Node) !void {
    return c.reportErrorFmt("`{}` has already been declared as a `{}`.", &.{v(name), v(sym.type)}, decl);
}

fn addUniqueSym(c: *cy.Chunk, mod: *Module, name: []const u8, sym: *cy.Sym, decl: ?*ast.Node) !void {
    const res = try mod.symMap.getOrPut(c.alloc, name);
    if (res.found_existing) {
        const existing = res.value_ptr.*;
        if (existing.type != .placeholder) {
            try reportDupSym(c, existing, name, decl);
        }

        // Move placeholder module to sym.
        const placeholder = existing.cast(.placeholder);
        const sym_mod = sym.getMod() orelse {
            return c.reportErrorFmt("`{}` can not contain symbols.", &.{v(name)}, decl);
        };
        sym_mod.* = placeholder.getMod().*;
        sym_mod.updateParentRefs(sym);

        placeholder.resolved = true;
    }
    res.value_ptr.* = sym;
}

fn prepareFuncSym(c: *cy.Chunk, parent: *cy.Sym, mod: *Module, name: []const u8, decl: ?*ast.FuncDecl) !*cy.sym.FuncSym {
    if (mod.symMap.get(name)) |sym| {
        if (sym.type == .func) {
            return sym.cast(.func);
        } else {
            try reportDupSym(c, sym, name, @ptrCast(decl));
        }
    }
    // Create func sym.
    const sym = try c.createFuncSym(parent, name);
    try mod.addSym(c.alloc, name, @ptrCast(sym));
    return sym;
}

pub const ChunkExt = struct {
    
    pub fn reserveUseAlias(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, decl: ?*ast.Node) !*cy.sym.UseAlias {
        const sym = try c.createUseAlias(parent, name, decl);
        const mod = parent.getMod().?;
        _ = try addUniqueSym(c, mod, name, @ptrCast(sym), decl);
        return sym;
    } 

    pub fn reserveTypeAlias(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, decl: *ast.TypeAliasDecl) !*cy.sym.TypeAlias {
        const sym = try c.createTypeAlias(parent, name, decl);
        const mod = parent.getMod().?;
        _ = try addUniqueSym(c, mod, name, @ptrCast(sym), @ptrCast(decl));
        return sym;
    } 

    pub fn reserveDistinctType(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, decl: *ast.DistinctDecl) !*cy.sym.DistinctType {
        const sym = try c.createDistinctType(parent, name, decl);
        const mod = parent.getMod().?;
        _ = try addUniqueSym(c, mod, name, @ptrCast(sym), @ptrCast(decl));
        return sym;
    }

    pub fn reserveCustomType(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, decl: *ast.CustomDecl) !*cy.sym.CustomType {
        const sym = try c.createCustomType(parent, name, decl);
        const mod = parent.getMod().?;
        _ = try addUniqueSym(c, mod, name, @ptrCast(sym), @ptrCast(decl));
        return sym;
    }

    pub fn declareTemplate(c: *cy.Chunk, parent: *cy.Sym, name: []const u8,
        sigId: cy.sema.FuncSigId, is_root: bool, params: []cy.sym.TemplateParam, kind: cy.sym.SymType,
        child_decl: *ast.Node, decl: *ast.TemplateDecl) !*cy.sym.Template {

        const sym = try c.createTemplate(parent, name, sigId, is_root, params, kind, child_decl);
        const mod = parent.getMod().?;
        try addUniqueSym(c, mod, name, @ptrCast(sym), @ptrCast(decl));
        return sym;
    }

    pub fn declareEnumMember(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, typeId: types.TypeId,
        is_choice_type: bool, val: u32, payloadType: cy.TypeId, decl: *ast.EnumMember) !*cy.sym.EnumMember {
        const sym = try c.createEnumMember(parent, name, typeId, is_choice_type, val, payloadType);
        const mod = parent.getMod().?;
        try addUniqueSym(c, mod, name, @ptrCast(sym), @ptrCast(decl));
        return sym;
    }

    pub fn reserveHostVar(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, decl: ?*ast.StaticVarDecl) !*cy.sym.HostVar {
        const sym = try c.createHostVar(parent, name, decl);
        const mod = parent.getMod().?;
        try addUniqueSym(c, mod, name, @ptrCast(sym), @ptrCast(decl));
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

    pub fn reserveUserVar(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, decl: ?*ast.StaticVarDecl) !*cy.sym.UserVar {
        const sym = try c.createUserVar(parent, name, decl);
        const mod = parent.getMod().?;
        _ = try addUniqueSym(c, mod, name, @ptrCast(sym), @ptrCast(decl));
        return sym;
    }

    pub fn resolveUserVar(c: *cy.Chunk, sym: *cy.sym.UserVar, type_id: cy.TypeId) void {
        _ = c;
        sym.type = type_id;
    }

    pub fn reserveContextVar(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, decl: *ast.ContextDecl) !*cy.sym.ContextVar {
        const sym = try c.createContextVar(parent, name, decl);
        const mod = parent.getMod().?;
        _ = try addUniqueSym(c, mod, name, @ptrCast(sym), @ptrCast(decl));
        return sym;
    }

    pub fn resolveContextVar(c: *cy.Chunk, sym: *cy.sym.ContextVar, type_id: cy.TypeId, idx: u8) void {
        _ = c;
        sym.type = type_id;
        sym.idx = idx;
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

    pub fn declareField(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, idx: usize, typeId: cy.TypeId, decl: ?*ast.Node) !*cy.sym.Field {
        const sym = try c.createField(parent, name, idx, typeId);
        const mod = parent.getMod().?;
        _ = try addUniqueSym(c, mod, name, @ptrCast(sym), decl);
        return sym;
    }

    pub fn reserveStructType(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, cstruct: bool, decl: *ast.ObjectDecl) !*cy.sym.ObjectType {
        const sym = try c.createStructType(parent, name, cstruct, decl);
        const mod = parent.getMod().?;
        _ = try addUniqueSym(c, mod, name, @ptrCast(sym), @ptrCast(decl));
        return sym;
    }

    pub fn reserveEnumType(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, isChoiceType: bool, decl: *ast.EnumDecl) !*cy.sym.EnumType {
        const sym = try c.createEnumType(parent, name, isChoiceType, decl);
        _ = try addUniqueSym(c, c.sym.getMod(), name, @ptrCast(sym), @ptrCast(decl));
        return sym;
    }

    pub fn reserveTraitType(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, decl: *ast.TraitDecl) !*cy.sym.TraitType {
        const sym = try c.createTraitType(parent, name, decl);
        _ = try addUniqueSym(c, c.sym.getMod(), name, @ptrCast(sym), @ptrCast(decl));
        return sym;
    }

    pub fn reserveObjectType(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, decl: *ast.Node) !*cy.sym.ObjectType {
        const sym = try c.createObjectType(parent, name, decl);
        _ = try addUniqueSym(c, c.sym.getMod(), name, @ptrCast(sym), decl);
        return sym;
    }

    pub fn declareFloatType(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, bits: u8, opt_type_id: ?types.TypeId, decl: ?*ast.Node) !*cy.sym.FloatType {
        const sym = try c.createFloatType(parent, name, bits, opt_type_id);
        const mod = parent.getMod().?;
        _ = try addUniqueSym(c, mod, name, @ptrCast(sym), decl);
        return sym;
    }

    pub fn declareIntType(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, bits: u8, opt_type_id: ?types.TypeId, decl: ?*ast.Node) !*cy.sym.IntType {
        const sym = try c.createIntType(parent, name, bits, opt_type_id);
        const mod = parent.getMod().?;
        _ = try addUniqueSym(c, mod, name, @ptrCast(sym), decl);
        return sym;
    }

    pub fn declareBoolType(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, opt_type_id: ?types.TypeId, decl: ?*ast.Node) !*cy.sym.BoolType {
        const sym = try c.createBoolType(parent, name, opt_type_id);
        const mod = parent.getMod().?;
        _ = try addUniqueSym(c, mod, name, @ptrCast(sym), decl);
        return sym;
    }

    pub fn addUserLambda(c: *cy.Chunk, parent: *cy.Sym, funcSigId: sema.FuncSigId, decl: *ast.LambdaExpr) !*cy.Func {
        const func = try c.createFunc(.userLambda, parent, null, @ptrCast(decl), false);
        try c.funcs.append(c.alloc, func);
        const func_sig = c.compiler.sema.getFuncSig(funcSigId);
        func.funcSigId = funcSigId;
        func.retType = func_sig.getRetType();
        func.reqCallTypeCheck = func_sig.reqCallTypeCheck;
        func.numParams = @intCast(func_sig.params_len);
        return func;
    }

    pub fn reserveTemplateFunc(
        c: *cy.Chunk, parent: *cy.Sym, name: []const u8, node: ?*ast.FuncDecl, is_method: bool,
    ) !*cy.Func {
        const mod = parent.getMod().?;
        const sym = try prepareFuncSym(c, parent, mod, name, node);
        const func = try c.createFunc(.template, parent, sym, @ptrCast(node), is_method);
        try c.funcs.append(c.alloc, func);
        sym.addFunc(func);
        return func;
    }

    pub fn resolveTemplateFunc(c: *cy.Chunk, func: *cy.Func, func_sig: sema.FuncSigId, template: *cy.sym.FuncTemplate) !void {
        func.data = .{ .template = template };
        try resolveFunc(c, func, func_sig);
    }

    pub fn reserveHostFunc(
        c: *cy.Chunk, parent: *cy.Sym, name: []const u8, node: ?*ast.FuncDecl, is_method: bool, is_variant: bool,
    ) !*cy.Func {
        const mod = parent.getMod().?;
        const sym = try prepareFuncSym(c, parent, mod, name, node);
        const func = try c.createFunc(.hostFunc, parent, sym, @ptrCast(node), is_method);
        if (is_variant) {
            try c.variantFuncSyms.append(c.alloc, func);
        } else {
            try c.funcs.append(c.alloc, func);
        }
        func.data = .{ .hostFunc = .{
            .ptr = undefined,
        }};
        sym.addFunc(func);
        return func;
    }

    pub fn resolveHostFunc(c: *cy.Chunk, func: *cy.Func, func_sig: sema.FuncSigId, func_ptr: cy.ZHostFuncFn) !void {
        try resolveFunc(c, func, func_sig);
        func.data.hostFunc.ptr = func_ptr;
    }

    pub fn reserveTraitFunc(
        c: *cy.Chunk, parent: *cy.Sym, name: []const u8, node: *ast.FuncDecl, vtable_idx: usize, is_variant: bool,
    ) !*cy.Func {
        const mod = parent.getMod().?;
        const sym = try prepareFuncSym(c, parent, mod, name, node);
        const func = try c.createFunc(.trait, parent, sym, @ptrCast(node), true);
        func.data = .{
            .trait = .{
                .vtable_idx = @intCast(vtable_idx),
            },
        };
        if (is_variant) {
           try c.variantFuncSyms.append(c.alloc, func);
        } else {
           try c.funcs.append(c.alloc, func);
        }
        sym.addFunc(func);
        return func;
    }

    pub fn reserveUserFunc(
        c: *cy.Chunk, parent: *cy.Sym, name: []const u8, node: *ast.FuncDecl, is_method: bool, is_variant: bool,
    ) !*cy.Func {
        const mod = parent.getMod().?;
        const sym = try prepareFuncSym(c, parent, mod, name, node);
        const func = try c.createFunc(.userFunc, parent, sym, @ptrCast(node), is_method);
        if (is_variant) {
           try c.variantFuncSyms.append(c.alloc, func);
        } else {
           try c.funcs.append(c.alloc, func);
        }
        sym.addFunc(func);
        return func;
    }

    pub fn resolveUserFunc(c: *cy.Chunk, func: *cy.Func, func_sig: sema.FuncSigId) !void {
        try resolveFunc(c, func, func_sig);
    }

    fn resolveFunc(c: *cy.Chunk, func: *cy.Func, func_sig_id: sema.FuncSigId) !void {
        const mod = func.parent.getMod().?;
        const sym = func.sym.?;
        if (!mod.isFuncUnique(sym, func_sig_id)) {
            return c.reportErrorFmt("`{}` has already been declared with the same function signature.", &.{v(func.name())}, @ptrCast(func.decl));
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
        func.numParams = @intCast(func_sig.params_len);
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

    pub fn mustFindSym(c: *cy.Chunk, modSym: *cy.Sym, name: []const u8, nodeId: *ast.Node) !*cy.Sym {
        const mod = modSym.getMod() orelse {
            const mod_name = try cy.sym.formatSymName(c.sema, &cy.tempBuf, modSym, .{ .from = c });
            return c.reportErrorFmt("Can not access `{}` from parent `{}`. Parent is not a module.", &.{v(name), v(mod_name)}, nodeId);
        };
        if (mod.getSym(name)) |sym| {
            return sym;
        }

        // TODO: A type variant sym's module should cache expanded methods.
        if (modSym.getVariant()) |variant| {
            if (variant.root_template.getMod().getSym(name)) |sym| {
                return sym;
            }
        }

        const mod_name = try cy.sym.formatSymName(c.sema, &cy.tempBuf, modSym.resolved(), .{ .from = c });
        return c.reportErrorFmt("Can not find the symbol `{}` in `{}`.", &.{v(name), v(mod_name)}, nodeId);
    }

    pub fn getResolvedDistinctSym(
        c: *cy.Chunk, modSym: *cy.Sym, name: []const u8, node: ?*ast.Node, comptime must: bool,
    ) anyerror!(if (must) *cy.Sym else ?*cy.Sym) {
        const mod = modSym.getMod() orelse {
            const mod_name = try cy.sym.formatSymName(c.sema, &cy.tempBuf, modSym, .{ .from = c });
            return c.reportErrorFmt("Can not access `{}` from parent `{}`. Parent is not a module.", &.{v(name), v(mod_name)}, node);
        };
        const sym = mod.symMap.get(name) orelse {
            if (must) {
                return c.reportErrorFmt(
                    \\Can not find the symbol `{}` in `{}`.
                , &.{v(name), v(modSym.name())}, node);
            } else {
                return null;
            }
        };
        switch (sym.type) {
            .context_var,
            .userVar,
            .hostVar,
            .struct_t,
            .object_t,
            .custom_t,
            .trait_t,
            .enum_t,
            .chunk,
            .bool_t,
            .int_t,
            .float_t,
            .distinct_t,
            .template,
            .placeholder,
            .dummy_t,
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
                        return c.reportErrorFmt("Symbol `{}` is ambiguous. There are multiple functions with the same name.", &.{v(name)}, node);
                    } else {
                        return null;
                    }
                } else {
                    return sym;
                }
            },
            .field => {
                if (must) {
                    return c.reportErrorFmt("Can not reference `{}` as a symbol.", &.{v(name)}, node);
                } else {
                    return null;
                }
            },
            .null => cy.unexpected(),
        }
    }

    pub fn getDistinctSym(
        c: *cy.Chunk, modSym: *cy.Sym, name: []const u8, nodeId: *ast.Node, comptime must: bool,
    ) anyerror!(if (must) *cy.Sym else ?*cy.Sym) {
        const mod = modSym.getMod() orelse {
            const mod_name = try cy.sym.formatSymName(c.sema, &cy.tempBuf, modSym, .{ .from = c });
            return c.reportErrorFmt("Can not access `{}` from parent `{}`. Parent is not a module.", &.{v(name), v(mod_name)}, nodeId);
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
            .context_var,
            .userVar,
            .hostVar,
            .struct_t,
            .object_t,
            .trait_t,
            .custom_t,
            .dummy_t,
            .enum_t,
            .chunk,
            .bool_t,
            .int_t,
            .float_t,
            .distinct_t,
            .template,
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