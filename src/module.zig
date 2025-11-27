const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const C = @import("capi.zig");
const sema = cy.sema;
const sema_type = cy.sema_type;
const types = cy.types;
const bt = types.BuiltinTypes;
const ir = cy.ir;
const fmt = cy.fmt;
const v = fmt.v;
const vmc = cy.vmc;
const log = cy.log.scoped(.module);
const ast = cy.ast;

pub const ModFuncKey = cy.hash.KeyU128;

/// A module contains symbols declared in Cyber source code and through the embedded API.
/// Overloaded functions are linked together so that type signatures can be checked sequentially.
pub const Module = struct {
    /// Name to sym.
    symMap: std.StringHashMapUnmanaged(*cy.Sym),

    /// Sym+funcSig to func sym. This is used to check whether an overloaded function is unique.
    /// This should only be populated for a function sym once it's overloaded.
    overloadedFuncMap: std.HashMapUnmanaged(ModFuncKey, *cy.Func, cy.hash.KeyU128Context, 80),

    pub fn init() Module {
        return .{
            .symMap = .{},
            .overloadedFuncMap = .{},
        };
    }

    pub fn deinit(self: *Module, alloc: std.mem.Allocator) void {
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

    fn isFuncUnique(m: *Module, func: *cy.sym.FuncSym, sig: *cy.FuncSig) bool {
        if (func.firstFuncSig == sig) {
            return false;
        }
        if (func.numFuncs == 1) {
            return true;
        }
        
        const key = ModFuncKey.initModFuncKey(func, sig);
        return !m.overloadedFuncMap.contains(key);
    }

    pub fn addSym(m: *Module, alloc: std.mem.Allocator, name: []const u8, sym: *cy.Sym) !void {
        try m.symMap.putNoClobber(alloc, name, sym);
    }

    pub fn updateParentRefs(m: *Module, parent: *cy.Sym) void {
        var iter = m.symMap.valueIterator();
        while (iter.next()) |val_ptr| {
            val_ptr.*.parent = parent;
        }
    }
};

pub fn reportDupSym(c: *cy.Chunk, sym: *cy.Sym, name: []const u8, decl: ?*ast.Node) !void {
    const id = try c.addReportFmt("`{}` has already been declared as a `{}`", &.{v(name), v(sym.type)}, decl);
    const last = try c.appendContextTrace(&c.compiler.reports.items[id], "First declared at:", sym.declNode());
    try c.appendErrorTrace(last);
    return error.CompileError;
}

pub fn addUniqueSym(c: *cy.Chunk, mod: *Module, name: []const u8, sym: *cy.Sym, decl: ?*ast.Node) !void {
    const res = try mod.symMap.getOrPut(c.alloc, name);
    if (res.found_existing) {
        const existing = res.value_ptr.*;
        try reportDupSym(c, existing, name, decl);
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

pub fn reserveUseAlias(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, decl: ?*ast.Node) !*cy.sym.UseAlias {
    const sym = try c.createUseAlias(parent, name, decl);
    const mod = parent.getMod().?;
    _ = try addUniqueSym(c, mod, name, @ptrCast(sym), @ptrCast(decl));
    return sym;
} 

pub fn reserve_type_const(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, decl: *ast.TypeConstDecl) !*cy.sym.TypeConst {
    const sym = try c.createTypeConst(parent, name, decl.hidden, decl);
    const mod = parent.getMod().?;
    _ = try addUniqueSym(c, mod, name, @ptrCast(sym), @ptrCast(decl));
    return sym;
} 

pub fn reserveTypeAlias(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, decl: *ast.TypeAliasDecl) !*cy.sym.TypeAlias {
    const sym = try c.createTypeAlias(parent, name, decl.hidden, decl);
    const mod = parent.getMod().?;
    _ = try addUniqueSym(c, mod, name, @ptrCast(sym), @ptrCast(decl));
    return sym;
} 

pub fn reserveHostObjectType(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, decl: *ast.CustomDecl) !*cy.sym.HostObjectType {
    const sym = try c.createHostObjectType(parent, name, decl);
    const mod = parent.getMod().?;
    _ = try addUniqueSym(c, mod, name, @ptrCast(sym), @ptrCast(decl));
    return sym;
}

pub fn reserveFuncTemplateSym(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, callable: bool, decl: *ast.TemplateDecl) !*cy.sym.FuncTemplate {
    const sym = try c.createFuncTemplate(parent, name, callable, decl);
    const mod = parent.getMod().?;
    try addUniqueSym(c, mod, name, @ptrCast(sym), @ptrCast(decl));
    return sym;
}

pub fn reserveTemplate(c: *cy.Chunk, parent: *cy.Sym, name: []const u8,
    kind: cy.sym.TemplateType, decl: *ast.TemplateDecl) !*cy.sym.Template {

    const sym = try c.createTemplate(parent, name, kind, decl);
    const mod = parent.getMod().?;
    try addUniqueSym(c, mod, name, @ptrCast(sym), @ptrCast(decl));
    return sym;
}

pub fn declareEnumCase(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, type_: *cy.Type,
    val: u32, decl: *ast.EnumMember) !*cy.sym.EnumCase {
    const sym = try c.createEnumCase(parent, name, type_, val);
    const mod = parent.getMod().?;
    try addUniqueSym(c, mod, name, @ptrCast(sym), @ptrCast(decl));
    return sym;
}

pub fn declareChoiceCase(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, type_: *cy.Type,
    val: u32, idx: u32, payload_t: *cy.Type, decl: *ast.Node) !*cy.sym.ChoiceCase {
    const sym = try c.createChoiceCase(parent, name, type_, val, idx, payload_t);
    const mod = parent.getMod().?;
    try addUniqueSym(c, mod, name, @ptrCast(sym), @ptrCast(decl));
    return sym;
}

pub fn declareUnionCase(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, type_: *cy.Type,
    idx: u32, payload_t: *cy.Type, decl: *ast.Node) !*cy.sym.UnionCase {
    const sym = try c.createUnionCase(parent, name, type_, idx, payload_t);
    const mod = parent.getMod().?;
    try addUniqueSym(c, mod, name, @ptrCast(sym), @ptrCast(decl));
    return sym;
}

pub fn reserveExternVar(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, decl: ?*ast.GlobalDecl) !*cy.sym.ExternVar {
    const sym = try c.createExternVar(parent, name, decl);
    const mod = parent.getMod().?;
    try addUniqueSym(c, mod, name, @ptrCast(sym), @ptrCast(decl));
    return sym;
}

pub fn reserveHostVar(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, decl: ?*ast.GlobalDecl) !*cy.sym.HostVar {
    const sym = try c.createHostVar(parent, name, decl);
    const mod = parent.getMod().?;
    try addUniqueSym(c, mod, name, @ptrCast(sym), @ptrCast(decl));
    return sym;
}

pub fn resolveHostVar(c: *cy.Chunk, sym: *cy.sym.HostVar, var_t: *cy.Type, val: *anyopaque) !void {
    _ = c;
    sym.type = var_t;
    sym.val = val;
} 

pub fn reserveConst(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, decl: ?*ast.ConstDecl) !*cy.sym.Const {
    const sym = try c.createConst(parent, name, decl);
    const mod = parent.getMod().?;
    _ = try addUniqueSym(c, mod, name, @ptrCast(sym), @ptrCast(decl));
    return sym;
}

pub fn reserveUserVar(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, decl: ?*ast.GlobalDecl) !*cy.sym.UserVar {
    const sym = try c.createUserVar(parent, name, decl);
    const mod = parent.getMod().?;
    _ = try addUniqueSym(c, mod, name, @ptrCast(sym), @ptrCast(decl));
    return sym;
}

pub fn resolveUserVarType(c: *cy.Chunk, sym: *cy.sym.UserVar, type_: *cy.Type) void {
    _ = c;
    sym.type = type_;
}

pub fn declareField(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, idx: usize, type_: *cy.Type, decl: ?*ast.Node) !*cy.sym.Field {
    const sym = try c.createField(parent, name, idx, type_);
    const mod = parent.getMod().?;
    _ = try addUniqueSym(c, mod, name, @ptrCast(sym), decl);
    return sym;
}

pub fn reserveTypeSym(c: *cy.Chunk, parent: *cy.Sym, name: []const u8, type_: *cy.Type, decl: ?*ast.Node) !*cy.sym.TypeSym {
    const sym = try c.createTypeSym(parent, name, type_, decl);
    const mod = parent.getMod().?;
    _ = try addUniqueSym(c, mod, name, @ptrCast(sym), @ptrCast(decl));
    return sym;
}

pub fn addUserLambda(c: *cy.Chunk, parent: *cy.Sym, decl: *ast.Node) !*cy.Func {
    const config = cy.sym.FuncConfig{
        .is_method = false,
        .extern_ = false,
    };
    const func = try c.createFunc(.userLambda, parent, config, decl);
    try c.appendFunc(func);
    return func;
}

pub fn resolveUserLambda(c: *cy.Chunk, func: *cy.Func, sig: *cy.FuncSig) !void {
    _ = c;
    func.sig = sig;
    func.info.resolved = true;
}

pub fn reserveVariantMember(
    c: *cy.Chunk, parent: *cy.Sym, name: []const u8, decl: *ast.Node,
) !*cy.sym.VariantMember {
    const sym = try c.createVariantMember(parent, name, @ptrCast(decl));
    const mod = parent.getMod().?;
    _ = try addUniqueSym(c, mod, name, @ptrCast(sym), @ptrCast(decl));
    return sym;
}

pub fn reserveVariantFunc(
    c: *cy.Chunk, parent: *cy.Sym, name: []const u8, decl: *ast.FuncDecl,
) !*cy.sym.VariantFunc {
    // const sym = try c.createVariantMember(parent, name, @ptrCast(decl));
    const mod = parent.getMod().?;
    if (mod.symMap.get(name)) |sym| {
        if (sym.type == .variant_func) {
            const sym_ = sym.cast(.variant_func);
            try sym_.rest().append(c.alloc, .{
                .decl = decl,
                .with_cstrs = .{ .ptr = undefined, .len = 0 },
                .resolved = false,
            });
            return sym_;
        } else {
            try reportDupSym(c, sym, name, @ptrCast(decl));
            unreachable;
        }
    } else {
        const sym = try c.createVariantFunc(parent, name, decl);
        try mod.addSym(c.alloc, name, @ptrCast(sym));
        return sym;
    }
}

pub fn reserveExternFunc(
    c: *cy.Chunk, parent: *cy.Sym, name: []const u8, config: cy.sym.FuncConfig, decl: ?*ast.FuncDecl,
) !*cy.Func {
    const mod = parent.getMod().?;
    const sym = try prepareFuncSym(c, parent, mod, name, decl);
    const func = try c.createFunc(.extern_, @ptrCast(sym), config, @ptrCast(decl));
    try c.appendFunc(func);
    func.data = .{
        .extern_ = .{ .name_ptr = name.ptr, .name_len = @intCast(name.len), .vm_variadic = false },
    };
    sym.addFunc(func);
    return func;
}

pub fn reserveHostFunc(
    c: *cy.Chunk, parent: *cy.Sym, name: []const u8, config: cy.sym.FuncConfig, decl: ?*ast.FuncDecl, deferred: bool,
) !*cy.Func {
    const mod = parent.getMod().?;
    const sym = try prepareFuncSym(c, parent, mod, name, decl);
    const func = try c.createFunc(.hostFunc, @ptrCast(sym), config, @ptrCast(decl));
    if (!deferred) {
        try c.appendFunc(func);
    } else {
        try c.deferred_funcs.append(c.alloc, func);
    }
    func.data = .{ .hostFunc = .{
        .ptr = undefined,
    }};
    sym.addFunc(func);
    return func;
}

pub fn resolveHostFunc(c: *cy.Chunk, func: *cy.Func, sig: *sema.FuncSig, func_ptr: cy.ZHostFn, eval_ptr: ?cy.ZCtEvalFuncFn) !void {
    try resolveFunc(c, func, sig);
    func.data.hostFunc.ptr = func_ptr;
    func.data.hostFunc.eval = eval_ptr;
}

pub fn resolveHostSemaFunc(c: *cy.Chunk, func: *cy.Func, sig: *cy.FuncSig, func_ptr: ?cy.ZSemaFn) !void {
    func.type = .host_sema;
    try resolveFunc(c, func, sig);
    func.data = .{ .host_sema = .{ .ptr = func_ptr }};
}

pub fn resolveHostCtFunc(c: *cy.Chunk, func: *cy.Func, sig: *cy.FuncSig, func_ptr: ?cy.ZCtFn, eval_ptr: ?cy.ZCtEvalFuncFn) !void {
    func.type = .host_ct;
    if (func_ptr == null) {
        func.info.const_eval_only = true;
    }
    try resolveFunc(c, func, sig);
    func.data = .{ .host_ct = .{ .ptr = func_ptr, .eval = eval_ptr }};
}

pub fn reserveTraitFunc(
    c: *cy.Chunk, parent: *cy.Sym, name: []const u8, node: *ast.FuncDecl, vtable_idx: usize,
) !*cy.Func {
    const mod = parent.getMod().?;
    const sym = try prepareFuncSym(c, parent, mod, name, node);
    const config = cy.sym.FuncConfig{
        .is_method = true,
        .extern_ = false,
    };
    const func = try c.createFunc(.trait, @ptrCast(sym), config, @ptrCast(node));
    func.data = .{
        .trait = .{
            .vtable_idx = @intCast(vtable_idx),
        },
    };
    sym.addFunc(func);
    return func;
}

pub fn reserveTemplateFunc(
    c: *cy.Chunk, parent: *cy.Sym, name: []const u8, config: cy.sym.FuncConfig, node: *ast.FuncDecl, deferred: bool,
) !*cy.Func {
    const mod = parent.getMod().?;
    const sym = try prepareFuncSym(c, parent, mod, name, node);
    const func = try c.createFunc(.template, @ptrCast(sym), config, @ptrCast(node));
    const template = try c.alloc.create(cy.sym.FuncTemplate);
    template.* = .{
        .head = cy.Sym.init(.func_template, parent, name),
        .func_params = node.params,
        .func_ret = node.ret,
        .params = &.{},
        .resolved = false,
        .callable = true,
        .ct = node.ct,
        .decl = @ptrCast(node),
        .variant_cache = .{},
        .instances = .{},
        .mod = undefined,
    };
    func.data = .{ .template = template };
    if (!deferred) {
        try c.appendFunc(func);
    } else {
        try c.deferred_funcs.append(c.alloc, func);
    }
    sym.addFunc(func);
    return func;
}

pub fn reserveFunc(
    c: *cy.Chunk, parent: *cy.Sym, name: []const u8, decl: *ast.FuncDecl,
) !*cy.Func {
    const mod = parent.getMod().?;
    const sym = try prepareFuncSym(c, parent, mod, name, decl);

    var extern_ = false;
    var global_init = false;
    for (decl.attrs.slice()) |attr| {
        if (attr.type == .extern_) {
            extern_ = true;
        } else if (attr.type == .global_init) {
            global_init = true;
        }
    }

    const config = cy.sym.FuncConfig{
        .is_method = decl.sig_t == .method,
        .extern_ = extern_,
    };
    if (config.is_method) {
        if (parent.type == .template) {
            return c.reportErrorFmt("A method must be bound to a template's instance `{}[]`.", &.{v(parent.name())}, @ptrCast(decl));
        }
    }
    const func = try c.createFunc(.unresolved, @ptrCast(sym), config, @ptrCast(decl));
    func.data = .{ .unresolved = {} };
    try c.funcs.append(c.alloc, func);
    sym.addFunc(func);
    return func;
}

pub fn reserveUserFunc(
    c: *cy.Chunk, parent: *cy.Sym, name: []const u8, config: cy.sym.FuncConfig, node: *ast.FuncDecl,
) !*cy.Func {
    const mod = parent.getMod().?;
    const sym = try prepareFuncSym(c, parent, mod, name, node);
    const func = try c.createFunc(.userFunc, @ptrCast(sym), config, @ptrCast(node));
    func.data = .{.userFunc = .{.loc = undefined}};
    try c.funcs.append(c.alloc, func);
    sym.addFunc(func);
    return func;
}

pub fn appendFunc(c: *cy.Chunk, func: *cy.Func) !void {
    try c.funcs.append(c.alloc, func);
}

pub fn resolveUserFunc(c: *cy.Chunk, func: *cy.Func, sig: *cy.FuncSig) !void {
    if (!func.info.const_eval_only) {
        for (sig.params()) |param| {
            if (param.get_type().info.ct) {
                func.info.const_eval_only = true;
                break;
            }
        }
    }
    try resolveFunc(c, func, sig);
}

pub fn resolveFunc(c: *cy.Chunk, func: *cy.Func, sig: *cy.FuncSig) !void {
    if (func.instance == null) {
        const sym = func.parent.cast(.func);
        const mod = sym.head.parent.?.getMod().?;
        if (!mod.isFuncUnique(sym, sig)) {
            return c.reportErrorFmt("`{}` has already been declared with the same function signature.", &.{v(func.name())}, @ptrCast(func.decl));
        }
        if (sym.first == func) {
            sym.firstFuncSig = sig;
        }
        if (sym.numFuncs > 1) {
            const key = ModFuncKey.initModFuncKey(sym, sig);
            try mod.overloadedFuncMap.putNoClobber(c.alloc, key, func);
        }
    }
    resolveFuncInfo(c, func, sig);
}

pub fn resolveFuncInfo(c: *cy.Chunk, func: *cy.Func, sig: *cy.FuncSig) void {
    _ = c;
    func.sig = sig;
    func.info.resolved = true;
}

pub fn get_pub_resolved_chunk_sym(c: *cy.Chunk, chunk: *cy.sym.Chunk, name: []const u8, node: *ast.Node) anyerror!?*cy.Sym {
    _ = node;
    const mod = chunk.getMod();
    const sym = mod.getSym(name) orelse return null;
    switch (sym.type) {
        .module_alias => {
            {
                // TODO: check visibility.
                return null;
            }
            return sym.cast(.module_alias).sym;
        },
        .use_alias => {
            const alias = sym.cast(.use_alias);
            {
                // TODO: check visibility.
                return null;
            }
            if (!alias.resolved) {
                try sema.resolveUseAlias(c, alias);
            }
            return alias.sym;
        },
        .type_alias => {
            const alias = sym.cast(.type_alias);
            if (!alias.resolved) {
                try sema_type.resolve_type_alias(c, alias);
            }
            return &alias.sym.head;
        },
        else => {
            return sym;
        }
    }
}

/// Note that resolved refers to a resolved symbol path and not the underlying symbol content.
pub fn getResolvedSym(c: *cy.Chunk, modSym: *cy.Sym, name: []const u8, node: *ast.Node) anyerror!?*cy.Sym {
    const mod = modSym.getMod() orelse {
        return null;
    };
    if (mod.getSym(name)) |sym| {
        switch (sym.type) {
            .module_alias => {
                return sym.cast(.module_alias).sym;
            },
            .use_alias => {
                const alias = sym.cast(.use_alias);
                if (!alias.resolved) {
                    try sema.resolveUseAlias(c, alias);
                }
                return alias.sym;
            },
            .type_alias => {
                const alias = sym.cast(.type_alias);
                if (!alias.resolved) {
                    try sema_type.resolve_type_alias(c, alias);
                }
                return &alias.sym.head;
            },
            else => {
                return sym;
            }
        }
    }

    // Lazily load instance sym.
    if (modSym.instance()) |variant| {
        if (variant.getSymTemplate().getMod().getSym(name)) |sym| {
            if (sym.type == .variant_func) {
                const func_sym = sym.cast(.variant_func);

                try sema.ensure_resolved_instance_func_decl(c, &func_sym.decl);
                if (func_sym.decl.with_cstrs.len > 0) {
                    try sema.check_instance_func_cstr(c, variant, &func_sym.decl, node);
                }
                var func = try c.reserveFunc(modSym, sym.name(), func_sym.decl.decl);
                try sema.ensure_resolved_func(c, func, true, node);

                for (func_sym.rest().items) |*decl| {
                    try sema.ensure_resolved_instance_func_decl(c, decl);
                    if (func_sym.decl.with_cstrs.len > 0) {
                        try sema.check_instance_func_cstr(c, variant, decl, node);
                    }
                    func = try c.reserveFunc(modSym, sym.name(), decl.decl);
                    try sema.ensure_resolved_func(c, func, true, node);
                }
                return mod.getSym(name).?;
            } else if (sym.type == .variant_member) {
                const member = sym.cast(.variant_member);
                if (member.decl.type() == .const_decl) {
                    return sema.reserveConst(c, modSym, sym.name(), member.decl.cast(.const_decl));
                } else if (member.decl.type() == .template) {
                    const template = member.decl.cast(.template);
                    if (template.child_decl.type() == .funcDecl) {
                        return @ptrCast(try sema.reserveFuncTemplateSym(c, modSym, sym.name(), template));
                    } else {
                        @panic("TODO");
                    }
                } else {
                    @panic("TODO");
                }
            }
        }
    }

    return null;
}

pub fn getResolvedSymOrFail(c: *cy.Chunk, modSym: *cy.Sym, name: []const u8, node: *ast.Node) !*cy.Sym {
    return (try c.getResolvedSym(modSym, name, node)) orelse {
        if (modSym.getMod() == null) {
            const mod_name = try cy.sym.formatSymName(c.sema, &cy.tempBuf, modSym, .{ .from = c });
            return c.reportErrorFmt("Can not access `{}` from parent `{}`. Parent is not a module.", &.{v(name), v(mod_name)}, node);
        } else {
            const mod_name = try cy.sym.formatSymName(c.sema, &cy.tempBuf, modSym.resolved(), .{ .from = c });
            return c.reportErrorFmt("Can not find the symbol `{}` in `{}`.", &.{v(name), v(mod_name)}, node);
        }
    };
}

/// Assumes resolved symbol.
pub fn checkResolvedSymIsDistinct(c: *cy.Chunk, sym: *cy.Sym, node: *ast.Node) !void {
    if (sym.isDistinctAssumeResolved()) {
        return;
    }
    // More than one func for sym.
    return c.reportErrorFmt("Symbol `{}` is ambiguous. There are multiple functions with the same name.", &.{v(sym.name())}, node);
}

pub fn accessResolvedSym(c: *cy.Chunk, rec_t: *cy.Type, name: []const u8, node: *ast.Node) !?*cy.Sym {
    if (try c.getResolvedSym(@ptrCast(rec_t.sym()), name, node)) |sym| {
        return sym;
    }
    if (try getRefLikeChildSym(c, rec_t, name, node)) |sym| {
        return sym;
    }
    return null;
}

pub fn accessResolvedSymOrFail(c: *cy.Chunk, rec_t: *cy.Type, name: []const u8, node: *ast.Node) !*cy.Sym {
    return (try c.accessResolvedSym(rec_t, name, node)) orelse {
        const type_name = try c.sema.allocTypeName(rec_t);
        defer c.alloc.free(type_name);
        return c.reportErrorFmt("Can not find the symbol `{}` for `{}`.", &.{v(name), v(type_name)}, node);
    };
}

pub fn getRefLikeChildSym(c: *cy.Chunk, type_: *cy.Type, name_: []const u8, node: *ast.Node) !?*cy.Sym {
    if (type_.kind() == .pointer) {
        const ptr_t = type_.cast(.pointer);
        if (ptr_t.child_t.kind() != .pointer) {
            const child_sym = ptr_t.child_t.sym();
            if (try c.getResolvedSym(@ptrCast(child_sym), name_, node)) |sym| {
                return sym;
            }
        }
    } else if (type_.kind() == .borrow) {
        const child_t = type_.cast(.borrow).child_t;
        if (try c.getResolvedSym(@ptrCast(child_t.sym()), name_, node)) |sym| {
            return sym;
        }
    } else if (type_.kind() == .ex_borrow) {
        const child_t = type_.cast(.ex_borrow).child_t;
        if (try c.getResolvedSym(@ptrCast(child_t.sym()), name_, node)) |sym| {
            return sym;
        }
    } else if (type_.kind() == .borrow_trait) {
        const trait = type_.cast(.borrow_trait).generic;
        if (try c.getResolvedSym(@ptrCast(trait.base.sym()), name_, node)) |sym| {
            return sym;
        }
    } else if (type_.kind() == .ref_trait) {
        const trait = type_.cast(.ref_trait).generic;
        if (try c.getResolvedSym(@ptrCast(trait.base.sym()), name_, node)) |sym| {
            return sym;
        }
    }
    return null;
}

test "module internals" {
    try t.eq(@offsetOf(Module, "symMap"), @offsetOf(vmc.Module, "symMap"));
    try t.eq(@offsetOf(Module, "overloadedFuncMap"), @offsetOf(vmc.Module, "overloadedFuncMap"));
}