const std = @import("std");
const builtin = @import("builtin");
const cy = @import("cyber.zig");
const C = @import("capi.zig");
const vmc = cy.vmc;
const bt = cy.types.BuiltinTypes;
const stdx = @import("stdx");
const t = stdx.testing;
const log = cy.log.scoped(.sym);
const fmt = cy.fmt;
const v = fmt.v;
const ast = cy.ast;
const ir = cy.ir;
const sema = cy.sema;

pub const SymType = enum(u8) {
    null,
    userVar,
    const_,
    hostVar,
    extern_var,
    func,
    type,
    ct_value,
    module_alias,
    use_alias,

    // One difference for type alias over `use` is it defaults to public visibility.
    // Another is that you should be able to have a const body that returns a type since functions can't return types.
    // Also, type alias can be a template while `use` cannot.
    type_alias,
    type_const,
    chunk,
    enum_case,
    choice_case,
    union_case,

    func_template,
    template,
    variant_func,
    variant_member,
    field,
};

/// Base symbol. All symbols stem from the same header.
/// Some symbols can contain their own `cy.Module`.
pub const Sym = extern struct {
    parent: ?*Sym align(8),
    type: SymType,
    metadata: u16,
    nameLen: u16,
    namePtr: [*]const u8,

    pub fn init(symT: SymType, parent: ?*Sym, name_: []const u8) Sym {
        return .{
            .namePtr = name_.ptr,
            .nameLen = @intCast(name_.len),
            .metadata = @bitCast(Metadata{ .padding = undefined, .resolving = false, .private = false }),
            .type = symT,
            .parent = parent,
        };
    }

    pub fn deinit(self: *Sym, alloc: std.mem.Allocator) void {
        _ = self;
        _ = alloc;
    }

    pub fn toC(self: *Sym) *C.Sym {
        return @ptrCast(self);
    }
    pub fn fromC(sym: ?*C.Sym) *Sym {
        return @ptrCast(@alignCast(sym));
    }

    pub fn destroy(self: *Sym, heap: *cy.Heap, alloc: std.mem.Allocator) void {
        self.deinit(alloc);
        switch (self.type) {
            .ct_value => {
                const ct_value = self.cast(.ct_value);
                alloc.destroy(ct_value);
            },
            .type => {
                const type_sym = self.cast(.type);
                type_sym.getMod().deinit(alloc);
                type_sym.deinit(alloc);
                alloc.destroy(type_sym);
            },
            .chunk => {
                const chunk_ = self.cast(.chunk);
                chunk_.getMod().deinit(alloc);
                alloc.destroy(chunk_);
            },
            .func_template => {
                const template = self.cast(.func_template);
                template.deinit(alloc);
                alloc.destroy(template);
            },
            .template => {
                const template = self.cast(.template);
                deinitTemplate(heap, template);
                alloc.destroy(template);
            },
            .variant_func => {
                const func = self.cast(.variant_func);
                func.deinit(alloc);
                alloc.destroy(func);
            },
            .variant_member => {
                const member = self.cast(.variant_member);
                alloc.destroy(member);
            },
            .type_alias => {
                alloc.destroy(self.cast(.type_alias));
            },
            .type_const => {
                alloc.destroy(self.cast(.type_const));
            },
            .use_alias => {
                alloc.destroy(self.cast(.use_alias));
            },
            .module_alias => {
                alloc.destroy(self.cast(.module_alias));
            },
            .func => {
                alloc.destroy(self.cast(.func));
            },
            .const_ => {
                const const_ = self.cast(.const_);
                alloc.destroy(const_);
            },
            .userVar => {
                const user_var = self.cast(.userVar);
                user_var.deinit(alloc);
                alloc.destroy(user_var);
            },
            .hostVar => {
                alloc.destroy(self.cast(.hostVar));
            },
            .extern_var => {
                alloc.destroy(self.cast(.extern_var));
            },
            .enum_case => {
                const impl = self.cast(.enum_case);
                alloc.destroy(impl);
            },
            .choice_case => {
                const impl = self.cast(.choice_case);
                alloc.destroy(impl);
            },
            .union_case => {
                const impl = self.cast(.union_case);
                alloc.destroy(impl);
            },
            .field => {
                const impl = self.cast(.field);
                alloc.destroy(impl);
            },
            .null => {},
        }
    }

    pub fn getMetadata(self: *Sym) *Metadata {
        return @ptrCast(&self.metadata);
    }

    pub fn setResolving(self: *Sym, resolving: bool) void {
        var metadata = self.getMetadata();
        metadata.resolving = resolving;
        self.metadata = @bitCast(metadata);
    }

    pub fn instance(self: *Sym) ?*cy.Instance {
        switch (self.type) {
            .type       => return self.cast(.type).instance,
            .ct_value   => return self.cast(.ct_value).instance,
            .type_alias => return self.cast(.type_alias).instance,
            .type_const => return self.cast(.type_const).instance,
            else => return null,
        }
    }

    pub fn isMutableValue(self: Sym) bool {
        return self.type == .userVar or self.type == .hostVar;
    }

    pub fn isValue(self: Sym) bool {
        return self.type == .userVar or self.type == .hostVar or self.type == .const_;
    }

    pub fn isType(self: *Sym) bool {
        switch (self.type) {
            .use_alias => return self.cast(.use_alias).sym.isType(),
            .type_alias,
            .type_const,
            .type => {
                return true;
            },
            .ct_value,
            .null,
            .const_,
            .extern_var,
            .userVar,
            .hostVar,
            .module_alias,
            .func,
            .func_template,
            .variant_func,
            .variant_member,
            .template,
            .field,
            .enum_case,
            .choice_case,
            .union_case,
            .chunk => {
                return false;
            },
        }
    }

    pub fn name(self: Sym) []const u8 {
        return self.namePtr[0..self.nameLen];
    }

    // TODO: Rename to `as`.
    pub fn cast(self: *Sym, comptime symT: SymType) *SymChild(symT) {
        if (cy.Trace) {
            if (symT != self.type) {
                cy.panicFmt("Invalid sym cast from {} to {}", .{self.type, symT});
            }
        }
        return @ptrCast(self);
    }

    pub fn isDistinctAssumeResolved(self: *Sym) bool {
        if (cy.Trace) {
            switch (self.type) {
                .null,
                .module_alias,
                .use_alias => {
                    @panic("Unexpected");
                },
                else => {},
            }
        }
        if (self.type == .func) {
            const func = self.cast(.func);
            return func.numFuncs == 1;
        }
        return true;
    }

    pub fn getMod(self: *Sym) ?*cy.Module {
        switch (self.type) {
            .chunk           => return @ptrCast(&self.cast(.chunk).mod),
            .type            => return @ptrCast(&self.cast(.type).mod),
            .template        => return @ptrCast(&self.cast(.template).mod),
            .func_template   => return @ptrCast(&self.cast(.func_template).mod),
            .type_const      => return @ptrCast(&self.cast(.type_const).mod),
            .ct_value,
            .type_alias,
            .use_alias,
            .module_alias,
            .enum_case,
            .choice_case,
            .union_case,
            .func,
            .variant_func,
            .variant_member,
            .const_,
            .userVar,
            .field,
            .extern_var,
            .hostVar => {
                return null;
            },
            .null => cy.unexpected(),
        }
    }

    pub fn getRootChunk(self: *Sym) *cy.Chunk {
        if (self.parent) |parent| {
            return parent.getRootChunk();
        } else {
            return self.cast(.chunk).chunk;
        }
    }

    pub fn getRootMod(self: *Sym) *cy.Module {
        if (self.parent) |parent| {
            return parent.getRootMod();
        } else {
            return self.getMod().?;
        }
    }

    pub fn getFirstFunc(self: *Sym) ?*Func {
        switch (self.type) {
            .func => {
                return self.cast(.func).first;
            },
            else => return null,
        }
    }

    pub fn getValueType(self: *Sym) ?*cy.Type {
        switch (self.type) {
            .extern_var => {
                return self.cast(.extern_var).type;
            },
            .hostVar => {
                return self.cast(.hostVar).type;
            },
            .userVar => {
                return self.cast(.userVar).type;
            },
            else => {
                return null;
            },
        }
    }

    pub fn getStaticType(self: *Sym) ?*cy.Type {
        switch (self.type) {
            .type            => return self.cast(.type).type,
            .use_alias       => return self.cast(.use_alias).sym.getStaticType(),
            .type_alias      => return self.cast(.type_alias).sym.type,
            .type_const      => return self.cast(.type_const).type,
            .null,
            .field,
            .func_template,
            .template,
            .variant_func,
            .variant_member,
            .enum_case,
            .choice_case,
            .union_case,
            .func,
            .module_alias,
            .chunk,
            .hostVar,
            .extern_var,
            .ct_value,
            .const_,
            .userVar         => return null,
        }
    }

    pub fn resolved(self: *Sym) *Sym {
        if (self.type == .module_alias) {
            return self.cast(.module_alias).sym;
        } else if (self.type == .use_alias) {
            return self.cast(.use_alias).sym;
        } else {
            return self;
        }
    }

    pub fn dump(self: *Sym, opts: SymDumpOptions) !void {
        if (!cy.Trace) return;
        const path = try self.writeAbsPathToBuf(&cy.tempBuf);
        fmt.printStderr("{}{} {}\n", &.{fmt.repeat(' ', opts.indent), v(path), v(@tagName(self.type))});
        if (opts.dumpModule) {
            if (self.getMod()) |mod| {
                for (mod.syms.items) |childSym| {
                    try childSym.dump(.{ .indent = 2, .dumpModule = false });
                }
            }
        }
    }

    pub fn declNode(self: *Sym) *ast.Node {
        switch (self.type) {
            .type_alias => {
                if (self.cast(.type_alias).decl) |decl| {
                    return @ptrCast(decl);
                }
            },
            .type_const => {
                return @ptrCast(self.cast(.type_const).decl);
            },
            .use_alias => {
                if (self.cast(.use_alias).decl) |decl| {
                    return @ptrCast(decl);
                }
            },
            .const_ => {
                if (self.cast(.const_).decl) |decl| {
                    return @ptrCast(decl);
                }
            },
            .userVar => {
                if (self.cast(.userVar).decl) |decl| {
                    return @ptrCast(decl);
                }
            },
            .hostVar => {
                if (self.cast(.hostVar).decl) |decl| {
                    return @ptrCast(decl);
                }
            },
            .extern_var => {
                if (self.cast(.extern_var).decl) |decl| {
                    return @ptrCast(decl);
                }
            },
            .type => {
                if (self.cast(.type).decl) |decl| {
                    return @ptrCast(decl);
                }
            },
            .chunk => {
                if (self.cast(.chunk).chunk.ast.root) |decl| {
                    return @ptrCast(decl);
                }
            },
            .template => {
                const decl = self.cast(.template).decl;
                return @ptrCast(decl);
            },
            .func_template => {
                const decl = self.cast(.func_template).decl;
                return @ptrCast(decl);
            },
            .func => {
                const decl = self.cast(.func).first.decl;
                return @ptrCast(decl);
            },
            else => {},
        }
        std.debug.panic("Unsupported: {}", .{self.type});
    }
};

const Metadata = packed struct {
    /// Used to detect circular reference when resolving.
    resolving: bool,

    private: bool,

    padding: u14,
};

const SymDumpOptions = struct {
    indent: u32 = 0,
    dumpModule: bool = false,
};

pub fn createSym(alloc: std.mem.Allocator, comptime symT: SymType, init: SymChild(symT)) !*SymChild(symT) {
    const sym = try alloc.create(SymChild(symT));
    sym.* = init;
    return sym;
}

fn SymChild(comptime symT: SymType) type {
    return switch (symT) {
        .func => FuncSym,
        .userVar => UserVar,
        .const_ => Const,
        .hostVar => HostVar,
        .extern_var => ExternVar,
        .type => TypeSym,
        .ct_value => CtValue,
        .enum_case => EnumCase,
        .choice_case => ChoiceCase,
        .union_case => UnionCase,
        .template => Template,
        .variant_func => VariantFunc,
        .variant_member => VariantMember,
        .func_template => FuncTemplate,
        .field => Field,
        .use_alias => UseAlias,
        .type_alias => TypeAlias,
        .type_const => TypeConst,
        .module_alias => ModuleAlias,
        .chunk => Chunk,
        .null => void,
    };
}

pub const ExternVar = extern struct {
    head: Sym,
    decl: ?*ast.GlobalDecl,
    type: *cy.Type,

    extern_name_ptr: [*]const u8,
    extern_name_len: usize,

    resolved: bool,

    pub fn externName(self: *ExternVar) []const u8 {
        return self.extern_name_ptr[0..self.extern_name_len];
    }
};

pub const HostVar = extern struct {
    head: Sym,
    decl: ?*ast.GlobalDecl,
    type: *cy.Type,

    ir: ?*ir.Expr,

    pub fn isResolved(self: *HostVar) bool {
        return self.ir != null;
    }
};

pub const UserVar = struct {
    head: Sym,
    decl: ?*ast.GlobalDecl,
    resolved: bool,
    type: *cy.Type,

    /// Whether init statement was emitted.
    emitted: bool,

    /// Used to detect circular reference while resolving `ir`.
    resolving_init: bool,

    pub fn isResolved(self: *UserVar) bool {
        return self.resolved;
    }

    pub fn deinit(self: *UserVar, alloc: std.mem.Allocator) void {
        _ = self;
        _ = alloc;
    }
};

pub const Const = struct {
    head: Sym,
    decl: ?*ast.ConstDecl,
    resolved: bool,
    type: *cy.Type,
    value: cy.Value,

    /// Exclusive compile-time types such as `GenericStr` will not have IR.
    ir: ?*ir.Expr,

    /// Used to detect circular reference while resolving `ir`.
    resolving_init: bool,

    pub fn isResolved(self: *Const) bool {
        return self.resolved;
    }
};

/// Sym that links to overloaded functions.
pub const FuncSym = extern struct {
    head: Sym,
    first: *Func,
    last: *Func,
    numFuncs: u16,

    /// Duped to perform uniqueness check without dereferencing the first ModuleFunc.
    firstFuncSig: *cy.FuncSig,

    pub fn isResolved(self: *FuncSym) bool {
        return self.numFuncs > 0;
    }

    pub fn addFunc(self: *FuncSym, func: *Func) void {
        if (self.numFuncs == 0) {
            // First func for sym.
            self.numFuncs = 1;
            self.first = func;
            self.last = func;
        } else {
            // Attach to end.
            self.last.next = func;
            self.last = func;
            self.numFuncs += 1;
        }
    }

    pub fn get_func(self: *FuncSym, i: usize) ?*Func {
        var cur: ?*Func = self.first;
        var cur_i: usize = 0;
        while (cur_i < i) {
            cur = cur.?.next orelse {
                return null;
            };
            cur_i += 1;
        }
        return cur;
    }
};

pub const WithCstr = struct {
    name: []const u8,
    cstr_t: *cy.Type,
};

pub const VariantFuncDecl = extern struct {
    decl: *ast.FuncDecl,

    with_cstrs: cy.ast.ConstSlice(WithCstr),

    resolved: bool,
};

/// Can hold overloaded funcs.
pub const VariantFunc = extern struct {
    head: Sym,
    decl: VariantFuncDecl,
    rest_: ZArrayList, 

    pub fn rest(self: *VariantFunc) *std.ArrayListUnmanaged(VariantFuncDecl) {
        return @ptrCast(&self.rest_);
    }

    pub fn deinit(self: *VariantFunc, alloc: std.mem.Allocator) void {
        alloc.free(self.decl.with_cstrs.slice());
        for (self.rest().items) |decl| {
            alloc.free(decl.with_cstrs.slice());
        }
        self.rest().deinit(alloc);
    }
};

pub const VariantMember = extern struct {
    head: Sym,
    decl: *ast.Node,
};

/// This is similar to SymType but has custom_t.
pub const TemplateType = enum {
    struct_t,
    enum_t,
    trait_t,
    custom_t,
    type_alias,
    type_const,
};

/// TODO: Consider splitting into Template and FuncTemplate.
pub const Template = struct {
    head: Sym,
    decl: *ast.TemplateDecl,

    kind: TemplateType,

    /// Owned.
    params: []TemplateParam,

    /// Template args to variant. Keys are not owned.
    instance_cache: std.HashMapUnmanaged(cy.template.VariantKey, *cy.Instance, cy.template.VariantKeyContext, 80),
    instances: std.ArrayListUnmanaged(*cy.Instance),

    mod: vmc.Module,

    pub fn isResolved(self: *Template) bool {
        return self.params.len > 0;
    }

    pub fn getMod(self: *Template) *cy.Module {
        return @ptrCast(&self.mod);
    }

    pub fn getExpandedSymFrom(self: *Template, from_template: *Template, from: *Sym) *Sym {
        if (self == from_template) {
            return from;
        }
        const parent = self.head.parent.?.cast(.template).getExpandedSymFrom(from_template, from);
        return parent.getMod().?.getSym(self.head.name()).?;
    }

    pub fn indexOfParam(self: *Template, name: []const u8) ?u32 {
        for (self.params, 0..) |param, i| {
            if (std.mem.eql(u8, name, param.name)) {
                return @intCast(i);
            }
        }
        return null;
    }
};

pub fn deinit_sym_ct_values(heap: *cy.Heap, sym: *Sym) void {
    switch (sym.type) {
        .const_ => {
            cy.sym.deinitConstValue(heap, sym.cast(.const_));
        },
        .type => {
            const _type = sym.cast(.type).type;
            if (_type.kind() == .struct_t) {
                const struct_t = _type.cast(.struct_t);
                if (!struct_t.base.info.resolved) {
                    return;
                }
                for (struct_t.fields()) |field| {
                    if (field.init_n) |_| {
                        heap.destructValue2(field.type, field.init_value);
                    }
                }
            }
        },
        .func_template => {
            const template = sym.cast(.func_template);
            for (template.instances.items) |variant| {
                cy.Instance.deinitValues(heap, variant);
            }
        },
        else => {},
    }
}

pub fn deinit_func_ct_values(heap: *cy.Heap, func: *Func) void {
    if (func.type == .generic or func.type == .generic_sema) {
        const template = func.data.generic;
        for (template.instances.items) |variant| {
            cy.Instance.deinitValues(heap, variant);
        }
    }
}

pub fn deinitConstValue(heap: *cy.Heap, const_: *Const) void {
    if (const_.resolved) {
        heap.destructValue2(const_.type, const_.value);
    }
}

fn deinitTemplate(heap: *cy.Heap, template: *Template) void {
    template.getMod().deinit(heap.alloc);

    for (template.instances.items) |variant| {
        cy.Instance.deinitValues(heap, variant);
        heap.alloc.destroy(variant);
    }
    template.instances.deinit(heap.alloc);
    template.instance_cache.deinit(heap.alloc);
    heap.alloc.free(template.params);
}

pub const TemplateParam = struct {
    name: []const u8,
};

pub const VariantParam = struct {
    name: []const u8,
    value: cy.Value,
};

pub const Field = extern struct {
    head: Sym,

    idx: u32,
    type: *cy.Type,
};

const CtValue = extern struct {
    head: Sym,
    decl: ?*ast.Node,
    instance: *cy.Instance,
};

pub const TypeConst = extern struct {
    head: Sym,
    decl: *ast.TypeConstDecl,
    type: *cy.Type,
    mod: vmc.Module,
    instance: ?*cy.Instance,
    resolved: bool,

    pub fn getMod(self: *TypeConst) *cy.Module {
        return @ptrCast(&self.mod);
    }
};

/// Generic type symbol.
pub const TypeSym = struct {
    head: Sym,
    decl: ?*ast.Node,
    type: *cy.Type,
    mod: vmc.Module,
    impls_: ZArrayList,
    instance: ?*cy.Instance,

    pub fn deinit(self: *TypeSym, alloc: std.mem.Allocator) void {
        self.impls().deinit(alloc);
    }

    pub fn getMod(self: *TypeSym) *cy.Module {
        return @ptrCast(&self.mod);
    }

    pub fn impls(self: *TypeSym) *std.ArrayListUnmanaged(*ast.ImplDecl) {
        return @ptrCast(&self.impls_);
    }

    pub fn has_decl(self: *TypeSym, name: []const u8) bool {
        if (self.instance) |variant| {
            return variant.getSymTemplate().getMod().getSym(name) != null;
        } else {
            return self.getMod().getSym(name) != null;
        }
    }
};

const ZArrayList = extern struct {
    items_ptr: ?*anyopaque = null,
    items_len: usize = 0,
    cap: usize = 0,
};

const ExtendEntry = struct {
    decl: *ast.ExtendDecl,
    trait: ?*cy.Type,
};

pub const EnumCase = extern struct {
    head: Sym,
    type: *cy.Type,
    val: u32,
};

pub const ChoiceCase = extern struct {
    head: Sym,
    type: *cy.Type,
    val: u32,
    payload_t: *cy.Type,
    idx: u32,
};

pub const UnionCase = extern struct {
    head: Sym,
    type: *cy.Type,
    payload_t: *cy.Type,
    idx: u32,
};

pub const TypeAlias = extern struct {
    head: Sym,

    decl: ?*ast.TypeAliasDecl,
    sym: *TypeSym,
    instance: ?*cy.Instance,
    resolved: bool,
};

pub const ModuleAlias = extern struct {
    head: Sym,
    declId: *ast.Node,
    sym: *Sym,
};

pub const UseAlias = extern struct {
    head: Sym,

    // UseAlias or ImportStmt (when doing `use * <spec>`)
    decl: ?*ast.Node,
    sym: *Sym,
    resolved: bool,

    // When true, resolve with `sym` rather than `decl`.
    resolve_with_sym: bool = false,
};

pub const Chunk = extern struct {
    head: Sym,
    chunk: *cy.Chunk,
    mod: vmc.Module,

    pub fn sym(self: *Chunk) *Sym {
        return @ptrCast(self);
    }

    pub fn getMod(self: *Chunk) *cy.Module {
        return @ptrCast(&self.mod);
    }
};

pub const FuncKind = enum {
    /// Unresolved until signature is resolved to determine the actual FuncKind.
    /// Can derive to `template`, `hostFunc`, `host_ct`, or `userFunc`.
    unresolved,

    /// Invoked at runtime.
    hostFunc,

    /// Invoked during sema.
    /// TODO: rename to `host_builtin`.
    /// TODO: Force attribute `#builtin` to be declared.
    host_ct,
    host_sema,

    userFunc,
    userLambda,
    trait,
    extern_,
    vm_extern_variant,
    generic,
    generic_sema,
};

const FuncInfo = packed struct {
    is_method: bool,

    /// A generator function has a signature that returns ^Generator[T]
    /// but it actually yields the next type `Option[S]`.
    generator: bool,

    /// Whether it has already emitted IR.
    emitted: bool,

    resolved: bool = false,

    /// Declared with `#fn`. Required if the signature contains types that can only exist at compile-time.
    /// Generic functions will raise an error during callsite resolving if it encounters a compile-time type,
    /// unless the template was declared with `#fn`.
    const_eval_only: bool = false,

    /// Whether to perform codegen for this function. Can be false after DCE.
    gen: bool = true,

    /// For visitors such as DCE.
    visited: bool = false,

    extern_: bool,

    /// When > 0, indicates to the applicable backend that calls to this function can be replaced with an intrinsic.
    gen_intrinsic: u2 = 0,

    padding: u6 = undefined,
};

pub const FuncConfig = struct {
    is_method: bool,
    extern_: bool,
};

pub const Func = struct {
    type: FuncKind,
    next: ?*Func,

    /// For non-lambdas, this can refer to a FuncSym or Template.
    parent: *Sym,

    sig: *cy.FuncSig,
    // FuncDecl/LambdaExpr
    decl: ?*ast.Node,

    data: union {
        host_sema: struct {
            ptr: ?cy.ZSemaFn,
        },
        host_ct: struct {
            ptr: ?cy.ZCtFn,
            eval: ?cy.ZCtEvalFuncFn,
        },
        hostFunc: struct {
            // NOTE: inline `cy.ZHostFn` to avoid `dependency loop detected` bug.
            ptr: *const fn(*cy.VM) callconv(.c) C.Ret,
            eval: ?cy.ZCtEvalFuncFn = null,
        },
        trait: struct {
            vtable_idx: u32,
        },
        userFunc: struct {
            /// Currently used to invalidate the IR func block when removing temporary functions.
            /// See `cte.expandValueTemplate`.
            loc: *ir.Stmt,
            eval: ?cy.ZCtEvalFuncFn = null,
        },
        userLambda: struct {
            func_block_ir: *ir.Stmt,
            is_closure: bool,
        },
        extern_: *ExternFunc,
        vm_extern_variant: struct {
            extern_func: *cy.Func,
        },
        generic: *FuncTemplate, // A Sym struct so that variants can have a parent sym.
        unresolved: void,
    },
    instance: ?*cy.Instance,
    info: FuncInfo,

    pub fn destroy(self: *Func, alloc: std.mem.Allocator) void {
        if (self.type == .generic or self.type == .generic_sema) {
            self.data.generic.deinit(alloc);
            alloc.destroy(self.data.generic);
        } else if (self.type == .extern_) {
            alloc.destroy(self.data.extern_);
        }
        alloc.destroy(self);
    }

    pub fn isResolved(self: Func) bool {
        return self.info.resolved;
    }

    pub fn isStatic(self: Func) bool {
        return self.type != .userLambda;
    }

    pub fn isMethod(self: Func) bool {
        return self.info.is_method;
    }

    pub fn hasStaticInitializer(self: Func) bool {
        return self.type == .hostFunc;
    }

    pub fn src(self: *Func) u32 {
        return self.decl.?.src();
    }

    pub fn parent_mod_sym(self: *Func) *cy.Sym {
        // TODO: Handle more cases.
        return self.parent.parent.?;
    }

    pub fn name(self: *Func) []const u8 {
        if (self.type == .userLambda) {
            return "lambda";
        }
        return self.parent.name();
    }
};

pub const ExternFunc = struct {
    name_ptr: [*]const u8,
    name_len: u32,
    vm_variadic: bool,

    /// Whether there is a function body.
    has_impl: bool,
    ir: *ir.FuncBlock,

    pub fn externName(self: *ExternFunc) []const u8 {
        return self.name_ptr[0..self.name_len];
    }
};

pub const FuncTemplateParam = struct {
    /// Trait types would be anonymous parameters.
    anonymous: bool,

    name: []const u8,

    // Function template. Which parameter contains the type spec.
    type_idx: u32,

    pub fn deinit(self: *const FuncTemplateParam, alloc: std.mem.Allocator) void {
        if (self.anonymous) {
            alloc.free(self.name);
        }
    }
};

/// Function templates or generic functions.
pub const FuncTemplate = struct {
    head: Sym,

    // FuncDecl or TemplateDecl
    decl: *ast.Node,

    sig: *cy.FuncSig,

    func_params: []const *ast.FuncParam,
    func_ret: ?*ast.Node,

    with_cstrs: []const WithCstr,

    params: []FuncTemplateParam,

    resolved: bool,

    /// Generic function if callable.
    callable: bool,

    /// Template args to variant. Keys are not owned.
    instance_cache: std.HashMapUnmanaged(cy.template.VariantKey, *cy.Instance, cy.template.VariantKeyContext, 80),
    instances: std.ArrayListUnmanaged(*cy.Instance),

    mod: vmc.Module,

    pub fn deinit(self: *FuncTemplate, alloc: std.mem.Allocator) void {
        for (self.instances.items) |variant| {
            alloc.destroy(variant);
        }
        self.instances.deinit(alloc);
        self.instance_cache.deinit(alloc);

        alloc.free(self.with_cstrs);

        for (self.params) |param| {
            param.deinit(alloc);
        }
        alloc.free(self.params);
    }

    pub fn isResolved(self: *FuncTemplate) bool {
        return self.resolved;
    }

    pub fn getMod(self: *FuncTemplate) *cy.Module {
        return @ptrCast(&self.mod);
    }

    pub fn indexOfParam(self: *FuncTemplate, name: []const u8) ?usize {
        for (self.params, 0..) |param, i| {
            if (std.mem.eql(u8, param.name, name)) {
                return i;
            }
        }
        return null;
    }
};

pub fn getSymValueType(c: *cy.Chunk, sym: *Sym) !?*cy.Type {
    switch (sym.type) {
        .userVar    => return sym.cast(.userVar).type,
        .hostVar    => return sym.cast(.hostVar).type,
        .extern_var => return sym.cast(.extern_var).type,
        .const_     => return sym.cast(.const_).type,
        .enum_case => return null,
        .choice_case => {
            return sym.cast(.choice_case).type;
        },
        .union_case => {
            return sym.cast(.union_case).type;
        },
        .type_alias  => return getSymValueType(c, @ptrCast(sym.cast(.type_alias).sym)),
        .use_alias  => return getSymValueType(c, sym.cast(.use_alias).sym),
        .type_const => return c.sema.type_t,
        .type => return c.sema.type_t,
        .func => {
            const func = sym.cast(.func);
            if (func.numFuncs == 1) {
                return try cy.sema.getFuncPtrType(c, func.first.sig);
            } else {
                return error.AmbiguousSymbol;
            }
        },
        .ct_value,
        .field,
        .null,
        .module_alias,
        .func_template,
        .variant_func,
        .variant_member,
        .template,
        .chunk => return null,
    }
}

pub fn createCtValue(c: *cy.Chunk, parent: *Sym, name: []const u8, variant: *cy.Instance, decl: ?*ast.Node) !*CtValue {
    const sym = try createSym(c.alloc, .ct_value, .{
        .head = Sym.init(.ct_value, parent, name),
        .decl = decl,
        .instance = variant,
    });
    try c.syms.append(c.alloc, @ptrCast(sym));
    return sym;
}

pub fn createTypeSym(c: *cy.Chunk, parent: *Sym, name: []const u8, opt_type: ?*cy.Type, decl: ?*ast.Node) !*TypeSym {
    const sym = try createSym(c.alloc, .type, .{
        .head = Sym.init(.type, parent, name),
        .decl = decl,
        .type = opt_type orelse undefined,
        .instance = null,
        .impls_ = .{},
        .mod = undefined,
    });
    if (opt_type) |type_| {
        type_.sym_ = sym;
    }
    @as(*cy.Module, @ptrCast(&sym.mod)).* = cy.Module.init();
    try c.syms.append(c.alloc, @ptrCast(sym));
    return sym;
}

pub fn createField(c: *cy.Chunk, parent: *Sym, name: []const u8, idx: usize, type_: *cy.Type) !*Field {
    const sym = try createSym(c.alloc, .field, .{
        .head = Sym.init(.field, parent, name),
        .idx = @intCast(idx),
        .type = type_,
    });
    try c.syms.append(c.alloc, @ptrCast(sym));
    return sym;
}

pub fn createEnumCase(c: *cy.Chunk, parent: *Sym, name: []const u8, type_: *cy.Type, val: u32) !*EnumCase {
    const sym = try createSym(c.alloc, .enum_case, .{
        .head = Sym.init(.enum_case, parent, name),
        .type = type_,
        .val = val,
    });
    try c.syms.append(c.alloc, @ptrCast(sym));
    return sym;
}

pub fn createChoiceCase(c: *cy.Chunk, parent: *Sym, name: []const u8, type_: *cy.Type,
    val: u32, idx: u32, payload_t: *cy.Type) !*ChoiceCase {
    const sym = try createSym(c.alloc, .choice_case, .{
        .head = Sym.init(.choice_case, parent, name),
        .type = type_,
        .val = val,
        .payload_t = payload_t,
        .idx = idx,
    });
    try c.syms.append(c.alloc, @ptrCast(sym));
    return sym;
}

pub fn createUnionCase(c: *cy.Chunk, parent: *Sym, name: []const u8, type_: *cy.Type,
    idx: u32, payload_t: *cy.Type) !*UnionCase {
    const sym = try createSym(c.alloc, .union_case, .{
        .head = Sym.init(.union_case, parent, name),
        .type = type_,
        .payload_t = payload_t,
        .idx = idx,
    });
    try c.syms.append(c.alloc, @ptrCast(sym));
    return sym;
}

pub fn createConst(c: *cy.Chunk, parent: *Sym, name: []const u8, decl: ?*ast.ConstDecl) !*Const {
    const sym = try createSym(c.alloc, .const_, .{
        .head = Sym.init(.const_, parent, name),
        .decl = decl,
        .type = undefined,
        .ir = undefined,
        .value = undefined,
        .resolved = false,
        .resolving_init = false,
    });
    try c.syms.append(c.alloc, @ptrCast(sym));
    return sym;
}

pub fn createUserVar(c: *cy.Chunk, parent: *Sym, name: []const u8, decl: ?*ast.GlobalDecl) !*UserVar {
    const sym = try createSym(c.alloc, .userVar, .{
        .head = Sym.init(.userVar, parent, name),
        .decl = decl,
        .type = undefined,
        .resolved = false,
        .emitted = false,
        .resolving_init = false,
    });
    try c.syms.append(c.alloc, @ptrCast(sym));
    return sym;
}

pub fn createHostVar(c: *cy.Chunk, parent: *Sym, name: []const u8, decl: ?*ast.GlobalDecl) !*HostVar {
    const sym = try createSym(c.alloc, .hostVar, .{
        .head = Sym.init(.hostVar, parent, name),
        .ir = null,
        .decl = decl,
        .type = &cy.types.NullType,
    });
    try c.syms.append(c.alloc, @ptrCast(sym));
    return sym;
}

pub fn createExternVar(c: *cy.Chunk, parent: *Sym, name: []const u8, decl: ?*ast.GlobalDecl) !*ExternVar {
    const sym = try createSym(c.alloc, .extern_var, .{
        .head = Sym.init(.extern_var, parent, name),
        .decl = decl,
        .type = &cy.types.NullType,
        .extern_name_ptr = undefined,
        .extern_name_len = 0,
        .resolved = false,
    });
    try c.syms.append(c.alloc, @ptrCast(sym));
    return sym;
}

pub fn createFuncTemplate(c: *cy.Chunk, parent: *Sym, name: []const u8, callable: bool, decl: *ast.TemplateDecl) !*FuncTemplate {
    const func_decl = decl.child_decl.cast(.funcDecl);
    const sym = try createSym(c.alloc, .func_template, .{
        .head = Sym.init(.func_template, parent, name),
        .func_params = func_decl.params.slice(),
        .func_ret = func_decl.ret,
        .with_cstrs = &.{},
        .params = &.{},
        .sig = undefined,
        .resolved = false,
        .callable = callable,
        .decl = @ptrCast(decl),
        .instance_cache = .{},
        .instances = .{},
        .mod = undefined,
    });
    sym.getMod().* = cy.Module.init();
    try c.syms.append(c.alloc, @ptrCast(sym));
    return sym;
}

pub fn createTemplate(c: *cy.Chunk, parent: *Sym, name: []const u8,
    kind: TemplateType, decl: *ast.TemplateDecl) !*Template {
    const sym = try createSym(c.alloc, .template, .{
        .head = Sym.init(.template, parent, name),
        .kind = kind,
        .decl = decl,
        .params = &.{},
        .instance_cache = .{},
        .instances = .{},
        .mod = undefined,
    });
    sym.getMod().* = cy.Module.init();
    try c.syms.append(c.alloc, @ptrCast(sym));
    return sym;
}

pub fn createChunkSym(c: *cy.Chunk, name: []const u8) !*Chunk {
    const sym = try createSym(c.alloc, .chunk, .{
        .head = Sym.init(.chunk, null, name),
        .chunk = c,
        .mod = undefined,
    });
    sym.head.getMetadata().private = true;
    @as(*cy.Module, @ptrCast(&sym.mod)).* = cy.Module.init();
    try c.syms.append(c.alloc, @ptrCast(sym));
    return sym;
}

pub fn createVariantFunc(c: *cy.Chunk, parent: *Sym, name: []const u8, decl: *ast.FuncDecl) !*VariantFunc {
    const sym = try createSym(c.alloc, .variant_func, .{
        .head = Sym.init(.variant_func, parent, name),
        .decl = .{
            .decl = decl,
            .with_cstrs = .{ .ptr = undefined, .len = 0 },
            .resolved = false,
        },
        .rest_ = .{},
    });
    try c.syms.append(c.alloc, @ptrCast(sym));
    return sym;
} 

pub fn createVariantMember(c: *cy.Chunk, parent: *Sym, name: []const u8, decl: *ast.Node) !*VariantMember {
    const sym = try createSym(c.alloc, .variant_member, .{
        .head = Sym.init(.variant_member, parent, name),
        .decl = decl,
    });
    try c.syms.append(c.alloc, @ptrCast(sym));
    return sym;
} 

pub fn createTypeConst(c: *cy.Chunk, parent: *Sym, name: []const u8, private: bool, decl: *ast.TypeConstDecl) !*TypeConst {
    const sym = try createSym(c.alloc, .type_const, .{
        .head = Sym.init(.type_const, parent, name),
        .decl = decl,
        .type = undefined,
        .instance = null,
        .resolved = false,
        .mod = undefined,
    });
    sym.getMod().* = cy.Module.init();
    sym.head.getMetadata().private = private;
    try c.syms.append(c.alloc, @ptrCast(sym));
    return sym;
} 

pub fn createTypeAlias(c: *cy.Chunk, parent: *Sym, name: []const u8, private: bool, decl: ?*ast.TypeAliasDecl) !*TypeAlias {
    const sym = try createSym(c.alloc, .type_alias, .{
        .head = Sym.init(.type_alias, parent, name),
        .decl = decl,
        .sym = undefined,
        .instance = null,
        .resolved = false,
    });
    sym.head.getMetadata().private = private;
    try c.syms.append(c.alloc, @ptrCast(sym));
    return sym;
} 

pub fn createUseAlias(c: *cy.Chunk, parent: *Sym, name: []const u8, decl: ?*ast.Node) !*UseAlias {
    const sym = try createSym(c.alloc, .use_alias, .{
        .head = Sym.init(.use_alias, parent, name),
        .decl = decl,
        .sym = undefined,
        .resolved = false,
    });
    sym.head.getMetadata().private = true;
    try c.syms.append(c.alloc, @ptrCast(sym));
    return sym;
} 

pub fn createModuleAlias(c: *cy.Chunk, parent: *Sym, name: []const u8, importedSym: *Sym, declId: *ast.Node) !*ModuleAlias {
    const sym = try createSym(c.alloc, .module_alias, .{
        .head = Sym.init(.import, parent, name),
        .declId = declId,
        .sym = importedSym,
    });
    try c.syms.append(c.alloc, @ptrCast(sym));
    return sym;
}

pub fn createFunc(c: *cy.Chunk, ftype: FuncKind, parent: *Sym, config: FuncConfig, decl: ?*ast.Node) !*Func {
    const func = try c.alloc.create(Func);
    func.* = .{
        .type = ftype,
        .sig = undefined,
        .info = .{
            .generator = false,
            .is_method = config.is_method,
            .emitted = false,
            .extern_ = config.extern_,
        },
        .parent = parent,
        .instance = null,
        .decl = @ptrCast(decl),
        .next = null,
        .data = undefined,
    };
    if (ftype == .userFunc) {
        func.data = .{.userFunc = .{.loc = undefined}};
    } else if (ftype == .userLambda) {
        func.data = .{.userLambda = .{.func_block_ir = undefined, .is_closure = false}};
    }
    return func;
}

pub fn createFuncSym(c: *cy.Chunk, parent: *Sym, name: []const u8) !*FuncSym {
    const sym = try createSym(c.alloc, .func, .{
        .head = Sym.init(.func, parent, name),
        .numFuncs = 0,
        .firstFuncSig = undefined,
        .first = undefined,
        .last = undefined,
    });
    try c.syms.append(c.alloc, @ptrCast(sym));
    return sym;
}

const SymFormatConfig = struct {
    from: ?*cy.Chunk = null,
    emit_template_args: bool = true,
};

pub fn shouldWriteSym(sym: *cy.Sym, config: SymFormatConfig) bool {
    _ = config;
    if (sym.type == .chunk) {
        // const chunk = sym.cast(.chunk);
        // if (config.from) |from| {
        //     if (chunk.chunk == from) {
        //         return false;
        //     }
        // }
        return false;
    }
    return true;
}

pub fn newFuncName(s: *cy.Sema, alloc: std.mem.Allocator, func: *cy.Func, config: SymFormatConfig) ![]const u8 {
    var buf = std.Io.Writer.Allocating.init(alloc);
    defer buf.deinit();
    try writeFuncName(s, &buf.writer, func, config);
    return buf.toOwnedSlice();
}

pub fn writeFuncName(s: *cy.Sema, w: *std.Io.Writer, func: *cy.Func, config: SymFormatConfig) !void {
    if (func.parent.type == .func_template or func.parent.type == .func) {
        const parent = func.parent.parent.?;
        if (shouldWriteSym(parent, config)) {
            try writeSymName(s, w, parent, config);
            try w.writeAll(".");
        }
    } else {
        const parent = func.parent;
        if (shouldWriteSym(parent, config)) {
            try writeSymName(s, w, parent, config);
            try w.writeAll(".");
        }
    }
    try write_func_base_name(s, w, func, config);
}

pub fn write_func_base_name(s: *cy.Sema, w: *std.Io.Writer, func: *cy.Func, config: SymFormatConfig) !void {
    try w.writeAll(func.name());
    if (func.instance) |variant| {
        if (!config.emit_template_args) {
            return;
        }
        try w.writeByte('[');
        try writeLocalFuncVariantParams(s, w, variant, config);
        try w.writeByte(']');
    }
}

pub fn allocSymName(s: *cy.Sema, alloc: std.mem.Allocator, sym: *cy.Sym, config: SymFormatConfig) ![]const u8 {
    var buf = std.Io.Writer.Allocating.init(alloc);
    defer buf.deinit();
    try writeSymName(s, &buf.writer, sym, config);
    return buf.toOwnedSlice();
}

pub fn formatSymName(s: *cy.Sema, buf: []u8, sym: *Sym, config: SymFormatConfig) ![]const u8 {
    var w = std.Io.Writer.fixed(buf);
    try writeSymName(s, &w, sym, config);
    return w.buffered();
}

pub fn writeSymName(s: *cy.Sema, w: *std.Io.Writer, sym: *cy.Sym, config: SymFormatConfig) anyerror!void {
    if (sym.instance()) |variant| {
        if (sym.parent.?.parent) |parent| {
            if (shouldWriteSym(parent, config)) {
                try writeSymName(s, w, parent, config);
                try w.writeByte('.');
            }
        }
        if (!config.emit_template_args) {
            try w.writeAll(sym.name());
            try w.writeAll("[]");
            return;
        }

        if (sym.type == .type) {
            const type_sym = sym.cast(.type);
            switch (type_sym.type.id()) {
                bt.F32 => {
                    try w.writeAll("f32");
                    return;
                },
                bt.F64 => {
                    try w.writeAll("float");
                    return;
                },
                bt.I64 => {
                    try w.writeAll("int");
                    return;
                },
                bt.I16 => {
                    try w.writeAll("i16");
                    return;
                },
                bt.I32 => {
                    try w.writeAll("i32");
                    return;
                },
                bt.I8 => {
                    try w.writeAll("i8");
                    return;
                },
                bt.R8 => {
                    try w.writeAll("byte");
                    return;
                },
                bt.R16 => {
                    try w.writeAll("r16");
                    return;
                },
                bt.R32 => {
                    try w.writeAll("r32");
                    return;
                },
                bt.R64 => {
                    try w.writeAll("r64");
                    return;
                },
                else => {},
            }
        }

        const template = variant.getSymTemplate();
        if (template == s.option_tmpl) {
            try w.writeAll("?");
            const child_t = variant.params[0].asPtr(*cy.Type);
            try writeSymName(s, w, &child_t.sym().head, config);
            return;
        } else if (template == s.result_tmpl) {
            try w.writeAll("!");
            const child_t = variant.params[0].asPtr(*cy.Type);
            try writeSymName(s, w, &child_t.sym().head, config);
            return;
        } else if (template == s.borrow_tmpl) {
            try w.writeAll("&");
            const child_t = variant.params[0].asPtr(*cy.Type);
            try writeSymName(s, w, &child_t.sym().head, config);
            return;
        } else if (template == s.ex_borrow_tmpl) {
            try w.writeAll("&>");
            const child_t = variant.params[0].asPtr(*cy.Type);
            try writeSymName(s, w, &child_t.sym().head, config);
            return;
        } else if (template == s.ref_tmpl) {
            try w.writeAll("^");
            const child_t = variant.params[0].asPtr(*cy.Type);
            try writeSymName(s, w, &child_t.sym().head, config);
            return;
        } else if (template == s.span_tmpl) {
            try w.writeAll("[&]");
            const child_t = variant.params[0].asPtr(*cy.Type);
            try writeSymName(s, w, &child_t.sym().head, config);
            return;
        } else if (template == s.slice_tmpl) {
            try w.writeAll("[]");
            const child_t = variant.params[0].asPtr(*cy.Type);
            try writeSymName(s, w, &child_t.sym().head, config);
            return;
        } else if (template == s.partial_vector_tmpl) {
            const n = variant.params[1].asInt();
            try w.print("[..{}]", .{n});
            const child_t = variant.params[0].asPtr(*cy.Type);
            try writeSymName(s, w, &child_t.sym().head, config);
            return;
        } else if (template == s.vector_tmpl) {
            const n = variant.params[1].asInt();
            try w.print("[{}]", .{n});
            const child_t = variant.params[0].asPtr(*cy.Type);
            try writeSymName(s, w, &child_t.sym().head, config);
            return;
        } else if (template == s.func_ptr_tmpl) {
            const sig = variant.params[0].asPtr(*cy.FuncSig);
            if (sig.extern_) {
                try w.writeAll("#extern ");
            }
            try w.writeAll("fn");
            try s.writeFuncSigStr(w, sig, config.from);
            return;
        } else if (template == s.func_sym_tmpl) {
            try w.writeAll("funcsym");
            const sig = variant.params[0].asPtr(*cy.FuncSig);
            try s.writeFuncSigStr(w, sig, config.from);
            return;
        }
        try w.writeAll(sym.name());
        try w.writeByte('[');
        try writeLocalVariantParams(s, w, variant, config);
        try w.writeByte(']');
    } else {
        if (sym.parent) |parent| {
            if (shouldWriteSym(parent, config)) {
                try writeSymName(s, w, parent, config);
                try w.writeByte('.');
            }
        }
        try w.writeAll(sym.name());
    }
}

fn writeVariantParam(s: *cy.Sema, w: anytype, arg: cy.TypeValue, config: SymFormatConfig) !void {
    switch (arg.type.id()) {
        bt.Type => {
            try writeSymName(s, w, @ptrCast(arg.value.asPtr(*cy.Type).sym()), config);
        },
        bt.I64 => {
            try w.print("{}", .{arg.value.asInt()});
        },
        bt.IntLit => {
            try w.print("{}", .{arg.value.as_int_lit()});
        },
        bt.Str => {
            try w.print("'{s}'", .{arg.value.asString()});
        },
        bt.FuncSig => {
            try w.print("<FuncSig>", .{});
        },
        else => {
            if (arg.type.kind() == .func_sym) {
                try w.writeAll("fn");
                try s.writeFuncSigStr(w, arg.value.asPtr(*cy.Func).sig, config.from);
            } else {
                std.debug.panic("Unsupported: {s}", .{arg.type.name()});
            }
        }
    }
}

fn writeLocalVariantParams(s: *cy.Sema, w: anytype, variant: *cy.Instance, config: SymFormatConfig) !void {
    try writeVariantParam(s, w, variant.getParamAt(0), config);
    for (1..variant.params.len) |i| {
        try w.writeAll(", ");
        try writeVariantParam(s, w, variant.getParamAt(i), config);
    }
}

fn writeTemplateParam(s: *cy.Sema, w: *std.Io.Writer, param_t: *cy.Type, param: cy.Value, config: SymFormatConfig) !void {
    switch (param_t.id()) {
        bt.Type => {
            const sym = param.asPtr(*cy.Type).sym();
            try writeSymName(s, w, @ptrCast(sym), config);
        },
        bt.PartialStructLayout => {
            try w.writeAll("PartialStructLayout{}");
        },
        bt.I32 => {
            try w.print("{}", .{param.asI32()});
        },
        bt.I64 => {
            try w.print("{}", .{param.asInt()});
        },
        bt.Str => {
            try w.print("'{s}'", .{param.asString()});
        },
        bt.FuncSig => {
            try w.print("<FuncSig>", .{});
        },
        else => {
            switch (param_t.kind()) {
                .enum_t => {
                    const case = param_t.cast(.enum_t).getCaseByTag(@intCast(param.asInt()));
                    try w.print("${s}", .{case.head.name()});
                },
                else => {
                    const name = try s.allocTypeName(param_t);
                    std.debug.panic("Unsupported: {s}", .{name});
                },
            }
        }
    }
}

fn writeLocalFuncVariantParams(s: *cy.Sema, w: *std.Io.Writer, variant: *cy.Instance, config: SymFormatConfig) !void {
    const params = variant.params;
    const param_types = variant.param_types;
    try writeTemplateParam(s, w, param_types[0], params[0], config);
    for (params[1..], 1..) |arg, i| {
        try w.writeAll(", ");
        try writeTemplateParam(s, w, param_types[i], arg, config);
    }
}

test "sym internals" {
    if (builtin.mode == .ReleaseFast) {
        if (cy.is32Bit) {
            try t.eq(@sizeOf(Sym), 16);
            try t.eq(@sizeOf(Func), 36);
        } else {
            try t.eq(@sizeOf(Sym), 24);
            try t.eq(@sizeOf(Func), 64);
        }
    } else {
        if (cy.is32Bit) {
            try t.eq(@sizeOf(Sym), 16);
            try t.eq(@sizeOf(Func), 44);
        } else {
            try t.eq(@sizeOf(Sym), 24);
            try t.eq(@sizeOf(Func), 72);
        }
    }

    try t.eq(@offsetOf(Template, "head"), 0);

    try t.eq(@offsetOf(Sym, "type"), @offsetOf(vmc.SemaSym, "type"));
    try t.eq(@offsetOf(Sym, "parent"), @offsetOf(vmc.SemaSym, "parent"));
    try t.eq(@offsetOf(Sym, "namePtr"), @offsetOf(vmc.SemaSym, "namePtr"));
    try t.eq(@offsetOf(Sym, "nameLen"), @offsetOf(vmc.SemaSym, "nameLen"));
    try t.eq(@offsetOf(Sym, "metadata"), @offsetOf(vmc.SemaSym, "metadata"));
}