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

pub const SymType = enum(u8) {
    null,
    context_var,
    userVar,
    hostVar,
    func,
    type,
    module_alias,
    use_alias,
    chunk,
    enum_case,
    choice_case,
    typeAlias,

    /// Unresolved distinct type.
    distinct_t,

    func_template,
    template,
    field,

    /// During the reserving symbol phase, any intermediate sym that
    /// hasn't been visited yet is assigned a `Placeholder`
    /// so that child syms can be reserved.
    /// Once the intermediate sym is visited, the placeholder sym is replaced
    /// and it's module is copied.
    placeholder,
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
            .metadata = @bitCast(Metadata{ .padding = undefined, .name_owned = false, .resolving = false }),
            .type = symT,
            .parent = parent,
        };
    }

    pub fn deinit(self: *Sym, alloc: std.mem.Allocator) void {
        if (self.getMetadata().name_owned) {
            alloc.free(self.name());
        }
    }

    pub fn toC(self: *Sym) C.Sym {
        return .{ .ptr = self };
    }
    pub fn fromC(sym: C.Sym) *Sym {
        return @ptrCast(@alignCast(sym.ptr));
    }

    /// This is mainly used at the end of execution to release values
    /// so that the global rc can be compared against 0.
    pub fn deinitValues(self: *Sym, vm: *cy.VM) void {
        if (self.getMod()) |mod| {
            mod.deinitValues(vm);
        }

        switch (self.type) {
            .template => {
                const template = self.cast(.template);
                for (template.variants.items) |variant| {
                    deinitVariantValues(vm, variant);
                }
            },
            .func_template => {
                const template = self.cast(.func_template);
                var iter = template.variant_cache.iterator();
                while (iter.next()) |e| {
                    const variant = e.value_ptr.*;
                    for (variant.args) |arg| {
                        vm.release(arg);
                    }
                    vm.alloc.free(variant.args);
                    variant.args = &.{};
                }
            },
            else => {},
        }
    }

    pub fn destroy(self: *Sym, vm: *cy.VM, alloc: std.mem.Allocator) void {
        self.deinit(alloc);
        switch (self.type) {
            .type => {
                const type_sym = self.cast(.type);
                type_sym.getMod().deinit(alloc);
                alloc.destroy(type_sym);
            },
            .chunk => {
                const chunk = self.cast(.chunk);
                chunk.getMod().deinit(alloc);
                alloc.destroy(chunk);
            },
            .func_template => {
                const template = self.cast(.func_template);
                deinitFuncTemplate(vm, template);
                alloc.destroy(template);
            },
            .template => {
                const template = self.cast(.template);
                deinitTemplate(vm, template);
                alloc.destroy(template);
            },
            .distinct_t => {
                const distinct_t = self.cast(.distinct_t);
                if (distinct_t.resolved == null) {
                    distinct_t.getMod().deinit(alloc);
                }
                alloc.destroy(distinct_t);
            },
            .typeAlias => {
                alloc.destroy(self.cast(.typeAlias));
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
            .context_var => {
                alloc.destroy(self.cast(.context_var));
            },
            .userVar => {
                const user_var = self.cast(.userVar);
                user_var.deinit(alloc);
                alloc.destroy(user_var);
            },
            .hostVar => {
                alloc.destroy(self.cast(.hostVar));
            },
            .placeholder => {
                const placeholder = self.cast(.placeholder);
                if (!placeholder.resolved) {
                    placeholder.getMod().deinit(alloc);
                }
                alloc.destroy(placeholder);
            },
            .enum_case => {
                const impl = self.cast(.enum_case);
                alloc.destroy(impl);
            },
            .choice_case => {
                const impl = self.cast(.choice_case);
                alloc.destroy(impl);
            },
            .field => {
                const impl = self.cast(.field);
                alloc.destroy(impl);
            },
            .null => {},
        }
    }

    pub fn getMetadata(self: Sym) Metadata {
        return @bitCast(self.metadata);
    }

    pub fn setNameOwned(self: *Sym, name_owned: bool) void {
        var metadata = self.getMetadata();
        metadata.name_owned = name_owned;
        self.metadata = @bitCast(metadata);
    }

    pub fn setResolving(self: *Sym, resolving: bool) void {
        var metadata = self.getMetadata();
        metadata.resolving = resolving;
        self.metadata = @bitCast(metadata);
    }

    pub fn getVariant(self: *Sym) ?*Variant {
        switch (self.type) {
            .distinct_t => return self.cast(.distinct_t).variant,
            .type       => return self.cast(.type).variant,
            else => return null,
        }
    }

    pub fn setVariant(self: *Sym, variant: *Variant) !void {
        switch (self.type) {
            .distinct_t => self.cast(.distinct_t).variant = variant,
            .type       => self.cast(.type).variant = variant,
            else => return error.Unsupported,
        }
    }

    pub fn isVariable(self: Sym) bool {
        return self.type == .userVar or self.type == .hostVar or self.type == .context_var;
    }

    pub fn isType(self: *Sym) bool {
        switch (self.type) {
            .use_alias => return self.cast(.use_alias).sym.isType(),
            .typeAlias,
            .type => {
                return true;
            },
            .null,
            .context_var,
            .userVar,
            .hostVar,
            .module_alias,
            .func,
            .func_template,
            .template,
            .distinct_t,
            .field,
            .enum_case,
            .choice_case,
            .placeholder,
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

    pub fn isDistinct(self: *Sym) bool {
        return self.type != .func or self.cast(.func).numFuncs == 1;
    }

    pub fn getMod(self: *Sym) ?*cy.Module {
        switch (self.type) {
            .chunk           => return @ptrCast(&self.cast(.chunk).mod),
            .type            => return @ptrCast(&self.cast(.type).mod),
            .placeholder     => return @ptrCast(&self.cast(.placeholder).mod),
            .distinct_t      => return @ptrCast(&self.cast(.distinct_t).mod),
            .typeAlias       => return @ptrCast(&self.cast(.typeAlias).mod),
            .template        => return @ptrCast(&self.cast(.template).mod),
            .func_template   => return @ptrCast(&self.cast(.func_template).mod),
            .use_alias,
            .module_alias,
            .enum_case,
            .choice_case,
            .func,
            .context_var,
            .userVar,
            .field,
            .hostVar => {
                return null;
            },
            .null => cy.unexpected(),
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

    pub fn getStaticType(self: *Sym) ?*cy.Type {
        switch (self.type) {
            .typeAlias       => return self.cast(.typeAlias).type,
            .type            => return self.cast(.type).type,
            .distinct_t      => return self.cast(.distinct_t).type,
            .use_alias       => return self.cast(.use_alias).sym.getStaticType(),
            .placeholder,
            .null,
            .field,
            .func_template,
            .template,
            .enum_case,
            .choice_case,
            .func,
            .module_alias,
            .chunk,
            .context_var,
            .hostVar,
            .userVar         => return null,
        }
    }

    pub fn resolved(self: *Sym) *Sym {
        if (self.type == .module_alias) {
            return self.cast(.module_alias).sym;
        } else if (self.type == .typeAlias) {
            return self.cast(.typeAlias).sym;
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
};

const Metadata = packed struct {
    name_owned: bool,

    /// Used to detect circular reference when resolving.
    resolving: bool,

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
        .context_var => ContextVar,
        .userVar => UserVar,
        .hostVar => HostVar,
        .type => TypeSym,
        .enum_case => EnumCase,
        .choice_case => ChoiceCase,
        .typeAlias => TypeAlias,
        .distinct_t => DistinctType,
        .template => Template,
        .func_template => FuncTemplate,
        .field => Field,
        .use_alias => UseAlias,
        .module_alias => ModuleAlias,
        .chunk => Chunk,
        .placeholder => Placeholder,
        .null => void,
    };
}

pub const Placeholder = extern struct {
    head: Sym,
    mod: vmc.Module,
    resolved: bool,
    pub fn getMod(self: *Placeholder) *cy.Module {
        return @ptrCast(&self.mod);
    }
};

pub const HostVar = extern struct {
    head: Sym,
    decl: ?*ast.StaticVarDecl,
    type: *cy.Type,
    val: cy.Value,

    // Index into `retainedVars`.
    retainedIdx: u16,
};

pub const ContextVar = extern struct {
    head: Sym,
    decl: *ast.ContextDecl,
    type: *cy.Type,
    idx: u8,

    pub fn isResolved(self: *ContextVar) bool {
        return self.type.kind() != .null;
    }
};

pub const UserVar = struct {
    head: Sym,
    decl: ?*ast.StaticVarDecl,
    type: *cy.Type,

    /// Holds the initializer expression.
    ir: u32,

    /// Whether init statement was emitted.
    emitted: bool,

    /// Owned.
    deps: []*UserVar,

    /// Used to detect circular reference while resolving `ir`.
    resolving_init: bool,

    pub fn isResolved(self: *UserVar) bool {
        return self.ir != cy.NullId;
    }

    pub fn deinit(self: *UserVar, alloc: std.mem.Allocator) void {
        alloc.free(self.deps);
    }
};

/// Sym that links to overloaded functions.
pub const FuncSym = extern struct {
    head: Sym,
    first: *Func,
    last: *Func,
    numFuncs: u16,

    /// Duped to perform uniqueness check without dereferencing the first ModuleFunc.
    firstFuncSig: cy.sema.FuncSigId,

    pub fn isResolved(self: *FuncSym) bool {
        return self.firstFuncSig != cy.NullId;
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
};

pub const TypeAlias = extern struct {
    head: Sym,
    decl: *ast.TypeAliasDecl,
    type: ?*cy.Type,
    sym: *Sym,
    mod: vmc.Module,
    resolved: bool,

    pub fn getMod(self: *TypeAlias) *cy.Module {
        return @ptrCast(&self.mod);
    }
};

/// Like Placeholder with a reserved type id.
pub const DistinctType = extern struct {
    head: Sym,
    decl: *ast.DistinctDecl,
    type: ?*cy.Type,
    mod: vmc.Module,
    variant: ?*Variant,
    resolved: ?*cy.Sym,

    pub fn getMod(self: *DistinctType) *cy.Module {
        return @ptrCast(&self.mod);
    }
};

pub const ValueType = extern struct {
    head: Sym,
};

pub const FuncTemplateParam = struct {
    name: []const u8,
    // Func parameter index where it is declared.
    decl_idx: u32,
    infer: bool,
};

pub const FuncTemplate = struct {
    head: Sym,
    decl: *ast.TemplateDecl,
    func_params: []const *ast.FuncParam,
    func_ret: ?*ast.Node,
    params: []FuncTemplateParam,
    resolved: bool,

    /// Template args to variant. Keys are not owned.
    variant_cache: std.HashMapUnmanaged([]const cy.Value, *Variant, VariantKeyContext, 80),
    variants: std.ArrayListUnmanaged(*Variant),

    mod: vmc.Module,

    pub fn deinit(self: *FuncTemplate, alloc: std.mem.Allocator) void {
        for (self.variants.items) |variant| {
            alloc.destroy(variant);
        }
        self.variants.deinit(alloc);
        self.variant_cache.deinit(alloc);

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

    pub fn chunk(self: *const FuncTemplate) *cy.Chunk {
        return self.head.parent.?.getMod().?.chunk;
    }
};

fn deinitFuncTemplate(vm: *cy.VM, template: *FuncTemplate) void {
    template.getMod().deinit(vm.alloc);

    for (template.variants.items) |variant| {
        vm.alloc.destroy(variant);
    }
    template.variants.deinit(vm.alloc);
    template.variant_cache.deinit(vm.alloc);
    vm.alloc.free(template.params);
}

/// This is similar to SymType but has custom_t.
pub const TemplateType = enum {
    struct_t,
    enum_t,
    distinct_t,
    custom_t,
    value,
};

/// TODO: Consider splitting into Template and FuncTemplate.
pub const Template = struct {
    head: Sym,
    decl: *ast.TemplateDecl,

    sigId: cy.sema.FuncSigId,

    kind: TemplateType,

    /// Owned.
    params: []TemplateParam,

    /// Template args to variant. Keys are not owned.
    variant_cache: std.HashMapUnmanaged([]const cy.Value, *Variant, VariantKeyContext, 80),
    variants: std.ArrayListUnmanaged(*Variant),

    mod: vmc.Module,

    pub fn isResolved(self: *Template) bool {
        return self.sigId != cy.NullId;
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

    pub fn chunk(self: *const Template) *cy.Chunk {
        return self.head.parent.?.getMod().?.chunk;
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

fn deinitTemplate(vm: *cy.VM, template: *Template) void {
    template.getMod().deinit(vm.alloc);

    for (template.variants.items) |variant| {
        vm.alloc.destroy(variant);
    }
    template.variants.deinit(vm.alloc);
    template.variant_cache.deinit(vm.alloc);
    vm.alloc.free(template.params);
}

pub const TemplateParam = struct {
    name: []const u8,
    type: *cy.Type,
};

const VariantType = enum(u8) {
    sym,
    ct_val,
    func,
};

pub const Variant = struct {
    type: VariantType,

    /// Owned args. Can be used to print params along with the template type.
    args: []const cy.Value,

    data: union {
        sym: struct {
            /// Back link to the template.
            /// Can be used to check what template the symbol comes from. Also used for pushing the variant context.
            template: *Template,
            sym: *Sym,
        },
        ct_val: struct {
            template: *Template,
            ct_val: cy.Value,
        },
        func: struct {
            template: *FuncTemplate,
            func: *cy.Func,
        },
    },

    pub fn getSymTemplate(self: *Variant) *Template {
        return self.data.sym.template;
    }
};

fn deinitVariantValues(vm: *cy.VM, variant: *Variant) void {
    for (variant.args) |arg| {
        vm.release(arg);
    }
    vm.alloc.free(variant.args);
    variant.args = &.{};
    if (variant.type == .ct_val) {
        vm.release(variant.data.ct_val.ct_val);
        variant.data.ct_val.ct_val = cy.Value.Void;
    }
}

const VariantKeyContext = struct {
    sema: *cy.sema.Sema,

    pub fn hash(self: VariantKeyContext, key: []const cy.Value) u64 {
        var c = std.hash.Wyhash.init(0);
        for (key) |val| {
            switch (val.getTypeId()) {
                bt.Type => {
                    const typeId = val.asHeapObject().type.type;
                    c.update(std.mem.asBytes(&typeId));
                },
                bt.FuncSig,
                bt.Integer => {
                    const i = val.asHeapObject().integer.val;
                    c.update(std.mem.asBytes(&i));
                },
                bt.String => {
                    const str = val.asString();
                    c.update(str);
                },
                else => {
                    const type_e = self.sema.getType(val.getTypeId());
                    if (type_e.kind() == .func_sym) {
                        c.update(std.mem.asBytes(&val.getTypeId()));
                        c.update(std.mem.asBytes(&val.asHeapObject().func_sym.func));
                        continue;
                    }
                    cy.rt.logZFmt("Unsupported value hash: {}", .{val.getTypeId()});
                    @panic("");
                }
            }
        }
        return c.final();
    }

    pub fn eql(self: VariantKeyContext, a: []const cy.Value, b: []const cy.Value) bool {
        if (a.len != b.len) {
            return false;
        }
        for (a, 0..) |av, i| {
            const atype = av.getTypeId();
            const btype = b[i].getTypeId();
            if (atype != btype) {
                return false;
            }
            switch (atype) {
                bt.Type => {
                    if (av.asHeapObject().type.type != b[i].asHeapObject().type.type) {
                        return false;
                    }
                },
                bt.FuncSig,
                bt.Integer => {
                    return av.asBoxInt() == b[i].asBoxInt();
                },
                bt.String => {
                    return std.mem.eql(u8, av.asString(), b[i].asString());
                },
                else => {
                    const type_e = self.sema.getType(atype);
                    if (type_e.kind() == .func_sym) {
                        return av.asHeapObject().func_sym.func == b[i].asHeapObject().func_sym.func;
                    }
                    cy.rt.logZFmt("Unsupported value comparison: {}", .{atype});
                    @panic("");
                },
            }
        }
        return true;
    }
};

pub const Field = extern struct {
    head: Sym,

    idx: u32,
    type: *cy.Type,
};

/// Generic type symbol.
pub const TypeSym = extern struct {
    head: Sym,
    decl: ?*ast.Node,
    type: *cy.Type,
    mod: vmc.Module,
    variant: ?*Variant,

    pub fn getMod(self: *TypeSym) *cy.Module {
        return @ptrCast(&self.mod);
    }
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
};

pub const ModuleAlias = extern struct {
    head: Sym,
    declId: *ast.Node,
    sym: *Sym,
};

pub const UseAlias = extern struct {
    head: Sym,

    /// Can be UseAlias or ImportStmt.
    decl: ?*ast.Node,
    sym: *Sym,
    resolved: bool,
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
    hostFunc,
    userFunc,
    userLambda,
    trait,
};

pub const Func = struct {
    type: FuncKind,
    next: ?*Func,

    /// For non-lambdas, this can refer to a FuncSym or Template.
    parent: *Sym,

    funcSigId: cy.sema.FuncSigId,
    // FuncDecl/LambdaExpr
    decl: ?*ast.Node,
    retType: *cy.Type,

    data: union {
        hostFunc: struct {
            ptr: cy.ZHostFuncFn,
        },
        hostInlineFunc: struct {
            ptr: cy.ZHostFuncFn,
        },
        trait: struct {
            vtable_idx: u32,
        },
        userFunc: struct {
            /// Currently used to invalidate the IR func block when removing temporary functions.
            /// See `cte.expandValueTemplate`.
            loc: u32,
        },
    },
    variant: ?*Variant,
    reqCallTypeCheck: bool,
    numParams: u8,

    is_method: bool,

    /// Whether the function was declared nested in a type.
    is_nested: bool,
    throws: bool,

    /// Whether it has already emitted IR.
    emitted: bool,

    /// Whether this func's static dependencies have also been resolved and emitted.
    /// This is useful for comptime evaluation to check whether it needs to compile deps.
    emitted_deps: bool = false,

    pub fn isResolved(self: Func) bool {
        return self.funcSigId != cy.NullId;
    }

    pub fn isStatic(self: Func) bool {
        return self.type != .userLambda;
    }

    pub fn isMethod(self: Func) bool {
        return self.is_method;
    }

    pub fn hasStaticInitializer(self: Func) bool {
        return self.type == .hostFunc;
    }

    pub fn chunk(self: *const Func) *cy.Chunk {
        return self.parent.parent.?.getMod().?.chunk;
    }

    pub fn name(self: Func) []const u8 {
        if (self.type == .userLambda) {
            return "lambda";
        }
        return self.parent.name();
    }
};

pub const ChunkExt = struct {

    pub fn getSymValueType(c: *cy.Chunk, sym: *Sym) !?*cy.Type {
        switch (sym.type) {
            .context_var => return sym.cast(.context_var).type,
            .userVar    => return sym.cast(.userVar).type,
            .hostVar    => return sym.cast(.hostVar).type,
            .enum_case => return null,
            .choice_case => {
                return sym.cast(.choice_case).type;
            },
            .use_alias  => return getSymValueType(c, sym.cast(.use_alias).sym),
            .type,
            .typeAlias => return c.sema.type_t,
            .func => {
                const func = sym.cast(.func);
                if (func.numFuncs == 1) {
                    return try cy.sema.getFuncPtrType(c, func.first.funcSigId);
                } else {
                    return error.AmbiguousSymbol;
                }
            },
            .placeholder,
            .field,
            .null,
            .module_alias,
            .distinct_t,
            .func_template,
            .template,
            .chunk => return null,
        }
    }

    pub fn createDistinctType(c: *cy.Chunk, parent: *Sym, name: []const u8, decl: *ast.DistinctDecl) !*DistinctType {
        const sym = try createSym(c.alloc, .distinct_t, .{
            .head = Sym.init(.distinct_t, parent, name),
            .decl = decl,
            .type = null,
            .mod = undefined,
            .resolved = null,
            .variant = null,
        });
        sym.getMod().* = cy.Module.init(c);
        try c.syms.append(c.alloc, @ptrCast(sym));
        return sym;
    }

    pub fn createTypeAlias(c: *cy.Chunk, parent: *Sym, name: []const u8, decl: *ast.TypeAliasDecl) !*TypeAlias {
        const sym = try createSym(c.alloc, .typeAlias, .{
            .head = Sym.init(.typeAlias, parent, name),
            .decl = decl,
            .type = null,  // Null indicates it needs to be resolved later on.
            .sym = undefined,
            .mod = undefined,
            .resolved = false,
        });
        @as(*cy.Module, @ptrCast(&sym.mod)).* = cy.Module.init(c);
        try c.syms.append(c.alloc, @ptrCast(sym));
        return sym;
    } 

    pub fn createTypeSym(c: *cy.Chunk, parent: *Sym, name: []const u8, type_: *cy.Type, decl: ?*ast.Node) !*TypeSym {
        const sym = try createSym(c.alloc, .type, .{
            .head = Sym.init(.type, parent, name),
            .decl = decl,
            .type = type_,
            .variant = null,
            .mod = undefined,
        });
        type_.sym_ = sym;
        @as(*cy.Module, @ptrCast(&sym.mod)).* = cy.Module.init(c);
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
        val: u32, payload_t: *cy.Type) !*ChoiceCase {
        const sym = try createSym(c.alloc, .choice_case, .{
            .head = Sym.init(.choice_case, parent, name),
            .type = type_,
            .val = val,
            .payload_t = payload_t,
        });
        try c.syms.append(c.alloc, @ptrCast(sym));
        return sym;
    }

    pub fn createContextVar(c: *cy.Chunk, parent: *Sym, name: []const u8, decl: *ast.ContextDecl) !*ContextVar {
        const sym = try createSym(c.alloc, .context_var, .{
            .head = Sym.init(.context_var, parent, name),
            .decl = decl,
            .type = &cy.types.NullType,
            .idx = cy.NullU8,
        });
        try c.syms.append(c.alloc, @ptrCast(sym));
        return sym;
    }

    pub fn createUserVar(c: *cy.Chunk, parent: *Sym, name: []const u8, decl: ?*ast.StaticVarDecl) !*UserVar {
        const sym = try createSym(c.alloc, .userVar, .{
            .head = Sym.init(.userVar, parent, name),
            .decl = decl,
            .type = undefined,
            .ir = cy.NullId,
            .emitted = false,
            .deps = &.{},
            .resolving_init = false,
        });
        try c.syms.append(c.alloc, @ptrCast(sym));
        return sym;
    }

    pub fn createHostVar(c: *cy.Chunk, parent: *Sym, name: []const u8, decl: ?*ast.StaticVarDecl) !*HostVar {
        const sym = try createSym(c.alloc, .hostVar, .{
            .head = Sym.init(.hostVar, parent, name),
            .retainedIdx = cy.NullU16,
            .val = cy.Value.Void,
            .decl = decl,
            .type = &cy.types.NullType,
        });
        try c.syms.append(c.alloc, @ptrCast(sym));
        return sym;
    }

    pub fn createFuncTemplate(c: *cy.Chunk, parent: *Sym, name: []const u8, tparams: []FuncTemplateParam, decl: *ast.TemplateDecl) !*FuncTemplate {
        const func_decl = decl.child_decl.cast(.funcDecl);
        const sym = try createSym(c.alloc, .func_template, .{
            .head = Sym.init(.func_template, parent, name),
            .func_params = func_decl.params,
            .func_ret = func_decl.ret,
            .params = tparams,
            .resolved = false,
            .decl = decl,
            .variant_cache = .{},
            .variants = .{},
            .mod = undefined,
        });
        sym.getMod().* = cy.Module.init(c);
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
            .sigId = cy.NullId,
            .variant_cache = .{},
            .variants = .{},
            .mod = undefined,
        });
        sym.getMod().* = cy.Module.init(c);
        try c.syms.append(c.alloc, @ptrCast(sym));
        return sym;
    }

    pub fn createChunkSym(c: *cy.Chunk, name: []const u8) !*Chunk {
        const sym = try createSym(c.alloc, .chunk, .{
            .head = Sym.init(.chunk, null, name),
            .chunk = c,
            .mod = undefined,
        });
        @as(*cy.Module, @ptrCast(&sym.mod)).* = cy.Module.init(c);
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

    pub fn createFunc(c: *cy.Chunk, ftype: FuncKind, parent: *Sym, node: ?*ast.Node, isMethod: bool) !*Func {
        const func = try c.alloc.create(Func);
        func.* = .{
            .type = ftype,
            .funcSigId = cy.NullId,
            .retType = undefined,
            .reqCallTypeCheck = undefined,
            .throws = false,
            .parent = parent,
            .is_method = isMethod,
            .is_nested = false,
            .numParams = undefined,
            .variant = null,
            .decl = node,
            .next = null,
            .data = undefined,
            .emitted = false,
        };
        return func;
    }

    pub fn createFuncSym(c: *cy.Chunk, parent: *Sym, name: []const u8) !*FuncSym {
        const sym = try createSym(c.alloc, .func, .{
            .head = Sym.init(.func, parent, name),
            .numFuncs = 0,
            .firstFuncSig = cy.NullId,
            .first = undefined,
            .last = undefined,
        });
        try c.syms.append(c.alloc, @ptrCast(sym));
        return sym;
    }
};

const SymFormatConfig = struct {
    from: ?*cy.Chunk = null,
    emit_template_args: bool = true,
};

pub fn writeFuncName(s: *cy.Sema, w: anytype, func: *cy.Func, config: SymFormatConfig) !void {
    try writeParentPrefix(s, w, func.parent, config);
    try w.writeAll(func.name());
    if (config.emit_template_args) {
        if (func.variant) |variant| {
            try w.writeByte('[');
            try writeLocalFuncTemplateArgs(s, w, variant, config);
            try w.writeByte(']');
        }
    }
}

pub fn allocSymName(s: *cy.Sema, alloc: std.mem.Allocator, sym: *cy.Sym, config: SymFormatConfig) ![]const u8 {
    var b: std.ArrayListUnmanaged(u8) = .{};
    const w = b.writer(alloc);
    try writeSymName(s, w, sym, config);
    return b.toOwnedSlice(alloc);
}

pub fn formatSymName(s: *cy.Sema, buf: []u8, sym: *Sym, config: SymFormatConfig) ![]const u8 {
    var fbs = std.io.fixedBufferStream(buf);
    try writeSymName(s, fbs.writer(), sym, config);
    return fbs.getWritten();
}

pub fn writeSymName(s: *cy.Sema, w: anytype, sym: *cy.Sym, config: SymFormatConfig) anyerror!void {
    try writeParentPrefix(s, w, sym, config);

    if (config.emit_template_args) {
        if (sym.getVariant()) |variant| {
            const template = variant.getSymTemplate();
            if (template == s.option_tmpl) {
                try w.writeAll("?");
                const arg = variant.args[0].asHeapObject();
                try s.writeTypeName(w, s.getType(arg.type.type), config.from);
                return;
            } else if (template == s.pointer_tmpl) {
                try w.writeAll("*");
                const arg = variant.args[0].asHeapObject();
                try s.writeTypeName(w, s.getType(arg.type.type), config.from);
                return;
            } else if (template == s.ptr_slice_tmpl) {
                try w.writeAll("[*]");
                const arg = variant.args[0].asHeapObject();
                try s.writeTypeName(w, s.getType(arg.type.type), config.from);
                return;
            } else if (template == s.ref_tmpl) {
                try w.writeAll("^");
                const arg = variant.args[0].asHeapObject();
                try s.writeTypeName(w, s.getType(arg.type.type), config.from);
                return;
            } else if (template == s.ref_slice_tmpl) {
                try w.writeAll("[]");
                const arg = variant.args[0].asHeapObject();
                try s.writeTypeName(w, s.getType(arg.type.type), config.from);
                return;
            } else if (template == s.func_ptr_tmpl) {
                try w.writeAll("func");
                const sig: cy.sema.FuncSigId = @intCast(variant.args[0].asBoxInt());
                try s.writeFuncSigStr(w, sig, config.from);
                return;
            } else if (template == s.func_union_tmpl) {
                try w.writeAll("Func");
                const sig: cy.sema.FuncSigId = @intCast(variant.args[0].asBoxInt());
                try s.writeFuncSigStr(w, sig, config.from);
                return;
            } else if (template == s.func_sym_tmpl) {
                try w.writeAll("funcsym");
                const sig: cy.sema.FuncSigId = @intCast(variant.args[0].asBoxInt());
                try s.writeFuncSigStr(w, sig, config.from);
                return;
            }
            try w.writeAll(sym.name());
            try w.writeByte('[');
            try writeLocalTemplateArgs(s, w, variant, config);
            try w.writeByte(']');
            return;
        }
    }
    try w.writeAll(sym.name());
}

fn writeParentPrefix(s: *cy.Sema, w: anytype, sym: *cy.Sym, config: SymFormatConfig) !void {
    const parent = sym.parent orelse return;
    if (parent.type == .chunk) {
        const chunk = parent.cast(.chunk);
        if (config.from != null and chunk.chunk != config.from.?) {
            // TODO: Print chunk name if different chunk and has a binded module name.
        }
        return;
    }
    try writeParentPrefix(s, w, parent, config);
    try w.print("{s}", .{parent.name()});
    if (config.emit_template_args) {
        if (parent.getVariant()) |variant| {
            try w.writeByte('[');
            try writeLocalTemplateArgs(s, w, variant, config);
            try w.writeByte(']');
        }
    }
    try w.writeByte('.');
}

fn writeTemplateArg(s: *cy.Sema, w: anytype, arg: cy.Value, config: SymFormatConfig) !void {
    switch (arg.getTypeId()) {
        bt.Type => {
            const type_ = s.getType(arg.asHeapObject().type.type);
            try writeSymName(s, w, @ptrCast(type_.sym()), config);
        },
        bt.Integer => {
            try w.print("{}", .{arg.asBoxInt()});
        },
        else => {
            const type_e = s.getType(arg.getTypeId());
            if (type_e.kind() == .func_sym) {
                try w.writeAll("func");
                const func = arg.asHeapObject().func_sym.func;
                try s.writeFuncSigStr(w, func.funcSigId, config.from);
            } else {
                return error.Unsupported;
            }
        }
    }
}

fn writeLocalTemplateArgs(s: *cy.Sema, w: anytype, variant: *cy.sym.Variant, config: SymFormatConfig) !void {
    const args = variant.args;
    try writeTemplateArg(s, w, args[0], config);
    for (args[1..]) |arg| {
        try w.writeByte(',');
        try writeTemplateArg(s, w, arg, config);
    }
}

fn writeLocalFuncTemplateArgs(s: *cy.Sema, w: anytype, variant: *cy.sym.Variant, config: SymFormatConfig) !void {
    const args = variant.args;
    if (args[0].getTypeId() != bt.Type) {
        return error.Unsupported;
    }
    var sym = s.getType(args[0].asHeapObject().type.type).sym();
    try writeSymName(s, w, @ptrCast(sym), config);
    for (args[1..]) |arg| {
        try w.writeByte(',');
        if (arg.getTypeId() != bt.Type) {
            return error.Unsupported;
        }
        sym = s.getType(arg.asHeapObject().type.type).sym();
        try writeSymName(s, w, @ptrCast(sym), config);
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