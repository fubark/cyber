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
    hostobj_t,
    object_t,
    struct_t,
    trait_t,
    type,
    module_alias,
    use_alias,
    chunk,
    enum_t,
    enumMember,
    typeAlias,

    /// Unresolved distinct type.
    distinct_t,

    template,
    field,

    /// During the reserving symbol phase, any intermediate sym that
    /// hasn't been visited yet is assigned a `Placeholder`
    /// so that child syms can be reserved.
    /// Once the intermediate sym is visited, the placeholder sym is replaced
    /// and it's module is copied.
    placeholder,

    dummy_t,
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
            else => {},
        }
    }

    pub fn destroy(self: *Sym, vm: *cy.VM, alloc: std.mem.Allocator) void {
        self.deinit(alloc);
        switch (self.type) {
            .hostobj_t => {
                const hostType = self.cast(.hostobj_t);
                hostType.getMod().deinit(alloc);
                alloc.destroy(hostType);
            },
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
            .struct_t => {
                const obj = self.cast(.struct_t);
                obj.deinit(alloc);
                alloc.destroy(obj);
            },
            .object_t => {
                const obj = self.cast(.object_t);
                obj.deinit(alloc);
                alloc.destroy(obj);
            },
            .trait_t => {
                const obj = self.cast(.trait_t);
                obj.deinit(alloc);
                alloc.destroy(obj);
            },
            .enum_t => {
                const enumType = self.cast(.enum_t);
                enumType.deinit(alloc);
                alloc.free(enumType.members[0..enumType.numMembers]);
                alloc.destroy(enumType);
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
            .dummy_t => {
                alloc.destroy(self.cast(.dummy_t));
            },
            .placeholder => {
                const placeholder = self.cast(.placeholder);
                if (!placeholder.resolved) {
                    placeholder.getMod().deinit(alloc);
                }
                alloc.destroy(placeholder);
            },
            .null,
            .enumMember,
            .field => {
            },
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
            .object_t   => return self.cast(.object_t).variant,
            .struct_t   => return self.cast(.struct_t).variant,
            .hostobj_t  => return self.cast(.hostobj_t).variant,
            .enum_t     => return self.cast(.enum_t).variant,
            .distinct_t => return self.cast(.distinct_t).variant,
            .type       => return self.cast(.type).variant,
            else => return null,
        }
    }

    pub fn setVariant(self: *Sym, variant: *Variant) !void {
        switch (self.type) {
            .object_t   => self.cast(.object_t).variant = variant,
            .struct_t   => self.cast(.struct_t).variant = variant,
            .hostobj_t  => self.cast(.hostobj_t).variant = variant,
            .enum_t     => self.cast(.enum_t).variant = variant,
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
            .hostobj_t,
            .typeAlias,
            .type,
            .struct_t,
            .object_t,
            .trait_t,
            .dummy_t,
            .enum_t => {
                return true;
            },
            .null,
            .context_var,
            .userVar,
            .hostVar,
            .module_alias,
            .func,
            .template,
            .distinct_t,
            .field,
            .enumMember,
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
            .enum_t          => return @ptrCast(&self.cast(.enum_t).mod),
            .type            => return @ptrCast(&self.cast(.type).mod),
            .struct_t        => return @ptrCast(&self.cast(.struct_t).mod),
            .object_t        => return @ptrCast(&self.cast(.object_t).mod),
            .hostobj_t       => return @ptrCast(&self.cast(.hostobj_t).mod),
            .placeholder     => return @ptrCast(&self.cast(.placeholder).mod),
            .distinct_t      => return @ptrCast(&self.cast(.distinct_t).mod),
            .typeAlias       => return @ptrCast(&self.cast(.typeAlias).mod),
            .template        => return @ptrCast(&self.cast(.template).mod),
            .trait_t         => return @ptrCast(&self.cast(.trait_t).mod),
            .use_alias,
            .module_alias,
            .enumMember,
            .func,
            .context_var,
            .userVar,
            .field,
            .dummy_t,
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

    pub fn getStaticType(self: *Sym) ?cy.TypeId {
        switch (self.type) {
            .enum_t          => return self.cast(.enum_t).type,
            .typeAlias       => return self.cast(.typeAlias).type,
            .type            => return self.cast(.type).type,
            .struct_t        => return self.cast(.struct_t).type,
            .object_t        => return self.cast(.object_t).type,
            .distinct_t      => return self.cast(.distinct_t).type,
            .trait_t         => return self.cast(.trait_t).type,
            .hostobj_t       => return self.cast(.hostobj_t).type,
            .use_alias       => return self.cast(.use_alias).sym.getStaticType(),
            .dummy_t         => return self.cast(.dummy_t).type,
            .placeholder,
            .null,
            .field,
            .template,
            .enumMember,
            .func,
            .module_alias,
            .chunk,
            .context_var,
            .hostVar,
            .userVar         => return null,
        }
    }

    pub fn getFields(self: *Sym) ?[]const FieldInfo {
        switch (self.type) {
            .enum_t          => {
                const enum_t = self.cast(.enum_t);
                if (enum_t.isChoiceType) {
                    return &[_]FieldInfo{
                        .{ .sym = undefined, .type = bt.Integer, .offset = 0 },
                        .{ .sym = undefined, .type = bt.Any, .offset = 0 }
                    };
                }
                return null;
            },
            .struct_t        => return self.cast(.struct_t).getFields(),
            .object_t        => return self.cast(.object_t).getFields(),
            .type,
            .placeholder,
            .typeAlias,
            .use_alias,
            .trait_t,
            .hostobj_t,
            .null,
            .field,
            .template,
            .enumMember,
            .func,
            .module_alias,
            .distinct_t,
            .chunk,
            .context_var,
            .hostVar,
            .dummy_t,
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
        .struct_t,
        .object_t => ObjectType,
        .type => TypeSym,
        .trait_t => TraitType,
        .hostobj_t => HostObjectType,
        .enum_t => EnumType,
        .enumMember => EnumMember,
        .typeAlias => TypeAlias,
        .distinct_t => DistinctType,
        .template => Template,
        .field => Field,
        .use_alias => UseAlias,
        .module_alias => ModuleAlias,
        .chunk => Chunk,
        .placeholder => Placeholder,
        .dummy_t => DummyType,
        .null => void,
    };
}

pub const DummyType = extern struct {
    head: Sym,
    type: cy.TypeId,
};

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
    type: cy.TypeId,
    val: cy.Value,

    // Index into `retainedVars`.
    retainedIdx: u16,
};

pub const ContextVar = extern struct {
    head: Sym,
    decl: *ast.ContextDecl,
    type: cy.TypeId,
    idx: u8,

    pub fn isResolved(self: *ContextVar) bool {
        return self.type != cy.NullId;
    }
};

pub const UserVar = struct {
    head: Sym,
    decl: ?*ast.StaticVarDecl,
    type: cy.TypeId,

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
    type: cy.TypeId,
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
    type: cy.TypeId,
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

/// This is similar to SymType but has custom_t.
pub const TemplateType = enum {
    object_t,
    struct_t,
    enum_t,
    distinct_t,
    custom_t,
    ct_func,
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
    type: cy.TypeId,
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
                    if (type_e.kind == .func_sym) {
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
                    if (type_e.kind == .func_sym) {
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

pub const TraitMember = struct {
    func: *cy.Func,
};

pub const TraitType = extern struct {
    head: Sym,
    type: cy.TypeId,

    decl: *ast.TraitDecl,
    members_ptr: [*]const TraitMember,
    members_len: u32,
    mod: vmc.Module,

    pub fn deinit(self: *TraitType, alloc: std.mem.Allocator) void {
        self.getMod().deinit(alloc);
        alloc.free(self.members());
    }

    pub fn members(self: *TraitType) []const TraitMember {
        return self.members_ptr[0..self.members_len];
    }

    pub fn getMod(self: *TraitType) *cy.Module {
        return @ptrCast(&self.mod);
    }
};

pub const Field = extern struct {
    head: Sym,
    idx: u32,
    type: cy.TypeId,
};

pub const FieldInfo = packed struct {
    sym: *Field,
    type: cy.TypeId,

    /// For struct/cstruct. Field offset from the start of the parent.
    offset: u32,
};

pub const Impl = struct {
    trait: *cy.sym.TraitType,

    funcs: []*cy.Func,

    fn deinit(self: Impl, alloc: std.mem.Allocator) void {
        alloc.free(self.funcs);
    }
};

pub const ObjectType = extern struct {
    head: Sym,
    type: cy.TypeId,

    // Can be ObjectDecl / TableDecl.
    decl: ?*ast.Node,
    fields: [*]const FieldInfo,
    numFields: u32,

    rt_size: cy.Nullable(u32),
    variant: ?*Variant,

    /// Linear lookup since most types don't have many impls.
    impls_ptr: [*]Impl,
    impls_len: u32,

    cstruct: bool,

    /// Only relevant for structs/cstructs.
    /// Used to detect circular dependency.
    resolving_struct: bool = false,

    mod: vmc.Module,

    pub fn isResolved(self: *ObjectType) bool {
        return self.numFields != cy.NullId;
    }

    pub fn impls(self: *ObjectType) []Impl {
        return self.impls_ptr[0..self.impls_len];
    }

    pub fn implements(self: *ObjectType, trait_t: *TraitType) bool {
        for (self.impls()) |impl| {
            if (impl.trait == trait_t) {
                return true;
            }
        }
        return false;
    }

    pub fn init(parent: *Sym, chunk: *cy.Chunk, name: []const u8, decl: ?*ast.Node, type_id: cy.TypeId) ObjectType {
        var new = ObjectType{
            .head = Sym.init(.object_t, parent, name),
            .decl = decl,
            .type = type_id,
            .fields = undefined,
            .variant = null,
            .numFields = cy.NullId,
            .rt_size = cy.NullId,
            .cstruct = false,
            .mod = undefined,
            .impls_ptr = undefined,
            .impls_len = 0,
        };
        @as(*cy.Module, @ptrCast(&new.mod)).* = cy.Module.init(chunk);
        return new;
    }

    pub fn deinit(self: *ObjectType, alloc: std.mem.Allocator) void {
        self.getMod().deinit(alloc);
        if (self.isResolved()) {
            const fields = self.getFields();
            for (fields) |f| {
                alloc.destroy(f.sym);
            }
            alloc.free(fields);
        }

        for (self.impls()) |impl| {
            impl.deinit(alloc);
        }
        alloc.free(self.impls());
    }

    pub fn getMod(self: *ObjectType) *cy.Module {
        return @ptrCast(&self.mod);
    }

    pub fn getFields(self: ObjectType) []const FieldInfo {
        return self.fields[0..self.numFields];
    }
};

pub const HostObjectType = extern struct {
    head: Sym,
    type: cy.TypeId,
    decl: *ast.CustomDecl,
    getChildrenFn: C.GetChildrenFn,
    finalizerFn: C.FinalizerFn,
    mod: vmc.Module,
    variant: ?*Variant,

    pub fn getMod(self: *HostObjectType) *cy.Module {
        return @ptrCast(&self.mod);
    }
};

/// Generic type symbol.
pub const TypeSym = extern struct {
    head: Sym,
    type: cy.TypeId,
    mod: vmc.Module,
    variant: ?*Variant,

    pub fn getMod(self: *TypeSym) *cy.Module {
        return @ptrCast(&self.mod);
    }
};

pub const EnumType = extern struct {
    head: Sym,
    decl: *ast.EnumDecl,
    type: cy.TypeId,
    members: [*]*EnumMember,
    numMembers: u32,
    mod: vmc.Module,

    variant: ?*Variant,

    isChoiceType: bool,

    pub fn deinit(self: *EnumType, alloc: std.mem.Allocator) void {
        self.getMod().deinit(alloc);
        for (self.members[0..self.numMembers]) |m| {
            alloc.destroy(m);
        }
    }

    pub fn isResolved(self: *EnumType) bool {
        return self.numMembers != 0;
    }

    pub fn getValueSym(self: *EnumType, val: u16) *EnumMember {
        return self.members[val];
    }

    pub fn getMod(self: *EnumType) *cy.Module {
        return @ptrCast(&self.mod);
    }

    pub fn getMemberByIdx(self: *EnumType, idx: u32) *EnumMember {
        return self.members[idx];
    }

    pub fn getMember(self: *EnumType, name: []const u8) ?*EnumMember {
        const mod = self.head.getMod().?;
        if (mod.getSym(name)) |res| {
            if (res.type == .enumMember) {
                return res.cast(.enumMember);
            }
        }
        return null;
    }

    pub fn getMemberTag(self: *EnumType, name: []const u8) ?u32 {
        const member = self.getMember(name) orelse return null;
        return member.val;
    }
};

pub const EnumMember = extern struct {
    head: Sym,
    type: cy.TypeId,
    val: u32,
    payloadType: cy.Nullable(cy.TypeId),
    is_choice_type: bool,
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

pub const FuncTemplate = struct {
    sig: cy.sema.FuncSigId,

    params: []const FuncTemplateParam,

    /// Template args to variant. Keys are not owned.
    variant_cache: std.HashMapUnmanaged([]const cy.Value, *Variant, VariantKeyContext, 80),
    variants: std.ArrayListUnmanaged(*Variant),

    pub fn deinit(self: *FuncTemplate, alloc: std.mem.Allocator) void {
        for (self.variants.items) |variant| {
            alloc.destroy(variant);
        }
        self.variants.deinit(alloc);
        self.variant_cache.deinit(alloc);
        alloc.free(self.params);
    }
};

pub const FuncTemplateParam = struct {
    name: []const u8,
};

pub const FuncKind = enum {
    hostFunc,
    userFunc,
    userLambda,
    trait,
    template,
};

pub const Func = struct {
    type: FuncKind,
    next: ?*Func,

    /// For non-lambdas, this can refer to a FuncSym or Template.
    parent: *Sym,

    funcSigId: cy.sema.FuncSigId,
    // FuncDecl/LambdaExpr
    decl: ?*ast.Node,
    retType: cy.TypeId,

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
        template: *FuncTemplate,
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

    pub fn deinit(self: *Func, alloc: std.mem.Allocator) void {
        if (self.type == .template and self.isResolved()) {
            self.data.template.deinit(alloc);
            alloc.destroy(self.data.template);
        }
    }

    pub fn deinitValues(self: *Func, vm: *cy.VM) void {
        if (self.type != .template) {
            return;
        }

        if (self.isResolved()) {
            const template = self.data.template;

            var iter = template.variant_cache.iterator();
            while (iter.next()) |e| {
                const variant = e.value_ptr.*;
                for (variant.args) |arg| {
                    vm.release(arg);
                }
                vm.alloc.free(variant.args);
                variant.args = &.{};
            }
        }
    }

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

    pub fn getSymValueType(c: *cy.Chunk, sym: *Sym) !?cy.TypeId {
        switch (sym.type) {
            .context_var => return sym.cast(.context_var).type,
            .userVar    => return sym.cast(.userVar).type,
            .hostVar    => return sym.cast(.hostVar).type,
            .enumMember => {
                const member = sym.cast(.enumMember);
                if (member.payloadType != cy.NullId and member.is_choice_type) {
                    return null;
                }
                return member.type;
            },
            .use_alias  => return getSymValueType(c, sym.cast(.use_alias).sym),
            .enum_t,
            .type,
            .struct_t,
            .typeAlias,
            .hostobj_t,
            .trait_t,
            .object_t   => return bt.Type,
            .func => {
                const func = sym.cast(.func);
                if (func.numFuncs == 1) {
                    return try cy.sema.getFuncPtrType(c, func.first.funcSigId);
                } else {
                    return error.AmbiguousSymbol;
                }
            },
            .dummy_t,
            .placeholder,
            .field,
            .null,
            .module_alias,
            .distinct_t,
            .template,
            .chunk => return null,
        }
    }

    pub fn createDistinctType(c: *cy.Chunk, parent: *Sym, name: []const u8, decl: *ast.DistinctDecl) !*DistinctType {
        const sym = try createSym(c.alloc, .distinct_t, .{
            .head = Sym.init(.distinct_t, parent, name),
            .decl = decl,
            .type = cy.NullId,
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
            .type = cy.NullId,  // Null indicates it needs to be resolved later on.
            .sym = undefined,
            .mod = undefined,
            .resolved = false,
        });
        @as(*cy.Module, @ptrCast(&sym.mod)).* = cy.Module.init(c);
        try c.syms.append(c.alloc, @ptrCast(sym));
        return sym;
    } 

    pub fn createFloatType(c: *cy.Chunk, parent: *Sym, name: []const u8, bits: u8, opt_type_id: ?cy.TypeId) !*TypeSym {
        const type_id = opt_type_id orelse try c.sema.pushType();
        const sym = try createSym(c.alloc, .type, .{
            .head = Sym.init(.type, parent, name),
            .type = type_id,
            .mod = undefined,
            .variant = null,
        });
        @as(*cy.Module, @ptrCast(&sym.mod)).* = cy.Module.init(c);
        c.compiler.sema.types.items[type_id] = .{
            .sym = @ptrCast(sym),
            .kind = .float,
            .data = .{ .float = .{
                .bits = bits,
            }},
            .info = .{},
        };
        try c.syms.append(c.alloc, @ptrCast(sym));
        return sym;
    }

    pub fn createTypeSymCopy(c: *cy.Chunk, parent: *Sym, name: []const u8, opt_type_id: ?cy.TypeId, src_t: cy.TypeId) !*TypeSym {
        const type_id = opt_type_id orelse try c.sema.pushType();
        const sym = try createSym(c.alloc, .type, .{
            .head = Sym.init(.type, parent, name),
            .type = type_id,
            .mod = undefined,
            .variant = null,
        });
        @as(*cy.Module, @ptrCast(&sym.mod)).* = cy.Module.init(c);
        c.sema.types.items[type_id] = c.sema.types.items[src_t];
        c.sema.types.items[type_id].sym = @ptrCast(sym);
        try c.syms.append(c.alloc, @ptrCast(sym));
        return sym;
    }

    pub fn createFuncPtrType(c: *cy.Chunk, parent: *Sym, name: []const u8, opt_type_id: ?cy.TypeId, sig: cy.sema.FuncSigId, decl: *ast.Node) !*HostObjectType {
        const type_id = opt_type_id orelse try c.sema.pushType();
        const sym = try createSym(c.alloc, .hostobj_t, .{
            .head = Sym.init(.hostobj_t, parent, name),
            .decl = decl.cast(.custom_decl),
            .type = type_id,
            .getChildrenFn = null,
            .finalizerFn = null,
            .variant = null,
            .mod = undefined,
        });
        c.compiler.sema.types.items[type_id] = .{
            .sym = @ptrCast(sym),
            .kind = .func_ptr,
            .data = .{ .func_ptr = .{
                .sig = sig,
            }},
            .info = .{},
        };
        try c.syms.append(c.alloc, @ptrCast(sym));
        return sym;
    }

    pub fn createFuncSymType(c: *cy.Chunk, parent: *Sym, name: []const u8, opt_type_id: ?cy.TypeId, sig: cy.sema.FuncSigId, decl: *ast.Node) !*HostObjectType {
        const type_id = opt_type_id orelse try c.sema.pushType();
        const sym = try createSym(c.alloc, .hostobj_t, .{
            .head = Sym.init(.hostobj_t, parent, name),
            .decl = decl.cast(.custom_decl),
            .type = type_id,
            .getChildrenFn = null,
            .finalizerFn = null,
            .variant = null,
            .mod = undefined,
        });
        c.compiler.sema.types.items[type_id] = .{
            .sym = @ptrCast(sym),
            .kind = .func_sym,
            .data = .{ .func_sym = .{
                .sig = sig,
            }},
            .info = .{},
        };
        try c.syms.append(c.alloc, @ptrCast(sym));
        return sym;
    }

    pub fn createFuncUnionType(c: *cy.Chunk, parent: *Sym, name: []const u8, opt_type_id: ?cy.TypeId, sig: cy.sema.FuncSigId, decl: *ast.Node) !*HostObjectType {
        const type_id = opt_type_id orelse try c.sema.pushType();
        const sym = try createSym(c.alloc, .hostobj_t, .{
            .head = Sym.init(.hostobj_t, parent, name),
            .decl = decl.cast(.custom_decl),
            .type = type_id,
            .getChildrenFn = null,
            .finalizerFn = null,
            .variant = null,
            .mod = undefined,
        });
        c.compiler.sema.types.items[type_id] = .{
            .sym = @ptrCast(sym),
            .kind = .func_union,
            .data = .{ .func_union = .{
                .sig = sig,
            }},
            .info = .{},
        };
        try c.syms.append(c.alloc, @ptrCast(sym));
        return sym;
    }

    pub fn createArrayType(c: *cy.Chunk, parent: *Sym, name: []const u8, opt_type_id: ?cy.TypeId, n: usize, elem_t: cy.TypeId) !*TypeSym {
        const type_id = opt_type_id orelse try c.sema.pushType();
        const sym = try createSym(c.alloc, .type, .{
            .head = Sym.init(.type, parent, name),
            .type = type_id,
            .mod = undefined,
            .variant = null,
        });
        @as(*cy.Module, @ptrCast(&sym.mod)).* = cy.Module.init(c);
        c.compiler.sema.types.items[type_id] = .{
            .sym = @ptrCast(sym),
            .kind = .array,
            .data = .{ .array = .{
                .n = n,
                .elem_t = elem_t,
            }},
            .info = .{},
        };
        try c.syms.append(c.alloc, @ptrCast(sym));
        return sym;
    }

    pub fn createIntType(c: *cy.Chunk, parent: *Sym, name: []const u8, bits: u8, opt_type_id: ?cy.TypeId) !*TypeSym {
        const type_id = opt_type_id orelse try c.sema.pushType();
        const sym = try createSym(c.alloc, .type, .{
            .head = Sym.init(.type, parent, name),
            .type = type_id,
            .mod = undefined,
            .variant = null,
        });
        @as(*cy.Module, @ptrCast(&sym.mod)).* = cy.Module.init(c);
        c.compiler.sema.types.items[type_id] = .{
            .sym = @ptrCast(sym),
            .kind = .int,
            .data = .{ .int = .{
                .bits = bits,
            }},
            .info = .{},
        };
        try c.syms.append(c.alloc, @ptrCast(sym));
        return sym;
    }

    pub fn createBoolType(c: *cy.Chunk, parent: *Sym, name: []const u8, opt_type_id: ?cy.TypeId) !*TypeSym {
        const type_id = opt_type_id orelse try c.sema.pushType();
        const sym = try createSym(c.alloc, .type, .{
            .head = Sym.init(.type, parent, name),
            .type = type_id,
            .mod = undefined,
            .variant = null,
        });
        @as(*cy.Module, @ptrCast(&sym.mod)).* = cy.Module.init(c);
        c.compiler.sema.types.items[type_id] = .{
            .sym = @ptrCast(sym),
            .kind = .bool,
            .data = undefined,
            .info = .{},
        };
        try c.syms.append(c.alloc, @ptrCast(sym));
        return sym;
    }

    pub fn createField(c: *cy.Chunk, parent: *Sym, name: []const u8, idx: usize, typeId: cy.TypeId) !*Field {
        const sym = try createSym(c.alloc, .field, .{
            .head = Sym.init(.field, parent, name),
            .idx = @intCast(idx),
            .type = typeId,
        });
        return sym;
    }

    pub fn createEnumMember(c: *cy.Chunk, parent: *Sym, name: []const u8, typeId: cy.TypeId,
        is_choice_type: bool, val: u32, payloadType: cy.TypeId) !*EnumMember {
        const sym = try createSym(c.alloc, .enumMember, .{
            .head = Sym.init(.enumMember, parent, name),
            .type = typeId,
            .val = val,
            .payloadType = payloadType,
            .is_choice_type = is_choice_type,
        });
        return sym;
    }

    pub fn createContextVar(c: *cy.Chunk, parent: *Sym, name: []const u8, decl: *ast.ContextDecl) !*ContextVar {
        const sym = try createSym(c.alloc, .context_var, .{
            .head = Sym.init(.context_var, parent, name),
            .decl = decl,
            .type = cy.NullId,
            .idx = cy.NullU8,
        });
        try c.syms.append(c.alloc, @ptrCast(sym));
        return sym;
    }

    pub fn createUserVar(c: *cy.Chunk, parent: *Sym, name: []const u8, decl: ?*ast.StaticVarDecl) !*UserVar {
        const sym = try createSym(c.alloc, .userVar, .{
            .head = Sym.init(.userVar, parent, name),
            .decl = decl,
            .type = cy.NullId,
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
            .type = cy.NullId,
        });
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

    pub fn createHostObjectType(
        c: *cy.Chunk, parent: *Sym, name: []const u8, decl: *ast.CustomDecl,
    ) !*HostObjectType {
        const sym = try createSym(c.alloc, .hostobj_t, .{
            .head = Sym.init(.hostobj_t, parent, name),
            .decl = decl,
            .type = cy.NullId,
            .getChildrenFn = null,
            .finalizerFn = null,
            .variant = null,
            .mod = undefined,
        });
        @as(*cy.Module, @ptrCast(&sym.mod)).* = cy.Module.init(c);
        try c.syms.append(c.alloc, @ptrCast(sym));
        return sym;
    }

    pub fn createStructType(c: *cy.Chunk, parent: *Sym, name: []const u8, cstruct: bool, decl: *ast.ObjectDecl) !*ObjectType {
        const sym = try createSym(c.alloc, .struct_t, .{
            .head = Sym.init(.struct_t, parent, name),
            .decl = @ptrCast(decl),
            .type = cy.NullId,
            .fields = undefined,
            .variant = null,
            .numFields = cy.NullId,
            .rt_size = cy.NullId,
            .cstruct = cstruct,
            .mod = undefined,
            .impls_ptr = undefined,
            .impls_len = 0,
        });
        @as(*cy.Module, @ptrCast(&sym.mod)).* = cy.Module.init(c);
        try c.syms.append(c.alloc, @ptrCast(sym));
        return sym;
    }

    pub fn createStructTypeUnnamed(c: *cy.Chunk, parent: *Sym, name: []const u8, cstruct: bool, tuple: bool, decl: *ast.ObjectDecl) !*ObjectType {
        const mod = parent.getMod().?;
        _ = mod;

        const typeId = try c.sema.pushType();
        const sym = try createSym(c.alloc, .struct_t, .{
            .head = Sym.init(.struct_t, parent, name),
            .decl = @ptrCast(decl),
            .type = typeId,
            .fields = undefined,
            .variant = null,
            .numFields = cy.NullId,
            .rt_size = cy.NullId,
            .cstruct = cstruct,
            .mod = undefined,
            .impls_ptr = undefined,
            .impls_len = 0,
        });
        @as(*cy.Module, @ptrCast(&sym.mod)).* = cy.Module.init(c);
        c.compiler.sema.types.items[typeId] = .{
            .sym = @ptrCast(sym),
            .kind = .struct_t,
            .data = .{ .struct_t = .{
                .nfields = cy.NullU16,
                .cstruct = cstruct,
                .has_boxed_fields = false,
                .fields = undefined,
                .tuple = tuple,
            }},
            .info = .{},
        };

        try c.syms.append(c.alloc, @ptrCast(sym));

        // Update node's `name` so it can do a lookup during resolving.
        decl.name = @ptrCast(sym);

        return sym;
    }

    /// TODO: Hash object members for static casting.
    pub fn createObjectTypeUnnamed(c: *cy.Chunk, parent: *Sym, name: []const u8, decl: *ast.ObjectDecl) !*ObjectType {
        const typeId = try c.sema.pushType();
        const sym = try createSym(c.alloc, .object_t, .{
            .head = Sym.init(.object_t, parent, name),
            .decl = @ptrCast(decl),
            .type = typeId,
            .fields = undefined,
            .variant = null,
            .numFields = cy.NullId,
            .rt_size = cy.NullId,
            .cstruct = false,
            .mod = undefined,
            .impls_ptr = undefined,
            .impls_len = 0,
        });
        @as(*cy.Module, @ptrCast(&sym.mod)).* = cy.Module.init(c);
        c.compiler.sema.types.items[typeId] = .{
            .sym = @ptrCast(sym),
            .kind = .object,
            .data = .{ .object = .{
                .numFields = cy.NullU16,
                .has_boxed_fields = false,
                .fields = undefined,
                .tuple = false,
            }},
            .info = .{},
        };

        try c.syms.append(c.alloc, @ptrCast(sym));

        // Update node's `name` so it can do a lookup during resolving.
        decl.name = @ptrCast(sym);
        return sym;
    }

    pub fn createFunc(c: *cy.Chunk, ftype: FuncKind, parent: *Sym, node: ?*ast.Node, isMethod: bool) !*Func {
        const func = try c.alloc.create(Func);
        func.* = .{
            .type = ftype,
            .funcSigId = cy.NullId,
            .retType = cy.NullId,
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

    pub fn createObjectType(c: *cy.Chunk, parent: *Sym, name: []const u8, decl: ?*ast.Node) !*ObjectType {
        const sym = try createSym(c.alloc, .object_t,
            ObjectType.init(parent, c, name, decl, cy.NullId)
        );
        try c.syms.append(c.alloc, @ptrCast(sym));
        return sym;
    }

    pub fn createTraitType(c: *cy.Chunk, parent: *Sym, name: []const u8, decl: *ast.TraitDecl) !*TraitType {
        const sym = try createSym(c.alloc, .trait_t, .{
            .head = Sym.init(.trait_t, parent, name),
            .type = cy.NullId,
            .decl = decl,
            .members_ptr = undefined,
            .members_len = 0,
            .mod = undefined,
        });
        @as(*cy.Module, @ptrCast(&sym.mod)).* = cy.Module.init(c);
        try c.syms.append(c.alloc, @ptrCast(sym));
        return sym;
    }

    pub fn createEnumType(c: *cy.Chunk, parent: *Sym, name: []const u8, isChoiceType: bool, decl: *ast.EnumDecl) !*EnumType {
        const typeId = try c.sema.pushType();
        const sym = try createSym(c.alloc, .enum_t, .{
            .head = Sym.init(.enum_t, parent, name),
            .type = typeId,
            .decl = decl,
            .members = undefined,
            .numMembers = 0,
            .isChoiceType = isChoiceType,
            .variant = null,
            .mod = undefined,
        });
        @as(*cy.Module, @ptrCast(&sym.mod)).* = cy.Module.init(c);
        c.compiler.sema.types.items[typeId] = .{
            .sym = @ptrCast(sym),
            .kind = if (isChoiceType) .choice else .@"enum",
            .info = .{},
            .data = undefined,
        };
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
                try s.writeTypeName(w, arg.type.type, config.from);
                return;
            } else if (template == s.pointer_tmpl) {
                try w.writeAll("*");
                const arg = variant.args[0].asHeapObject();
                try s.writeTypeName(w, arg.type.type, config.from);
                return;
            } else if (template == s.ptr_slice_tmpl) {
                try w.writeAll("[*]");
                const arg = variant.args[0].asHeapObject();
                try s.writeTypeName(w, arg.type.type, config.from);
                return;
            } else if (template == s.ref_slice_tmpl) {
                try w.writeAll("[]");
                const arg = variant.args[0].asHeapObject();
                try s.writeTypeName(w, arg.type.type, config.from);
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
            const sym = s.getTypeSym(arg.asHeapObject().type.type);
            try writeSymName(s, w, sym, config);
        },
        bt.Integer => {
            try w.print("{}", .{arg.asBoxInt()});
        },
        else => {
            const type_e = s.getType(arg.getTypeId());
            if (type_e.kind == .func_sym) {
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
    var sym = s.getTypeSym(args[0].asHeapObject().type.type);
    try writeSymName(s, w, sym, config);
    for (args[1..]) |arg| {
        try w.writeByte(',');
        if (arg.getTypeId() != bt.Type) {
            return error.Unsupported;
        }
        sym = s.getTypeSym(arg.asHeapObject().type.type);
        try writeSymName(s, w, sym, config);
    }
}

test "sym internals" {
    if (builtin.mode == .ReleaseFast) {
        if (cy.is32Bit) {
            try t.eq(@sizeOf(Sym), 16);
            try t.eq(@sizeOf(Func), 36);
        } else {
            try t.eq(@sizeOf(Sym), 24);
            try t.eq(@sizeOf(Func), 56);
        }
    } else {
        if (cy.is32Bit) {
            try t.eq(@sizeOf(Sym), 16);
            try t.eq(@sizeOf(Func), 44);
        } else {
            try t.eq(@sizeOf(Sym), 24);
            try t.eq(@sizeOf(Func), 64);
        }
    }

    try t.eq(@offsetOf(Template, "head"), 0);

    try t.eq(@offsetOf(Sym, "type"), @offsetOf(vmc.SemaSym, "type"));
    try t.eq(@offsetOf(Sym, "parent"), @offsetOf(vmc.SemaSym, "parent"));
    try t.eq(@offsetOf(Sym, "namePtr"), @offsetOf(vmc.SemaSym, "namePtr"));
    try t.eq(@offsetOf(Sym, "nameLen"), @offsetOf(vmc.SemaSym, "nameLen"));
    try t.eq(@offsetOf(Sym, "metadata"), @offsetOf(vmc.SemaSym, "metadata"));
}