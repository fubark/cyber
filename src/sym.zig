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
    custom_t,
    object_t,
    struct_t,
    trait_t,
    // pointer_t,
    bool_t,
    int_t,
    float_t,
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
    pub fn deinitRetained(self: *Sym, vm: *cy.VM) void {
        if (self.getMod()) |mod| {
            mod.deinitRetained(vm);
        }

        switch (self.type) {
            .template => {
                const template = self.cast(.template);

                var iter = template.variant_cache.iterator();
                while (iter.next()) |e| {
                    const variant = e.value_ptr.*;
                    for (variant.args) |arg| {
                        cy.arc.release(vm, arg);
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
            .custom_t => {
                const hostType = self.cast(.custom_t);
                hostType.getMod().deinit(alloc);
                alloc.destroy(hostType);
            },
            .bool_t => {
                const bool_t = self.cast(.bool_t);
                bool_t.getMod().deinit(alloc);
                alloc.destroy(bool_t);
            },
            .int_t => {
                const int_t = self.cast(.int_t);
                int_t.getMod().deinit(alloc);
                alloc.destroy(int_t);
            },
            .float_t => {
                const float_t = self.cast(.float_t);
                float_t.getMod().deinit(alloc);
                alloc.destroy(float_t);
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
                template.deinit(alloc, vm);
                alloc.destroy(template);
            },
            .distinct_t => {
                const distinct_t = self.cast(.distinct_t);
                if (!distinct_t.resolved) {
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
                alloc.destroy(self.cast(.userVar));
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
            .object_t => return self.cast(.object_t).variant,
            .custom_t => return self.cast(.custom_t).variant,
            .enum_t   => return self.cast(.enum_t).variant,
            else => return null,
        }
    }

    pub fn isVariable(self: Sym) bool {
        return self.type == .userVar or self.type == .hostVar;
    }

    pub fn isType(self: *Sym) bool {
        switch (self.type) {
            .use_alias => return self.cast(.use_alias).sym.isType(),
            .custom_t,
            .bool_t,
            .int_t,
            .float_t,
            .typeAlias,
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
            .struct_t        => return @ptrCast(&self.cast(.struct_t).mod),
            .object_t        => return @ptrCast(&self.cast(.object_t).mod),
            .custom_t        => return @ptrCast(&self.cast(.custom_t).mod),
            .bool_t          => return @ptrCast(&self.cast(.bool_t).mod),
            .int_t           => return @ptrCast(&self.cast(.int_t).mod),
            .float_t         => return @ptrCast(&self.cast(.float_t).mod),
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

    pub fn getValueType(self: *Sym) !?cy.TypeId {
        switch (self.type) {
            .context_var => return self.cast(.context_var).type,
            .userVar    => return self.cast(.userVar).type,
            .hostVar    => return self.cast(.hostVar).type,
            .enumMember => {
                const member = self.cast(.enumMember);
                if (member.payloadType != cy.NullId and member.is_choice_type) {
                    return null;
                }
                return member.type;
            },
            .use_alias  => return self.cast(.use_alias).sym.getValueType(),
            .enum_t,
            .int_t,
            .float_t,
            .struct_t,
            .bool_t,
            .typeAlias,
            .custom_t,
            .trait_t,
            .object_t   => return bt.MetaType,
            .func => {
                const func = self.cast(.func);
                if (func.numFuncs == 1) {
                    return getFuncType(func.first);
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

    pub fn getStaticType(self: *Sym) ?cy.TypeId {
        switch (self.type) {
            .bool_t          => return self.cast(.bool_t).type,
            .int_t           => return self.cast(.int_t).type,
            .float_t         => return self.cast(.float_t).type,
            .enum_t          => return self.cast(.enum_t).type,
            .typeAlias       => return self.cast(.typeAlias).type,
            .struct_t        => return self.cast(.struct_t).type,
            .object_t        => return self.cast(.object_t).type,
            .trait_t         => return self.cast(.trait_t).type,
            .custom_t        => return self.cast(.custom_t).type,
            .use_alias       => return self.cast(.use_alias).sym.getStaticType(),
            .dummy_t         => return self.cast(.dummy_t).type,
            .placeholder,
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
            .userVar         => return null,
        }
    }

    pub fn getFields(self: *Sym) ?[]const FieldInfo {
        switch (self.type) {
            .enum_t          => return &[_]FieldInfo{
                .{ .sym = undefined, .type = bt.Integer },
                .{ .sym = undefined, .type = bt.Any },
            },
            .struct_t        => return self.cast(.struct_t).getFields(),
            .object_t        => return self.cast(.object_t).getFields(),
            .placeholder,
            .bool_t,
            .int_t,
            .float_t,
            .typeAlias,
            .use_alias,
            .trait_t,
            .custom_t,
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

fn getFuncType(func: *Func) cy.TypeId {
    _ = func;
    return bt.Any;
}

fn SymChild(comptime symT: SymType) type {
    return switch (symT) {
        .func => FuncSym,
        .context_var => ContextVar,
        .userVar => UserVar,
        .hostVar => HostVar,
        .struct_t,
        .object_t => ObjectType,
        .trait_t => TraitType,
        .custom_t => CustomType,
        .bool_t => BoolType,
        .int_t => IntType,
        .float_t => FloatType,
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

pub const UserVar = extern struct {
    head: Sym,
    decl: ?*ast.StaticVarDecl,
    type: cy.TypeId,

    pub fn isResolved(self: *UserVar) bool {
        return self.type != cy.NullId;
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
    variant: u32,
    resolved: bool,

    pub fn getMod(self: *DistinctType) *cy.Module {
        return @ptrCast(&self.mod);
    }
};

pub const ValueType = extern struct {
    head: Sym,
};

/// TODO: Consider splitting into Template and FuncTemplate.
pub const Template = struct {
    head: Sym,
    child_decl: *ast.Node,

    sigId: cy.sema.FuncSigId,

    kind: SymType,

    is_root: bool,

    /// Owned by root template.
    params: []TemplateParam,

    /// Template args to variant. Keys are not owned.
    variant_cache: std.HashMapUnmanaged([]const cy.Value, *Variant, VariantKeyContext, 80),

    mod: vmc.Module,

    fn deinit(self: *Template, alloc: std.mem.Allocator, vm: *cy.VM) void {
        self.getMod().deinit(alloc);

        var iter = self.variant_cache.iterator();
        while (iter.next()) |e| {
            const variant = e.value_ptr.*;
            for (variant.args) |arg| {
                vm.release(arg);
            }
            alloc.free(variant.args);
            alloc.destroy(variant);
        }
        self.variant_cache.deinit(alloc);
        if (self.is_root) {
            alloc.free(self.params);
        }
    }

    pub fn getMod(self: *Template) *cy.Module {
        return @ptrCast(&self.mod);
    }

    pub fn root(self: *Template) *Template {
        if (self.is_root) {
            return self;
        } else {
            return self.head.parent.?.cast(.template).root();
        }
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

pub const TemplateParam = struct {
    name: []const u8,
    type: cy.TypeId,
};

const VariantType = enum(u8) {
    specialization,
    sym,
};

pub const Variant = struct {
    type: VariantType,

    /// Back link to the root template.
    /// Can be used to check what template the symbol comes from. Also used for pushing the variant context.
    root_template: *Template,

    /// Owned args. Can be used to print params along with the template type.
    args: []const cy.Value,

    data: union {
        specialization: *ast.Node,
        sym: *Sym,
    },
};

const VariantKeyContext = struct {
    pub fn hash(_: VariantKeyContext, key: []const cy.Value) u64 {
        var c = std.hash.Wyhash.init(0);
        for (key) |val| {
            switch (val.getTypeId()) {
                bt.Type => {
                    const typeId = val.asHeapObject().type.type;
                    c.update(std.mem.asBytes(&typeId));
                },
                else => {
                    cy.rt.logZFmt("Unsupported value hash: {}", .{val.getTypeId()});
                    @panic("");
                }
            }
        }
        return c.final();
    }

    pub fn eql(_: VariantKeyContext, a: []const cy.Value, b: []const cy.Value) bool {
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
                else => {
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

pub const CustomType = extern struct {
    head: Sym,
    type: cy.TypeId,
    decl: *ast.CustomDecl,
    getChildrenFn: C.GetChildrenFn,
    finalizerFn: C.FinalizerFn,
    mod: vmc.Module,
    variant: ?*Variant,

    pub fn getMod(self: *CustomType) *cy.Module {
        return @ptrCast(&self.mod);
    }
};

pub const FloatType = extern struct {
    head: Sym,
    type: cy.TypeId,
    mod: vmc.Module,
    bits: u8,

    pub fn getMod(self: *FloatType) *cy.Module {
        return @ptrCast(&self.mod);
    }
};

pub const IntType = extern struct {
    head: Sym,
    type: cy.TypeId,
    mod: vmc.Module,
    bits: u8,

    pub fn getMod(self: *IntType) *cy.Module {
        return @ptrCast(&self.mod);
    }
};

pub const BoolType = extern struct {
    head: Sym,
    type: cy.TypeId,
    mod: vmc.Module,

    pub fn getMod(self: *BoolType) *cy.Module {
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

pub const FuncVariant = struct {
    /// Owned args. Can be used to print params along with the template func.
    args: []const cy.Value,

    template: *FuncTemplate,

    func: *Func,
};

pub const FuncTemplate = struct {
    sig: cy.sema.FuncSigId,

    params: []const FuncTemplateParam,

    /// Template args to variant. Keys are not owned.
    variant_cache: std.HashMapUnmanaged([]const cy.Value, *FuncVariant, VariantKeyContext, 80),

    pub fn deinit(self: *FuncTemplate, alloc: std.mem.Allocator) void {
        var iter = self.variant_cache.iterator();
        while (iter.next()) |e| {
            alloc.destroy(e.value_ptr.*);
        }
        self.variant_cache.deinit(alloc);
        alloc.free(self.params);
    }
};

pub const FuncTemplateParam = struct {
    name: []const u8,
};

pub const FuncType = enum {
    hostFunc,
    userFunc,
    userLambda,
    trait,
    template,
};

pub const Func = struct {
    type: FuncType,
    next: ?*Func,
    sym: ?*FuncSym,

    /// For non-lambdas, this is equivalent to `sym.parent`.
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
    },
    variant: ?*FuncVariant,
    reqCallTypeCheck: bool,
    numParams: u8,

    is_method: bool,

    /// Whether the function was declared nested in a type.
    is_nested: bool,
    throws: bool,

    /// Whether it has already emitted IR.
    emitted: bool,

    pub fn deinit(self: *Func, alloc: std.mem.Allocator) void {
        if (self.type == .template and self.isResolved()) {
            self.data.template.deinit(alloc);
            alloc.destroy(self.data.template);
        }
    }

    pub fn deinitRetained(self: *Func, vm: *cy.VM) void {
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
        return self.parent.getMod().?.chunk;
    }

    pub fn name(self: Func) []const u8 {
        if (self.type == .userLambda) {
            return "lambda";
        }
        return self.sym.?.head.name();
    }
};

pub const ChunkExt = struct {

    pub fn createDistinctType(c: *cy.Chunk, parent: *Sym, name: []const u8, decl: *ast.DistinctDecl) !*DistinctType {
        const sym = try createSym(c.alloc, .distinct_t, .{
            .head = Sym.init(.distinct_t, parent, name),
            .decl = decl,
            .type = cy.NullId,
            .mod = undefined,
            .resolved = false,
            .variant = cy.NullId,
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

    pub fn createFloatType(c: *cy.Chunk, parent: *Sym, name: []const u8, bits: u8, opt_type_id: ?cy.TypeId) !*FloatType {
        const type_id = opt_type_id orelse try c.sema.pushType();
        const sym = try createSym(c.alloc, .float_t, .{
            .head = Sym.init(.float_t, parent, name),
            .type = type_id,
            .mod = undefined,
            .bits = bits,
        });
        @as(*cy.Module, @ptrCast(&sym.mod)).* = cy.Module.init(c);
        c.compiler.sema.types.items[type_id] = .{
            .sym = @ptrCast(sym),
            .kind = .float,
            .data = undefined,
            .info = .{},
        };
        try c.syms.append(c.alloc, @ptrCast(sym));
        return sym;
    }

    pub fn createIntType(c: *cy.Chunk, parent: *Sym, name: []const u8, bits: u8, opt_type_id: ?cy.TypeId) !*IntType {
        const type_id = opt_type_id orelse try c.sema.pushType();
        const sym = try createSym(c.alloc, .int_t, .{
            .head = Sym.init(.int_t, parent, name),
            .type = type_id,
            .mod = undefined,
            .bits = bits,
        });
        @as(*cy.Module, @ptrCast(&sym.mod)).* = cy.Module.init(c);
        c.compiler.sema.types.items[type_id] = .{
            .sym = @ptrCast(sym),
            .kind = .int,
            .data = undefined,
            .info = .{},
        };
        try c.syms.append(c.alloc, @ptrCast(sym));
        return sym;
    }

    pub fn createBoolType(c: *cy.Chunk, parent: *Sym, name: []const u8, opt_type_id: ?cy.TypeId) !*BoolType {
        const type_id = opt_type_id orelse try c.sema.pushType();
        const sym = try createSym(c.alloc, .bool_t, .{
            .head = Sym.init(.bool_t, parent, name),
            .type = type_id,
            .mod = undefined,
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
        sigId: cy.sema.FuncSigId, is_root: bool, params: []TemplateParam, kind: SymType,
        child_decl: *ast.Node) !*Template {
        const sym = try createSym(c.alloc, .template, .{
            .head = Sym.init(.template, parent, name),
            .kind = kind,
            .child_decl = child_decl,
            .is_root = is_root,
            .params = params,
            .sigId = sigId,
            .variant_cache = .{},
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

    pub fn createCustomType(
        c: *cy.Chunk, parent: *Sym, name: []const u8, decl: *ast.CustomDecl,
    ) !*CustomType {
        const sym = try createSym(c.alloc, .custom_t, .{
            .head = Sym.init(.custom_t, parent, name),
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

    pub fn createStructType(c: *cy.Chunk, parent: *Sym, name: []const u8, decl: *ast.ObjectDecl) !*ObjectType {
        const sym = try createSym(c.alloc, .struct_t, .{
            .head = Sym.init(.struct_t, parent, name),
            .decl = @ptrCast(decl),
            .type = cy.NullId,
            .fields = undefined,
            .variant = null,
            .numFields = cy.NullId,
            .rt_size = cy.NullId,
            .mod = undefined,
            .impls_ptr = undefined,
            .impls_len = 0,
        });
        @as(*cy.Module, @ptrCast(&sym.mod)).* = cy.Module.init(c);
        try c.syms.append(c.alloc, @ptrCast(sym));
        return sym;
    }

    pub fn createStructTypeUnnamed(c: *cy.Chunk, parent: *Sym, name: []const u8, decl: *ast.ObjectDecl) !*ObjectType {
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
            .mod = undefined,
            .impls_ptr = undefined,
            .impls_len = 0,
        });
        @as(*cy.Module, @ptrCast(&sym.mod)).* = cy.Module.init(c);
        c.compiler.sema.types.items[typeId] = .{
            .sym = @ptrCast(sym),
            .kind = .@"struct",
            .data = .{ .@"struct" = .{
                .numFields = cy.NullU16,
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
            }},
            .info = .{},
        };

        try c.syms.append(c.alloc, @ptrCast(sym));

        // Update node's `name` so it can do a lookup during resolving.
        decl.name = @ptrCast(sym);
        return sym;
    }

    pub fn createFunc(c: *cy.Chunk, ftype: FuncType, parent: *Sym, sym: ?*FuncSym, node: ?*ast.Node, isMethod: bool) !*Func {
        const func = try c.alloc.create(Func);
        func.* = .{
            .type = ftype,
            .funcSigId = cy.NullId,
            .retType = cy.NullId,
            .reqCallTypeCheck = undefined,
            .sym = sym,
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

    pub fn createEnumTypeVariant(c: *cy.Chunk, parent: *Sym, template: *Template, isChoiceType: bool, variant: *Variant) !*EnumType {
        const name = template.head.name();
        const sym = try createEnumType(c, parent, name, isChoiceType, template.child_decl.cast(.enumDecl));
        sym.variant = variant;
        if (template == c.sema.option_tmpl) {
            c.compiler.sema.types.items[sym.type].kind = .option;
        }
        return sym;
    }
};

const SymFormatConfig = struct {
    from: ?*cy.Chunk = null,
    emit_template_args: bool = true,
};

pub fn writeFuncName(s: *cy.Sema, w: anytype, func: *cy.Func, config: SymFormatConfig) !void {
    try writeParentPrefix(s, w, @ptrCast(func.sym.?), config);
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
            if (variant.root_template == s.option_tmpl) {
                const arg = variant.args[0].asHeapObject();
                const name = s.getTypeBaseName(arg.type.type);
                try w.print("?{s}", .{name});
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

fn writeLocalTemplateArgs(s: *cy.Sema, w: anytype, variant: *cy.sym.Variant, config: SymFormatConfig) !void {
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

fn writeLocalFuncTemplateArgs(s: *cy.Sema, w: anytype, variant: *cy.sym.FuncVariant, config: SymFormatConfig) !void {
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
            try t.eq(@sizeOf(Func), 40);
        } else {
            try t.eq(@sizeOf(Sym), 24);
            try t.eq(@sizeOf(Func), 64);
        }
    } else {
        if (cy.is32Bit) {
            try t.eq(@sizeOf(Sym), 16);
            try t.eq(@sizeOf(Func), 40);
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