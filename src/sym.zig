const std = @import("std");
const builtin = @import("builtin");
const cy = @import("cyber.zig");
const cc = @import("capi.zig");
const vmc = cy.vmc;
const bt = cy.types.BuiltinTypes;
const stdx = @import("stdx");
const t = stdx.testing;
const log = cy.log.scoped(.sym);
const fmt = cy.fmt;
const v = fmt.v;

pub const SymType = enum(u8) {
    null,
    userVar,
    hostVar,
    func,
    custom_object_t,
    object_t,
    struct_t,
    // pointer_t,
    bool_t,
    int_t,
    float_t,
    module_alias,
    chunk,
    enum_t,
    enumMember,
    typeAlias,

    /// Unresolved distinct type.
    distinct_t,

    typeTemplate,
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
            .metadata = @bitCast(Metadata{ .padding = undefined, .name_owned = false, .distinct_t = false }),
            .type = symT,
            .parent = parent,
        };
    }

    pub fn deinit(self: *Sym, alloc: std.mem.Allocator) void {
        if (self.getMetadata().name_owned) {
            alloc.free(self.name());
        }
    }

    pub fn toC(self: *Sym) cc.Sym {
        return .{ .ptr = self };
    }
    pub fn fromC(sym: cc.Sym) *cy.Sym {
        return @ptrCast(@alignCast(sym.ptr));
    }

    /// This is mainly used at the end of execution to release values
    /// so that the global rc can be compared against 0.
    pub fn deinitRetained(self: *Sym, vm: *cy.VM) void {
        if (self.getMod()) |mod| {
            mod.deinitRetained(vm);
        }

        switch (self.type) {
            .typeTemplate => {
                const template = self.cast(.typeTemplate);

                for (template.variants.items) |*variant| {
                    for (variant.params) |param| {
                        cy.arc.release(vm, param);
                    }
                    vm.alloc.free(variant.params);
                    variant.params = &.{};
                }
            },
            else => {},
        }
    }

    pub fn destroy(self: *Sym, vm: *cy.VM, alloc: std.mem.Allocator) void {
        self.deinit(alloc);

        var size: usize = undefined;
        switch (self.type) {
            .custom_object_t => {
                const hostType = self.cast(.custom_object_t);
                hostType.getMod().deinit(alloc);
                size = @sizeOf(CustomObjectType);
            },
            .bool_t => {
                const bool_t = self.cast(.bool_t);
                bool_t.getMod().deinit(alloc);
                size = @sizeOf(BoolType);
            },
            .int_t => {
                const int_t = self.cast(.int_t);
                int_t.getMod().deinit(alloc);
                size = @sizeOf(IntType);
            },
            .float_t => {
                const float_t = self.cast(.float_t);
                float_t.getMod().deinit(alloc);
                size = @sizeOf(FloatType);
            },
            .chunk => {
                const chunk = self.cast(.chunk);
                chunk.getMod().deinit(alloc);
                size = @sizeOf(Chunk);
            },
            .struct_t => {
                const obj = self.cast(.struct_t);
                obj.getMod().deinit(alloc);
                alloc.free(obj.fields[0..obj.numFields]);
                size = @sizeOf(ObjectType);
            },
            .object_t => {
                const obj = self.cast(.object_t);
                obj.getMod().deinit(alloc);
                alloc.free(obj.fields[0..obj.numFields]);
                size = @sizeOf(ObjectType);
            },
            .enum_t => {
                const enumType = self.cast(.enum_t);
                enumType.getMod().deinit(alloc);
                alloc.free(enumType.members[0..enumType.numMembers]);
                size = @sizeOf(EnumType);
            },
            .typeTemplate => {
                const typeTemplate = self.cast(.typeTemplate);

                for (typeTemplate.variants.items) |variant| {
                    for (variant.params) |param| {
                        vm.release(param);
                    }
                    alloc.free(variant.params);
                    alloc.free(variant.patchNodes);
                    variant.sym.destroy(vm, alloc);
                }
                typeTemplate.variants.deinit(alloc);
                typeTemplate.variantCache.deinit(alloc);
                alloc.free(typeTemplate.params);

                size = @sizeOf(TypeTemplate);
            },
            .distinct_t => {
                size = @sizeOf(TypeSym);
            },
            .field => {
                size = @sizeOf(Field);
            },
            .typeAlias => {
                size = @sizeOf(TypeAlias);
            },
            .enumMember => {
                size = @sizeOf(EnumMember);
            },
            .module_alias => {
                size = @sizeOf(ModuleAlias);
            },
            .func => {
                size = @sizeOf(FuncSym);
            },
            .userVar => {
                size = @sizeOf(UserVar);
            },
            .hostVar => {
                size = @sizeOf(HostVar);
            },
            .placeholder => {
                size = @sizeOf(Placeholder);
            },
            .null => {
                size = 0;
            },
            //     const child = self.cast(symT);
            //     size = @sizeOf();
            // },
        }

        if (self.getMetadata().distinct_t) {
            size = @sizeOf(TypeSym);
        }

        const slice = @as([*]const align(8) u8, @ptrCast(self))[0..size];
        alloc.free(slice);
    }

    pub fn getMetadata(self: Sym) Metadata {
        return @bitCast(self.metadata);
    }

    pub fn setNameOwned(self: *Sym, name_owned: bool) void {
        var metadata = self.getMetadata();
        metadata.name_owned = name_owned;
        self.metadata = @bitCast(metadata);
    }

    pub fn setDistinctType(self: *Sym, distinct_t: bool) void {
        var metadata = self.getMetadata();
        metadata.distinct_t = distinct_t;
        self.metadata = @bitCast(metadata);
    }

    pub fn isVariable(self: Sym) bool {
        return self.type == .userVar or self.type == .hostVar;
    }

    pub fn isType(self: Sym) bool {
        switch (self.type) {
            .custom_object_t,
            .bool_t,
            .int_t,
            .float_t,
            .typeAlias,
            .struct_t,
            .object_t,
            .enum_t => {
                return true;
            },
            .null,
            .userVar,
            .hostVar,
            .module_alias,
            .func,
            .typeTemplate,
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
            .custom_object_t => return @ptrCast(&self.cast(.custom_object_t).mod),
            .bool_t          => return @ptrCast(&self.cast(.bool_t).mod),
            .int_t           => return @ptrCast(&self.cast(.int_t).mod),
            .float_t         => return @ptrCast(&self.cast(.float_t).mod),
            .placeholder     => return @ptrCast(&self.cast(.placeholder).mod),
            .distinct_t      => return @ptrCast(&self.cast(.distinct_t).mod),
            .typeAlias       => return @ptrCast(&self.cast(.typeAlias).mod),
            .module_alias,
            .typeTemplate,
            .enumMember,
            .func,
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

    pub fn getValueType(self: *Sym) !?cy.TypeId {
        switch (self.type) {
            .userVar    => return self.cast(.userVar).type,
            .hostVar    => return self.cast(.hostVar).type,
            .enumMember => return self.cast(.enumMember).type,
            .enum_t,
            .int_t,
            .float_t,
            .struct_t,
            .bool_t,
            .typeAlias,
            .custom_object_t,
            .object_t   => return bt.MetaType,
            .func => {
                const func = self.cast(.func);
                if (func.numFuncs == 1) {
                    return getFuncType(func.first);
                } else {
                    return error.AmbiguousSymbol;
                }
            },
            .placeholder,
            .field,
            .null,
            .module_alias,
            .distinct_t,
            .typeTemplate,
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
            .custom_object_t => return self.cast(.custom_object_t).type,
            .placeholder,
            .null,
            .field,
            .typeTemplate,
            .enumMember,
            .func,
            .module_alias,
            .distinct_t,
            .chunk,
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
            .custom_object_t,
            .null,
            .field,
            .typeTemplate,
            .enumMember,
            .func,
            .module_alias,
            .distinct_t,
            .chunk,
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

    pub fn formatAbsPath(self: *const Sym, buf: []u8) ![]const u8 {
        var fbuf = std.io.fixedBufferStream(buf);
        try self.writeAbsPath(fbuf.writer());
        return fbuf.getWritten();
    }

    pub fn allocAbsPath(self: *const Sym, alloc: std.mem.Allocator) ![]const u8 {
        const parent = self.parent orelse {
            return alloc.dupe(u8, self.name());
        };

        var buf: std.ArrayListUnmanaged(u8) = .{};
        defer buf.deinit(alloc);
        const w = buf.writer(alloc);
        try parent.writeAbsPath(w);
        try buf.append(alloc, '.');
        try buf.appendSlice(alloc, self.name());
        return buf.toOwnedSlice(alloc);
    }

    fn writeAbsPath(self: *const Sym, w: anytype) !void {
        const parent = self.parent orelse {
            try w.writeAll(self.name());
            return;
        };
        try parent.writeAbsPath(w);
        try w.writeByte('.');
        try w.writeAll(self.name());
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
    distinct_t: bool,
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
        .userVar => UserVar,
        .hostVar => HostVar,
        .struct_t,
        .object_t => ObjectType,
        .custom_object_t => CustomObjectType,
        .bool_t => BoolType,
        .int_t => IntType,
        .float_t => FloatType,
        .enum_t => EnumType,
        .enumMember => EnumMember,
        .typeAlias => TypeAlias,
        .distinct_t => DistinctType,
        .typeTemplate => TypeTemplate,
        .field => Field,
        .module_alias => ModuleAlias,
        .chunk => Chunk,
        .placeholder => Placeholder,
        .null => void,
    };
}

pub const Placeholder = extern struct {
    head: Sym,
    mod: vmc.Module,

    /// Once the placeholder is replaced, keep a reference to the sym so that
    /// parent references can be updated during the sym resolve step.
    sym: *cy.Sym,

    pub fn getMod(self: *Placeholder) *cy.Module {
        return @ptrCast(&self.mod);
    }
};

/// A type sym union is defined so that TypeCopy can allocate enough memory for the largest Sym.
pub const TypeSym = extern union {
    object_t: ObjectType,
    custom_object_t: CustomObjectType,
    bool_t: BoolType,
    int_t: IntType,
    float_t: FloatType,
    enum_t: EnumType,
    distinct_t: DistinctType,
};

pub const HostVar = extern struct {
    head: Sym,
    declId: cy.Nullable(cy.NodeId),
    type: cy.TypeId,
    val: cy.Value,

    // Index into `retainedVars`.
    retainedIdx: u16,
};

pub const UserVar = extern struct {
    head: Sym,
    declId: cy.NodeId,
    type: cy.TypeId,
};

/// Sym that links to overloaded functions.
pub const FuncSym = extern struct {
    head: Sym,
    first: *Func,
    last: *Func,
    numFuncs: u16,

    /// Duped to perform uniqueness check without dereferencing the first ModuleFunc.
    firstFuncSig: cy.sema.FuncSigId,
};

pub const TypeAlias = extern struct {
    head: Sym,
    declId: cy.NodeId,
    type: cy.TypeId,
    sym: *Sym,
    mod: vmc.Module,
    resolved: bool,

    pub fn getMod(self: *TypeAlias) *cy.Module {
        return @ptrCast(&self.mod);
    }
};

pub const DistinctType = extern struct {
    head: Sym,
    decl_id: cy.NodeId,
    type: cy.TypeId,
    mod: vmc.Module,

    pub fn getMod(self: *DistinctType) *cy.Module {
        return @ptrCast(&self.mod);
    }
};

pub const ValueType = extern struct {
    head: Sym,
};

pub const TemplateKind = enum {
    object_t,
    enum_t,
};

pub const TypeTemplate = struct {
    head: Sym,
    declId: cy.NodeId,
    sigId: cy.sema.FuncSigId,

    kind: TemplateKind,

    /// Owned.
    params: []const TemplateParam,

    /// Template args to variant. Keys are not owned.
    variantCache: std.HashMapUnmanaged([]const cy.Value, u32, VariantKeyContext, 80),

    variants: std.ArrayListUnmanaged(Variant),

    /// Unowned, tracked root nodes in this template that begin a compile-time expression.
    /// This array allows each variant to build their replacement AST
    /// without scanning the entire template again for compile-time expressions.
    ctNodes: []const cy.NodeId,

    pub fn chunk(self: *const TypeTemplate) *cy.Chunk {
        return self.head.parent.?.getMod().?.chunk;
    }
};

pub const TemplateParam = struct {
    name: []const u8,
    type: cy.TypeId,
};

const Variant = struct {
    /// Not used after the template expansion. Consider removing.
    patchNodes: []const cy.NodeId,

    /// Owned args. Can be used to print params along with the template type.
    params: []const cy.Value,

    sym: *Sym,
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

pub const Field = extern struct {
    head: Sym,
    idx: u32,
    type: cy.TypeId,
};

pub const FieldInfo = packed struct {
    sym: *cy.Sym,
    type: cy.TypeId,
};

pub const ObjectType = extern struct {
    head: Sym,
    type: cy.TypeId,
    declId: cy.NodeId,
    fields: [*]const FieldInfo,
    numFields: u32,

    rt_size: cy.Nullable(u32),

    /// If not null, the parent points to TypeTemplate sym.
    variantId: u32,

    mod: vmc.Module,

    pub fn init(parent: *Sym, chunk: *cy.Chunk, name: []const u8, decl_id: cy.NodeId, type_id: cy.TypeId) ObjectType {
        var new = ObjectType{
            .head = cy.Sym.init(.object_t, parent, name),
            .declId = decl_id,
            .type = type_id,
            .fields = undefined,
            .variantId = cy.NullId,
            .numFields = 0,
            .rt_size = cy.NullId,
            .mod = undefined,
        };
        @as(*cy.Module, @ptrCast(&new.mod)).* = cy.Module.init(chunk);
        return new;
    }

    pub fn getMod(self: *ObjectType) *cy.Module {
        return @ptrCast(&self.mod);
    }

    pub fn getFields(self: ObjectType) []const FieldInfo {
        return self.fields[0..self.numFields];
    }
};

pub const CustomObjectType = extern struct {
    head: Sym,
    type: cy.TypeId,
    declId: cy.NodeId,
    getChildrenFn: cc.ObjectGetChildrenFn,
    finalizerFn: cc.ObjectFinalizerFn,
    mod: vmc.Module,

    pub fn getMod(self: *CustomObjectType) *cy.Module {
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
    type: cy.TypeId,
    members: [*]*EnumMember,
    numMembers: u32,
    mod: vmc.Module,

    /// If not null, the parent points to TypeTemplate sym.
    variantId: u32,

    isChoiceType: bool,

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
};

pub const ModuleAlias = extern struct {
    head: Sym,
    declId: cy.NodeId,
    sym: *Sym,
};

pub const Chunk = extern struct {
    head: Sym,
    mod: vmc.Module,

    pub fn sym(self: *Chunk) *cy.Sym {
        return @ptrCast(self);
    }

    pub fn getMod(self: *Chunk) *cy.Module {
        return @ptrCast(&self.mod);
    }
};

pub const FuncType = enum {
    hostFunc,
    userFunc,
    userLambda,
};

pub const Func = struct {
    type: FuncType,
    next: ?*Func,
    sym: ?*FuncSym,

    /// For non-lambdas, this is equivalent to `sym.parent`.
    parent: *Sym,

    funcSigId: cy.sema.FuncSigId,
    declId: cy.NodeId,
    retType: cy.TypeId,
    data: extern union {
        hostFunc: extern struct {
            ptr: cy.ZHostFuncFn,
        },
        hostInlineFunc: extern struct {
            ptr: cy.ZHostFuncFn,
        },
    },
    reqCallTypeCheck: bool,
    numParams: u8,
    isMethod: bool,
    throws: bool,

    pub fn isGenerated(self: Func) bool {
        return self.declId == cy.NullId;
    }

    pub fn isStatic(self: Func) bool {
        return self.type != .userLambda;
    }

    pub fn hasStaticInitializer(self: Func) bool {
        return self.type == .hostFunc;
    }

    pub fn name(self: Func) []const u8 {
        if (self.type == .userLambda) {
            return "lambda";
        }
        return self.sym.?.head.name();
    }
};

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
        try t.eq(@sizeOf(Sym), 24);
        if (cy.is32Bit) {
            try t.eq(@sizeOf(Func), 8);
        } else {
            try t.eq(@sizeOf(Func), 56);
        }
    }

    try t.eq(@offsetOf(TypeTemplate, "head"), 0);

    try t.eq(@offsetOf(Sym, "type"), @offsetOf(vmc.SemaSym, "type"));
    try t.eq(@offsetOf(Sym, "parent"), @offsetOf(vmc.SemaSym, "parent"));
    try t.eq(@offsetOf(Sym, "namePtr"), @offsetOf(vmc.SemaSym, "namePtr"));
    try t.eq(@offsetOf(Sym, "nameLen"), @offsetOf(vmc.SemaSym, "nameLen"));
    try t.eq(@offsetOf(Sym, "metadata"), @offsetOf(vmc.SemaSym, "metadata"));
}