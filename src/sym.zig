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
            .metadata = @bitCast(Metadata{ .padding = undefined, .name_owned = false }),
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
    pub fn fromC(sym: cc.Sym) *Sym {
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
        switch (self.type) {
            .custom_object_t => {
                const hostType = self.cast(.custom_object_t);
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
            .enum_t => {
                const enumType = self.cast(.enum_t);
                enumType.deinit(alloc);
                alloc.free(enumType.members[0..enumType.numMembers]);
                alloc.destroy(enumType);
            },
            .template => {
                const template = self.cast(.template);

                for (template.variants.items) |variant| {
                    for (variant.params) |param| {
                        vm.release(param);
                    }
                    alloc.free(variant.params);
                }
                template.variants.deinit(alloc);
                template.variantCache.deinit(alloc);
                alloc.free(template.params);
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
            .userVar => {
                alloc.destroy(self.cast(.userVar));
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

    pub fn isVariable(self: Sym) bool {
        return self.type == .userVar or self.type == .hostVar;
    }

    pub fn isType(self: *Sym) bool {
        switch (self.type) {
            .use_alias => return self.cast(.use_alias).sym.isType(),
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
            .custom_object_t => return @ptrCast(&self.cast(.custom_object_t).mod),
            .bool_t          => return @ptrCast(&self.cast(.bool_t).mod),
            .int_t           => return @ptrCast(&self.cast(.int_t).mod),
            .float_t         => return @ptrCast(&self.cast(.float_t).mod),
            .placeholder     => return @ptrCast(&self.cast(.placeholder).mod),
            .distinct_t      => return @ptrCast(&self.cast(.distinct_t).mod),
            .typeAlias       => return @ptrCast(&self.cast(.typeAlias).mod),
            .use_alias,
            .module_alias,
            .template,
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
            .custom_object_t => return self.cast(.custom_object_t).type,
            .use_alias       => return self.cast(.use_alias).sym.getStaticType(),
            .placeholder,
            .null,
            .field,
            .template,
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
            .use_alias,
            .custom_object_t,
            .null,
            .field,
            .template,
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
    padding: u15,
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
        .template => Template,
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
    variant: u32,

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
    declId: cy.NodeId,
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
    decl: cy.NodeId,
    type: cy.TypeId,
    mod: vmc.Module,
    resolved: bool,

    pub fn getMod(self: *DistinctType) *cy.Module {
        return @ptrCast(&self.mod);
    }
};

pub const ValueType = extern struct {
    head: Sym,
};

pub const Template = struct {
    head: Sym,
    child_decl: cy.NodeId,

    sigId: cy.sema.FuncSigId,

    kind: SymType,

    /// Owned.
    params: []const TemplateParam,

    /// Template args to variant. Keys are not owned.
    variantCache: std.HashMapUnmanaged([]const cy.Value, u32, VariantKeyContext, 80),

    variants: std.ArrayListUnmanaged(Variant),

    pub fn chunk(self: *const Template) *cy.Chunk {
        return self.head.parent.?.getMod().?.chunk;
    }
};

pub const TemplateParam = struct {
    name: []const u8,
    type: cy.TypeId,
};

pub const Variant = struct {
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
    sym: *Field,
    type: cy.TypeId,
};

pub const ObjectType = extern struct {
    head: Sym,
    type: cy.TypeId,
    declId: cy.NodeId,
    fields: [*]const FieldInfo,
    numFields: u32,

    rt_size: cy.Nullable(u32),

    /// If not null, the parent points to Template sym.
    variantId: u32,

    mod: vmc.Module,

    pub fn isResolved(self: *ObjectType) bool {
        return self.numFields != cy.NullId;
    }

    pub fn init(parent: *Sym, chunk: *cy.Chunk, name: []const u8, decl_id: cy.NodeId, type_id: cy.TypeId) ObjectType {
        var new = ObjectType{
            .head = Sym.init(.object_t, parent, name),
            .declId = decl_id,
            .type = type_id,
            .fields = undefined,
            .variantId = cy.NullId,
            .numFields = cy.NullId,
            .rt_size = cy.NullId,
            .mod = undefined,
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
    decl: cy.NodeId,
    type: cy.TypeId,
    members: [*]*EnumMember,
    numMembers: u32,
    mod: vmc.Module,

    /// If not null, the parent points to Template sym.
    variantId: u32,

    isChoiceType: bool,

    pub fn deinit(self: *EnumType, alloc: std.mem.Allocator) void {
        self.getMod().deinit(alloc);
        for (self.members[0..self.numMembers]) |m| {
            alloc.destroy(m);
        }
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
    declId: cy.NodeId,
    sym: *Sym,
};

pub const UseAlias = extern struct {
    head: Sym,
    decl: cy.NodeId,
    sym: *Sym,
    resolved: bool,
};

pub const Chunk = extern struct {
    head: Sym,
    mod: vmc.Module,

    pub fn sym(self: *Chunk) *Sym {
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
    is_implicit_method: bool,
    throws: bool,

    /// Whether it has already emitted IR.
    emitted: bool,

    pub fn isResolved(self: Func) bool {
        return self.funcSigId != cy.NullId;
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

pub const ChunkExt = struct {

    pub fn createDistinctType(c: *cy.Chunk, parent: *Sym, name: []const u8, decl_id: cy.NodeId, opt_type_id: ?cy.TypeId) !*DistinctType {
        const type_id = opt_type_id orelse try c.sema.pushType();
        const sym = try createSym(c.alloc, .distinct_t, .{
            .head = Sym.init(.distinct_t, parent, name),
            .decl = decl_id,
            .type = type_id,
            .mod = undefined,
            .resolved = false,
        });
        sym.getMod().* = cy.Module.init(c);
        c.compiler.sema.types.items[type_id] = .{
            .sym = @ptrCast(sym),
            .kind = .null,
            .data = undefined,
        };
        try c.syms.append(c.alloc, @ptrCast(sym));
        return sym;
    }

    pub fn createTypeAlias(c: *cy.Chunk, parent: *Sym, name: []const u8, declId: cy.NodeId) !*TypeAlias {
        const sym = try createSym(c.alloc, .typeAlias, .{
            .head = Sym.init(.typeAlias, parent, name),
            .declId = declId,
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
        };
        try c.syms.append(c.alloc, @ptrCast(sym));
        return sym;
    }

    pub fn createField(c: *cy.Chunk, parent: *Sym, name: []const u8, idx: u32, typeId: cy.TypeId) !*Field {
        const sym = try createSym(c.alloc, .field, .{
            .head = Sym.init(.field, parent, name),
            .idx = idx,
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

    pub fn createUserVar(c: *cy.Chunk, parent: *Sym, name: []const u8, declId: cy.NodeId) !*UserVar {
        const sym = try createSym(c.alloc, .userVar, .{
            .head = Sym.init(.userVar, parent, name),
            .declId = declId,
            .type = cy.NullId,
        });
        try c.syms.append(c.alloc, @ptrCast(sym));
        return sym;
    }

    pub fn createHostVar(c: *cy.Chunk, parent: *Sym, name: []const u8, declId: cy.NodeId) !*HostVar {
        const sym = try createSym(c.alloc, .hostVar, .{
            .head = Sym.init(.hostVar, parent, name),
            .retainedIdx = cy.NullU16,
            .val = cy.Value.Void,
            .declId = declId,
            .type = cy.NullId,
        });
        try c.syms.append(c.alloc, @ptrCast(sym));
        return sym;
    }

    pub fn createTemplate(c: *cy.Chunk, parent: *Sym, name: []const u8,
        sigId: cy.sema.FuncSigId, params: []const TemplateParam, kind: SymType,
        child_decl: cy.NodeId) !*Template {
        const sym = try createSym(c.alloc, .template, .{
            .head = Sym.init(.template, parent, name),
            .kind = kind,
            .child_decl = child_decl,
            .params = params,
            .sigId = sigId,
            .variants = .{},
            .variantCache = .{},
        });
        try c.syms.append(c.alloc, @ptrCast(sym));
        return sym;
    }

    pub fn createChunkSym(c: *cy.Chunk, name: []const u8) !*Chunk {
        const sym = try createSym(c.alloc, .chunk, .{
            .head = Sym.init(.chunk, null, name),
            .mod = undefined,
        });
        @as(*cy.Module, @ptrCast(&sym.mod)).* = cy.Module.init(c);
        try c.syms.append(c.alloc, @ptrCast(sym));
        return sym;
    }

    pub fn createUseAlias(c: *cy.Chunk, parent: *Sym, name: []const u8, decl: cy.NodeId) !*UseAlias {
        const sym = try createSym(c.alloc, .use_alias, .{
            .head = Sym.init(.use_alias, parent, name),
            .decl = decl,
            .sym = undefined,
            .resolved = false,
        });
        try c.syms.append(c.alloc, @ptrCast(sym));
        return sym;
    } 

    pub fn createModuleAlias(c: *cy.Chunk, parent: *Sym, name: []const u8, importedSym: *Sym, declId: cy.NodeId) !*ModuleAlias {
        const sym = try createSym(c.alloc, .module_alias, .{
            .head = Sym.init(.import, parent, name),
            .declId = declId,
            .sym = importedSym,
        });
        try c.syms.append(c.alloc, @ptrCast(sym));
        return sym;
    }

    pub fn createCustomObjectType(
        c: *cy.Chunk, parent: *Sym, name: []const u8, declId: cy.NodeId,
        getChildrenFn: cc.ObjectGetChildrenFn, finalizerFn: cc.ObjectFinalizerFn, opt_type_id: ?cy.TypeId,
    ) !*CustomObjectType {
        const type_id = opt_type_id orelse try c.sema.pushType();
        const sym = try createSym(c.alloc, .custom_object_t, .{
            .head = Sym.init(.custom_object_t, parent, name),
            .declId = declId,
            .type = type_id,
            .getChildrenFn = getChildrenFn,
            .finalizerFn = finalizerFn,
            .mod = undefined,
        });
        @as(*cy.Module, @ptrCast(&sym.mod)).* = cy.Module.init(c);
        c.compiler.sema.types.items[type_id] = .{
            .sym = @ptrCast(sym),
            .kind = .custom_object,
            .data = .{ .custom_object = .{
                .getChildrenFn = getChildrenFn,
                .finalizerFn = finalizerFn,
            }},
        };
        try c.syms.append(c.alloc, @ptrCast(sym));
        return sym;
    }

    pub fn createStructType(c: *cy.Chunk, parent: *Sym, name: []const u8, declId: cy.NodeId, opt_type_id: ?cy.TypeId) !*ObjectType {
        const typeId = opt_type_id orelse try c.sema.pushType();
        const sym = try createSym(c.alloc, .struct_t, .{
            .head = Sym.init(.struct_t, parent, name),
            .declId = declId,
            .type = typeId,
            .fields = undefined,
            .variantId = cy.NullId,
            .numFields = cy.NullId,
            .rt_size = cy.NullId,
            .mod = undefined,
        });
        @as(*cy.Module, @ptrCast(&sym.mod)).* = cy.Module.init(c);
        c.compiler.sema.types.items[typeId] = .{
            .sym = @ptrCast(sym),
            .kind = .@"struct",
            .data = .{ .@"struct" = .{
                .numFields = cy.NullU16,
            }},
        };
        try c.syms.append(c.alloc, @ptrCast(sym));
        return sym;
    }

    pub fn createStructTypeUnnamed(c: *cy.Chunk, parent: *Sym, name: []const u8, declId: cy.NodeId) !*ObjectType {
        const mod = parent.getMod().?;

        const typeId = try c.sema.pushType();
        const sym = try createSym(c.alloc, .struct_t, .{
            .head = Sym.init(.struct_t, parent, name),
            .declId = declId,
            .type = typeId,
            .fields = undefined,
            .variantId = cy.NullId,
            .numFields = cy.NullId,
            .rt_size = cy.NullId,
            .mod = undefined,
        });
        @as(*cy.Module, @ptrCast(&sym.mod)).* = cy.Module.init(c);
        c.compiler.sema.types.items[typeId] = .{
            .sym = @ptrCast(sym),
            .kind = .@"struct",
            .data = .{ .@"struct" = .{
                .numFields = cy.NullU16,
            }},
        };

        const sym_id = c.syms.items.len;
        try c.syms.append(c.alloc, @ptrCast(sym));

        // Update node's `name` so it can do a lookup during resolving.
        const node = mod.chunk.ast.node(declId);
        mod.chunk.parser.ast.nodePtr(node.data.objectDecl.header).data.objectHeader.name = @intCast(sym_id);

        return sym;
    }

    /// TODO: Hash object members for static casting.
    pub fn createObjectTypeUnnamed(c: *cy.Chunk, parent: *Sym, name: []const u8, declId: cy.NodeId) !*ObjectType {
        const typeId = try c.sema.pushType();
        const sym = try createSym(c.alloc, .object_t, .{
            .head = Sym.init(.object_t, parent, name),
            .declId = declId,
            .type = typeId,
            .fields = undefined,
            .variantId = cy.NullId,
            .numFields = cy.NullId,
            .rt_size = cy.NullId,
            .mod = undefined,
        });
        @as(*cy.Module, @ptrCast(&sym.mod)).* = cy.Module.init(c);
        c.compiler.sema.types.items[typeId] = .{
            .sym = @ptrCast(sym),
            .kind = .object,
            .data = .{ .object = .{
                .numFields = cy.NullU16,
            }},
        };

        const sym_id = c.syms.items.len;
        try c.syms.append(c.alloc, @ptrCast(sym));

        // Update node's `name` so it can do a lookup during resolving.
        const node = c.ast.node(declId);
        c.parser.ast.nodePtr(node.data.objectDecl.header).data.objectHeader.name = @intCast(sym_id);
        return sym;
    }

    pub fn createFunc(c: *cy.Chunk, ftype: FuncType, parent: *Sym, sym: ?*FuncSym, nodeId: cy.NodeId, isMethod: bool) !*Func {
        const func = try c.alloc.create(Func);
        func.* = .{
            .type = ftype,
            .funcSigId = cy.NullId,
            .retType = cy.NullId,
            .reqCallTypeCheck = undefined,
            .sym = sym,
            .throws = false,
            .parent = parent,
            .isMethod = isMethod,
            .is_implicit_method = false,
            .numParams = undefined,
            .declId = nodeId,
            .next = null,
            .data = undefined,
            .emitted = false,
        };
        try c.funcs.append(c.alloc, func);
        return func;
    }

    pub fn createFuncSym(c: *cy.Chunk, parent: *Sym, name: []const u8) !*FuncSym {
        const sym = try createSym(c.alloc, .func, .{
            .head = Sym.init(.func, parent, name),
            .numFuncs = 0,
            .firstFuncSig = cy.NullId,
            .first = undefined,
            .last = undefined,
            .variant = cy.NullId,
        });
        try c.syms.append(c.alloc, @ptrCast(sym));
        return sym;
    }

    pub fn createFuncSymVariant(c: *cy.Chunk, parent: *Template, variant: u32) !*FuncSym {
        const name = parent.head.name();
        const sym = try createFuncSym(c, @ptrCast(parent), name);
        sym.variant = variant;
        return sym;
    }

    pub fn createObjectType(c: *cy.Chunk, parent: *Sym, name: []const u8, declId: cy.NodeId, opt_type_id: ?cy.TypeId) !*ObjectType {
        const type_id = opt_type_id orelse try c.sema.pushType();
        const sym = try createSym(c.alloc, .object_t,
            ObjectType.init(parent, c, name, declId, type_id)
        );
        c.compiler.sema.types.items[type_id] = .{
            .sym = @ptrCast(sym),
            .kind = .object,
            .data = .{ .object = .{
                .numFields = cy.NullU16,
            }},
        };
        try c.syms.append(c.alloc, @ptrCast(sym));
        return sym;
    }

    pub fn createObjectTypeVariant(c: *cy.Chunk, parent: *Template, variantId: u32) !*ObjectType {
        const name = parent.head.name();
        const sym = try createObjectType(c, @ptrCast(parent), name, parent.child_decl, null);
        sym.variantId = variantId;
        c.compiler.sema.types.items[sym.type] = .{
            .sym = @ptrCast(sym),
            .kind = .object,
            .data = .{ .object = .{
                .numFields = cy.NullU16,
            }},
        };
        return sym;
    }

    pub fn createEnumType(c: *cy.Chunk, parent: *Sym, name: []const u8, isChoiceType: bool, declId: cy.NodeId) !*EnumType {
        const typeId = try c.sema.pushType();
        const sym = try createSym(c.alloc, .enum_t, .{
            .head = Sym.init(.enum_t, parent, name),
            .type = typeId,
            .decl = declId,
            .members = undefined,
            .numMembers = 0,
            .isChoiceType = isChoiceType,
            .variantId = cy.NullId,
            .mod = undefined,
        });
        @as(*cy.Module, @ptrCast(&sym.mod)).* = cy.Module.init(c);
        c.compiler.sema.types.items[typeId] = .{
            .sym = @ptrCast(sym),
            .kind = if (isChoiceType) .choice else .@"enum",
            .data = undefined,
        };
        try c.syms.append(c.alloc, @ptrCast(sym));
        return sym;
    }

    pub fn createEnumTypeVariant(c: *cy.Chunk, parent: *Template, isChoiceType: bool, variantId: u32) !*EnumType {
        const name = parent.head.name();
        const sym = try createEnumType(c, @ptrCast(parent), name, isChoiceType, parent.child_decl);
        sym.variantId = variantId;
        if (parent == c.sema.option_tmpl) {
            c.compiler.sema.types.items[sym.type].kind = .option;
        }
        return sym;
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

    try t.eq(@offsetOf(Template, "head"), 0);

    try t.eq(@offsetOf(Sym, "type"), @offsetOf(vmc.SemaSym, "type"));
    try t.eq(@offsetOf(Sym, "parent"), @offsetOf(vmc.SemaSym, "parent"));
    try t.eq(@offsetOf(Sym, "namePtr"), @offsetOf(vmc.SemaSym, "namePtr"));
    try t.eq(@offsetOf(Sym, "nameLen"), @offsetOf(vmc.SemaSym, "nameLen"));
    try t.eq(@offsetOf(Sym, "metadata"), @offsetOf(vmc.SemaSym, "metadata"));
}