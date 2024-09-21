const std = @import("std");
const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const C = @import("capi.zig");
const rt = cy.rt;
const sema = cy.sema;
const fmt = @import("fmt.zig");
const v = fmt.v;
const vmc = @import("vm_c.zig");
const log = cy.log.scoped(.types);
const ast = cy.ast;

pub const TypeId = u32;

pub const TypeKind = enum(u8) {
    null = vmc.TYPE_KIND_NULL,
    bool = vmc.TYPE_KIND_BOOL,
    int = vmc.TYPE_KIND_INT,
    float = vmc.TYPE_KIND_FLOAT,
    hostobj = vmc.TYPE_KIND_HOSTOBJ,
    enum_t = vmc.TYPE_KIND_ENUM,
    choice = vmc.TYPE_KIND_CHOICE,
    struct_t = vmc.TYPE_KIND_STRUCT,
    option = vmc.TYPE_KIND_OPTION,
    trait = vmc.TYPE_KIND_TRAIT,
    bare = vmc.TYPE_KIND_BARE,
    ct_ref = vmc.TYPE_KIND_CTREF,
    array = vmc.TYPE_KIND_ARRAY,
    func_ptr = vmc.TYPE_KIND_FUNC_PTR,
    func_union = vmc.TYPE_KIND_FUNC_UNION,
    func_sym = vmc.TYPE_KIND_FUNC_SYM,
    pointer = vmc.TYPE_KIND_PTR,
};

pub const TypeInfo = packed struct {
    is_future: bool = false,

    /// Whether this type or a child parameter contains a ct_ref.
    ct_ref: bool = false,

    padding: u6 = undefined,
};

pub const Type = extern struct {
    kind_: TypeKind,
    sym_: *cy.sym.TypeSym,
    id_: TypeId,
    info: TypeInfo,
    has_get_method: bool = false,
    has_set_method: bool = false,
    has_init_pair_method: bool = false,

    /// Optional since not all types have inlined retained members.
    /// TODO: Retain layouts should belong to specific types but it's here in `Type` for convenience.
    retain_layout: ?*RetainLayout = null,
    retain_layout_owned: bool = true,

    pub fn id(self: *Type) TypeId {
        return self.id_;
    }

    pub fn kind(self: *Type) TypeKind {
        return self.kind_;
    }

    pub fn name(self: *Type) []const u8 {
        return self.sym_.head.name();
    }

    pub fn isVmObject(self: *Type) bool {
        switch (self.kind()) {
            .choice,
            .option,
            .array,
            .struct_t => {
                return true;
            },
            else => {
                return false;
            }
        }
    }

    pub fn selfOrPointee(self: *Type) *Type {
        if (self.kind() == .pointer) {
            return self.cast(.pointer).child_t;
        } else {
            return self;
        }
    }

    pub fn size(self: *Type) u64 {
        switch (self.kind()) {
            .struct_t => return self.cast(.struct_t).size,
            .option => return self.cast(.option).size,
            .array => return self.cast(.array).size,
            .hostobj => return 1,
            .int => return 1,
            .float => return 1,
            .pointer => return 1,
            else => std.debug.panic("TODO: {} size", .{self.kind()}),
        }
    }

    pub fn sym(self: *Type) *cy.sym.TypeSym {
        if (cy.Trace) {
            if (self.kind() == .null) {
                cy.panic("Null type does not have a symbol.");
            }
        }
        return self.sym_;
    }

    pub fn cast(self: *Type, comptime tkind: TypeKind) *TypeImpl(tkind) {
        if (cy.Trace) {
            if (self.kind() != tkind) {
                std.debug.panic("Expected {}, found {}.", .{tkind, self.kind()});
            }
        }
        return @ptrCast(@alignCast(self));
    }

    pub fn destroy(self: *Type, alloc: std.mem.Allocator) void {
        if (self.retain_layout) |layout| {
            if (self.retain_layout_owned) {
                layout.destroy(alloc);
            }
        }
        switch (self.kind()) {
            .null => {},
            .choice => {
                const impl = self.cast(.choice);
                impl.deinit(alloc);
                alloc.destroy(impl);
            },
            .enum_t => {
                const impl = self.cast(.enum_t);
                impl.deinit(alloc);
                alloc.destroy(impl);
            },
            .struct_t => {
                const impl = self.cast(.struct_t);
                impl.deinit(alloc);
                alloc.destroy(impl);
            },
            .trait => {
                const impl = self.cast(.trait);
                impl.deinit(alloc);
                alloc.destroy(impl);
            },
            .pointer => {
                const impl = self.cast(.pointer);
                alloc.destroy(impl);
            },
            .func_union => {
                const impl = self.cast(.func_union);
                alloc.destroy(impl);
            },
            .func_sym => {
                const impl = self.cast(.func_sym);
                alloc.destroy(impl);
            },
            .func_ptr => {
                const impl = self.cast(.func_ptr);
                alloc.destroy(impl);
            },
            .hostobj => {
                const impl = self.cast(.hostobj);
                alloc.destroy(impl);
            },
            .bool => {
                const impl = self.cast(.bool);
                alloc.destroy(impl);
            },
            .int => {
                const impl = self.cast(.int);
                alloc.destroy(impl);
            },
            .float => {
                const impl = self.cast(.float);
                alloc.destroy(impl);
            },
            .array => {
                const impl = self.cast(.array);
                alloc.destroy(impl);
            },
            .ct_ref => {
                const impl = self.cast(.ct_ref);
                alloc.destroy(impl);
            },
            .bare => {
                const impl = self.cast(.bare);
                alloc.destroy(impl);
            },
            .option => {
                const impl = self.cast(.option);
                alloc.destroy(impl);
            },
        }
    }

    /// Whether this type is boxed by default. 
    /// The inverse means that the type is unboxable to an 8-byte Value.
    pub fn isBoxed(self: *Type) bool {
        switch (self.id()) {
            bt.String,
            bt.Map,
            bt.MapIter,
            bt.Fiber,
            bt.Type,
            bt.Dyn,
            bt.ExternFunc,
            bt.Symbol,
            bt.Error,
            bt.Float,
            bt.Any => return true,
            bt.Integer,
            bt.Void,
            bt.Byte,
            bt.Boolean => return false,
            else => {
                switch (self.kind()) {
                    .option,
                    .choice,
                    .array,
                    .trait,
                    .hostobj,
                    .struct_t => return true,
                    .enum_t,
                    .bool,
                    .float,
                    .int => return false,
                    .pointer => {
                        if (self.cast(.pointer).ref) {
                            return true;
                        } else {
                            return false;
                        }
                    },
                    .func_ptr,
                    .func_union,
                    .func_sym => {
                        return true;
                    },
                    .null,
                    .bare,
                    .ct_ref => {
                        std.debug.panic("Unexpected type kind: {}", .{self.kind()});
                    },
                }
            }
        }
    }

    pub fn isRefType(self: *Type) bool {
        return self.kind() == .pointer and self.cast(.pointer).ref;
    }

    pub fn isPointer(self: *Type) bool {
        return self.kind() == .pointer and !self.cast(.pointer).ref;
    }

    pub fn fields(self: *Type) ?[]const Field {
        switch (self.kind()) {
            .struct_t => return self.cast(.struct_t).fields(),
            .choice => return self.cast(.choice).fields(),
            .option => return self.cast(.option).fields(),
            else => return null, 
        }
    }
};

fn TypeImpl(comptime kind: TypeKind) type {
    return switch (kind) {
        .ct_ref     => CtRef,
        .trait      => Trait,
        .option     => Option,
        .choice     => Choice,
        .enum_t     => Enum,
        .hostobj    => HostObject,
        .array      => Array,
        .struct_t   => Struct,
        .pointer    => Pointer,
        .func_ptr,
        .func_sym,
        .func_union => Func,
        .int        => Int,
        .float      => Float,
        else => Empty,
    };
}

pub const Float = extern struct {
    base: Type = undefined,

    bits: u32,
};

pub const Int = extern struct {
    base: Type = undefined,

    bits: u32,
};

pub const Empty = extern struct {
    base: Type = undefined,
};

pub const CtRef = extern struct {
    base: Type = undefined,

    ct_param_idx: u32,
};

pub const Enum = extern struct {
    base: Type = undefined,

    cases_ptr: [*]*cy.sym.EnumCase = undefined,
    cases_len: u32 = 0,

    pub fn deinit(self: *Enum, alloc: std.mem.Allocator) void {
        alloc.free(self.cases());
    }

    pub fn isResolved(self: *Enum) bool {
        return self.cases_len != 0;
    }

    pub fn getValueSym(self: *Enum, val: u16) *cy.sym.EnumCase {
        return self.cases_ptr[val];
    }

    pub fn getCaseByIdx(self: *Enum, idx: u32) *cy.sym.EnumCase {
        return self.cases_ptr[idx];
    }

    pub fn cases(self: *Enum) []*cy.sym.EnumCase {
        return self.cases_ptr[0..self.cases_len];
    }

    pub fn getCase(self: *Enum, name: []const u8) ?*cy.sym.EnumCase {
        const mod = self.base.sym().getMod();
        if (mod.getSym(name)) |res| {
            if (res.type == .enum_case) {
                return res.cast(.enum_case);
            }
        }
        return null;
    }

    pub fn getCaseTag(self: *Enum, name: []const u8) ?u32 {
        const case = self.getCase(name) orelse return null;
        return case.val;
    }
};

pub const Option = extern struct {
    base: Type = undefined,

    child_t: *Type,
    size: cy.Nullable(u32) = cy.NullId,

    fields_arr: [2]Field = undefined,

    pub fn isResolved(self: *Option) bool {
        return self.size != cy.NullId;
    }

    pub fn fields(self: *Option) []const Field {
        return &self.fields_arr;
    }
};

pub const Choice = extern struct {
    base: Type = undefined,

    /// Maximum payload type size in addition to the tag size.
    size: cy.Nullable(u32) = cy.NullId,

    cases_ptr: [*]*cy.sym.ChoiceCase = undefined,
    cases_len: u32 = 0,

    fields_arr: [2]Field = undefined,

    pub fn deinit(self: *Choice, alloc: std.mem.Allocator) void {
        alloc.free(self.cases());
    }

    pub fn isResolved(self: *Choice) bool {
        return self.cases_len != 0;
    }

    pub fn cases(self: *Choice) []*cy.sym.ChoiceCase {
        return self.cases_ptr[0..self.cases_len];
    }

    pub fn getCaseByIdx(self: *Choice, idx: u32) *cy.sym.ChoiceCase {
        return self.cases_ptr[idx];
    }

    pub fn getCase(self: *Choice, name: []const u8) ?*cy.sym.ChoiceCase {
        const mod = self.base.sym().getMod();
        if (mod.getSym(name)) |res| {
            if (res.type == .choice_case) {
                return res.cast(.choice_case);
            }
        }
        return null;
    }

    pub fn fields(self: *Choice) []const Field {
        return &self.fields_arr;
    }
};

pub const HostObject = extern struct {
    base: Type = undefined,

    getChildrenFn: C.GetChildrenFn = null,
    finalizerFn: C.FinalizerFn = null,

    /// If `true`, invoke finalizer before releasing children.
    pre: bool = false,
};

/// TODO: Hash fields for static casting.
pub const Struct = extern struct {
    base: Type = undefined,

    /// Top level fields.
    fields_ptr: [*]Field = undefined,
    fields_len: u32 = cy.NullId,
    fields_owned: bool = true,

    size: cy.Nullable(u32) = cy.NullId,

    /// Linear lookup since most types don't have many impls.
    impls_ptr: [*]Impl = undefined,
    impls_len: u32 = cy.NullId,

    cstruct: bool,
    tuple: bool,

    /// Only relevant for structs/cstructs.
    /// Used to detect circular dependency.
    resolving_struct: bool = false,

    fn deinit(self: *Struct, alloc: std.mem.Allocator) void {
        if (self.isResolved()) {
            if (self.fields_owned) {
                alloc.free(self.fields());
            }

            for (self.impls()) |impl| {
                impl.deinit(alloc);
            }
            alloc.free(self.impls());
        }
    }

    pub fn fields(self: *Struct) []Field {
        return self.fields_ptr[0..self.fields_len];
    }

    pub fn isResolved(self: *Struct) bool {
        return self.fields_len != cy.NullId;
    }

    pub fn impls(self: *Struct) []Impl {
        return self.impls_ptr[0..self.impls_len];
    }

    pub fn implements(self: *Struct, trait_t: *Trait) bool {
        for (self.impls()) |impl| {
            if (impl.trait == trait_t) {
                return true;
            }
        }
        return false;
    }
};

pub const Impl = struct {
    trait: *Trait,

    funcs: []*cy.Func,

    fn deinit(self: Impl, alloc: std.mem.Allocator) void {
        alloc.free(self.funcs);
    }
};

pub const TraitMember = struct {
    func: *cy.Func,
};

pub const Trait = extern struct {
    base: Type = undefined,

    members_ptr: [*]const TraitMember = undefined,
    members_len: u32 = 0,

    pub fn deinit(self: *Trait, alloc: std.mem.Allocator) void {
        alloc.free(self.members());
    }

    pub fn members(self: *Trait) []const TraitMember {
        return self.members_ptr[0..self.members_len];
    }
};

pub const Func = extern struct {
    base: Type = undefined,

    sig: cy.sema.FuncSigId,
};

pub const Array = extern struct {
    base: Type = undefined,

    n: u64,
    elem_t: *cy.Type,
    size: u64 = 0,
    resolved: bool = false,
};

pub const Pointer = extern struct {
    base: Type = undefined,

    ref: bool,
    child_t: *Type,
};

pub const Field = extern struct {
    sym: *cy.sym.Field,
    type: *cy.Type,
    /// For struct/cstruct. Field offset from the start of the parent.
    offset: u32,
};

const RetainLayoutKind = enum(u8) {
    struct_k,
    array,
    union_k,
    option,
};

pub const RetainCase = extern struct {
    /// If `null`, case is a simple retained value.
    layout: ?*RetainLayout
};

pub const RetainLayout = extern struct {
    kind: RetainLayoutKind,
    data: extern union {
        option: RetainCase,
        union_k: extern struct {
            ptr: [*]const ?RetainCase,
            len: usize,

            pub fn layouts(self: *@This()) []const ?RetainCase {
                return self.ptr[0..self.len];
            }
        },
        array: extern struct {
            n: u32,
            elem_size: u32,

            /// If `null`, elem is a simple retained value.
            layout: ?*RetainLayout,
        },
        struct_k: extern struct {
            ptr: [*]const RetainEntry,
            len: usize,

            pub fn entries(self: *@This()) []const RetainEntry {
                return self.ptr[0..self.len];
            }
        },
    },

    pub fn destroy(self: *RetainLayout, alloc: std.mem.Allocator) void {
        switch (self.kind) {
            .union_k => {
                alloc.free(self.data.union_k.layouts());
            },
            .struct_k => {
                const entries = self.data.struct_k.entries();
                for (entries) |e| {
                    if (e.kind == .refs) {
                        alloc.free(e.data.refs.ptr[0..e.data.refs.len]);
                    }
                }
                alloc.free(entries);
            },
            .option,
            .array => {},
        }
        alloc.destroy(self);
    }
};

const RetainEntryKind = enum(u8) {
    refs,
    layout,
};

pub const RetainEntry = extern struct {
    kind: RetainEntryKind,

    /// `Value` offset from the parent type. Will eventually be a byte offset instead.
    offset: u32, 

    data: extern union {
        refs: extern struct {
            ptr: [*]const u8,
            len: usize,
        },
        layout: *RetainLayout,
    },
};

pub const RtType = extern struct {
    type: *Type,
    kind: TypeKind,
    // Duped to avoid lookup from `sym`.
    // symType: cy.sym.SymType,
    has_get_method: bool = false,
    has_set_method: bool = false,
    has_init_pair_method: bool = false,

    info: TypeInfo,

    data: extern union {
        // Even though this increases the size of other type entries, it might not be worth
        // separating into another table since it would add another indirection.
        host_object: extern struct {
            getChildrenFn: C.GetChildrenFn,
            finalizerFn: C.FinalizerFn,
        },
        option: extern struct {
            // Size of tag type or child type.
            max_fields: u16,

            child_t: cy.TypeId,
        },
        // This is duped from StructType so that object creation/destruction avoids the lookup from `sym`.
        struct_t: extern struct {
            /// Total size, currently indicates number of `Value`s but will end up being the byte size.
            size: u16,

            cstruct: bool,
            tuple: bool,

            // boxed_entries_ptr: [*]BoxedEntry,
            // boxed_entries_len: u16,

            // pub fn boxedEntries(self: *const @This()) []BoxedEntry {
            //     return self.boxed_entries_ptr[0..self.boxed_entries_len];
            // }
        },
        ct_ref: extern struct {
            ct_param_idx: u32,
        },
        array: extern struct {
            n: usize,
            elem_t: cy.TypeId,
        },
        float: extern struct {
            bits: u8,
        },
        int: extern struct {
            bits: u8,
        },
        pointer: extern struct {
            ref: bool,
            child_t: cy.TypeId,
        },
        func_ptr: extern struct {
            sig: cy.sema.FuncSigId,
        },
        func_union: extern struct {
            sig: cy.sema.FuncSigId,
        },
        func_sym: extern struct {
            sig: cy.sema.FuncSigId,
        },
    },

    pub fn name(self: *const RtType) []const u8 {
        return self.type.name();
    }

    pub fn sym(self: *const RtType) *cy.sym.TypeSym {
        return self.type.sym();
    }

    pub fn isBoxed(self: *const RtType) bool {
        return self.type.isBoxed();
    }
};

test "types internals." {
    try t.eq(@sizeOf(RtType), @sizeOf(vmc.TypeEntry));
    try t.eq(@offsetOf(RtType, "kind"), @offsetOf(vmc.TypeEntry, "kind"));
    try t.eq(@offsetOf(RtType, "has_get_method"), @offsetOf(vmc.TypeEntry, "has_get_method"));
    try t.eq(@offsetOf(RtType, "has_set_method"), @offsetOf(vmc.TypeEntry, "has_set_method"));
    try t.eq(@offsetOf(RtType, "data"), @offsetOf(vmc.TypeEntry, "data"));
}

pub const PrimitiveEnd: TypeId = vmc.PrimitiveEnd;
pub const BuiltinEnd: TypeId = vmc.BuiltinEnd;

const bt = BuiltinTypes;
pub const BuiltinTypes = struct {
    pub const Any: TypeId = vmc.TYPE_ANY;
    pub const Boolean: TypeId = vmc.TYPE_BOOLEAN;
    pub const Byte: TypeId = vmc.TYPE_BYTE;
    pub const TagLit: TypeId = vmc.TYPE_TAGLIT;
    pub const Float: TypeId = vmc.TYPE_FLOAT;
    pub const Integer: TypeId = vmc.TYPE_INTEGER;
    pub const String: TypeId = vmc.TYPE_STRING;
    pub const Symbol: TypeId = vmc.TYPE_SYMBOL;
    pub const Tuple: TypeId = vmc.TYPE_TUPLE;
    pub const Placeholder4: TypeId = vmc.TYPE_PLACEHOLDER4;
    pub const Placeholder5: TypeId = vmc.TYPE_PLACEHOLDER5;
    pub const Map: TypeId = vmc.TYPE_MAP;
    pub const MapIter: TypeId = vmc.TYPE_MAP_ITER;
    pub const Void: TypeId = vmc.TYPE_VOID;
    pub const Null: TypeId = vmc.TYPE_NULL;
    pub const Error: TypeId = vmc.TYPE_ERROR;
    pub const Fiber: TypeId = vmc.TYPE_FIBER;
    pub const Type: TypeId = vmc.TYPE_TYPE;
    pub const ExprType: TypeId = vmc.TYPE_EXPRTYPE;
    pub const FuncSig: TypeId = vmc.TYPE_FUNC_SIG;
    pub const Placeholder2: TypeId = vmc.TYPE_PLACEHOLDER2;
    pub const Placeholder3: TypeId = vmc.TYPE_PLACEHOLDER3;
    pub const Placeholder1: TypeId = vmc.TYPE_PLACEHOLDER1;
    pub const Func: TypeId = vmc.TYPE_FUNC;
    pub const TccState: TypeId = vmc.TYPE_TCC_STATE;
    pub const ExternFunc: TypeId = vmc.TYPE_EXTERN_FUNC;
    pub const Range: TypeId = vmc.TYPE_RANGE;
    pub const Table: TypeId = vmc.TYPE_TABLE;
    pub const Memory: TypeId = vmc.TYPE_MEMORY;

    /// Used to indicate no type value.
    // pub const Undefined: TypeId = vmc.TYPE_UNDEFINED;

    /// A dynamic type does not have a static type.
    /// This is not the same as bt.Any which is a static type.
    pub const Dyn: TypeId = vmc.TYPE_DYN;
};

pub var NullType = Type{
    .kind_ = .null,
    .sym_ = undefined,
    .id_ = 0,
    .info = undefined,
};

pub const ChunkExt = struct {
};

pub const SemaExt = struct {

    /// Assumes `src_t` is fully resolved.
    pub fn createTypeCopy(s: *cy.Sema, id: TypeId, src_t: *cy.Type) !*Type {
        switch (src_t.kind()) {
            .int => {
                const impl = src_t.cast(.int);
                return s.createTypeWithId(.int, id, .{ .bits = impl.bits });
            },
            .hostobj => {
                const impl = src_t.cast(.hostobj);
                return s.createTypeWithId(.hostobj, id, .{
                    .getChildrenFn = impl.getChildrenFn,
                    .finalizerFn = impl.finalizerFn,
                    .pre = impl.pre,
                });
            },
            .struct_t => {
                const impl = src_t.cast(.struct_t);
                const new_t = try s.createTypeWithId(.struct_t, id, .{
                    .size = impl.size,
                    .cstruct = impl.cstruct,
                    .tuple = impl.tuple,
                    .fields_ptr = impl.fields_ptr,
                    .fields_len = impl.fields_len,
                    .fields_owned = false,
                });
                new_t.retain_layout = src_t.retain_layout;
                new_t.retain_layout_owned = false;
                return new_t;
            },
            else => {
                std.debug.panic("TODO: {}", .{src_t.kind()});
            },
        }
    }

    /// Type creation updates the sema type table as well as the runtime type table.
    /// Some helpers at compile-time rely on an updated runtime type table. 
    pub fn createType(s: *cy.Sema, comptime kind: TypeKind, data: TypeImpl(kind)) !*Type {
        const new = try s.alloc.create(TypeImpl(kind));
        new.* = data;
        const id = s.types.items.len;
        new.base = .{
            .sym_ = undefined,
            .kind_ = kind,
            .info = .{},
            .id_ = @intCast(id),
        };
        try s.types.append(s.alloc, @ptrCast(new));

        s.compiler.vm.c.types = s.types.items.ptr;
        s.compiler.vm.c.types_len = s.types.items.len;
        return @ptrCast(new);
    }

    pub fn createTypeWithId(s: *cy.Sema, comptime kind: TypeKind, id: TypeId, data: TypeImpl(kind)) !*Type {
        const new = try s.alloc.create(TypeImpl(kind));
        new.* = data;
        new.base = .{
            .sym_ = undefined,
            .kind_ = kind,
            .info = .{},
            .id_ = id,
        };
        if (id >= s.types.items.len) {
            const old_len = s.types.items.len;
            try s.types.resize(s.alloc, id + 1);
            for (old_len..id) |i| {
                s.types.items[i] = &NullType;
            }

            s.compiler.vm.c.types = s.types.items.ptr;
            s.compiler.vm.c.types_len = s.types.items.len;
        } else {
            if (s.types.items[id].kind() != .null) {
                return error.DuplicateTypeId;
            }
        }
        s.types.items[id] = @ptrCast(new);
        return @ptrCast(new);
    }

    pub fn reserveType(s: *cy.Sema) !TypeId {
        const id = s.types.items.len;
        try s.types.append(s.alloc, undefined);
        s.types.items[id] = &NullType;

        s.compiler.vm.c.types = s.types.items.ptr;
        s.compiler.vm.c.types_len = s.types.items.len;
        return @intCast(id);
    }

    pub fn getType(s: *cy.Sema, id: TypeId) *Type {
        return s.types.items[id];
    }

    pub fn allocRtTypeName(s: *cy.Sema, id: u32) ![]const u8 {
        var buf: std.ArrayListUnmanaged(u8) = .{};
        defer buf.deinit(s.alloc);

        const w = buf.writer(s.alloc);
        if (id & vmc.REF_TYPE_BIT == vmc.REF_TYPE_BIT) {
            try w.writeAll("^");
        }
        const type_ = s.getType(id & vmc.TYPE_MASK);
        try cy.sym.writeSymName(s, w, @ptrCast(type_.sym()), .{ .from = null });
        return buf.toOwnedSlice(s.alloc);
    }

    pub fn allocTypeName(s: *cy.Sema, type_: *Type) ![]const u8 {
        var buf: std.ArrayListUnmanaged(u8) = .{};
        defer buf.deinit(s.alloc);

        const w = buf.writer(s.alloc);
        try s.writeTypeName(w, type_, null);
        return buf.toOwnedSlice(s.alloc);
    }

    pub fn writeTypeName(s: *cy.Sema, w: anytype, type_: *Type, from: ?*cy.Chunk) !void {
        if (type_.kind() == .null) {
            try w.writeByte('_');
            return;
        }
        try cy.sym.writeSymName(s, w, @ptrCast(type_.sym()), .{ .from = from });
    }

    pub fn getRtCompareType(s: *cy.Sema, type_: *Type) u32 {
        _ = s;
        if (type_.kind() == .pointer and type_.cast(.pointer).ref) {
            return vmc.REF_TYPE_BIT | type_.cast(.pointer).child_t.id();
        }
        return type_.id();
    }
};

pub fn isAnyOrDynamic(id: TypeId) bool {
    return id == bt.Any or id == bt.Dyn;
}

/// Check type constraints on target func signature.
pub fn isTypeFuncSigCompat(c: *cy.Compiler, args: []const *Type, ret_cstr: ReturnCstr, targetId: sema.FuncSigId) bool {
    const target = c.sema.getFuncSig(targetId);
    if (cy.Trace) {
        const sigStr = c.sema.formatFuncSig(targetId, &cy.tempBuf, null) catch cy.fatal();
        log.tracev("matching against: {s}", .{sigStr});
    }

    // First check params length.
    if (args.len != target.params_len) {
        return false;
    }

    // Check each param type. Attempt to satisfy constraints.
    for (target.params(), args) |cstrType, argType| {
        if (isTypeCompat(c, argType, cstrType)) {
            continue;
        }
        if (argType.id() == bt.Dyn) {
            if (isTypeCompat(c, cstrType, argType)) {
                // Only defer to runtime type check if arg type is a parent type of cstrType.
                continue;
            }
        }
        log.tracev("`{s}` not compatible with param `{s}`", .{argType.name(), cstrType.name()});
        return false;
    }

    // Check return type. Target is the source return type.
    return isValidReturnType(c, target.ret, ret_cstr);
}

pub const ReturnCstr = enum(u8) {
    any,       // exprStmt.
    not_void,  // expr.
};

pub fn isValidReturnType(_: *cy.Compiler, type_: *cy.Type, cstr: ReturnCstr) bool {
    switch (cstr) {
        .any => {
            return true;
        },
        .not_void => {
            return type_.id() != bt.Void;
        },
    }
}

pub fn isRtTypeCompat(c: *cy.Compiler, shape_t: TypeId, is_ref: bool, cstr_t: TypeId, rec: bool) bool {
    if (is_ref) {
        if (cstr_t == bt.Any or cstr_t == bt.Dyn) {
            return true;
        }
        const cstr_te = c.sema.getType(cstr_t);
        if (cstr_te.kind == .pointer and cstr_te.data.pointer.ref and cstr_te.data.pointer.child_t == shape_t) {
            return true;
        } 
    } else {
        if (shape_t == cstr_t) {
            return true;
        }
        if (cstr_t == bt.Any or cstr_t == bt.Dyn) {
            return true;
        }
        if (rec) {
            const cstr_te = c.sema.getType(cstr_t);
            if (cstr_te.kind == .pointer and cstr_te.data.pointer.ref and cstr_te.data.pointer.child_t == shape_t) {
                return true;
            } 
        }
    }
    return false;
}

pub fn isTypeCompat(_: *cy.Compiler, src_t: *Type, cstr_t: *Type) bool {
    if (src_t == cstr_t) {
        return true;
    }
    if (cstr_t.id() == bt.Any or cstr_t.id() == bt.Dyn) {
        return true;
    }
    return false;
}

/// Check type constraints on target func signature.
pub fn isFuncSigCompat(c: *cy.Compiler, id: sema.FuncSigId, targetId: sema.FuncSigId) bool {
    const src = c.sema.getFuncSig(id);
    const target = c.sema.getFuncSig(targetId);

    // First check params length.
    if (src.params_len != target.params_len) {
        return false;
    }

    // Check each param type. Attempt to satisfy constraints.
    for (target.params(), src.params()) |cstsymId, typeSymId| {
        if (!isTypeCompat(c, typeSymId, cstsymId)) {
            return false;
        }
    }

    // Check return type. Source return type is the constraint.
    return isTypeCompat(c, target.retSymId, src.retSymId);
}

pub fn toRtConcreteType(type_: *cy.Type) ?*cy.Type {
    return switch (type_.id()) {
        bt.Dyn,
        bt.Any => null,
        else => return type_,
    };
}

pub fn typeEqualOrChildOf(a: TypeId, b: TypeId) bool {
    if (b == bt.Any) {
        return true;
    }
    if (a == b) {
        return true;
    }
    // TODO: Check if a is a child type of b.
    return false;
}

pub fn isSameType(t1: TypeId, t2: TypeId) bool {
    return t1 == t2;
}

pub fn unionOf(c: *cy.Compiler, a: *Type, b: *Type) *Type {
    if (a == b) {
        return a;
    } else {
        if (a.id() == bt.Dyn or b.id() == bt.Dyn) {
            return c.sema.dyn_t;
        } else {
            return c.sema.any_t;
        }
    }
}
