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
    null,
    bool,
    int,
    float,
    object,
    custom,
    @"enum",
    choice,
    struct_t,
    option,
    trait,
    bare,
    table,
    ct_ref,
    ct_infer,
    distinct,
    array,
};

pub const TypeInfo = packed struct {
    is_future: bool = false,

    /// If `true`, invoke finalizer before releasing children.
    custom_pre: bool = false,

    /// Whether this type or a child parameter contains a ct_infer.
    ct_infer: bool = false,

    /// Whether this type or a child parameter contains a ct_ref.
    ct_ref: bool = false,

    load_all_methods: bool = false,

    padding: u3 = undefined,
};

pub const Type = extern struct {
    sym: *cy.Sym,
    kind: TypeKind,
    // Duped to avoid lookup from `sym`.
    // symType: cy.sym.SymType,
    has_get_method: bool = false,
    has_set_method: bool = false,
    has_init_pair_method: bool = false,
    cyclable: bool = true,

    info: TypeInfo,

    data: extern union {
        table: extern struct {
            numFields: u16,
        },
        // This is duped from ObjectType so that object creation/destruction avoids the lookup from `sym`.
        object: extern struct {
            numFields: u16,
            has_boxed_fields: bool,

            /// Each entry indicates whether the corresponding field is boxed.
            fields: [*]bool,
        },
        // Even though this increases the size of other type entries, it might not be worth
        // separating into another table since it would add another indirection.
        custom: extern struct {
            getChildrenFn: C.GetChildrenFn,
            finalizerFn: C.FinalizerFn,
        },
        struct_t: extern struct {
            // Includes fields for nested structs.
            nfields: u16,

            cstruct: bool,
            has_boxed_fields: bool,

            /// Inlined fields. Each entry indicates whether the corresponding field is boxed.
            fields: [*]bool,
        },
        ct_ref: extern struct {
            ct_param_idx: u32,
        },
        ct_infer: extern struct {
            ct_param_idx: u32,
        },
    },
};

test "types internals." {
    try t.eq(@sizeOf(Type), @sizeOf(vmc.TypeEntry));
    try t.eq(@offsetOf(Type, "sym"), @offsetOf(vmc.TypeEntry, "sym"));
    try t.eq(@offsetOf(Type, "kind"), @offsetOf(vmc.TypeEntry, "kind"));
    try t.eq(@offsetOf(Type, "has_get_method"), @offsetOf(vmc.TypeEntry, "has_get_method"));
    try t.eq(@offsetOf(Type, "has_set_method"), @offsetOf(vmc.TypeEntry, "has_set_method"));
    try t.eq(@offsetOf(Type, "data"), @offsetOf(vmc.TypeEntry, "data"));
}

pub const CompactType = packed struct {
    /// Should always be a static typeId.
    id: u31,
    dynamic: bool,

    pub fn init(id: TypeId) CompactType {
        if (id == bt.Dyn) {
            return CompactType.initDynamic(bt.Any);
        } else {
            return CompactType.initStatic(id);
        }
    }

    pub fn init2(id: TypeId, dynamic: bool) CompactType {
        if (id == bt.Dyn) {
            return CompactType.initDynamic(bt.Any);
        } else {
            if (dynamic) {
                return CompactType.initDynamic(id);
            } else {
                return CompactType.initStatic(id);
            }
        }
    }

    pub fn initStatic(id: TypeId) CompactType {
        return .{ .id = @intCast(id), .dynamic = false };
    }

    pub fn initDynamic(id: TypeId) CompactType {
        return .{ .id = @intCast(id), .dynamic = true };
    }

    pub fn toDeclType(self: CompactType) TypeId {
        if (self.dynamic) {
            return bt.Dyn;
        } else {
            return self.id;
        }
    }

    pub fn isDynAny(self: CompactType) bool {
        return self.dynamic and self.id == bt.Any;
    }
};

pub const PrimitiveEnd: TypeId = vmc.PrimitiveEnd;
pub const BuiltinEnd: TypeId = vmc.BuiltinEnd;

const bt = BuiltinTypes;
pub const BuiltinTypes = struct {
    pub const Any: TypeId = vmc.TYPE_ANY;
    pub const Boolean: TypeId = vmc.TYPE_BOOLEAN;
    pub const Placeholder1: TypeId = vmc.TYPE_PLACEHOLDER1;
    pub const Placeholder2: TypeId = vmc.TYPE_PLACEHOLDER2;
    pub const Byte: TypeId = vmc.TYPE_BYTE;
    pub const TagLit: TypeId = vmc.TYPE_TAGLIT;
    pub const Float: TypeId = vmc.TYPE_FLOAT;
    pub const Integer: TypeId = vmc.TYPE_INTEGER;
    pub const String: TypeId = vmc.TYPE_STRING;
    pub const Symbol: TypeId = vmc.TYPE_SYMBOL;
    pub const Tuple: TypeId = vmc.TYPE_TUPLE;
    pub const ListDyn: TypeId = vmc.TYPE_LIST_DYN;
    pub const ListIterDyn: TypeId = vmc.TYPE_LIST_ITER_DYN;
    pub const Map: TypeId = vmc.TYPE_MAP;
    pub const MapIter: TypeId = vmc.TYPE_MAP_ITER;
    pub const Void: TypeId = vmc.TYPE_VOID;
    pub const Error: TypeId = vmc.TYPE_ERROR;
    pub const Fiber: TypeId = vmc.TYPE_FIBER;
    pub const MetaType: TypeId = vmc.TYPE_METATYPE;
    pub const Type: TypeId = vmc.TYPE_TYPE;
    pub const Closure: TypeId = vmc.TYPE_CLOSURE;
    pub const Lambda: TypeId = vmc.TYPE_LAMBDA;
    pub const UpValue: TypeId = vmc.TYPE_UPVALUE;
    pub const HostFunc: TypeId = vmc.TYPE_HOST_FUNC;
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

pub const SemaExt = struct {

    pub fn isUnboxedType(s: *cy.Sema, id: cy.TypeId) bool {
        switch (id) {
            bt.Byte,
            bt.Integer => return true,
            bt.Symbol,
            bt.Error => return false,
            else => {
                if (s.types.items[id].kind == .int) {
                    return true;
                }
                return false;
            },
        }
    }

    pub fn pushType(s: *cy.Sema) !TypeId {
        const typeId = s.types.items.len;
        try s.types.append(s.alloc, .{
            .sym = undefined,
            .kind = .null,
            .data = undefined,
            .info = .{},
        });
        return @intCast(typeId);
    }

    pub fn isPointerType(s: *cy.Sema, id: cy.TypeId) bool {
        const sym = s.getTypeSym(id);
        const variant = sym.getVariant() orelse return false;
        return variant.root_template == s.pointer_tmpl;
    }

    pub fn getPointerChildType(s: *cy.Sema, id: cy.TypeId) cy.TypeId {
        const sym = s.getTypeSym(id);
        return sym.getVariant().?.args[0].castHeapObject(*cy.heap.Type).type;
    }

    pub fn getType(s: *cy.Sema, id: TypeId) Type {
        return s.types.items[id];
    }

    pub fn getTypeKind(s: *cy.Sema, id: TypeId) TypeKind {
        return s.types.items[id].kind;
    }

    pub fn getTypeBaseName(s: *cy.Sema, id: TypeId) []const u8 {
        return s.types.items[id].sym.name();
    }

    pub fn allocTypeName(s: *cy.Sema, id: TypeId) ![]const u8 {
        var buf: std.ArrayListUnmanaged(u8) = .{};
        defer buf.deinit(s.alloc);

        const w = buf.writer(s.alloc);
        const sym = s.getTypeSym(id);
        try cy.sym.writeSymName(s, w, sym, .{ .from = null });
        return buf.toOwnedSlice(s.alloc);
    }

    pub fn writeTypeName(s: *cy.Sema, w: anytype, id: cy.TypeId, from: ?*cy.Chunk) !void {
        if (id == cy.NullId) {
            try w.writeByte('_');
            return;
        }
        const sym = s.getTypeSym(id);
        try cy.sym.writeSymName(s, w, sym, .{ .from = from });
    }

    pub fn writeCompactType(s: *cy.Sema, w: anytype, ctype: CompactType, comptime showRecentType: bool) !void {
        if (showRecentType) {
            if (ctype.dynamic) {
                try w.writeAll("dyn ");
            }
            try s.writeTypeName(w, ctype.id);
        } else {
            if (ctype.dynamic) {
                try w.writeAll("dynamic");
            } else {
                try s.writeTypeName(w, ctype.id);
            }
        }
    }

    pub fn getTypeSym(s: *cy.Sema, id: TypeId) *cy.Sym {
        if (cy.Trace) {
            if (s.types.items[id].kind == .null) {
                cy.panicFmt("Type `{}` is uninited.", .{ id });
            }
        }
        return s.types.items[id].sym;
    }

    pub fn isUserObjectType(s: *cy.Sema, id: TypeId) bool {
        if (id < BuiltinEnd) {
            return false;
        }
        return s.types.items[id].kind == .object;
    }

    pub fn isStructType(s: *cy.Sema, id: TypeId) bool {
        if (id < BuiltinEnd) {
            return false;
        }
        return s.types.items[id].kind == .struct_t;
    }

    pub fn isEnumType(s: *cy.Sema, typeId: TypeId) bool {
        if (typeId < PrimitiveEnd) {
            return false;
        }
        return s.types.items[typeId].kind == .@"enum";
    }

    pub fn isChoiceType(s: *cy.Sema, typeId: TypeId) bool {
        if (typeId < PrimitiveEnd) {
            return false;
        }
        return s.types.items[typeId].kind == .choice;
    }

    pub fn isRcCandidateType(s: *cy.Sema, id: TypeId) bool {
        switch (id) {
            bt.String,
            bt.ListDyn,
            bt.ListIterDyn,
            bt.Map,
            bt.MapIter,
            bt.Fiber,
            bt.MetaType,
            bt.Dyn,
            bt.ExternFunc,
            bt.Any => return true,
            bt.Integer,
            bt.Float,
            bt.Symbol,
            bt.Void,
            bt.Error,
            // bt.Undefined,
            bt.Boolean => return false,
            else => {
                const sym = s.getTypeSym(id);
                switch (sym.type) {
                    .array_t,
                    .trait_t,
                    .custom_t,
                    .struct_t,
                    .object_t => return true,
                    .enum_t => {
                        return sym.cast(.enum_t).isChoiceType;
                    },
                    .int_t => return false,
                    else => {
                        cy.panicFmt("Unexpected sym type: {} {}", .{id, sym.type});
                    }
                }
            }
        }
    }
};

pub const ChunkExt = struct {

    pub fn checkForZeroInit(c: *cy.Chunk, typeId: TypeId, node: *ast.Node) !void {
        var res = hasZeroInit(c, typeId);
        if (res == .missingEntry) {
            const sym = c.sema.getTypeSym(typeId);
            if (sym.type == .object_t) {
                res = try visitTypeHasZeroInit(c, sym.cast(.object_t));
            } else if (sym.type == .struct_t) {
                res = try visitTypeHasZeroInit(c, sym.cast(.struct_t));
            } else return error.Unexpected;
        }
        switch (res) {
            .hasZeroInit => return,
            .missingEntry => return error.Unexpected,
            .unsupported => {
                const name = c.sema.getTypeBaseName(typeId);
                return c.reportErrorFmt("Unsupported zero initializer for `{}`.", &.{v(name)}, node);
            },
            .circularDep => {
                const name = c.sema.getTypeBaseName(typeId);
                return c.reportErrorFmt("Can not zero initialize `{}` because of circular dependency.", &.{v(name)}, node);
            }
        }
    }
};

pub fn isAnyOrDynamic(id: TypeId) bool {
    return id == bt.Any or id == bt.Dyn;
}

/// Check type constraints on target func signature.
pub fn isTypeFuncSigCompat(c: *cy.Compiler, args: []const CompactType, ret_cstr: ReturnCstr, targetId: sema.FuncSigId) bool {
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
        if (isTypeSymCompat(c, argType.id, cstrType.type)) {
            continue;
        }
        if (argType.dynamic) {
            if (isTypeSymCompat(c, cstrType.type, argType.id)) {
                // Only defer to runtime type check if arg type is a parent type of cstrType.
                continue;
            }
        }
        log.tracev("`{s}` not compatible with param `{s}`", .{c.sema.getTypeBaseName(argType.id), c.sema.getTypeBaseName(cstrType.type)});
        return false;
    }

    // Check return type. Target is the source return type.
    return isValidReturnType(c, target.ret, ret_cstr);
}

pub const ReturnCstr = enum(u8) {
    any,       // exprStmt.
    not_void,  // expr.
};

pub fn isValidReturnType(_: *cy.Compiler, type_id: TypeId, cstr: ReturnCstr) bool {
    switch (cstr) {
        .any => {
            return true;
        },
        .not_void => {
            return type_id != bt.Void;
        },
    }
}

pub fn isTypeSymCompat(_: *cy.Compiler, typeId: TypeId, cstrType: TypeId) bool {
    if (typeId == cstrType) {
        return true;
    }
    if (cstrType == bt.Any or cstrType == bt.Dyn) {
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
        if (!isTypeSymCompat(c, typeSymId, cstsymId)) {
            return false;
        }
    }

    // Check return type. Source return type is the constraint.
    return isTypeSymCompat(c, target.retSymId, src.retSymId);
}

pub fn toRtConcreteType(typeId: TypeId) ?cy.TypeId {
    return switch (typeId) {
        bt.Dyn,
        bt.Any => null,
        else => return typeId,
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

pub fn unionOf(c: *cy.Compiler, a: TypeId, b: TypeId) TypeId {
    _ = c;
    if (a == b) {
        return a;
    } else {
        if (a == bt.Dyn or b == bt.Dyn) {
            return bt.Dyn;
        } else {
            return bt.Any;
        }
    }
}

const ZeroInitResult = enum {
    hasZeroInit,
    missingEntry,
    unsupported,
    circularDep,
};

fn hasZeroInit(c: *cy.Chunk, typeId: TypeId) ZeroInitResult {
    switch (typeId) {
        bt.Dyn,
        bt.Any,
        bt.Boolean,
        bt.Integer,
        bt.Float,
        bt.ListDyn,
        bt.Map,
        bt.String => return .hasZeroInit,
        else => {
            const sym = c.sema.getTypeSym(typeId);
            if (sym.type == .object_t or sym.type == .struct_t) {
                if (c.typeDepsMap.get(sym)) |entryId| {
                    if (!c.typeDeps.items[entryId].visited) {
                        // Still being visited, which indicates a circular reference.
                        return .circularDep;
                    }
                    if (c.typeDeps.items[entryId].hasCircularDep) {
                        return .circularDep;
                    }
                    if (c.typeDeps.items[entryId].hasUnsupported) {
                        return .unsupported;
                    }
                    return .hasZeroInit;
                } else {
                    return .missingEntry;
                }
            }
            return .unsupported;
        },
    }
}

fn visitTypeHasZeroInit(c: *cy.Chunk, obj: *cy.sym.ObjectType) !ZeroInitResult {
    const entryId = c.typeDeps.items.len;
    try c.typeDeps.append(c.alloc, .{ .visited = false, .hasCircularDep = false, .hasUnsupported = false });
    try c.typeDepsMap.put(c.alloc, @ptrCast(obj), @intCast(entryId));

    var finalRes = ZeroInitResult.hasZeroInit;
    for (obj.fields[0..obj.numFields]) |field| {
        var res = hasZeroInit(c, field.type);
        if (res == .missingEntry) {
            const childSym = c.sema.getTypeSym(field.type).cast(.object_t);
            res = try visitTypeHasZeroInit(c, childSym);
        }
        switch (res) {
            .hasZeroInit => continue,
            .missingEntry => cy.unexpected(),
            .unsupported => {
                if (finalRes == .hasZeroInit) {
                    finalRes = .unsupported;
                }
            },
            .circularDep => {
                if (finalRes == .hasZeroInit) {
                    finalRes = .circularDep;
                }
            },
        }
    }

    if (finalRes == .circularDep) {
        c.typeDeps.items[entryId].hasCircularDep = true;
    } else if (finalRes == .unsupported) {
        c.typeDeps.items[entryId].hasUnsupported = true;
    }
    c.typeDeps.items[entryId].visited = true;
    return finalRes;
}