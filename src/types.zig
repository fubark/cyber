const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const rt = cy.rt;
const sema = cy.sema;
const SymbolId = sema.SymbolId;
const fmt = @import("fmt.zig");
const v = fmt.v;
const vmc = @import("vm_c.zig");
const log = cy.log.scoped(.types);

pub const TypeId = SymbolId;

const bt = BuiltinTypeSymIds;
pub const BuiltinTypeSymIds = struct {
    /// Name ids are reserved to match type sym ids.
    pub const Any: TypeId = vmc.SEMA_TYPE_ANY;
    pub const Boolean: TypeId = vmc.SEMA_TYPE_BOOLEAN;
    pub const Float: TypeId = vmc.SEMA_TYPE_FLOAT;
    pub const Integer: TypeId = vmc.SEMA_TYPE_INTEGER;
    pub const String: TypeId = vmc.SEMA_TYPE_STRING;
    pub const Rawstring: TypeId = vmc.SEMA_TYPE_RAWSTRING;
    pub const Symbol: TypeId = vmc.SEMA_TYPE_SYMBOL;
    pub const Tuple: TypeId = vmc.SEMA_TYPE_TUPLE;
    pub const List: TypeId = vmc.SEMA_TYPE_LIST;
    pub const ListIter: TypeId = vmc.SEMA_TYPE_LIST_ITER;
    pub const Map: TypeId = vmc.SEMA_TYPE_MAP;
    pub const MapIter: TypeId = vmc.SEMA_TYPE_MAP_ITER;
    pub const Pointer: TypeId = vmc.SEMA_TYPE_POINTER;
    pub const None: TypeId = vmc.SEMA_TYPE_NONE;
    pub const Error: TypeId = vmc.SEMA_TYPE_ERROR;
    pub const Fiber: TypeId = vmc.SEMA_TYPE_FIBER;
    pub const MetaType: TypeId = vmc.SEMA_TYPE_METATYPE;

    /// Internal types.

    /// Used to indicate no type value.
    pub const Undefined: TypeId = vmc.SEMA_TYPE_UNDEFINED;
    /// Strings that aren't retained.
    pub const StaticString: TypeId = vmc.SEMA_TYPE_STATICSTRING;

    /// A dynamic type does not have a static type.
    /// This is not the same as bt.Any which is a static type.
    pub const Dynamic: TypeId = vmc.SEMA_TYPE_DYNAMIC;

    pub const End: TypeId = vmc.NumSemaTypes;
};

test "Reserved names map to reserved sym ids." {
    try t.eq(sema.NameAny, bt.Any);
    try t.eq(sema.NameBoolean, bt.Boolean);
    try t.eq(sema.NameFloat, bt.Float);
    try t.eq(sema.NameInt, bt.Integer);
    try t.eq(sema.NameString, bt.String);
    try t.eq(sema.NameRawstring, bt.Rawstring);
    try t.eq(sema.NameSymbol, bt.Symbol);
    try t.eq(sema.NameList, bt.List);
    try t.eq(sema.NameListIterator, bt.ListIter);
    try t.eq(sema.NameMap, bt.Map);
    try t.eq(sema.NameMapIterator, bt.MapIter);
    try t.eq(sema.NamePointer, bt.Pointer);
    try t.eq(sema.NameNone, bt.None);
    try t.eq(sema.NameError, bt.Error);
    try t.eq(sema.NameFiber, bt.Fiber);
    try t.eq(sema.NameMetatype, bt.MetaType);
}

pub fn isAnyOrDynamic(id: TypeId) bool {
    return id == bt.Any or id == bt.Dynamic;
}

/// Check type constraints on target func signature.
pub fn isTypeFuncSigCompat(c: *cy.VMcompiler, args: []const TypeId, ret: TypeId, targetId: sema.FuncSigId) bool {
    const target = c.sema.getFuncSig(targetId);
    // const sigStr = try getFuncSigTempStr(chunk.compiler, funcSigId);
    // log.debug("matching against: {s}", .{sigStr});

    // First check params length.
    if (args.len != target.paramLen) {
        return false;
    }

    // Check each param type. Attempt to satisfy constraints.
    for (target.params(), args) |cstrType, argType| {
        if (!isTypeSymCompat(c, argType, cstrType)) {
            return false;
        }
    }

    // Check return type. Target is the source return type.
    return isTypeSymCompat(c, target.retSymId, ret);
}

pub fn isTypeSymCompat(_: *cy.VMcompiler, typeSymId: TypeId, cstrType: TypeId) bool {
    if (typeSymId == cstrType) {
        return true;
    }
    if (cstrType == bt.Any or cstrType == bt.Dynamic) {
        return true;
    }
    if (cstrType == bt.String and typeSymId == bt.StaticString) {
        return true;
    }
    return false;
}

/// Check type constraints on target func signature.
pub fn isFuncSigCompat(c: *cy.VMcompiler, id: sema.FuncSigId, targetId: sema.FuncSigId) bool {
    const src = c.sema.getFuncSig(id);
    const target = c.sema.getFuncSig(targetId);

    // First check params length.
    if (src.paramLen != target.paramLen) {
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

pub fn toRtConcreteType(typeId: TypeId) ?rt.TypeId {
    return switch (typeId) {
        bt.Any => null,
        bt.Float => rt.FloatT,
        bt.Integer => rt.IntegerT,
        bt.Symbol => rt.SymbolT,
        bt.List => rt.ListT,
        bt.Boolean => rt.BooleanT,

        // There are multiple string types.
        bt.String => null,
        bt.Rawstring => null,

        bt.Map => rt.MapT,
        bt.Pointer => rt.PointerT,
        bt.None => rt.NoneT,
        bt.Fiber => rt.FiberT,
        bt.MetaType => rt.MetaTypeT,
        bt.Error => rt.ErrorT,
        else => cy.panicFmt("Unsupported type {}", .{typeId}),
    };
}

pub fn assertTypeSym(c: *cy.Chunk, symId: SymbolId) !void {
    if (symId < bt.End) {
        return;
    }
    const sym = c.compiler.sema.getSymbol(symId);
    if (sym.symT == .object) {
        return;
    } else {
        const name = sema.getName(c.compiler, sym.key.resolvedSymKey.nameId);
        return c.reportError("`{}` is not a valid type.", &.{v(name)});
    }
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

pub fn isEnumType(c: *cy.VMcompiler, typeId: TypeId) bool {
    if (typeId < bt.End) {
        return false;
    }
    return c.sema.getSymbol(typeId).symT == .enumType;
}

pub fn isRcCandidateType(c: *cy.VMcompiler, id: TypeId) bool {
    switch (id) {
        bt.String,
        bt.Rawstring,
        bt.List,
        bt.Map,
        bt.Pointer,
        bt.Fiber,
        bt.MetaType,
        bt.Dynamic,
        bt.Any => return true,
        bt.Integer,
        bt.Float,
        bt.StaticString,
        bt.Symbol,
        bt.None,
        bt.Error,
        bt.Undefined,
        bt.Boolean => return false,
        else => {
            const sym = c.sema.getSymbol(id);
            if (sym.symT == .object) {
                return true;
            } else if (sym.symT == .enumType) {
                return false;
            } else {
                cy.panicFmt("Unexpected sym type: {} {}", .{id, sym.symT});
            }
        }
    }
}