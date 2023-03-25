const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const sema = cy.sema;
const ResolvedSymId = sema.ResolvedSymId;
const fmt = @import("fmt.zig");
const v = fmt.v;

const bt = BuiltinTypeSymIds;
pub const BuiltinTypeSymIds = struct {
    pub const Any: ResolvedSymId = 0;
    pub const Boolean: ResolvedSymId = 1;
    pub const Number: ResolvedSymId = 2;
    pub const Integer: ResolvedSymId = 3;
    pub const String: ResolvedSymId = 4;
    pub const Rawstring: ResolvedSymId = 5;
    pub const TagLiteral: ResolvedSymId = 6;
    pub const List: ResolvedSymId = 7;
    pub const Map: ResolvedSymId = 8;
    pub const Pointer: ResolvedSymId = 9;
    pub const None: ResolvedSymId = 10;
    pub const Error: ResolvedSymId = 11;
};

test "Reserved names map to reserved sym ids." {
    try t.eq(sema.NameAny, bt.Any);
    try t.eq(sema.NameBoolean, bt.Boolean);
    try t.eq(sema.NameNumber, bt.Number);
    try t.eq(sema.NameInt, bt.Integer);
    try t.eq(sema.NameString, bt.String);
    try t.eq(sema.NameRawstring, bt.Rawstring);
    try t.eq(sema.NameTagLiteral, bt.TagLiteral);
    try t.eq(sema.NameList, bt.List);
    try t.eq(sema.NameMap, bt.Map);
    try t.eq(sema.NamePointer, bt.Pointer);
    try t.eq(sema.NameNone, bt.None);
    try t.eq(sema.NameError, bt.Error);
}

/// Names and resolved builtin sym ids are reserved to index into `BuiltinTypes`.
const BuiltinTypes = [_]Type{
    AnyType,
    BoolType,
    NumberType,
    IntegerType,
    StringType,
    RawstringType,
    TagLiteralType,
    ListType,
    MapType,
    PointerType,
    NoneType,
    ErrorType,
};

/// Names and resolved builtin sym ids are reserved to index into `BuiltinTypeTags`.
pub const BuiltinTypeTags = [_]TypeTag{
    .any,
    .boolean,
    .number,
    .int,
    .string,
    .rawstring,
    .tagLiteral,
    .list,
    .map,
    .pointer,
    .none,
    .err,
};

/// Check type constraints on target func signature.
pub fn isTypeFuncSigCompat(c: *cy.VMcompiler, args: []const Type, ret: Type, targetId: sema.ResolvedFuncSigId) bool {
    const target = c.sema.getResolvedFuncSig(targetId);
    // const sigStr = try getResolvedFuncSigTempStr(chunk.compiler, rFuncSigId);
    // log.debug("matching against: {s}", .{sigStr});

    // First check params length.
    if (args.len != target.paramLen) {
        return false;
    }

    // Check each param type. Attempt to satisfy constraints.
    for (target.params(), args) |tTypeSymId, sType| {
        const sTypeSymId = typeToResolvedSym(sType);
        if (sTypeSymId == tTypeSymId) {
            continue;
        }
        if (tTypeSymId == bt.Any) {
            continue;
        }
        if (tTypeSymId == bt.Integer) {
            if (sType.typeT == .number and sType.inner.number.canRequestInteger) {
                continue;
            } else {
                return false;
            }
        }
        return false;
    }

    // Check return type. Target is the source return type.
    const tTypeSymId = typeToResolvedSym(ret);
    const sTypeSymId = target.retSymId;
    if (sTypeSymId == tTypeSymId) {
        return true;
    }
    if (tTypeSymId == bt.Any) {
        return true;
    }
    return false;
}

/// Check type constraints on target func signature.
pub fn isFuncSigCompat(c: *cy.VMcompiler, id: sema.ResolvedFuncSigId, targetId: sema.ResolvedFuncSigId) bool {
    const src = c.sema.getResolvedFuncSig(id);
    const target = c.sema.getResolvedFuncSig(targetId);

    // First check params length.
    if (src.paramLen != target.paramLen) {
        return false;
    }

    // Check each param type. Attempt to satisfy constraints.
    for (target.params(), src.params()) |tTypeSymId, sTypeSymId| {
        if (sTypeSymId == tTypeSymId) {
            continue;
        }
        if (tTypeSymId == bt.Any) {
            continue;
        }
        return false;
    }

    // Check return type. Target is the source return type.
    const tTypeSymId = src.retSymId;
    const sTypeSymId = target.retSymId;
    if (sTypeSymId == tTypeSymId) {
        return true;
    }
    if (tTypeSymId == bt.Any) {
        return true;
    }
    return false;
}

/// Type -> ResolvedSymId
pub fn typeToResolvedSym(type_: Type) ResolvedSymId {
    return switch (type_.typeT) {
        .any => bt.Any,
        .number => bt.Number,
        .int => bt.Integer,
        .tagLiteral => bt.TagLiteral,
        .list => bt.List,
        .boolean => bt.Boolean,
        .string => bt.String,
        .rawstring => bt.Rawstring,
        .map => bt.Map,
        .tag => bt.Any, // TODO: Handle tagtype.
        .pointer => bt.Pointer,
        .none => bt.None,
        .err => bt.Error,
        .rsym => type_.inner.rsym.rSymId,
        else => stdx.panicFmt("Unsupported type {}", .{type_.typeT}),
    };
}

/// ResolvedSymId -> Type
pub fn typeFromResolvedSym(chunk: *cy.CompileChunk, rSymId: ResolvedSymId) !Type {
    if (rSymId < BuiltinTypes.len) {
        return BuiltinTypes[rSymId];
    } else {
        const rSym = chunk.compiler.sema.resolvedSyms.items[rSymId];
        if (rSym.symT == .object) {
            return initResolvedSymType(rSymId);
        } else {
            const name = sema.getName(chunk.compiler, rSym.key.absResolvedSymKey.nameId);
            return chunk.reportError("`{}` is not a valid type.", &.{v(name)});
        }
    }
}

pub const TypeTag = enum {
    any,
    boolean,
    number,
    int,
    list,
    map,
    fiber,
    string,
    rawstring,
    box,
    tag,
    tagLiteral,
    pointer,
    none,
    err,

    /// Type from a resolved type sym.
    rsym,

    undefined,
};

pub const Type = struct {
    typeT: TypeTag,
    rcCandidate: bool,
    inner: packed union {
        tag: packed struct {
            tagId: u8,
        },
        number: packed struct {
            canRequestInteger: bool,
        },
        rsym: packed struct {
            rSymId: sema.ResolvedSymId,
        },
        string: packed struct {
            isStaticString: bool,
        },
    } = undefined,

    fn isFlexibleType(self: Type) bool {
        switch (self.typeT) {
            .number => return self.inner.number.canRequestInteger,
            .tagLiteral => return true,
            else => return false,
        }
    }

    pub fn canBeInt(self: Type) bool {
        return self.typeT == .int or (self.typeT == .number and self.inner.number.canRequestInteger);
    }
};

pub const UndefinedType = Type{
    .typeT = .undefined,
    .rcCandidate = false,
};

pub const NoneType = Type{
    .typeT = .none,
    .rcCandidate = false,
};

pub const PointerType = Type{
    .typeT = .pointer,
    .rcCandidate = true,
};

pub const AnyType = Type{
    .typeT = .any,
    .rcCandidate = true,
};

pub const BoolType = Type{
    .typeT = .boolean,
    .rcCandidate = false,
};

pub const IntegerType = Type{
    .typeT = .int,
    .rcCandidate = false,
};

pub const NumberType = Type{
    .typeT = .number,
    .rcCandidate = false,
    .inner = .{
        .number = .{
            .canRequestInteger = false,
        },
    },
};

/// Number constants are numbers by default, but some constants can be requested as an integer during codegen.
/// Once a constant has been assigned to a variable, it becomes a `NumberType`.
pub const NumberOrRequestIntegerType = Type{
    .typeT = .number,
    .rcCandidate = false,
    .inner = .{
        .number = .{
            .canRequestInteger = true,
        },
    },
};

pub const StaticStringType = Type{
    .typeT = .string,
    .rcCandidate = false,
    .inner = .{
        .string = .{
            .isStaticString = true,
        }
    },
};

pub const StringType = Type{
    .typeT = .string,
    .rcCandidate = true,
    .inner = .{
        .string = .{
            .isStaticString = false,
        }
    },
};

pub const RawstringType = Type{
    .typeT = .rawstring,
    .rcCandidate = true,
};

pub const FiberType = Type{
    .typeT = .fiber,
    .rcCandidate = true,
};

pub const ListType = Type{
    .typeT = .list,
    .rcCandidate = true,
};

pub const TagLiteralType = Type{
    .typeT = .tagLiteral,
    .rcCandidate = false,
};

pub const ErrorType = Type{
    .typeT = .err,
    .rcCandidate = false,
};

pub fn initResolvedSymType(rSymId: ResolvedSymId) Type {
    return .{
        .typeT = .rsym,
        .rcCandidate = true,
        .inner = .{
            .rsym = .{
                .rSymId = rSymId,
            },
        },
    };
}

pub fn initTagType(tagId: u32) Type {
    return .{
        .typeT = .tag,
        .rcCandidate = false,
        .inner = .{
            .tag = .{
                .tagId = @intCast(u8, tagId),
            },
        },
    };
}

pub const MapType = Type{
    .typeT = .map,
    .rcCandidate = true,
};

test "Internals." {
    try t.eq(@sizeOf(Type), 8);
}