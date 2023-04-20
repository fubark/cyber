const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const sema = cy.sema;
const vmc = @import("vm_c.zig");

pub const TypeId = u32;

pub const NoneT: TypeId = vmc.TYPE_NONE;
pub const BooleanT: TypeId = vmc.TYPE_BOOLEAN;
pub const ErrorT: TypeId = vmc.TYPE_ERROR;
pub const StaticAstringT: TypeId = vmc.TYPE_STATIC_ASTRING; // ASCII string.
pub const StaticUstringT: TypeId = vmc.TYPE_STATIC_USTRING; // UTF8 string.
pub const EnumT: TypeId = vmc.TYPE_ENUM;
pub const SymbolT: TypeId = vmc.TYPE_SYMBOL;
pub const IntegerT: TypeId = vmc.TYPE_INTEGER;
pub const NumberT: TypeId = vmc.TYPE_NUMBER;

/// Reserved object types known at comptime.
/// Starts at 9 since primitive types go up to 8.
pub const ListT: TypeId = 9;
pub const ListIteratorT: TypeId = 10;
pub const MapT: TypeId = 11;
pub const MapIteratorT: TypeId = 12;
pub const ClosureT: TypeId = 13;
pub const LambdaT: TypeId = 14;
pub const AstringT: TypeId = 15;
pub const UstringT: TypeId = 16;
pub const StringSliceT: TypeId = 17;
pub const RawstringT: TypeId = 18;
pub const RawstringSliceT: TypeId = 19;
pub const FiberT: TypeId = 20;
pub const BoxT: TypeId = 21;
pub const NativeFuncT: TypeId = 22;
pub const TccStateT: TypeId = 23;
pub const PointerT: TypeId = 24;
pub const FileT: TypeId = 25;
pub const DirT: TypeId = 26;
pub const DirIteratorT: TypeId = 27;
pub const MetaTypeT: TypeId = 28;
pub const AnyT: TypeId = 29;
pub const StringUnionT: TypeId = 30;
pub const RawstringUnionT: TypeId = 31;

pub const TypeKey = cy.hash.KeyU64;

pub const Type = struct {
    name: []const u8,
    numFields: u32,
    rTypeSymId: sema.ResolvedSymId,
};

pub const FieldTableKey = cy.hash.KeyU64;
pub const FieldId = u32;

pub const MethodKey = cy.hash.KeyU64;
pub const MethodTableKey = cy.hash.KeyU64;
pub const MethodId = u32;

const MethodEntryType = enum {
    singleUntypedFunc,
    singleUntypedNativeFunc1,
    singleUntypedNativeFunc2,

    /// Single typed func.
    singleTypedFunc,

    /// Single typed native func.
    singleTypedNativeFunc,
};

/// Stored in `methodTable`.
pub const MethodEntry = struct {
    type: MethodEntryType,

    /// The full signature of the function.
    rFuncSigId: u32,

    inner: MethodInner,
};

const MethodInner = packed union {
    nativeFunc1: cy.NativeObjFuncPtr,
    nativeFunc2: cy.NativeObjFunc2Ptr,
    func: packed struct {
        pc: u32,
        stackSize: u32,
    },
};

/// Keeping this small is better for function calls.
/// Secondary symbol data should be moved to `methodSymExtras`.
pub const MethodSym = struct {
    mruEntryType: MethodEntryType,
    /// Most recent sym used is cached avoid hashmap lookup. 
    mruTypeId: TypeId,

    /// The full signature of the function.
    /// This is only used for `singleTypedFunc` which do runtime type checks on the arguments.
    rFuncSigId: u32,

    inner: MethodInner,

    pub fn initSingleUntypedFunc(funcSigId: sema.ResolvedFuncSigId, pc: usize, stackSize: u32) MethodSym {
        return .{
            .mruEntryType = .singleUntypedFunc,
            .mruTypeId = undefined,
            .rFuncSigId = funcSigId,
            .inner = .{
                .func = .{
                    .pc = @intCast(u32, pc),
                    .stackSize = stackSize,
                },
            },
        };
    }

    pub fn initSingleUntypedNativeFunc1(funcSigId: sema.ResolvedFuncSigId, func: cy.NativeObjFuncPtr) MethodSym {
        return .{
            .mruEntryType = .singleUntypedNativeFunc1,
            .mruTypeId = undefined,
            .rFuncSigId = funcSigId,
            .inner = .{
                .nativeFunc1 = func,
            },
        };
    }

    pub fn initSingleUntypedNativeFunc2(funcSigId: sema.ResolvedFuncSigId, func: cy.NativeObjFunc2Ptr) MethodSym {
        return .{
            .mruEntryType = .singleUntypedNativeFunc2,
            .mruTypeId = undefined,
            .rFuncSigId = funcSigId,
            .inner = .{
                .nativeFunc2 = func,
            },
        };
    }

    pub fn initSingleTypedFunc(funcSigId: sema.ResolvedFuncSigId, pc: usize, stackSize: u32) MethodSym {
        return .{
            .mruEntryType = .singleTypedFunc,
            .mruTypeId = undefined,
            .rFuncSigId = funcSigId,
            .inner = .{
                .func = .{
                    .pc = @intCast(u32, pc),
                    .stackSize = stackSize,
                },
            },
        };
    }

    pub fn initSingleTypedNativeFunc(funcSigId: sema.ResolvedFuncSigId, func: cy.NativeObjFuncPtr) MethodSym {
        return .{
            .mruEntryType = .singleTypedNativeFunc,
            .mruTypeId = undefined,
            .rFuncSigId = funcSigId,
            .inner = .{
                .nativeFunc1 = func,
            },
        };
    }
};

pub const VarKey = cy.hash.KeyU64;

test "Internals." {
    try t.eq(@alignOf(MethodSym), 8);
    try t.eq(@sizeOf(MethodSym), 24);

    try t.eq(@sizeOf(Type), 24);
}