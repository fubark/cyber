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

pub const MethodGroupKey = sema.NameSymId;
pub const TypeMethodGroupKey = cy.hash.KeyU64;

pub const MethodType = enum {
    untypedFunc,
    untypedNativeFunc1,
    untypedNativeFunc2,
    typedFunc,
    typedNativeFunc,
};

pub const MethodData = extern union {
    typedNativeFunc: extern struct {
        ptr: cy.NativeObjFuncPtr,
        funcSigId: sema.ResolvedFuncSigId,
        /// Includes self param.
        numParams: u8,
    },
    untypedNativeFunc1: extern struct {
        ptr: cy.NativeObjFuncPtr,
        numParams: u8,
    },
    untypedNativeFunc2: extern struct {
        ptr: cy.NativeObjFunc2Ptr,
        numParams: u8,
    },
    typedFunc: extern struct {
        funcSigId: sema.ResolvedFuncSigId,
        pc: u32,
        stackSize: u32,
        numParams: u8,
    },
    untypedFunc: extern struct {
        numParams: u8,
        pc: u32,
        stackSize: u32,
    },
};

pub const MethodExt = struct {
    /// Signature is kept for every method.
    funcSigId: sema.ResolvedFuncSigId,
};

/// Used to initialize `Method` and `MethodExt`.
pub const MethodInit = struct {
    type: MethodType,
    data: MethodData,
    funcSigId: sema.ResolvedFuncSigId,

    pub fn initUntypedFunc(funcSigId: sema.ResolvedFuncSigId, pc: usize, stackSize: u32, numParams: u8) MethodInit {
        return .{
            .type = .untypedFunc,
            .data = .{
                .untypedFunc = .{
                    .pc = @intCast(pc),
                    .stackSize = stackSize,
                    .numParams = numParams,
                },
            },
            .funcSigId = funcSigId,
        };
    }

    pub fn initUntypedNativeFunc1(funcSigId: sema.ResolvedFuncSigId, func: cy.NativeObjFuncPtr, numParams: u8) MethodInit {
        return .{
            .type = .untypedNativeFunc1,
            .data = .{
                .untypedNativeFunc1 = .{
                    .ptr = func,
                    .numParams = numParams,
                },
            },
            .funcSigId = funcSigId,
        };
    }

    pub fn initUntypedNativeFunc2(funcSigId: sema.ResolvedFuncSigId, func: cy.NativeObjFunc2Ptr, numParams: u8) MethodInit {
        return .{
            .type = .untypedNativeFunc2,
            .data = .{
                .untypedNativeFunc2 = .{
                    .ptr = func,
                    .numParams = numParams,
                },
            },
            .funcSigId = funcSigId,
        };
    }

    pub fn initTypedFunc(funcSigId: sema.ResolvedFuncSigId, pc: usize, stackSize: u32, numParams: u8) MethodInit {
        return .{
            .type = .typedFunc,
            .data = .{
                .typedFunc = .{
                    .funcSigId = funcSigId,
                    .pc = @intCast(pc),
                    .stackSize = stackSize,
                    .numParams = numParams,
                },
            },
            .funcSigId = funcSigId,
        };
    }

    pub fn initTypedNativeFunc(funcSigId: sema.ResolvedFuncSigId, func: cy.NativeObjFuncPtr, numParams: u8) MethodInit {
        return .{
            .type = .typedNativeFunc,
            .data = .{
                .typedNativeFunc = .{
                    .funcSigId = funcSigId,
                    .ptr = func,
                    .numParams = numParams,
                },
            },
            .funcSigId = funcSigId,
        };
    }
};

pub const Method = struct {
    type: MethodType,

    data: MethodData,

    next: vmc.MethodId,
};

pub const TypeMethodGroup = struct {
    mruMethodId: vmc.MethodId,

    head: vmc.MethodId,

    /// For faster append.
    tail: vmc.MethodId,
};

/// Keeping this small is better for function calls.
/// Secondary symbol data should be moved to `methodGroupExts`.
pub const MethodGroup = struct {
    /// Most recent type  is cached to avoid hashmap lookup. 
    mruTypeId: TypeId,

    mruMethodType: MethodType,
    mruMethodData: MethodData,
    mruTypeMethodOverloaded: bool,
};

pub const MethodGroupExt = struct {
    namePtr: [*]const u8,
    /// NullId if only one method in the group.
    mruTypeMethodGroupId: vmc.TypeMethodGroupId,
    mruMethodId: vmc.MethodId,
    /// So debug info can be obtained for a method group with just one method.
    initialFuncSigId: sema.ResolvedFuncSigId,

    nameLen: u16,
    nameIsOwned: bool,

    pub fn getName(self: *const MethodGroupExt) []const u8 {
        return self.namePtr[0..self.nameLen];
    }
};

pub const FuncId = u32;

pub const FuncSymbolEntryType = enum {
    nativeFunc1,
    func,
    closure,

    /// Placeholder for a func symbol that contains a linked list of overloaded functions.
    /// The arg types are checked at runtime.
    // multiple,

    none,
};

pub const FuncSymDetail = struct {
    nameId: sema.NameSymId,
};

/// TODO: Rename to FuncSymbol.
pub const FuncSymbolEntry = extern struct {
    entryT: u32,
    innerExtra: extern union {
        nativeFunc1: extern struct {
            /// Used to wrap a native func as a function value.
            typedFlagNumParams: u16,
            rFuncSigId: u16,

            pub fn numParams(self: @This()) u16 {
                return self.typedFlagNumParams & ~(@as(u16, 1) << 15);
            }
        },
        none: extern struct {
            rFuncSigId: u32,
        },
        func: extern struct {
            rFuncSigId: u32,
        },
    } = undefined,
    inner: extern union {
        nativeFunc1: cy.NativeFuncPtr,
        func: extern struct {
            pc: u32,
            /// Stack size required by the func.
            stackSize: u16,
            /// Num params used to wrap as function value.
            numParams: u16,
        },
        closure: *cy.Closure,
    },

    pub fn initNativeFunc1(func: cy.NativeFuncPtr, isTyped: bool, numParams: u32, rFuncSigId: sema.ResolvedFuncSigId) FuncSymbolEntry {
        const isTypedMask: u16 = if (isTyped) 1 << 15 else 0;
        return .{
            .entryT = @intFromEnum(FuncSymbolEntryType.nativeFunc1),
            .innerExtra = .{
                .nativeFunc1 = .{
                    .typedFlagNumParams = isTypedMask | @as(u16, @intCast(numParams)),
                    .rFuncSigId = @intCast(rFuncSigId),
                }
            },
            .inner = .{
                .nativeFunc1 = func,
            },
        };
    }

    pub fn initFunc(pc: usize, stackSize: u16, numParams: u16, rFuncSigId: cy.sema.ResolvedFuncSigId) FuncSymbolEntry {
        return .{
            .entryT = @intFromEnum(FuncSymbolEntryType.func),
            .innerExtra = .{
                .func = .{
                    .rFuncSigId = rFuncSigId,
                },
            },
            .inner = .{
                .func = .{
                    .pc = @intCast(pc),
                    .stackSize = stackSize,
                    .numParams = numParams,
                },
            },
        };
    }

    pub fn initClosure(closure: *cy.Closure) FuncSymbolEntry {
        return .{
            .entryT = @intFromEnum(FuncSymbolEntryType.closure),
            .inner = .{
                .closure = closure,
            },
        };
    }
};

pub const VarKey = cy.hash.KeyU64;

pub const VarSym = struct {
    value: cy.Value,

    pub fn init(val: cy.Value) VarSym {
        return .{
            .value = val,
        };
    }
};

pub const FieldSymbolMap = vmc.FieldSymbolMap;

test "Internals." {
    try t.eq(@sizeOf(MethodData), 16);
    try t.eq(@sizeOf(Method), 24);
    try t.eq(@sizeOf(MethodExt), 4);
    try t.eq(@sizeOf(TypeMethodGroup), 12);
    try t.eq(@alignOf(MethodGroup), 8);
    try t.eq(@sizeOf(MethodGroup), 24);
    try t.eq(@sizeOf(MethodGroupExt), 24);

    try t.eq(@sizeOf(FuncSymbolEntry), 16);
    var funcSymEntry: FuncSymbolEntry = undefined;
    try t.eq(@intFromPtr(&funcSymEntry.entryT), @intFromPtr(&funcSymEntry));
    try t.eq(@intFromPtr(&funcSymEntry.innerExtra), @intFromPtr(&funcSymEntry) + 4);
    try t.eq(@intFromPtr(&funcSymEntry.inner), @intFromPtr(&funcSymEntry) + 8);

    try t.eq(@sizeOf(FieldSymbolMap), 16);

    try t.eq(@sizeOf(Type), 24);
}