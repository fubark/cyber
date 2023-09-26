const stdx = @import("stdx");
const build_options = @import("build_options");
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
pub const FloatT: TypeId = vmc.TYPE_FLOAT;

/// Reserved object types known at comptime.
/// Starts at 9 since primitive types go up to 8.
pub const ListT: TypeId = vmc.TYPE_LIST;
pub const ListIteratorT: TypeId = vmc.TYPE_LIST_ITER;
pub const MapT: TypeId = vmc.TYPE_MAP;
pub const MapIteratorT: TypeId = vmc.TYPE_MAP_ITER;
pub const ClosureT: TypeId = vmc.TYPE_CLOSURE;
pub const LambdaT: TypeId = vmc.TYPE_LAMBDA;
pub const AstringT: TypeId = vmc.TYPE_ASTRING;
pub const UstringT: TypeId = vmc.TYPE_USTRING;
pub const StringSliceT: TypeId = vmc.TYPE_STRING_SLICE;
pub const RawstringT: TypeId = vmc.TYPE_RAWSTRING;
pub const RawstringSliceT: TypeId = vmc.TYPE_RAWSTRING_SLICE;
pub const FiberT: TypeId = vmc.TYPE_FIBER;
pub const BoxT: TypeId = vmc.TYPE_BOX;
pub const NativeFuncT: TypeId = vmc.TYPE_NATIVE_FUNC;
pub const TccStateT: TypeId = vmc.TYPE_TCC_STATE;
pub const PointerT: TypeId = vmc.TYPE_POINTER;
pub const FileT: TypeId = vmc.TYPE_FILE;
pub const DirT: TypeId = vmc.TYPE_DIR;
pub const DirIteratorT: TypeId = vmc.TYPE_DIR_ITER;
pub const MetaTypeT: TypeId = vmc.TYPE_METATYPE;
pub const AnyT: TypeId = 29;
pub const StringUnionT: TypeId = 30;
pub const RawstringUnionT: TypeId = 31;
pub const NumBuiltinTypes: TypeId = 32;

pub const TypeKey = cy.hash.KeyU64;

pub const FieldTableKey = cy.hash.KeyU64;
pub const FieldId = u32;

pub const MethodGroupKey = sema.NameSymId;
pub const TypeMethodGroupKey = cy.hash.KeyU64;

pub const MethodType = enum {
    untyped,
    untypedNative1,
    untypedNative2,

    /// Native func that intends to do custom optimization.
    /// Only funcs with untyped params are supported.
    optimizing,

    /// A func is typed if at least one of the params is not the any type.
    /// The return type does not count.
    typed,
    typedNative,
};

pub const MethodData = extern union {
    typedNative: extern struct {
        ptr: vmc.HostFuncFn,
        funcSigId: sema.FuncSigId,
        /// Includes self param.
        numParams: u8,
    },
    untypedNative1: extern struct {
        ptr: vmc.HostFuncFn,
        numParams: u8,
    },
    untypedNative2: extern struct {
        ptr: cy.ZHostFuncPairFn,
        numParams: u8,
    },
    optimizing: extern struct {
        ptr: cy.QuickenFuncFn,
        numParams: u8,
    },
    typed: extern struct {
        funcSigId: sema.FuncSigId,
        pc: u32,
        stackSize: u32,
        numParams: u8,
    },
    untyped: extern struct {
        numParams: u8,
        pc: u32,
        stackSize: u32,
    },
};

pub const MethodExt = struct {
    /// Signature is kept for every method.
    funcSigId: sema.FuncSigId,
};

/// Used to initialize `Method` and `MethodExt`.
pub const MethodInit = struct {
    type: MethodType,
    data: MethodData,
    funcSigId: sema.FuncSigId,

    pub fn initUntyped(funcSigId: sema.FuncSigId, pc: usize, stackSize: u32, numParams: u8) MethodInit {
        return .{
            .type = .untyped,
            .data = .{
                .untyped = .{
                    .pc = @intCast(pc),
                    .stackSize = stackSize,
                    .numParams = numParams,
                },
            },
            .funcSigId = funcSigId,
        };
    }

    pub fn initHostUntyped(funcSigId: sema.FuncSigId, func: cy.ZHostFuncFn, numParams: u8) MethodInit {
        return .{
            .type = .untypedNative1,
            .data = .{
                .untypedNative1 = .{
                    .ptr = @ptrCast(func),
                    .numParams = numParams,
                },
            },
            .funcSigId = funcSigId,
        };
    }

    pub fn initUntypedNative2(funcSigId: sema.FuncSigId, func: cy.ZHostFuncPairFn, numParams: u8) MethodInit {
        return .{
            .type = .untypedNative2,
            .data = .{
                .untypedNative2 = .{
                    .ptr = func,
                    .numParams = numParams,
                },
            },
            .funcSigId = funcSigId,
        };
    }

    pub fn initTyped(funcSigId: sema.FuncSigId, pc: usize, stackSize: u32, numParams: u8) MethodInit {
        return .{
            .type = .typed,
            .data = .{
                .typed = .{
                    .funcSigId = funcSigId,
                    .pc = @intCast(pc),
                    .stackSize = stackSize,
                    .numParams = numParams,
                },
            },
            .funcSigId = funcSigId,
        };
    }

    pub fn initHostTyped(funcSigId: sema.FuncSigId, func: cy.ZHostFuncFn, numParams: u8) MethodInit {
        return .{
            .type = .typedNative,
            .data = .{
                .typedNative = .{
                    .funcSigId = funcSigId,
                    .ptr = @ptrCast(func),
                    .numParams = numParams,
                },
            },
            .funcSigId = funcSigId,
        };
    }

    pub fn initHostQuicken(funcSigId: sema.FuncSigId, func: cy.QuickenFuncFn, numParams: u8) MethodInit {
        return .{
            .type = .optimizing,
            .data = .{
                .optimizing = .{
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
    initialFuncSigId: sema.FuncSigId,

    nameLen: u16,
    nameIsOwned: bool,

    pub fn getName(self: *const MethodGroupExt) []const u8 {
        return self.namePtr[0..self.nameLen];
    }
};

pub const FuncId = u32;

pub const FuncSymbolType = enum {
    hostFunc,
    func,
    closure,
    hostQuickenFunc,

    /// Placeholder for a func symbol that contains a linked list of overloaded functions.
    /// The arg types are checked at runtime.
    // multiple,

    none,
};

pub const FuncSymDetail = struct {
    nameId: sema.NameSymId,
};

pub const FuncSymbol = extern struct {
    entryT: u32,
    innerExtra: extern union {
        // hostQuickenFunc shares the same field.
        hostFunc: extern struct {
            /// Used to wrap a host func as a function value.
            typedFlagNumParams: u16,
            funcSigId: u16,

            pub fn numParams(self: @This()) u16 {
                return self.typedFlagNumParams & ~(@as(u16, 1) << 15);
            }
        },
        none: extern struct {
            funcSigId: u32,
        },
        func: extern struct {
            funcSigId: u32,
        },
    } = undefined,
    inner: extern union {
        hostQuickenFunc: cy.QuickenFuncFn,
        hostFunc: vmc.HostFuncFn,
        func: extern struct {
            pc: u32,
            /// Stack size required by the func.
            stackSize: u16,
            /// Num params used to wrap as function value.
            numParams: u16,
        },
        closure: *cy.Closure,
    },

    pub fn initHostQuickenFunc(func: cy.QuickenFuncFn, isTyped: bool, numParams: u32, funcSigId: sema.FuncSigId) FuncSymbol {
        const isTypedMask: u16 = if (isTyped) 1 << 15 else 0;
        return .{
            .entryT = @intFromEnum(FuncSymbolType.hostQuickenFunc),
            .innerExtra = .{
                .hostFunc = .{
                    .typedFlagNumParams = isTypedMask | @as(u16, @intCast(numParams)),
                    .funcSigId = @intCast(funcSigId),
                }
            },
            .inner = .{
                .hostQuickenFunc = func,
            },
        };
    }

    pub fn initHostFunc(func: vmc.HostFuncFn, isTyped: bool, numParams: u32, funcSigId: sema.FuncSigId) FuncSymbol {
        const isTypedMask: u16 = if (isTyped) 1 << 15 else 0;
        return .{
            .entryT = @intFromEnum(FuncSymbolType.hostFunc),
            .innerExtra = .{
                .hostFunc = .{
                    .typedFlagNumParams = isTypedMask | @as(u16, @intCast(numParams)),
                    .funcSigId = @intCast(funcSigId),
                }
            },
            .inner = .{
                .hostFunc = func,
            },
        };
    }

    pub fn initFunc(pc: usize, stackSize: u16, numParams: u16, funcSigId: cy.sema.FuncSigId) FuncSymbol {
        return .{
            .entryT = @intFromEnum(FuncSymbolType.func),
            .innerExtra = .{
                .func = .{
                    .funcSigId = funcSigId,
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

    pub fn initClosure(closure: *cy.Closure) FuncSymbol {
        return .{
            .entryT = @intFromEnum(FuncSymbolType.closure),
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

test "runtime internals." {
    try t.eq(@sizeOf(MethodData), 16);
    try t.eq(@sizeOf(Method), 24);
    try t.eq(@sizeOf(MethodExt), 4);
    try t.eq(@sizeOf(TypeMethodGroup), 12);
    try t.eq(@sizeOf(MethodGroup), 24);

    try t.eq(@sizeOf(FuncSymbol), 16);
    var funcSymEntry: FuncSymbol = undefined;
    try t.eq(@intFromPtr(&funcSymEntry.entryT), @intFromPtr(&funcSymEntry));
    try t.eq(@intFromPtr(&funcSymEntry.innerExtra), @intFromPtr(&funcSymEntry) + 4);
    try t.eq(@intFromPtr(&funcSymEntry.inner), @intFromPtr(&funcSymEntry) + 8);

    try t.eq(@sizeOf(FieldSymbolMap), 16);

    if (cy.is32Bit) {
        try t.eq(@alignOf(MethodGroup), 4);
        try t.eq(@sizeOf(MethodGroupExt), 20);
        try t.eq(@sizeOf(vmc.Type), 16);
    } else {
        try t.eq(@alignOf(MethodGroup), 8);
        try t.eq(@sizeOf(MethodGroupExt), 24);
        try t.eq(@sizeOf(vmc.Type), 24);
    }
}