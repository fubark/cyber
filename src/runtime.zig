const std = @import("std");
const stdx = @import("stdx");
const build_options = @import("build_options");
const t = stdx.testing;
const cy = @import("cyber.zig");
const sema = cy.sema;
const vmc = @import("vm_c.zig");
const pmc = @import("pm_c.zig");
const api = @import("capi.zig");
const logger = cy.log.scoped(.runtime);

pub const TypeKey = cy.hash.KeyU64;

pub const FieldTableKey = cy.hash.KeyU64;
pub const FieldId = u32;

pub const MethodGroupKey = vmc.NameId;
pub const TypeMethodGroupKey = cy.hash.KeyU64;

pub const MethodType = enum {
    untyped,
    untypedHost,

    /// Host func that intends to do custom optimization.
    /// Only funcs with untyped params are supported.
    optimizing,

    /// A func is typed if at least one of the params is not the any type.
    /// The return type does not count.
    typed,
    typedHost,
};

pub const MethodData = extern union {
    typedHost: extern struct {
        ptr: vmc.HostFuncFn,
        func_sig: sema.FuncSigId,
        /// Includes self param.
        numParams: u8,
    },
    untypedHost: extern struct {
        ptr: vmc.HostFuncFn,
        numParams: u8,
    },
    optimizing: extern struct {
        ptr: vmc.HostFuncFn,
        func_sig: sema.FuncSigId,
        numParams: u8,
    },
    typed: extern struct {
        func_sig: sema.FuncSigId,
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
            .type = .untypedHost,
            .data = .{
                .untypedHost = .{
                    .ptr = @ptrCast(func),
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
                    .func_sig = funcSigId,
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
            .type = .typedHost,
            .data = .{
                .typedHost = .{
                    .func_sig = funcSigId,
                    .ptr = @ptrCast(func),
                    .numParams = numParams,
                },
            },
            .funcSigId = funcSigId,
        };
    }

    pub fn initHostInline(funcSigId: sema.FuncSigId, func: cy.ZHostFuncFn, numParams: u8) MethodInit {
        return .{
            .type = .optimizing,
            .data = .{
                .optimizing = .{
                    .func_sig = funcSigId,
                    .ptr = @ptrCast(func),
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
    mruTypeId: cy.TypeId,

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
    func,
    hostFunc,
    hostInlineFunc,
    closure,
    none,
};

pub const FuncSymDetail = struct {
    namePtr: [*]const u8,
    nameLen: u32,
    funcSigId: sema.FuncSigId,
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
        hostInlineFunc: vmc.HostFuncFn,
        hostFunc: vmc.HostFuncFn,
        func: extern struct {
            pc: u32,
            /// Stack size required by the func.
            stackSize: u16,
            /// Num params used to wrap as function value.
            numParams: u8,
            reqCallTypeCheck: bool,
        },
        closure: *cy.Closure,
    },

    pub fn initNone() FuncSymbol {
        return .{
            .entryT = @intFromEnum(FuncSymbolType.none),
            .inner = undefined,
        };
    }

    pub fn initHostInlineFunc(func: vmc.HostFuncFn, isTyped: bool, numParams: u32, funcSigId: sema.FuncSigId) FuncSymbol {
        const isTypedMask: u16 = if (isTyped) 1 << 15 else 0;
        return .{
            .entryT = @intFromEnum(FuncSymbolType.hostInlineFunc),
            .innerExtra = .{
                .hostFunc = .{
                    .typedFlagNumParams = isTypedMask | @as(u16, @intCast(numParams)),
                    .funcSigId = @intCast(funcSigId),
                }
            },
            .inner = .{
                .hostInlineFunc = func,
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

    pub fn initFunc(pc: usize, stackSize: u16, numParams: u16, funcSigId: cy.sema.FuncSigId, reqCallTypeCheck: bool) FuncSymbol {
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
                    .numParams = @intCast(numParams),
                    .reqCallTypeCheck = reqCallTypeCheck,
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

pub const VarSym = struct {
    value: cy.Value,

    pub fn init(val: cy.Value) VarSym {
        return .{
            .value = val,
        };
    }
};

pub const FieldSymbolMap = vmc.FieldSymbolMap;

pub fn getName(vm: *const cy.VM, nameId: vmc.NameId) []const u8 {
    const name = vm.names.buf[nameId];
    return name.ptr[0..name.len];
}

pub fn ensureNameSym(c: *cy.VM, name: []const u8) !vmc.NameId {
    return ensureNameSymExt(c, name, false);
}

pub fn ensureNameSymExt(vm: *cy.VM, name: []const u8, dupe: bool) !vmc.NameId {
    const res = try @call(.never_inline, std.StringHashMapUnmanaged(vmc.NameId).getOrPut, .{ &vm.nameMap, vm.alloc, name});
    if (res.found_existing) {
        return res.value_ptr.*;
    } else {
        const id: u32 = @intCast(vm.names.len);
        if (dupe) {
            const new = try vm.alloc.dupe(u8, name);
            try vm.names.append(vm.alloc, .{
                .ptr = new.ptr,
                .len = @intCast(new.len),
                .owned = true,
            });
            res.key_ptr.* = new;
        } else {
            try vm.names.append(vm.alloc, .{
                .ptr = @ptrCast(name.ptr),
                .len = @intCast(name.len),
                .owned = false,
            });
        }
        res.value_ptr.* = id;
        return id;
    }
}

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
    } else {
        try t.eq(@alignOf(MethodGroup), 8);
        try t.eq(@sizeOf(MethodGroupExt), 24);
    }
}

pub fn ErrorUnion(comptime T: type) type {
    return extern struct {
        err: StaticSymbol,
        val: T,

        pub fn hasError(self: @This()) bool {
            return 0 != @as(u64, @bitCast(self.err));
        }
    };
}

pub const Context = if (build_options.rt == .pm) *Fiber else *cy.VM;
pub const TypeHandle = if (build_options.rt == .pm) *TypeTable else cy.TypeId;
pub const CompatError = if (build_options.rt == .pm) StaticSymbol else cy.Value;
pub const Error = StaticSymbol;
pub const Symbol = if (build_options.rt == .pm) StaticSymbol else cy.Value;

pub const AnyTable = extern struct {
    type: *TypeTable, 
};

pub const TypeTable = extern struct {
    is_struct: bool,
    data: extern union {
        @"struct": extern struct {
            // Size in bytes.
            size: u32,
        },
    },
    name: [*:0]const u8,
    toPrintString: *const fn(Context, Any) StaticString,
};

const StaticString = extern struct {
    ptr: [*]const u8,
    len: u64,
    buf: *StaticBuffer,
    ascii: bool,

    pub fn slice(self: StaticString) []const u8 {
        return self.ptr[0..self.len];
    }
};

const StaticBuffer = extern struct {
    len: usize,
};

const Fiber = extern struct {
    pm: *void,
    panicPayload: u64,
    panicType: u8,

    pub fn release(self: Fiber, obj: *anyopaque) void {
        _ = self;
        _ = obj;
    }

    pub fn releaseBox(v: Any) void {
        _ = v;
    
        cy.panic("TODO");
    }
};

/// Portable Any.
pub const Any = if (build_options.rt == .pm) StaticAny else cy.Value;

pub const StaticSymbol = packed struct {
    name: u48,
    len: u16,

    pub fn initNull() StaticSymbol {
        return @bitCast(@as(u64, 0));
    }

    pub fn isNull(self: StaticSymbol) bool {
        return 0 == @as(u64, @bitCast(self));
    }

    pub fn init(name: []const u8) StaticSymbol {
        return .{ .name = @truncate(@intFromPtr(name.ptr)), .len = @truncate(name.len) };
    }
};

pub fn wrapErrorValue(comptime T: type, val: T) ErrorUnion(T) {
    return .{ .err = @bitCast(@as(u64, 0)), .val = val };
}

pub fn wrapError(comptime T: type, err: Error) ErrorUnion(T) {
    return .{ .err = err, .val = undefined };
}

const StaticAny = extern struct {
    value: u64,
    vtable: *const AnyTable,

    pub fn getTypeId(self: *const Any) *TypeTable {
        return self.vtable.type;
    }
};

pub fn getTypeName(c: Context, type_h: TypeHandle) []const u8 {
    if (build_options.rt == .vm) {
        return c.types[type_h].sym.name();
    } else {
        return std.mem.span(type_h.name);
    }
}

pub fn prepThrowError(c: Context, tag: cy.bindings.Symbol) CompatError {
    if (build_options.rt == .vm) {
        const id: u8 = @intFromEnum(tag);
        c.curFiber.panicPayload = cy.Value.initErrorSymbol(id).val;
        c.curFiber.panicType = vmc.PANIC_NATIVE_THROW;
        return cy.Value.Interrupt;
    } else {
        c.panicPayload = 0;
        c.panicType = vmc.PANIC_NATIVE_THROW;
        return Symbol.init(@tagName(tag));
    }
}

pub fn getSymName(c: Context, id: u32) []const u8 {
    if (build_options.rt == .vm) {
        return c.syms.buf[id].name;
    } else {
        const tag = c.pm[0].syms[id];
        return tag.name.buf[0..tag.name.len];
    }
}

pub fn errMsg(c: Context, str: []const u8) void {
    if (build_options.rt == .vm) {
        c.errorFn.?(@ptrCast(c), api.initStr(str));
    } else {
        const w = std.io.getStdErr().writer();
        w.writeAll(str) catch cy.fatal();
        w.writeByte('\n') catch cy.fatal();
    }
}

pub fn errFmt(c: Context, format: []const u8, args: []const cy.fmt.FmtValue) void {
    if (build_options.rt == .vm) {
        const w = c.clearTempString();
        cy.fmt.print(w, format, args);
        c.errorFn.?(@ptrCast(c), api.initStr(c.getTempString()));
    } else {
        const w = std.io.getStdErr().writer();
        cy.fmt.print(w, format, args);
        w.writeByte('\n') catch cy.fatal();
    }
}

pub fn errZFmt(c: Context, comptime format: []const u8, args: anytype) void {
    if (build_options.rt == .vm) {
        const w = c.clearTempString();
        std.fmt.format(w, format, args) catch cy.fatal();
        c.errorFn.?(@ptrCast(c), api.initStr(c.getTempString()));
    } else {
        const w = std.io.getStdErr().writer();
        std.fmt.format(w, format, args) catch cy.fatal();
        w.writeByte('\n') catch cy.fatal();
    }
}

pub fn print(c: Context, str: []const u8) void {
    if (build_options.rt == .vm) {
        c.printFn.?(@ptrCast(c), api.initStr(str));
    } else {
        const w = std.io.getStdOut().writer();
        w.writeAll(str) catch cy.fatal();
        w.writeByte('\n') catch cy.fatal();
    }
}

pub fn printFmt(c: Context, format: []const u8, args: []const cy.fmt.FmtValue) void {
    if (build_options.rt == .vm) {
        const w = c.clearTempString();
        cy.fmt.print(w, format, args);
        c.printFn.?(@ptrCast(c), api.initStr(c.getTempString()));
    } else {
        const w = std.io.getStdOut().writer();
        cy.fmt.print(w, format, args);
        w.writeByte('\n') catch cy.fatal();
    }
}

pub fn printZFmt(c: Context, comptime format: []const u8, args: anytype) void {
    if (build_options.rt == .vm) {
        const w = c.clearTempString();
        std.fmt.format(w, format, args);
        c.printFn.?(@ptrCast(c), api.initStr(c.getTempString()));
    } else {
        const w = std.io.getStdOut().writer();
        std.fmt.format(w, format, args);
        w.writeByte('\n') catch cy.fatal();
    }
}

pub fn log(str: []const u8) void {
    if (build_options.rt == .vm) {
        cy.log.logFn.?(api.initStr(str));
    } else {
        const w = std.io.getStdErr().writer();
        w.writeAll(str) catch cy.fatal();
        w.writeByte('\n') catch cy.fatal();
    }
}

pub fn logZFmt(comptime format: []const u8, args: anytype) void {
    if (build_options.rt == .vm) {
        cy.log.zfmt(format, args);
    } else {
        const w = std.io.getStdErr().writer();
        w.print(format, args) catch cy.fatal();
        w.writeByte('\n') catch cy.fatal();
    }
}

pub fn logFmt(format: []const u8, args: []const cy.fmt.FmtValue) void {
    if (build_options.rt == .vm) {
        cy.log.fmt(format, args);
    } else {
        const w = std.io.getStdErr().writer();
        cy.fmt.print(w, format, args);
        w.writeByte('\n') catch cy.fatal();
    }
}

pub fn writeStderr(s: []const u8) void {
    @setCold(true);
    const w = cy.fmt.lockStderrWriter();
    defer cy.fmt.unlockPrint();
    _ = w.writeAll(s) catch |err| {
        logger.tracev("{}", .{err});
        cy.fatal();
    };
}