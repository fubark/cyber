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
        funcSigId: sema.FuncSigId,
        /// Includes self param.
        numParams: u8,
    },
    untypedHost: extern struct {
        ptr: vmc.HostFuncFn,
        numParams: u8,
    },
    optimizing: extern struct {
        ptr: vmc.HostFuncFn,
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
            .type = .typedHost,
            .data = .{
                .typedHost = .{
                    .funcSigId = funcSigId,
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

pub const Context = *align(8) anyopaque;

pub fn prepThrowError(ctx: Context, tag: cy.bindings.Symbol) cy.Value {
    if (build_options.rt == .vm) {
        const vm: *cy.VM = @ptrCast(ctx);
        const id: u8 = @intFromEnum(tag);
        vm.curFiber.panicPayload = cy.Value.initErrorSymbol(id).val;
        vm.curFiber.panicType = vmc.PANIC_NATIVE_THROW;
        return cy.Value.Interrupt;
    } else {
        const f: *pmc.Fiber = @ptrCast(ctx);
        const id: u8 = @intFromEnum(tag);
        f.panicPayload = cy.Value.initErrorSymbol(id).val;
        f.panicType = vmc.PANIC_NATIVE_THROW;
        return cy.Value.Interrupt;
    }
}

pub fn getSymName(c: Context, id: u32) []const u8 {
    if (build_options.rt == .vm) {
        const vm: *cy.VM = @ptrCast(c);
        return vm.syms.buf[id].name;
    } else {
        const f: *pmc.Fiber = @ptrCast(c);
        const tag = f.pm[0].syms[id];
        return tag.name.buf[0..tag.name.len];
    }
}

pub fn print(c: Context, str: []const u8) void {
    if (build_options.rt == .vm) {
        const vm: *cy.VM = @ptrCast(c);
        vm.printFn.?(@ptrCast(vm), api.initStr(str));
    } else {
        const w = std.io.getStdOut().writer();
        w.writeAll(str) catch cy.fatal();
        w.writeByte('\n') catch cy.fatal();
    }
}

pub fn glog(str: []const u8) void {
    if (build_options.rt == .vm) {
        cy.log.logFn.?(api.initStr(str));
    } else {
        const w = std.io.getStdErr().writer();
        w.writeAll(str) catch cy.fatal();
        w.writeByte('\n') catch cy.fatal();
    }
}

pub fn glogZFmt(comptime format: []const u8, args: anytype) void {
    if (build_options.rt == .vm) {
        cy.log.zfmt(format, args);
    } else {
        const w = std.io.getStdErr().writer();
        w.print(format, args) catch cy.fatal();
        w.writeByte('\n') catch cy.fatal();
    }
}

pub fn log(c: Context, str: []const u8) void {
    if (build_options.rt == .vm) {
        const vm: *cy.VM = @ptrCast(c);
        vm.logFn.?(@ptrCast(vm), api.initStr(str));
    } else {
        const w = std.io.getStdErr().writer();
        w.writeAll(str) catch cy.fatal();
        w.writeByte('\n') catch cy.fatal();
    }
}

pub fn logFmt(c: Context, format: []const u8, args: []const cy.fmt.FmtValue) void {
    if (build_options.rt == .vm) {
        const vm: *cy.VM = @ptrCast(c);
        const w = vm.clearTempString();
        cy.fmt.print(w, format, args);
        vm.log(vm.getTempString());
    } else {
        const w = std.io.getStdErr().writer();
        cy.fmt.print(w, format, args);
        w.writeByte('\n') catch cy.fatal();
    }
}

pub fn logZFmt(c: Context, comptime format: []const u8, args: anytype) void {
    if (build_options.rt == .vm) {
        const vm: *cy.VM = @ptrCast(c);
        const w = vm.clearTempString();
        std.fmt.format(w, format, args) catch cy.fatal();
        vm.log(vm.getTempString());
    } else {
        const w = std.io.getStdErr().writer();
        w.print(format, args) catch cy.fatal();
        w.writeByte('\n') catch cy.fatal();
    }
}

pub fn writeStderr(s: []const u8) void {
    @setCold(true);
    const w = cy.fmt.lockStderrWriter();
    defer cy.fmt.unlockPrint();
    _ = w.writeAll(s) catch |err| {
        logger.gtracev("{}", .{err});
        cy.fatal();
    };
}