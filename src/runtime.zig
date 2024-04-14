const std = @import("std");
const stdx = @import("stdx");
const build_options = @import("build_options");
const t = stdx.testing;
const cy = @import("cyber.zig");
const sema = cy.sema;
const vmc = @import("vm_c.zig");
const pmc = @import("pm_c.zig");
const api = @import("capi.zig");
const lib = @import("lib.zig");
const logger = cy.log.scoped(.runtime);

pub const TypeKey = cy.hash.KeyU64;

pub const FieldTableKey = cy.hash.KeyU64;
pub const FieldId = u32;

pub const MethodKey = vmc.NameId;
pub const TypeMethodKey = cy.hash.KeyU64;

pub const TypeMethod = struct {
    id: u32,

    // TODO: Compact into u32.
    overloaded: bool,
};

/// Keeping this small is better for function calls.
pub const Method = struct {
    /// Most recent type is cached to avoid hashmap lookup. 
    mru_type: cy.TypeId,

    /// If overloaded this is an index into `overloaded_funcs`, otherwise `funcSyms`
    mru_id: u32,
    mru_overloaded: bool,

    /// Whether there are multiple types that share this method name.
    has_multiple_types: bool,
};

pub const FuncId = u32;
pub const MethodId = u32;
pub const TypeMethodId = u32;

pub const FuncSymbolType = enum(u8) {
    func,
    host_func,
    null,
};

pub const FuncSymDetail = struct {
    namePtr: [*]const u8,
    nameLen: u32,
    funcSigId: sema.FuncSigId,

    pub fn name(self: FuncSymDetail) []const u8 {
        return self.namePtr[0..self.nameLen];
    }
};

pub const FuncSymbol = extern struct {
    type: FuncSymbolType,

    /// Since dynamic method calls reuse the same func symbol table,
    /// this is needed during for the overloaded search path.
    is_method: bool,

    /// A func requires type checking if at least one of the params is not the `any` or `dynamic` type.
    req_type_check: bool,

    nparams: u8,

    sig: cy.sema.FuncSigId,

    data: extern union {
        host_func: vmc.HostFuncFn,
        func: extern struct {
            pc: u32,
            /// Stack size required by the func.
            stackSize: u16,
        },
    },

    pub fn initNull() FuncSymbol {
        return .{
            .type = .null,
            .is_method = undefined,
            .sig = undefined,
            .nparams = undefined,
            .req_type_check = undefined,
            .data = undefined,
        };
    }

    pub fn initHostFunc(func: vmc.HostFuncFn, req_type_check: bool, is_method: bool, numParams: u32, funcSigId: sema.FuncSigId) FuncSymbol {
        return .{
            .type = .host_func,
            .is_method = is_method,
            .req_type_check = req_type_check,
            .nparams = @intCast(numParams),
            .sig = funcSigId,
            .data = .{
                .host_func = func,
            },
        };
    }

    pub fn initFunc(pc: usize, stackSize: u16, numParams: u16, funcSigId: cy.sema.FuncSigId, reqCallTypeCheck: bool, is_method: bool) FuncSymbol {
        return .{
            .type = .func,
            .is_method = is_method,
            .req_type_check = reqCallTypeCheck,
            .sig = funcSigId,
            .nparams = @intCast(numParams),
            .data = .{
                .func = .{
                    .pc = @intCast(pc),
                    .stackSize = stackSize,
                },
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
    try t.eq(@sizeOf(TypeMethod), 8);
    try t.eq(@sizeOf(Method), 12);
    try t.eq(@alignOf(Method), 4);

    try t.eq(@sizeOf(FuncSymbol), 16);
    try t.eq(@offsetOf(FuncSymbol, "type"), @offsetOf(vmc.FuncSymbol, "type"));
    try t.eq(@offsetOf(FuncSymbol, "sig"), @offsetOf(vmc.FuncSymbol, "sig"));
    try t.eq(@offsetOf(FuncSymbol, "nparams"), @offsetOf(vmc.FuncSymbol, "nparams"));
    try t.eq(@offsetOf(FuncSymbol, "is_method"), @offsetOf(vmc.FuncSymbol, "is_method"));
    try t.eq(@offsetOf(FuncSymbol, "req_type_check"), @offsetOf(vmc.FuncSymbol, "req_type_check"));
    try t.eq(@offsetOf(FuncSymbol, "data"), @offsetOf(vmc.FuncSymbol, "data"));

    try t.eq(@sizeOf(FieldSymbolMap), 16);
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
pub const TypeHandle = if (build_options.rt == .pm) *const TypeTable else cy.TypeId;
pub const CompatError = if (build_options.rt == .pm) StaticSymbol else cy.Value;
pub const Error = StaticSymbol;
pub const Symbol = if (build_options.rt == .pm) StaticSymbol else cy.Value;

pub const TypeTable = extern struct {
    // Size in bytes.
    size: usize,
    name: [*:0]const u8,
    toPrintString: *const fn(Context, Any) StaticString,
    kind: u8,
};

pub const TypeKindObject: u8 = 0;
pub const TypeKindStruct: u8 = 1;

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
        return .{ .name = @intCast(@intFromPtr(name.ptr)), .len = @truncate(name.len) };
    }
};

pub fn wrapErrorValue(comptime T: type, val: T) ErrorUnion(T) {
    return .{ .err = @bitCast(@as(u64, 0)), .val = val };
}

pub fn wrapError(comptime T: type, e: Error) ErrorUnion(T) {
    return .{ .err = e, .val = undefined };
}

const StaticAny = extern struct {
    value: u64,
    type: *const TypeTable,

    pub fn getTypeId(self: *const Any) *const TypeTable {
        return self.type;
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

pub const ErrorWriter = struct {
    c: Context,

    pub fn write(self: ErrorWriter, bytes: []const u8) !void {
        return self.writeAll(bytes);
    }

    pub fn writeAll(self: ErrorWriter, bytes: []const u8) !void {
        err(self.c, bytes);
    }

    pub fn writeByte(self: ErrorWriter, byte: u8) !void {
        const array = [1]u8{byte};
        return self.writeAll(&array);
    }

    pub fn writeByteNTimes(self: ErrorWriter, byte: u8, n: usize) !void {
        var bytes: [256]u8 = undefined;
        @memset(bytes[0..], byte);

        var remaining = n;
        while (remaining > 0) {
            const to_write = @min(remaining, bytes.len);
            try self.writeAll(bytes[0..to_write]);
            remaining -= to_write;
        }
    }
};

pub fn err(c: Context, str: []const u8) void {
    if (build_options.rt == .vm) {
        c.print_err.?(@ptrCast(c), api.toStr(str));
    } else {
        const w = std.io.getStdErr().writer();
        w.writeAll(str) catch cy.fatal();
    }
}

pub fn errFmt(c: Context, format: []const u8, args: []const cy.fmt.FmtValue) void {
    if (build_options.rt == .vm) {
        const w = c.clearTempString();
        cy.fmt.print(w, format, args);
        c.print_err.?(@ptrCast(c), api.toStr(c.getTempString()));
    } else {
        const w = std.io.getStdErr().writer();
        cy.fmt.print(w, format, args);
    }
}

pub fn errZFmt(c: Context, comptime format: []const u8, args: anytype) void {
    if (build_options.rt == .vm) {
        const w = c.clearTempString();
        std.fmt.format(w, format, args) catch cy.fatal();
        c.print_err.?(@ptrCast(c), api.toStr(c.getTempString()));
    } else {
        const w = std.io.getStdErr().writer();
        std.fmt.format(w, format, args) catch cy.fatal();
    }
}

pub fn print(c: Context, str: []const u8) void {
    if (build_options.rt == .vm) {
        c.print.?(@ptrCast(c), api.toStr(str));
    } else {
        const w = std.io.getStdOut().writer();
        w.writeAll(str) catch cy.fatal();
    }
}

pub fn printFmt(c: Context, format: []const u8, args: []const cy.fmt.FmtValue) void {
    if (build_options.rt == .vm) {
        const w = c.clearTempString();
        cy.fmt.print(w, format, args);
        c.print.?(@ptrCast(c), api.toStr(c.getTempString()));
    } else {
        const w = std.io.getStdOut().writer();
        cy.fmt.print(w, format, args);
    }
}

pub fn printZFmt(c: Context, comptime format: []const u8, args: anytype) void {
    if (build_options.rt == .vm) {
        const w = c.clearTempString();
        std.fmt.format(w, format, args) catch cy.fatal();
        c.print.?(@ptrCast(c), api.toStr(c.getTempString()));
    } else {
        const w = std.io.getStdOut().writer();
        std.fmt.format(w, format, args) catch cy.fatal();
    }
}

pub fn log(str: []const u8) void {
    if (build_options.rt == .vm) {
        lib.clLog.?(api.toStr(str));
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
    _ = w.writeAll(s) catch |e| {
        logger.tracev("{}", .{e});
        cy.fatal();
    };
}