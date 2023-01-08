const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const t = stdx.testing;

const log = stdx.log.scoped(.fmt);

// This provides a simple string interpolation without comptime.

const FmtValueType = enum {
    char,
    string,
    u8,
    u16,
    u32,
    u64,
    f64,
    bool,
    ptr,
    enumt,
    err,
};

pub const FmtValue = struct {
    valT: FmtValueType,
    inner: union {
        u8: u8,
        u16: u16,
        u32: u32,
        u64: u64,
        f64: f64,
        string: []const u8,
        char: u8,
        bool: bool,
        ptr: ?*anyopaque,
    },
};

fn toFmtValueType(comptime T: type) FmtValueType {
    switch (T) {
        bool => return .bool,
        u8 => return .u8,
        u16 => return .u16,
        u32 => return .u32,
        usize,
        u64 => return .u64,
        f64 => return .f64,
        []u8,
        []const u8 => return .string,
        else => {
            if (@typeInfo(T) == .Enum) {
                return .enumt;
            } else if (@typeInfo(T) == .ErrorSet) {
                return .err;
            } else if (@typeInfo(T) == .Optional) {
                if (@typeInfo(@typeInfo(T).Optional.child) == .Pointer) {
                    return .ptr;
                } else {
                    @compileError(std.fmt.comptimePrint("Unexpected type: {}", .{T}));
                }
            } else if (@typeInfo(T) == .Pointer) {
                return .ptr;
            } else {
                @compileError(std.fmt.comptimePrint("Unexpected type: {}", .{T}));
            }
        },
    }
}

pub fn v(val: anytype) FmtValue {
    const fmt_t = comptime toFmtValueType(@TypeOf(val));
    switch (fmt_t) {
        .bool => {
            return .{
                .valT = .bool,
                .inner = .{
                    .bool = val,
                },
            };
        },
        .char => {
            return .{
                .valT = .char,
                .inner = .{
                    .u8 = val,
                }
            };
        },
        .u8 => {
            return .{
                .valT = .u8,
                .inner = .{
                    .u8 = val,
                }
            };
        },
        .u16 => {
            return .{
                .valT = .u16,
                .inner = .{
                    .u16 = val,
                }
            };
        },
        .u32 => {
            return .{
                .valT = .u32,
                .inner = .{
                    .u32 = val,
                }
            };
        },
        .u64 => return u64v(val),
        .f64 => return .{
            .valT = .f64,
            .inner = .{
                .f64 = val,
            },
        },
        .string => return str(val),
        .enumt => return enumv(val),
        .err => return str(@errorName(val)),
        .ptr =>  return .{
            .valT = .ptr,
            .inner = .{
                .ptr = val,
            }
        },
    }
}

pub fn char(ch: u8) FmtValue {
    return .{
        .valT = .char,
        .inner = .{
            .char = ch,
        }
    };
}

pub fn u64v(n: u64) FmtValue {
    return .{
        .valT = .u64,
        .inner = .{
            .u64 = n,
        }
    };
}

pub fn enumv(e: anytype) FmtValue {
    return .{
        .valT = .string,
        .inner = .{
            .string = @tagName(e),
        }
    };
}

pub fn str(s: []const u8) FmtValue {
    return .{
        .valT = .string,
        .inner = .{
            .string = s,
        }
    };
}

fn formatValue(writer: anytype, val: FmtValue) !void {
    switch (val.valT) {
        .bool => {
            if (val.inner.bool) {
                try writer.writeAll("true");
            } else {
                try writer.writeAll("false");
            }
        },
        .char => {
            try writer.writeByte(val.inner.char);
        },
        .u8 => {
            try std.fmt.formatInt(val.inner.u8, 10, .lower, .{}, writer);
        },
        .u16 => {
            try std.fmt.formatInt(val.inner.u16, 10, .lower, .{}, writer);
        },
        .u32 => {
            try std.fmt.formatInt(val.inner.u32, 10, .lower, .{}, writer);
        },
        .u64 => {
            try std.fmt.formatInt(val.inner.u64, 10, .lower, .{}, writer);
        },
        .f64 => {
            try std.fmt.formatFloatDecimal(val.inner.f64, .{}, writer);
        },
        .err,
        .enumt,
        .string => {
            try writer.writeAll(val.inner.string);
        },
        .ptr => {
            try writer.writeByte('@');
            try std.fmt.formatInt(@ptrToInt(val.inner.ptr), 16, .lower, .{}, writer);
        },
        // else => stdx.panicFmt("unsupported {}", .{val.valT}),
    }
}

pub fn format(writer: anytype, fmt: []const u8, vals: []const FmtValue) !void {
    var valIdx: u32 = 0;
    var i: u32 = 0;
    while (i < fmt.len) {
        const ch = fmt[i];
        if (ch == '{') {
            if (i + 1 < fmt.len) {
                if (fmt[i + 1] == '}') {
                    if (valIdx == vals.len) {
                        log.debug("Format expected {}th value, got {} values", .{valIdx + 1, vals.len});
                        return error.FormatError;
                    }
                    try formatValue(writer, vals[valIdx]);
                    valIdx += 1;
                    i += 2;
                    continue;
                }
            }
        }
        try writer.writeByte(ch);
        i += 1;
        continue;
    }
    if (valIdx < vals.len) {
        log.debug("Format had {} placeholders, got {} values", .{valIdx + 1, vals.len});
        return error.FormatError;
    }
}

pub fn allocFormat(alloc: std.mem.Allocator, fmt: []const u8, vals: []const FmtValue) ![]const u8 {
    @setCold(true);
    var buf: std.ArrayListUnmanaged(u8) = .{};
    defer buf.deinit(alloc);
    var w = buf.writer(alloc);
    try format(w, fmt, vals);
    return buf.toOwnedSlice(alloc);
}

var printMutex = std.Thread.Mutex{};

pub fn printStdout(fmt: []const u8, vals: []const FmtValue) void {
    printStdoutOrErr(fmt, vals) catch |err| {
        log.debug("{}", .{err});
        stdx.fatal();
    };
}

pub fn printStdoutOrErr(fmt: []const u8, vals: []const FmtValue) !void {
    @setCold(true);
    printMutex.lock();
    defer printMutex.unlock();
    const w = std.io.getStdOut().writer();
    if (builtin.is_test) {
        if (@enumToInt(std.log.Level.debug) <= @enumToInt(std.testing.log_level)) {
            try format(w, fmt, vals);
        }
    } else {
        try format(w, fmt, vals);
    }
}

pub fn printDeprecated(name: []const u8, fmt: []const u8, vals: []const FmtValue) void {
    printStderr("{} is deprecated: ", &.{v(name)});
    printStderr(fmt, vals);
    printStderr("\n", &.{});
}

pub fn printStderr(fmt: []const u8, vals: []const FmtValue) void {
    printStderrOrErr(fmt, vals) catch |err| {
        log.debug("{}", .{err});
        stdx.fatal();
    };
}

pub fn printStderrOrErr(fmt: []const u8, vals: []const FmtValue) !void {
    @setCold(true);
    printMutex.lock();
    defer printMutex.unlock();
    const w = std.io.getStdErr().writer();
    try format(w, fmt, vals);
}

pub fn panic(fmt: []const u8, vals: []const FmtValue) noreturn {
    @setCold(true);
    printStderr(fmt, vals);
    @panic("");
}