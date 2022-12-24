const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const t = stdx.testing;

const log = stdx.log.scoped(.fmt);

// This provides a simple string interpolation without comptime.

const FmtValueType = enum {
    char,
    string,
    u64,
    f64,
    bool,
    ptr,
};

pub const FmtValue = struct {
    valT: FmtValueType,
    inner: union {
        u64: u64,
        f64: f64,
        string: []const u8,
        char: u8,
        bool: bool,
        ptr: *anyopaque,
    },
};

pub fn v(val: anytype) FmtValue {
    switch (@TypeOf(val)) {
        bool => {
            return .{
                .valT = .bool,
                .inner = .{
                    .bool = val,
                },
            };
        },
        u8,
        u32,
        u64,
        usize => return u64v(val),
        f64 => return .{
            .valT = .f64,
            .inner = .{
                .f64 = val,
            },
        },
        []u8,
        []const u8 => return str(val),
        else => {
            if (@typeInfo(@TypeOf(val)) == .Enum) {
                return enumv(val);
            } else if (@typeInfo(@TypeOf(val)) == .ErrorSet) {
                return str(@errorName(val));
            } else if (@typeInfo(@TypeOf(val)) == .Pointer) {
                return .{
                    .valT = .ptr,
                    .inner = .{
                        .ptr = val,
                    }
                };
            } else {
                @compileError(std.fmt.comptimePrint("Unexpected type: {}", .{@TypeOf(val)}));
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
        .u64 => {
            try std.fmt.formatInt(val.inner.u64, 10, .lower, .{}, writer);
        },
        .f64 => {
            try std.fmt.formatFloatDecimal(val.inner.f64, .{}, writer);
        },
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

pub fn printStdout(fmt: []const u8, vals: []const FmtValue) !void {
    @setCold(true);
    printMutex.lock();
    defer printMutex.unlock();
    const w = std.io.getStdOut().writer();
    if (builtin.is_test) {
        var msg: []const u8 = undefined;
        if (fmt[fmt.len-1] == '\n') {
            msg = try allocFormat(t.alloc, fmt[0..fmt.len-1], vals);
        } else {
            msg = try allocFormat(t.alloc, fmt[0..fmt.len], vals);
        }
        defer t.alloc.free(msg);
        log.debug("{s}", .{msg});
    } else {
        try format(w, fmt, vals);
    }
}

pub fn printStderr(fmt: []const u8, vals: []const FmtValue) !void {
    @setCold(true);
    printMutex.lock();
    defer printMutex.unlock();
    const w = std.io.getStdErr().writer();
    try format(w, fmt, vals);
}

pub fn panic(fmt: []const u8, vals: []const FmtValue) noreturn {
    @setCold(true);
    printStderr(fmt, vals) catch {};
    @panic("");
}