const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const os = @import("std/os.zig");

const log = cy.log.scoped(.fmt);

// This provides a simple string interpolation without comptime.

const FmtValueType = enum(u8) {
    char,
    string,
    i8,
    u8,
    i16,
    u16,
    u32,
    i32,
    i48,
    u64,
    u64_hex,
    i64,
    f64,
    bool,
    ptr,
    enumt,
    err,
    stringz,
    sliceU8,
    repeat,
};

pub const FmtValue = extern struct {
    data: extern union {
        u8: u8,
        u16: u16,
        u32: u32,
        u64: u64,
        f64: f64,
        string: [*]const u8,
        stringz: [*:0]const u8,
        char: u8,
        bool: bool,
        ptr: ?*anyopaque,
        slice: [*]const u8,
        repeat: extern struct {
            char: u8,
            n: u32,
        },
    },
    data2: extern union {
        string: u32, // string len.
        slice: u32, // slice len.
    } = undefined,
    type: FmtValueType,
};

test "fmt internals." {
    try t.eq(@sizeOf(FmtValue), 16);
}

fn toFmtValueType(comptime T: type) FmtValueType {
    switch (T) {
        bool => return .bool,
        i8 => return .i8,
        u8 => return .u8,
        u16 => return .u16,
        i16 => return .i16,
        u32 => return .u32,
        i32 => return .i32,
        i48 => return .i48,
        usize, u64 => return .u64,
        i64 => return .i64,
        f64 => return .f64,
        []u8, []const u8 => return .string,
        [*:0]u8 => return .stringz,
        [:0]const u8 => return .string,
        else => {
            if (@typeInfo(T) == .@"enum") {
                return .enumt;
            } else if (@typeInfo(T) == .error_set) {
                return .err;
            } else if (@typeInfo(T) == .optional) {
                if (@typeInfo(@typeInfo(T).Optional.child) == .Pointer) {
                    return .ptr;
                } else {
                    @compileError(std.fmt.comptimePrint("Unexpected type: {}", .{T}));
                }
            } else if (@typeInfo(T) == .pointer) {
                return .ptr;
            } else {
                @compileError(std.fmt.comptimePrint("Unexpected type: {}", .{T}));
            }
        },
    }
}

pub inline fn v(val: anytype) FmtValue {
    const fmt_t = comptime toFmtValueType(@TypeOf(val));
    switch (fmt_t) {
        .bool => {
            return .{
                .type = .bool,
                .data = .{
                    .bool = val,
                },
            };
        },
        .char => {
            return .{ .type = .char, .data = .{
                .u8 = val,
            } };
        },
        .i8 => {
            return .{ .type = .i8, .data = .{
                .u8 = @bitCast(val),
            } };
        },
        .u8 => {
            return .{ .type = .u8, .data = .{
                .u8 = val,
            } };
        },
        .i16 => {
            return .{ .type = .i16, .data = .{
                .u16 = @bitCast(val),
            } };
        },
        .u16 => {
            return .{ .type = .u16, .data = .{
                .u16 = val,
            } };
        },
        .i32 => {
            return .{ .type = .i32, .data = .{
                .u32 = @bitCast(val),
            } };
        },
        .u32 => {
            return .{ .type = .u32, .data = .{
                .u32 = val,
            } };
        },
        .i48 => {
            return .{ .type = .i48, .data = .{
                .u64 = @as(u48, @bitCast(val)),
            } };
        },
        .i64 => {
            return .{ .type = .i64, .data = .{
                .u64 = @bitCast(val),
            } };
        },
        .u64 => return u64v(val),
        .f64 => return .{
            .type = .f64,
            .data = .{
                .f64 = val,
            },
        },
        .string => return str(val),
        .stringz => return .{
            .type = .stringz,
            .data = .{
                .stringz = val,
            },
        },
        .enumt => return enumv(val),
        .err => return str(@errorName(val)),
        .ptr =>  return .{
            .type = .ptr,
            .data = .{
                .ptr = @ptrFromInt(@intFromPtr(val)),
            }
        },
        .u64_hex,
        .sliceU8,
        .repeat => {
            cy.unexpected();
        },
    }
}

pub fn char(ch: u8) FmtValue {
    return .{
        .type = .char,
        .data = .{
            .char = ch,
        },
    };
}

pub fn sliceU8(slice: []const u8) FmtValue {
    return .{
        .type = .sliceU8,
        .data = .{
            .slice = slice.ptr,
        },
        .data2 = .{
            .slice = @intCast(slice.len),
        },
    };
}

pub fn u64Hex(n: u64) FmtValue {
    return .{
        .type = .u64_hex,
        .data = .{
            .u64 = n,
        },
    };
}

pub fn repeat(ch: u8, n: u32) FmtValue {
    return .{
        .type = .repeat,
        .data = .{
            .repeat = .{
                .char = ch,
                .n = n,
            },
        },
    };
}

pub fn u64v(n: u64) FmtValue {
    return .{ .type = .u64, .data = .{
        .u64 = n,
    } };
}

pub fn enumv(e: anytype) FmtValue {
    const name = @tagName(e);
    return .{
        .type = .string,
        .data = .{
            .string = name.ptr,
        },
        .data2 = .{
            .string = @intCast(name.len),
        },
    };
}

pub fn str(s: []const u8) FmtValue {
    return .{
        .type = .string,
        .data = .{
            .string = s.ptr,
        },
        .data2 = .{
            .string = @intCast(s.len),
        },
    };
}

fn formatValue(writer: *std.io.Writer, val: FmtValue) !void {
    switch (val.type) {
        .bool => {
            if (val.data.bool) {
                try writer.writeAll("true");
            } else {
                try writer.writeAll("false");
            }
        },
        .char => {
            try writer.writeByte(val.data.char);
        },
        .i8 => {
            try writer.printInt(@as(i8, @bitCast(val.data.u8)), 10, .lower, .{});
        },
        .u8 => {
            try writer.printInt(val.data.u8, 10, .lower, .{});
        },
        .i16 => {
            try writer.printInt(@as(i16, @bitCast(val.data.u16)), 10, .lower, .{});
        },
        .u16 => {
            try writer.printInt(val.data.u16, 10, .lower, .{});
        },
        .i32 => {
            try writer.printInt(@as(i32, @bitCast(val.data.u32)), 10, .lower, .{});
        },
        .u32 => {
            try writer.printInt(val.data.u32, 10, .lower, .{});
        },
        .i64 => {
            try writer.printInt(@as(i64, @bitCast(val.data.u64)), 10, .lower, .{});
        },
        .u64 => {
            try writer.printInt(val.data.u64, 10, .lower, .{});
        },
        .u64_hex => {
            try writer.printInt(val.data.u64, 16, .lower, .{});
        },
        .f64 => {
            try writer.printFloat(val.data.f64, .{});
        },
        .err, .enumt, .string => {
            try writer.writeAll(val.data.string[0..val.data2.string]);
        },
        .stringz => {
            const str_ = std.mem.sliceTo(val.data.stringz, 0);
            try writer.writeAll(str_);
        },
        .ptr => {
            try writer.writeByte('@');
            try writer.printInt(@intFromPtr(val.data.ptr), 16, .lower, .{});
        },
        .sliceU8 => {
            try writer.writeByte('{');
            if (val.data2.slice > 0) {
                try writer.printInt(val.data.slice[0], 10, .lower, .{});
                for (val.data.slice[1..val.data2.slice]) |it| {
                    try writer.writeAll(", ");
                    try writer.printInt(it, 10, .lower, .{});
                }
            }
            try writer.writeByte('}');
        },
        .repeat => {
            _ = try writer.splatByte(val.data.repeat.char, val.data.repeat.n);
        },
        else => cy.panicFmt("unsupported {}", .{val.type}),
    }
}

/// Extracted from std/fmt.zig
fn formatFloatValue(
    value: anytype,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    var buf: [std.fmt.format_float.bufferSize(.decimal, f64)]u8 = undefined;

    if (fmt.len == 0 or comptime std.mem.eql(u8, fmt, "e")) {
        const s = std.fmt.formatFloat(&buf, value, .{ .mode = .scientific, .precision = options.precision }) catch |err| switch (err) {
            error.BufferTooSmall => "(float)",
        };
        return std.fmt.formatBuf(s, options, writer);
    } else if (comptime std.mem.eql(u8, fmt, "d")) {
        const s = std.fmt.formatFloat(&buf, value, .{ .mode = .decimal, .precision = options.precision }) catch |err| switch (err) {
            error.BufferTooSmall => "(float)",
        };
        return std.fmt.formatBuf(s, options, writer);
    } else if (comptime std.mem.eql(u8, fmt, "x")) {
        var buf_stream = std.io.fixedBufferStream(&buf);
        std.fmt.formatFloatHexadecimal(value, options, buf_stream.writer()) catch |err| switch (err) {
            error.NoSpaceLeft => unreachable,
        };
        return std.fmt.formatBuf(buf_stream.getWritten(), options, writer);
    } else {
        std.fmt.invalidFmtError(fmt, value);
    }
}

pub fn format(writer: *std.Io.Writer, fmt: []const u8, vals: []const FmtValue) !void {
    var valIdx: u32 = 0;
    var i: u32 = 0;
    while (i < fmt.len) {
        const ch = fmt[i];
        if (ch == '{') {
            if (i + 1 < fmt.len) {
                if (fmt[i + 1] == '}') {
                    if (valIdx == vals.len) {
                        log.tracev("Format expected {}th value, got {} values", .{ valIdx + 1, vals.len });
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
        log.tracev("Format had {} placeholders, got {} values", .{ valIdx, vals.len });
        return error.FormatError;
    }
}

pub fn allocFormat(alloc: std.mem.Allocator, fmt: []const u8, vals: []const FmtValue) ![]u8 {
    @branchHint(.cold);
    var buf: std.ArrayListUnmanaged(u8) = .{};
    defer buf.deinit(alloc);
    var adapter = buf.writer(alloc).adaptToNewApi(&.{});
    try format(&adapter.new_interface, fmt, vals);
    return buf.toOwnedSlice(alloc);
}

var printMutex = std.Thread.Mutex{};

pub fn printStdout(fmt: []const u8, vals: []const FmtValue) void {
    printStdoutOrErr(fmt, vals) catch |err| {
        log.tracev("{}", .{err});
        cy.fatal();
    };
}

pub fn printStdoutOrErr(fmt: []const u8, vals: []const FmtValue) !void {
    @branchHint(.cold);
    printMutex.lock();
    defer printMutex.unlock();
    const w = std.io.getStdOut().writer();
    if (builtin.is_test) {
        if (@intFromEnum(std.log.Level.debug) <= @intFromEnum(std.testing.log_level)) {
            try format(w, fmt, vals);
        }
    } else {
        try format(w, fmt, vals);
    }
}

pub fn printDeprecated(name: []const u8, sinceVersion: []const u8, fmt: []const u8, vals: []const FmtValue) void {
    printStderr("{} is deprecated since {}: ", &.{ v(name), v(sinceVersion) });
    printStderr(fmt, vals);
    printStderr("\n", &.{});
}

pub fn print(w: *std.Io.Writer, fmt: []const u8, vals: []const FmtValue) void {
    format(w, fmt, vals) catch |err| {
        log.tracev("{}", .{err});
        cy.fatal();
    };
}

pub fn printStderr(fmt: []const u8, vals: []const FmtValue) void {
    printStderrOrErr(fmt, vals) catch |err| {
        log.tracev("{}", .{err});
        cy.fatal();
    };
}

pub const StderrWriter = if (!cy.isWasmFreestanding) std.fs.File.Writer else HostFileWriter;

pub fn lockStderrWriter() StderrWriter {
    if (!cy.isWasmFreestanding) {
        printMutex.lock();
        return std.fs.File.stderr().writer(&.{});
    } else {
        return HostFileWriter{ .fid = 2 };
    }
}

pub fn unlockPrint() void {
    if (!cy.isWasmFreestanding) {
        printMutex.unlock();
    }
}

pub fn printStderrOrErr(fmt: []const u8, vals: []const FmtValue) !void {
    @branchHint(.cold);
    var w = lockStderrWriter();
    defer unlockPrint();
    try format(&w.interface, fmt, vals);
}

const HostFileWriter = struct {
    fid: u32,

    pub const Error = error{};

    pub fn writeByte(self: HostFileWriter, byte: u8) Error!void {
        os.hostFileWrite(self.fid, @ptrCast(&byte), 1);
    }

    pub fn writeAll(self: HostFileWriter, data: []const u8) Error!void {
        os.hostFileWrite(self.fid, data.ptr, data.len);
    }

    pub fn write(self: HostFileWriter, data: []const u8) Error!u64 {
        os.hostFileWrite(self.fid, data.ptr, data.len);
        return data.len;
    }

    pub fn writeByteNTimes(self: HostFileWriter, byte: u8, n: usize) Error!void {
        var bytes: [256]u8 = undefined;
        @memset(bytes[0..], byte);

        var remaining = n;
        while (remaining > 0) {
            const to_write = @min(remaining, bytes.len);
            try self.writeAll(bytes[0..to_write]);
            remaining -= to_write;
        }
    }

    pub fn writeBytesNTimes(self: HostFileWriter, bytes: []const u8, n: usize) anyerror!void {
        var i: usize = 0;
        while (i < n) : (i += 1) {
            try self.writeAll(bytes);
        }
    }
};

pub const Repeat = struct {
    s: []const u8,
    count: usize,

    pub fn format(
        self: Repeat,
        writer: *std.Io.Writer,
    ) !void {
        for (0..self.count) |_| {
            try writer.writeAll(self.s);
        }
    }
};
