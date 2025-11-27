const std = @import("std");
const builtin = @import("builtin");
const expectEqual = std.testing.expectEqual;

pub const alloc = std.testing.allocator;

pub const expect = std.testing.expect;
pub const expect_error = std.testing.expectError;
pub const eq = std.testing.expectEqual;

pub fn eqT(comptime T: type, exp: T, act: T) !void {
    try std.testing.expectEqual(exp, act);
}

pub fn neq(exp: anytype, act: @TypeOf(exp)) !void {
    // TODO: expectEqual still prints debug messages when not equal.
    std.testing.expectEqual(exp, act) catch return;
    return error.Equal;
}

test "neq" {
    neq(1, 1) catch {};
    try neq(1, 2);
}

pub fn eqApprox(exp: anytype, act: @TypeOf(exp), tolerance: @TypeOf(exp)) !void {
    try std.testing.expectApproxEqAbs(exp, act, tolerance);
}

pub fn eqApproxEps(exp: anytype, act: @TypeOf(exp)) !void {
    const eps = comptime std.math.epsilon(@TypeOf(exp));
    try eqApprox(act, exp, eps);
}

// Format using decimal.
pub fn eqApproxDec(exp: anytype, act: @TypeOf(exp), tolerance: @TypeOf(exp)) !void {
    const T = @TypeOf(exp);
    switch (@typeInfo(T)) {
        .Float => if (!std.math.approxEqAbs(T, exp, act, tolerance)) {
            std.debug.print("actual {d}, not within absolute tolerance {d:.3} of expected {d}\n", .{ act, tolerance, exp });
            return error.TestExpectedApproxEqAbs;
        },
        .ComptimeFloat => @compileError("Cannot approximately compare two comptime_float values"),
        else => @compileError("Unable to compare non floating point values"),
    }
}

pub fn eqUnionEnum(act: anytype, exp: @Type(.EnumLiteral)) !void {
    if (@typeInfo(@TypeOf(act)) == .Union) {
        try eq(@intFromEnum(act), @intFromEnum(@as(@typeInfo(@TypeOf(act)).Union.tag_type.?, exp)));
    } else {
        unreachable;
    }
}

pub fn eqStrFree(alloc_: std.mem.Allocator, exp: []const u8, act: []const u8) !void {
    defer alloc_.free(act);
    try std.testing.expectEqualStrings(exp, act);
}

pub const eqStr = std.testing.expectEqualStrings;
pub const eqSlice = std.testing.expectEqualSlices;

pub fn eqStringSlices(exp: []const []const u8, act: []const []const u8) !void {
    const S = struct {
        fn check(exp_: []const u8, act_: []const u8) !void {
            try eqSlice(u8, exp_, act_);
        }
    };
    try eqSliceCb([]const u8, S.check, exp, act);
}

pub fn eqSliceCb(comptime T: type, cb: fn (exp: T, act: T) anyerror!void, exp_slice: []const T, act_slice: []const T) !void {
    try eq(exp_slice.len, act_slice.len);
    for (act_slice, 0..) |act, i| {
        cb(exp_slice[i], act) catch |err| {
            std.debug.print("expected {any}, found {any} at idx {}\n", .{ exp_slice[i], act, i });
            return err;
        };
    }
}

pub fn fail() !void {
    return error.Fail;
}

pub fn setLogLevel(level: std.log.Level) void {
    std.testing.log_level = level;
}

var mocks: std.ArrayList(*Mock) = std.ArrayList(*Mock).initCapacity(
    if (builtin.is_test) alloc else std.heap.page_allocator, 0,
) catch @panic("error");

pub fn unitTrace(loc: std.builtin.SourceLocation) void {
    for (mocks.items) |mock| {
        if (mock.funcs.getEntry(loc.fn_name)) |entry| {
            entry.value_ptr.call_count += 1;
        }
    }
}

pub export fn unitTraceC(fn_name_ptr: [*]const u8, fn_name_len: usize) void {
    const fn_name = fn_name_ptr[0..fn_name_len];
    for (mocks.items) |mock| {
        if (mock.funcs.getEntry(fn_name)) |entry| {
            entry.value_ptr.call_count += 1;
        }
    }
}

pub const Mock = struct {
    const Self = @This();

    funcs: std.StringHashMap(FuncMock),

    pub fn create() *Self {
        const m = alloc.create(Mock) catch unreachable;
        m.* = .{
            .funcs = std.StringHashMap(FuncMock).init(alloc),
        };
        mocks.append(m) catch unreachable;
        return m;
    }

    pub fn destroy(self: *Self) void {
        self.funcs.deinit();
        alloc.destroy(self);
        for (mocks.items, 0..) |it, i| {
            if (it == self) {
                _ = mocks.orderedRemove(i);
                if (mocks.items.len == 0) {
                    mocks.clearAndFree();
                }
                break;
            }
        }
    }

    pub fn add(self: *Self, comptime E: @Type(.EnumLiteral)) void {
        std.debug.print("add {s}\n", .{@tagName(E)});
        std.debug.assert(!self.funcs.contains(@tagName(E)));
        self.funcs.put(@tagName(E), .{
            .call_count = 0,
        }) catch unreachable;
    }

    pub fn getNumCalls(self: Self, comptime E: @Type(.EnumLiteral)) u32 {
        return self.funcs.get(@tagName(E)).?.call_count;
    }
};

const FuncMock = struct {
    call_count: u32,
};