const std = @import("std");
const builtin = @import("builtin");
const expectEqual = std.testing.expectEqual;

pub const alloc = std.testing.allocator;

pub fn expect(pred: bool) !void {
    try std.testing.expect(pred);
}

pub fn expectError(act_err_union: anytype, exp: anyerror) !void {
    return std.testing.expectError(exp, act_err_union);
}

pub fn eq(act: anytype, exp: @TypeOf(act)) !void {
    try std.testing.expectEqual(exp, act);
}

// Currently zig doesn't let you use @TypeOf on a arg to the right. We'll want this when it does.
// pub fn eq(act: @TypeOf(exp), exp: anytype) !void {
//     try std.testing.expectEqual(exp, act);
// }

pub fn eqT(comptime T: type, act: T, exp: T) !void {
    try std.testing.expectEqual(exp, act);
}

pub fn neq(act: anytype, exp: @TypeOf(act)) !void {
    // TODO: expectEqual still prints debug messages when not equal.
    std.testing.expectEqual(exp, act) catch return;
    return error.Equal;
}

test "neq" {
    neq(1, 1) catch {};
    try neq(1, 2);
}

pub fn eqApprox(act: anytype, exp: @TypeOf(act), tolerance: @TypeOf(act)) !void {
    try std.testing.expectApproxEqAbs(exp, act, tolerance);
}

pub fn eqApproxEps(act: anytype, exp: @TypeOf(act)) !void {
    const eps = comptime std.math.epsilon(@TypeOf(exp));
    try eqApprox(act, exp, eps);
}

// Format using decimal.
pub fn eqApproxDec(act: anytype, exp: @TypeOf(act), tolerance: @TypeOf(act)) !void {
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
        try eq(@enumToInt(act), @enumToInt(@as(@typeInfo(@TypeOf(act)).Union.tag_type.?, exp)));
    } else {
        unreachable;
    }
}

pub fn eqStr(act: []const u8, exp: []const u8) !void {
    try std.testing.expectEqualStrings(exp, act);
}

pub fn eqSlice(comptime T: type, act: []const T, exp: []const T) !void {
    try std.testing.expectEqualSlices(T, exp, act);
}

pub fn eqStringSlices(act: []const []const u8, exp: []const []const u8) !void {
    const S = struct {
        fn check(act_: []const u8, exp_: []const u8) !void {
            try eqSlice(u8, act_, exp_);
        }
    };
    try eqSliceCb([]const u8, S.check, act, exp);
}

pub fn eqSliceCb(comptime T: type, cb: fn (act: T, exp: T) anyerror!void, act_slice: []const T, exp_slice: []const T) !void {
    try eq(act_slice.len, exp_slice.len);
    for (act_slice) |act, i| {
        cb(act, exp_slice[i]) catch |err| {
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

var mocks: std.ArrayList(*Mock) = std.ArrayList(*Mock).init(
    if (builtin.is_test) alloc else std.heap.page_allocator,
);

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
        for (mocks.items) |it, i| {
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