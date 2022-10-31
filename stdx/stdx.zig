// Copied only what is used from the cosmic project.

pub const testing = @import("testing.zig");

pub const log = @import("log.zig");
pub const time = @import("time.zig");

pub const stack = @import("ds/stack.zig");
pub const Stack = stack.Stack;

const utils = @import("utils.zig");
pub const IndexSlice = utils.IndexSlice;
pub const ptrCastAlign = utils.ptrCastAlign;

pub const debug = @import("debug.zig");
pub const panic = debug.panic;
pub const panicFmt = debug.panicFmt;

pub inline fn unsupported() noreturn {
    panic("unsupported");
}

pub inline fn fatal() noreturn {
    panic("error");
}