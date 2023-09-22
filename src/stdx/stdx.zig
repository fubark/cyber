// Copied only what is used from the cosmic project.
// TODO: Remove this package.

pub const testing = @import("testing.zig");

pub const time = @import("time.zig");
pub const heap = @import("heap.zig");

pub const stack = @import("ds/stack.zig");
pub const Stack = stack.Stack;

pub const debug = @import("debug.zig");