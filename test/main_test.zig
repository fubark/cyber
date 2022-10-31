const std = @import("std");
const t = std.testing;

test {
    const cyber = @import("../src/cyber.zig");
    t.refAllDecls(cyber);

    _ = @import("behavior_test.zig");
} 