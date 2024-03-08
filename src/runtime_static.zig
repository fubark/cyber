const std = @import("std");
const cb = @import("cyber.zig");

comptime {
    // Export std.
    const mtest = @import("std/test.zig");
    @export(mtest.eq_c, .{ .name = "cb_test_eq", .linkage = .Strong });

    const m = @import("builtins/builtins.zig");
    @export(m.print_c, .{ .name = "cb_print", .linkage = .Strong });
}