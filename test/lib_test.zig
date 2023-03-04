const std = @import("std");
const t = std.testing;

// Same as main_test.zig including the tests in lib.zig.

test {
    const cyber = @import("../src/cyber.zig");
    t.refAllDecls(cyber);

    const vm = @import("../src/vm.zig");
    t.refAllDecls(vm);

    _ = @import("behavior_test.zig");
    
    _ = @import("../src/lib.zig");
} 