const std = @import("std");
const t = std.testing;

test {
    const cyber = @import("../src/cyber.zig");
    t.refAllDecls(cyber);

    const vm = @import("../src/vm.zig");
    t.refAllDecls(vm);

    _ = @import("behavior_test.zig");
    
    const lib = @import("../src/lib.zig");
    std.testing.refAllDecls(lib);
}