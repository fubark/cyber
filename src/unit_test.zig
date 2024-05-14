/// Used for unit testing.
const cy = @import("cyber.zig");
const cli = @import("cli.zig");
const lib = @import("lib.zig");

const std = @import("std");
const t = std.testing;

test {
    t.refAllDecls(cy);
    t.refAllDecls(lib);
    t.refAllDecls(cli);
}