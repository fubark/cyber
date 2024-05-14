/// Used for behavior test.

pub const cy = @import("cyber.zig");
pub const cli = @import("cli.zig");

const build_options = @import("build_options");
const std = @import("std");

comptime {
    if (!build_options.link_test) {
        const lib = @import("lib.zig");
        for (std.meta.declarations(lib)) |decl| {
            _ = &@field(lib, decl.name);
        }
    }
}