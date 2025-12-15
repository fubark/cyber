const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const stdx = @import("stdx");
const cy = @import("../cyber.zig");
const C = @import("../capi.zig");
const fmt = @import("../fmt.zig");
const bindings = @import("../builtins/bindings.zig");
const log = cy.log.scoped(.testmod);

const Src = @embedFile("test.cy");

const funcs = [_]struct{[]const u8, C.BindFunc}{
};

pub fn bind(_: *C.VM, mod: *C.Sym) callconv(.c) C.Bytes {
    for (funcs) |e| {
        C.mod_add_func(mod, e.@"0", e.@"1");
    }
    return C.to_bytes(Src);
}

comptime {
    @export(&bind, .{ .name = "cl_mod_bind_test", .linkage = .strong });
}