const std = @import("std");

pub fn createModule(b: *std.Build, rtarget: std.Build.ResolvedTarget, optimize: std.builtin.OptimizeMode) *std.Build.Module {
    const tcc = b.addTranslateC(.{
        .root_source_file = b.path("lib/tcc/vendor/libtcc.h"),
        .target = rtarget,
        .optimize = optimize,
    });
    const mod = b.createModule(.{
        .root_source_file = tcc.getOutput(),
    });
    mod.addIncludePath(b.path("lib/tcc/vendor"));
    return mod;
}

pub fn addImport(root: *std.Build.Module, name: []const u8, mod: *std.Build.Module) void {
    root.addImport(name, mod);
    root.addIncludePath(root.owner.path("lib/tcc/vendor"));
}

const BuildOptions = struct {
    selinux: bool = false,
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
};

pub fn buildAndLink(b: *std.Build, mod: *std.Build.Module, opts: BuildOptions) !void {
    const root = b.createModule(.{
        .target = opts.target,
        .optimize = opts.optimize,
    });
    const obj = b.addObject(.{
        .name = "tcc",
        .root_module = root,
    });
    obj.addIncludePath(b.path("lib/tcc/vendor"));
    obj.linkLibC();
    obj.root_module.sanitize_c = .off;

    var c_flags = try std.ArrayList([]const u8).initCapacity(b.allocator, 0);
    if (opts.selinux) {
        try c_flags.append(b.allocator, "-DHAVE_SELINUX=1");
    }
    // c_flags.append("-D_GNU_SOURCE=1") catch @panic("error");
    if (opts.target.result.os.tag == .windows) {}
    if (opts.optimize == .Debug) {
        // For debugging:
        // c_flags.append("-O0") catch @panic("error");
    }

    var sources = try std.ArrayList([]const u8).initCapacity(b.allocator, 0);
    try sources.appendSlice(b.allocator, &.{
        "lib/tcc/vendor/libtcc.c",
        // "/vendor/lib/libtcc1.c",
    });

    for (sources.items) |src| {
        obj.addCSourceFile(.{
            .file = b.path(src),
            .flags = c_flags.items,
        });
    }
    mod.addObject(obj);
}

inline fn thisDir() []const u8 {
    return comptime std.fs.path.dirname(@src().file) orelse @panic("error");
}
