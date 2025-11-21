const std = @import("std");

pub fn createModule(b: *std.Build) *std.Build.Module {
    const mod = b.createModule(.{
        .root_source_file = b.path("lib/mimalloc/mimalloc.zig"),
    });
    mod.addIncludePath(b.path("lib/mimalloc/vendor/include"));
    return mod;
}

const BuildOptions = struct {
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
};

pub fn buildAndLink(b: *std.Build, mod: *std.Build.Module, opts: BuildOptions) !void {
    const root = b.createModule(.{
        .target = opts.target,
        .optimize = opts.optimize,
    });
    const lib = b.addLibrary(.{
        .linkage = .static,
        .name = "mimalloc",
        .root_module = root,
    });
    lib.addIncludePath(b.path("lib/mimalloc/vendor/include"));
    lib.linkLibC();
    // lib.disable_sanitize_c = true;

    var c_flags = try std.ArrayList([]const u8).initCapacity(b.allocator, 0);
    // c_flags.append("-D_GNU_SOURCE=1") catch @panic("error");
    if (opts.target.result.os.tag == .windows) {} else if (opts.target.result.os.tag == .macos) {
        if (opts.target.result.cpu.arch == .aarch64) {
            lib.addSystemIncludePath(.{ .cwd_relative = "/Applications/Xcode_15.0.1.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include" });
        } else {
            // Github macos-12 runner (https://github.com/actions/runner-images/blob/main/images/macos/macos-12-Readme.md).
            lib.addSystemIncludePath(.{ .cwd_relative = "/Applications/Xcode_14.0.1.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include" });
        }
        lib.addSystemIncludePath(.{ .cwd_relative = "/Library/Developer/CommandLineTools/SDKs/MacOSX12.1.sdk/usr/include" });
        lib.addSystemIncludePath(.{ .cwd_relative = "/Library/Developer/CommandLineTools/SDKs/MacOSX12.3.sdk/usr/include" });
    }
    if (opts.optimize == .Debug) {
        // For debugging:
        // c_flags.append("-O0") catch @panic("error");
        // if (step.target.getCpuArch().isWasm()) {
        //     // Compile with some optimization or number of function locals will exceed max limit in browsers.
        //     c_flags.append("-O1") catch @panic("error");
        // }
    } else {
        try c_flags.append(b.allocator, "-DNDEBUG=1");
        try c_flags.append(b.allocator, "-DMI_SECURE=0");

        // Use with MIMALLOC_SHOW_STATS=1 env variable to show stats summary.
        // MI_STAT=2 includes bucket info.
        // c_flags.append("-DMI_STAT=2") catch @panic("error");
        try c_flags.append(b.allocator, "-DMI_STAT=0");
    }

    if (opts.target.result.os.tag == .windows) {
        mod.linkSystemLibrary("bcrypt", .{});
    }

    var sources = try std.ArrayList([]const u8).initCapacity(b.allocator, 0);
    try sources.appendSlice(b.allocator, &.{
        "lib/mimalloc/vendor/src/alloc.c",
        "lib/mimalloc/vendor/src/alloc-aligned.c",
        "lib/mimalloc/vendor/src/page.c",
        "lib/mimalloc/vendor/src/heap.c",
        "lib/mimalloc/vendor/src/random.c",
        "lib/mimalloc/vendor/src/options.c",
        "lib/mimalloc/vendor/src/bitmap.c",
        "lib/mimalloc/vendor/src/os.c",
        "lib/mimalloc/vendor/src/init.c",
        "lib/mimalloc/vendor/src/segment.c",
        "lib/mimalloc/vendor/src/segment-map.c",
        "lib/mimalloc/vendor/src/arena.c",
        "lib/mimalloc/vendor/src/stats.c",
        "lib/mimalloc/vendor/src/prim/prim.c",
        "lib/mimalloc/vendor/src/libc.c",
    });
    for (sources.items) |src| {
        lib.addCSourceFile(.{
            .file = b.path(src),
            .flags = c_flags.items,
        });
    }
    if (opts.target.result.os.tag != .windows) {
        lib.addCSourceFile(.{
            .file = b.path("lib/mimalloc/vendor/src/alloc-posix.c"),
            .flags = c_flags.items,
        });
    }
    mod.linkLibrary(lib);
}

inline fn thisDir() []const u8 {
    return comptime std.fs.path.dirname(@src().file) orelse @panic("error");
}
