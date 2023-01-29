const std = @import("std");

pub const pkg = std.build.Pkg{
    .name = "mimalloc",
    .source = .{ .path = srcPath() ++ "/mimalloc.zig" },
    .dependencies = &.{},
};

pub fn addPackage(step: *std.build.LibExeObjStep) void {
    var new_pkg = pkg;
    step.addPackage(new_pkg);
    step.addIncludePath(srcPath() ++ "/vendor/include");
}

const BuildOptions = struct {
};

pub fn buildAndLink(step: *std.build.LibExeObjStep, opts: BuildOptions) void {
    _ = opts;
    const b = step.builder;
    const lib = b.addStaticLibrary("mimalloc", null);
    lib.setTarget(step.target);
    lib.setBuildMode(step.build_mode);
    lib.addIncludePath(srcPath() ++ "/vendor/include");
    lib.linkLibC();
    // lib.disable_sanitize_c = true;

    var c_flags = std.ArrayList([]const u8).init(b.allocator);
    // c_flags.append("-D_GNU_SOURCE=1") catch @panic("error");
    if (step.target.getOsTag() == .windows) {
    } else if (step.target.getOsTag() == .macos) {
        // Github macos-12 runner (https://github.com/actions/runner-images/blob/main/images/macos/macos-12-Readme.md).
        lib.addSystemIncludePath("/Applications/Xcode_14.0.1.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include");
        lib.addSystemIncludePath("/Library/Developer/CommandLineTools/SDKs/MacOSX12.1.sdk/usr/include");
        lib.addSystemIncludePath("/Library/Developer/CommandLineTools/SDKs/MacOSX12.3.sdk/usr/include");
    }
    if (step.build_mode == .Debug) {
        // For debugging:
        // c_flags.append("-O0") catch @panic("error");
        // if (step.target.getCpuArch().isWasm()) {
        //     // Compile with some optimization or number of function locals will exceed max limit in browsers.
        //     c_flags.append("-O1") catch @panic("error");
        // }
    } else {
        c_flags.append("-DNDEBUG=1") catch @panic("error");
        c_flags.append("-DMI_SECURE=0") catch @panic("error");

        // Use with MIMALLOC_SHOW_STATS=1 env variable to show stats summary.
        // MI_STAT=2 includes bucket info.
        // c_flags.append("-DMI_STAT=2") catch @panic("error");
        c_flags.append("-DMI_STAT=0") catch @panic("error");
    }

    if (step.target.getOsTag() == .windows) {
        step.linkSystemLibrary("bcrypt");
    }

    var sources = std.ArrayList([]const u8).init(b.allocator);
    sources.appendSlice(&.{
        "/vendor/src/alloc.c",
        "/vendor/src/alloc-aligned.c",
        "/vendor/src/page.c",
        "/vendor/src/heap.c",
        "/vendor/src/random.c",
        "/vendor/src/segment-cache.c",
        "/vendor/src/options.c",
        "/vendor/src/bitmap.c",
        "/vendor/src/os.c",
        "/vendor/src/init.c",
        "/vendor/src/segment.c",
        "/vendor/src/arena.c",
        "/vendor/src/stats.c",
    }) catch @panic("error");
    for (sources.items) |src| {
        lib.addCSourceFile(b.fmt("{s}{s}", .{srcPath(), src}), c_flags.items);
    }
    step.linkLibrary(lib);
}

inline fn srcPath() []const u8 {
    return comptime std.fs.path.dirname(@src().file) orelse @panic("error");
}
