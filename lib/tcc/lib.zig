const std = @import("std");

pub const pkg = std.build.Pkg{
    .name = "tcc",
    .source = .{ .path = srcPath() ++ "/tcc.zig" },
    .dependencies = &.{},
};

pub fn addPackage(step: *std.build.LibExeObjStep) void {
    var new_pkg = pkg;
    step.addPackage(new_pkg);
    step.addIncludePath(srcPath() ++ "/vendor");
}

const BuildOptions = struct {
};

pub fn buildAndLink(step: *std.build.LibExeObjStep, opts: BuildOptions) void {
    _ = opts;
    const b = step.builder;
    const lib = b.addStaticLibrary("mimalloc", null);
    lib.setTarget(step.target);
    lib.setBuildMode(step.build_mode);
    lib.addIncludePath(srcPath() ++ "/vendor");
    lib.linkLibC();
    lib.disable_sanitize_c = true;

    var c_flags = std.ArrayList([]const u8).init(b.allocator);
    // c_flags.append("-D_GNU_SOURCE=1") catch @panic("error");
    if (step.target.getOsTag() == .windows) {
    }
    if (step.build_mode == .Debug) {
        // For debugging:
        // c_flags.append("-O0") catch @panic("error");
    }

    var sources = std.ArrayList([]const u8).init(b.allocator);
    sources.appendSlice(&.{
        "/vendor/libtcc.c",
        // "/vendor/lib/libtcc1.c",
    }) catch @panic("error");
    for (sources.items) |src| {
        lib.addCSourceFile(b.fmt("{s}{s}", .{srcPath(), src}), c_flags.items);
    }
    step.linkLibrary(lib);
}

inline fn srcPath() []const u8 {
    return comptime std.fs.path.dirname(@src().file) orelse @panic("error");
}