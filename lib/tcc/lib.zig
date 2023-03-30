const std = @import("std");

pub fn createModule(b: *std.Build) *std.build.Module {
    return b.createModule(.{
        .source_file = .{ .path = thisDir() ++ "/tcc.zig" },
    });
}

pub fn addModule(step: *std.build.CompileStep, name: []const u8, mod: *std.build.Module) void {
    step.addModule(name, mod);
    step.addIncludePath(thisDir() ++ "/vendor");
}

const BuildOptions = struct {
    selinux: bool = false,
};

pub fn buildAndLink(b: *std.Build, step: *std.build.CompileStep, opts: BuildOptions) void {
    const lib = b.addStaticLibrary(.{
        .name = "tcc",
        .target = step.target,
        .optimize = step.optimize,
    });
    lib.addIncludePath(thisDir() ++ "/vendor");
    lib.linkLibC();
    lib.disable_sanitize_c = true;

    var c_flags = std.ArrayList([]const u8).init(b.allocator);
    if (opts.selinux) {
        c_flags.append("-DHAVE_SELINUX=1") catch @panic("error");
    }
    // c_flags.append("-D_GNU_SOURCE=1") catch @panic("error");
    if (lib.target.getOsTag() == .windows) {
    }
    if (lib.optimize == .Debug) {
        // For debugging:
        // c_flags.append("-O0") catch @panic("error");
    }

    var sources = std.ArrayList([]const u8).init(b.allocator);
    sources.appendSlice(&.{
        "/vendor/libtcc.c",
        // "/vendor/lib/libtcc1.c",
    }) catch @panic("error");
    for (sources.items) |src| {
        lib.addCSourceFile(b.fmt("{s}{s}", .{thisDir(), src}), c_flags.items);
    }
    step.linkLibrary(lib);
}

inline fn thisDir() []const u8 {
    return comptime std.fs.path.dirname(@src().file) orelse @panic("error");
}