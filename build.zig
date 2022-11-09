const std = @import("std");
const cy_config = @import("src/config.zig");
const mimalloc = @import("lib/mimalloc/lib.zig");

const Options = struct {
    linkMimalloc: bool = false,
};

pub fn build(b: *std.build.Builder) !void {
    const target = b.standardTargetOptions(.{});
    const mode = b.standardReleaseOptions();

    {
        const exe = b.addExecutable("cyber", "src/main.zig");
        exe.setBuildMode(mode);
        if (mode == .ReleaseSafe) {
            exe.strip = true;
        }
        exe.setTarget(target);
        exe.setOutputDir("zig-out/cyber");

        // exe.linkLibC();
        exe.addPackage(stdxPkg);
        mimalloc.addPackage(exe);
        mimalloc.buildAndLink(exe, .{});

        b.step("cli", "Build main cli.").dependOn(&exe.step);
    }

    {
        const step = b.addTest("./test/main_test.zig");
        step.setBuildMode(mode);
        step.setTarget(target);
        step.setMainPkgPath(".");

        const build_options = b.addOptions();
        build_options.addOption(cy_config.Engine, "cyEngine", .vm);
        step.addPackage(build_options.getPackage("build_options"));

        step.addPackage(stdxPkg);

        b.step("test", "Run tests.").dependOn(&step.step);
    }
}

const stdxPkg = std.build.Pkg{
    .name = "stdx",
    .source = .{ .path = srcPath() ++ "/stdx/stdx.zig" },
};

fn srcPath() []const u8 {
    return std.fs.path.dirname(@src().file) orelse unreachable;
}