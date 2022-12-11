const std = @import("std");
const cy_config = @import("src/config.zig");
const mimalloc = @import("lib/mimalloc/lib.zig");
const tcc = @import("lib/tcc/lib.zig");

const Options = struct {
    linkMimalloc: bool = false,
};

pub fn build(b: *std.build.Builder) !void {
    const target = b.standardTargetOptions(.{});
    const mode = b.standardReleaseOptions();

    {
        const exe = b.addExecutable("cyber", "src/main.zig");
        exe.setBuildMode(mode);
        if (mode != .Debug) {
            exe.strip = true;
        }
        exe.setTarget(target);
        exe.setOutputDir("zig-out/cyber");

        // Allow exported symbols in exe to be visible to dlopen.
        exe.rdynamic = true;

        const build_options = b.addOptions();
        build_options.addOption(bool, "trace", false);
        // build_options.addOption(bool, "trace", true);
        exe.addPackage(build_options.getPackage("build_options"));

        // exe.linkLibC();
        exe.addPackage(stdxPkg);
        mimalloc.addPackage(exe);
        mimalloc.buildAndLink(exe, .{});
        tcc.addPackage(exe);
        tcc.buildAndLink(exe, .{});

        b.step("cli", "Build main cli.").dependOn(&exe.step);
    }

    {
        const step = b.addTest("./test/main_test.zig");
        step.setBuildMode(mode);
        step.setTarget(target);
        step.setMainPkgPath(".");
        {
            const build_options = b.addOptions();
            build_options.addOption(cy_config.Engine, "cyEngine", .vm);
            build_options.addOption(bool, "trace", false);
            step.addPackage(build_options.getPackage("build_options"));
        }
        step.addPackage(stdxPkg);
        step.rdynamic = true;

        tcc.addPackage(step);
        tcc.buildAndLink(step, .{});

        const traceTest = addTraceTest(b, mode, target);
        traceTest.step.dependOn(&step.step);
        b.step("test", "Run tests.").dependOn(&traceTest.step);
    }

    {
        // Just trace test.
        const traceTest = addTraceTest(b, mode, target);
        b.step("test-trace", "Run trace tests.").dependOn(&traceTest.step);
    }
}

fn addTraceTest(b: *std.build.Builder, mode: std.builtin.Mode, target: std.zig.CrossTarget) *std.build.LibExeObjStep {
    const step = b.addTest("./test/trace_test.zig");
    step.setBuildMode(mode);
    step.setTarget(target);
    step.setMainPkgPath(".");
    {
        const build_options = b.addOptions();
        build_options.addOption(cy_config.Engine, "cyEngine", .vm);
        build_options.addOption(bool, "trace", true);
        step.addPackage(build_options.getPackage("build_options"));
    }
    step.addPackage(stdxPkg);

    tcc.addPackage(step);
    tcc.buildAndLink(step, .{});
    return step;
}

const stdxPkg = std.build.Pkg{
    .name = "stdx",
    .source = .{ .path = srcPath() ++ "/stdx/stdx.zig" },
};

fn srcPath() []const u8 {
    return std.fs.path.dirname(@src().file) orelse unreachable;
}