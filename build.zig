const std = @import("std");
const cy_config = @import("src/config.zig");
const mimalloc = @import("lib/mimalloc/lib.zig");
const tcc = @import("lib/tcc/lib.zig");

// FIND: v0.1
const Version = "0.1";

const Options = struct {
    linkMimalloc: bool = false,
};

pub fn build(b: *std.build.Builder) !void {
    const target = b.standardTargetOptions(.{});
    const mode = b.standardReleaseOptions();

    const selinux = b.option(bool, "selinux", "Whether you are building on linux distro with selinux. eg. Fedora.") orelse false;

    {
        const exe = b.addExecutable("cyber", "src/main.zig");
        exe.setBuildMode(mode);
        if (mode != .Debug) {
            exe.strip = true;
        }
        exe.setTarget(target);
        exe.setOutputDir("zig-out/cyber");

        // Allow dynamic libraries to be loaded by filename in the cwd.
        exe.addRPath(".");

        // Allow exported symbols in exe to be visible to dlopen.
        exe.rdynamic = true;

        const buildTag = std.os.getenv("BUILD") orelse "local";
        const commitTag = std.os.getenv("COMMIT") orelse "local";
        const build_options = b.addOptions();
        build_options.addOption([]const u8, "version", Version);
        build_options.addOption([]const u8, "build", buildTag);
        build_options.addOption([]const u8, "commit", commitTag);
        build_options.addOption(bool, "trace", false);
        // build_options.addOption(bool, "trace", true);
        exe.addPackage(build_options.getPackage("build_options"));

        // exe.linkLibC();
        exe.addPackage(stdxPkg);
        mimalloc.addPackage(exe);
        mimalloc.buildAndLink(exe, .{});
        tcc.addPackage(exe);
        tcc.buildAndLink(exe, .{
            .selinux = selinux,
        });
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
        tcc.buildAndLink(step, .{
            .selinux = selinux,
        });

        const traceTest = addTraceTest(b, mode, target, .{
            .selinux = selinux,
        });
        traceTest.step.dependOn(&step.step);
        b.step("test", "Run tests.").dependOn(&traceTest.step);
    }

    {
        // Just trace test.
        const traceTest = addTraceTest(b, mode, target, .{
            .selinux = selinux,
        });
        b.step("test-trace", "Run trace tests.").dependOn(&traceTest.step);
    }

    const printStep = b.allocator.create(PrintStep) catch unreachable;
    printStep.* = PrintStep.init(b, Version);
    b.step("version", "Get the short version.").dependOn(&printStep.step);
}

const Config = struct {
    selinux: bool = false,
};

fn addTraceTest(b: *std.build.Builder, mode: std.builtin.Mode, target: std.zig.CrossTarget, config: Config) *std.build.LibExeObjStep {
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
    tcc.buildAndLink(step, .{
        .selinux = config.selinux,
    });
    return step;
}

const stdxPkg = std.build.Pkg{
    .name = "stdx",
    .source = .{ .path = srcPath() ++ "/stdx/stdx.zig" },
};

fn srcPath() []const u8 {
    return std.fs.path.dirname(@src().file) orelse unreachable;
}

pub const PrintStep = struct {
    step: std.build.Step,
    builder: *std.build.Builder,
    str: []const u8,

    pub fn init(builder: *std.build.Builder, str: []const u8) PrintStep {
        return PrintStep{
            .builder = builder,
            .step = std.build.Step.init(.custom, "print", builder.allocator, make),
            .str = builder.dupe(str),
        };
    }

    fn make(step: *std.build.Step) anyerror!void {
        const self = @fieldParentPtr(PrintStep, "step", step);
        std.io.getStdOut().writer().writeAll(self.str) catch unreachable;
    }
};