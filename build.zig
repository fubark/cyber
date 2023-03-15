const std = @import("std");
const cy_config = @import("src/config.zig");
const mimalloc = @import("lib/mimalloc/lib.zig");
const tcc = @import("lib/tcc/lib.zig");

// FIND: v0.2
const Version = "0.2";

const Options = struct {
    linkMimalloc: bool = false,
};

var stdx: *std.build.Module = undefined;
var fastArm64: bool = undefined;

pub fn build(b: *std.build.Builder) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const selinux = b.option(bool, "selinux", "Whether you are building on linux distro with selinux. eg. Fedora.") orelse false;
    fastArm64 = b.option(bool, "fast-arm64", "Experimental: Computed gotos for arm64.") orelse false;
    const testFilter = b.option([]const u8, "test-filter", "Test filter.");

    stdx = b.createModule(.{
        .source_file = .{ .path = srcPath() ++ "/src/stdx/stdx.zig" },
    });

    {
        const exe = b.addExecutable(.{
            .name = "cyber",
            .root_source_file = .{ .path = "src/main.zig" },
            .target = target,
            .optimize = optimize,
        });
        if (exe.optimize != .Debug) {
            exe.strip = true;
        }
        exe.setOutputDir("zig-out/cyber");

        // Allow dynamic libraries to be loaded by filename in the cwd.
        exe.addRPath(".");

        // Allow exported symbols in exe to be visible to dlopen.
        exe.rdynamic = true;

        try addBuildOptions(b, exe, false);
        // exe.emit_asm = .emit;

        // exe.linkLibC();
        exe.addModule("stdx", stdx);
        mimalloc.addModule(exe);
        mimalloc.buildAndLink(exe, .{});
        tcc.addModule(exe);
        tcc.buildAndLink(exe, .{
            .selinux = selinux,
        });
        b.step("cli", "Build main cli.").dependOn(&exe.step);
    }

    {
        const lib = b.addSharedLibrary(.{
            .name = "cyber",
            .root_source_file = .{ .path = "src/lib.zig" },
            .target = target,
            .optimize = optimize,
        });
        if (lib.optimize != .Debug) {
            lib.strip = true;
        }
        lib.setOutputDir("zig-out/lib");
        lib.addIncludePath(srcPath() ++ "/src");

        // Allow dynamic libraries to be loaded by filename in the cwd.
        // lib.addRPath(".");

        // Allow exported symbols to be visible to dlopen.
        // Also needed to export symbols in wasm lib.
        lib.rdynamic = true;

        try addBuildOptions(b, lib, false);

        // lib.linkLibC();
        lib.addModule("stdx", stdx);
        mimalloc.addModule(lib);
        mimalloc.buildAndLink(lib, .{});

        if (!target.getCpuArch().isWasm()) {
            tcc.addModule(lib);
            tcc.buildAndLink(lib, .{
                .selinux = selinux,
            });
        } else {
            lib.addAnonymousModule("tcc", .{
                .source_file = .{ .path = srcPath() ++ "/src/nopkg.zig" },
            });
            // Disable stack protector since the compiler isn't linking __stack_chk_guard/__stack_chk_fail.
            lib.stack_protector = false;
        }
        b.step("lib", "Build as a library.").dependOn(&lib.step);
    }

    {
        const lib = b.addSharedLibrary(.{
            .name = "test",
            .root_source_file = .{ .path = "test/wasm_test.zig" },
            .target = target,
            .optimize = optimize,
        });
        if (lib.optimize != .Debug) {
            lib.strip = true;
        }
        lib.setOutputDir("zig-out/test");
        lib.setMainPkgPath(".");
        lib.addIncludePath(srcPath() ++ "/src");

        // Allow dynamic libraries to be loaded by filename in the cwd.
        // lib.addRPath(".");

        // Allow exported symbols to be visible to dlopen.
        // Also needed to export symbols in wasm lib.
        lib.rdynamic = true;

        try addBuildOptions(b, lib, false);

        // lib.linkLibC();
        lib.addModule("stdx", stdx);
        mimalloc.addModule(lib);
        mimalloc.buildAndLink(lib, .{});

        if (!target.getCpuArch().isWasm()) {
            tcc.addModule(lib);
            tcc.buildAndLink(lib, .{
                .selinux = selinux,
            });
        } else {
            lib.addAnonymousModule("tcc", .{
                .source_file = .{ .path = srcPath() ++ "/src/nopkg.zig" },
            });
            // Disable stack protector since the compiler isn't linking __stack_chk_guard/__stack_chk_fail.
            lib.stack_protector = false;
        }
        b.step("wasm-test", "Build the wasm test runner.").dependOn(&lib.step);
    }

    {
        const step = b.addTest(.{
            .root_source_file = .{ .path = "./test/main_test.zig" },
            .target = target,
            .optimize = optimize,
        });
        step.setMainPkgPath(".");
        step.setFilter(testFilter);

        try addBuildOptions(b, step, false);
        step.addModule("stdx", stdx);
        step.rdynamic = true;

        tcc.addModule(step);
        tcc.buildAndLink(step, .{
            .selinux = selinux,
        });

        const traceTest = try addTraceTest(b, optimize, target, .{
            .selinux = selinux,
        });
        traceTest.step.dependOn(&step.step);
        b.step("test", "Run tests.").dependOn(&traceTest.step);
    }

    {
        const step = b.addTest(.{
            .root_source_file = .{ .path = "./test/lib_test.zig" },
            .target = target,
            .optimize = optimize,
        });
        step.setMainPkgPath(".");
        step.addIncludePath(srcPath() ++ "/src");
        step.setFilter(testFilter);

        try addBuildOptions(b, step, false);
        step.addModule("stdx", stdx);
        step.rdynamic = true;

        tcc.addModule(step);
        tcc.buildAndLink(step, .{
            .selinux = selinux,
        });

        const traceTest = try addTraceTest(b, optimize, target, .{
            .selinux = selinux,
        });
        traceTest.step.dependOn(&step.step);
        b.step("test-lib", "Run tests.").dependOn(&traceTest.step);
    }

    {
        // Just trace test.
        const traceTest = try addTraceTest(b, optimize, target, .{
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

fn addBuildOptions(b: *std.build.Builder, step: *std.build.LibExeObjStep, trace: bool) !void {
    const buildTag = std.process.getEnvVarOwned(b.allocator, "BUILD") catch |err| b: {
        if (err == error.EnvironmentVariableNotFound) {
            break :b "local";
        } else {
            return err;
        }
    };
    const commitTag = std.process.getEnvVarOwned(b.allocator, "COMMIT") catch |err| b: {
        if (err == error.EnvironmentVariableNotFound) {
            break :b "local";
        } else {
            return err;
        }
    };
    const build_options = b.addOptions();
    build_options.addOption([]const u8, "version", Version);
    build_options.addOption([]const u8, "build", buildTag);
    build_options.addOption([]const u8, "commit", commitTag);
    build_options.addOption(cy_config.Engine, "cyEngine", .vm);
    build_options.addOption(bool, "trace", trace);
    build_options.addOption([]const u8, "full_version", b.fmt("Cyber {s} build-{s}-{s}", .{Version, buildTag, commitTag}));
    build_options.addOption(bool, "fastArm64",
        step.target.getCpuArch() == .aarch64 and step.optimize != .Debug and fastArm64);
    // build_options.addOption(bool, "trace", true);

    step.addOptions("build_options", build_options);
}

fn addTraceTest(b: *std.build.Builder, optimize: std.builtin.OptimizeMode, target: std.zig.CrossTarget, config: Config) !*std.build.LibExeObjStep {
    const step = b.addTest(.{
        .root_source_file = .{ .path = "./test/trace_test.zig" },
        .optimize = optimize,
        .target = target,
    });
    step.setMainPkgPath(".");
    try addBuildOptions(b, step, true);
    step.addModule("stdx", stdx);

    tcc.addModule(step);
    tcc.buildAndLink(step, .{
        .selinux = config.selinux,
    });
    return step;
}

const stdxPkg = std.build.Pkg{
    .name = "stdx",
    .source = .{ .path = srcPath() ++ "/src/stdx/stdx.zig" },
};

inline fn srcPath() []const u8 {
    return comptime std.fs.path.dirname(@src().file) orelse unreachable;
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
