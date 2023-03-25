const std = @import("std");
const config = @import("src/config.zig");
const mimalloc = @import("lib/mimalloc/lib.zig");
const tcc = @import("lib/tcc/lib.zig");

// FIND: v0.2
const Version = "0.2";

var stdx: *std.build.Module = undefined;
var useMalloc: bool = undefined;
var selinux: bool = undefined;
var engine: config.Engine = .zig;
var testFilter: ?[]const u8 = undefined;

pub fn build(b: *std.build.Builder) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    selinux = b.option(bool, "selinux", "Whether you are building on linux distro with selinux. eg. Fedora.") orelse false;
    testFilter = b.option([]const u8, "test-filter", "Test filter.");
    useMalloc = b.option(bool, "use-malloc", "Use C allocator.") orelse false;

    const opts = getDefaultOptions(target, optimize);

    stdx = b.createModule(.{
        .source_file = .{ .path = thisDir() ++ "/src/stdx/stdx.zig" },
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
        exe.addIncludePath(thisDir() ++ "/src");
        exe.setOutputDir("zig-out/cyber");

        // Allow dynamic libraries to be loaded by filename in the cwd.
        exe.addRPath(".");

        // Allow exported symbols in exe to be visible to dlopen.
        exe.rdynamic = true;

        try addBuildOptions(b, exe, opts);
        if (engine == .c) {
            try buildCVM(exe, opts);
        }
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
        lib.addIncludePath(thisDir() ++ "/src");

        // Allow dynamic libraries to be loaded by filename in the cwd.
        // lib.addRPath(".");

        // Allow exported symbols to be visible to dlopen.
        // Also needed to export symbols in wasm lib.
        lib.rdynamic = true;

        try addBuildOptions(b, lib, opts);

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
                .source_file = .{ .path = thisDir() ++ "/src/nopkg.zig" },
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
        lib.addIncludePath(thisDir() ++ "/src");

        // Allow dynamic libraries to be loaded by filename in the cwd.
        // lib.addRPath(".");

        // Allow exported symbols to be visible to dlopen.
        // Also needed to export symbols in wasm lib.
        lib.rdynamic = true;

        try addBuildOptions(b, lib, opts);

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
                .source_file = .{ .path = thisDir() ++ "/src/nopkg.zig" },
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
        step.addIncludePath(thisDir() ++ "/src");

        try addBuildOptions(b, step, opts);
        step.addModule("stdx", stdx);
        step.rdynamic = true;

        if (engine == .c) {
            try buildCVM(step, opts);
        }

        tcc.addModule(step);
        tcc.buildAndLink(step, .{
            .selinux = opts.selinux,
        });

        const traceTest = try addTraceTest(b, opts);
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
        step.addIncludePath(thisDir() ++ "/src");
        step.setFilter(testFilter);

        try addBuildOptions(b, step, opts);
        step.addModule("stdx", stdx);
        step.rdynamic = true;

        tcc.addModule(step);
        tcc.buildAndLink(step, .{
            .selinux = opts.selinux,
        });

        const traceTest = try addTraceTest(b, opts);
        traceTest.step.dependOn(&step.step);
        b.step("test-lib", "Run tests.").dependOn(&traceTest.step);
    }

    {
        // Just trace test.
        const traceTest = try addTraceTest(b, opts);
        b.step("test-trace", "Run trace tests.").dependOn(&traceTest.step);
    }

    const printStep = b.allocator.create(PrintStep) catch unreachable;
    printStep.* = PrintStep.init(b, Version);
    b.step("version", "Get the short version.").dependOn(&printStep.step);
}

const Options = struct {
    selinux: bool,
    linkMimalloc: bool = false,
    trackGlobalRc: bool,
    trace: bool,
    target: std.zig.CrossTarget,
    optimize: std.builtin.OptimizeMode,
};

fn getDefaultOptions(target: std.zig.CrossTarget, optimize: std.builtin.OptimizeMode) Options {
    return .{
        .selinux = selinux,
        .trackGlobalRc = optimize != .ReleaseFast,
        .trace = false,
        .target = target,
        .optimize = optimize,
    };
}

fn addBuildOptions(b: *std.build.Builder, step: *std.build.LibExeObjStep, opts: Options) !void {
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
    build_options.addOption(bool, "useMalloc", useMalloc);
    build_options.addOption(config.Engine, "engine", engine);
    build_options.addOption(bool, "trace", opts.trace);
    build_options.addOption(bool, "trackGlobalRC", opts.trackGlobalRc);
    build_options.addOption([]const u8, "full_version", b.fmt("Cyber {s} build-{s}-{s}", .{Version, buildTag, commitTag}));
    // build_options.addOption(bool, "trace", true);

    step.addOptions("build_options", build_options);
}

fn addTraceTest(b: *std.build.Builder, opts: Options) !*std.build.LibExeObjStep {
    const step = b.addTest(.{
        .root_source_file = .{ .path = "./test/trace_test.zig" },
        .optimize = opts.optimize,
        .target = opts.target,
    });
    step.setFilter(testFilter);
    step.setMainPkgPath(".");
    step.addIncludePath(thisDir() ++ "/src");

    var newOpts = opts;
    newOpts.trace = true;
    try addBuildOptions(b, step, newOpts);
    step.addModule("stdx", stdx);

    tcc.addModule(step);
    tcc.buildAndLink(step, .{
        .selinux = opts.selinux,
    });
    if (engine == .c) {
        try buildCVM(step, newOpts);
    }
    return step;
}

const stdxPkg = std.build.Pkg{
    .name = "stdx",
    .source = .{ .path = thisDir() ++ "/src/stdx/stdx.zig" },
};

inline fn thisDir() []const u8 {
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

pub fn buildCVM(step: *std.build.CompileStep, opts: Options) !void {
    const b = step.builder;
    var cflags = std.ArrayList([]const u8).init(b.allocator);
    if (step.optimize == .Debug) {
        try cflags.append("-DDEBUG=1");
    }
    try cflags.append("-DCGOTO=1");
    if (opts.trackGlobalRc) {
        try cflags.append("-DTRACK_GLOBAL_RC=1");
    }
    step.addIncludePath(thisDir() ++ "/src");
    step.addCSourceFile(thisDir() ++ "/src/vm.c", cflags.items);
}