const std = @import("std");
const builtin = @import("builtin");
const config = @import("src/config.zig");
const mimalloc_lib = @import("lib/mimalloc/lib.zig");
const tcc_lib = @import("lib/tcc/lib.zig");

// FIND: v0.2
const Version = "0.2";

var useMalloc: bool = undefined;
var selinux: bool = undefined;
var vmEngine: config.Engine = undefined;
var testFilter: ?[]const u8 = undefined;

var stdx: *std.build.Module = undefined;
var tcc: *std.build.Module = undefined;
var mimalloc: *std.build.Module = undefined;

pub fn build(b: *std.build.Builder) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    selinux = b.option(bool, "selinux", "Whether you are building on linux distro with selinux. eg. Fedora.") orelse false;
    testFilter = b.option([]const u8, "test-filter", "Test filter.");
    useMalloc = b.option(bool, "use-malloc", "Use C allocator.") orelse false;
    vmEngine = b.option(config.Engine, "vm", "Build with `zig` or `c` VM.") orelse .zig;

    stdx = b.createModule(.{
        .source_file = .{ .path = thisDir() ++ "/src/stdx/stdx.zig" },
    });
    tcc = tcc_lib.createModule(b);
    mimalloc = mimalloc_lib.createModule(b);

    {
        const step = b.step("cli", "Build main cli.");

        const opts = getDefaultOptions(target, optimize);

        const exe = b.addExecutable(.{
            .name = "cyber",
            .root_source_file = .{ .path = "src/main.zig" },
            .target = target,
            .optimize = optimize,
        });
        if (exe.optimize != .Debug) {
            exe.strip = true;
        }
        exe.addIncludePath(.{ .path = thisDir() ++ "/src" });

        // Allow dynamic libraries to be loaded by filename in the cwd.
        exe.addRPath(.{ .path = "." });

        // Allow exported symbols in exe to be visible to dlopen.
        exe.rdynamic = true;

        try addBuildOptions(b, exe, opts);
        if (vmEngine == .c) {
            try buildCVM(b.allocator, exe, opts);
        }
        // exe.emit_asm = .emit;

        // exe.linkLibC();
        exe.addModule("stdx", stdx);
        if (opts.useMimalloc) {
            mimalloc_lib.addModule(exe, "mimalloc", mimalloc);
            mimalloc_lib.buildAndLink(b, exe, .{});
        }
        tcc_lib.addModule(exe, "tcc", tcc);
        tcc_lib.buildAndLink(b, exe, .{
            .selinux = selinux,
        });
        step.dependOn(&exe.step);
        step.dependOn(&b.addInstallArtifact(exe, .{}).step);
    }

    {
        const step = b.step("lib", "Build as a library.");

        const opts = getDefaultOptions(target, optimize);

        const lib = b.addSharedLibrary(.{
            .name = "cyber",
            .root_source_file = .{ .path = "src/lib.zig" },
            .target = target,
            .optimize = optimize,
        });
        if (lib.optimize != .Debug) {
            lib.strip = true;
        }
        lib.addIncludePath(.{. path = thisDir() ++ "/src" });

        // Allow dynamic libraries to be loaded by filename in the cwd.
        // lib.addRPath(".");

        // Allow exported symbols to be visible to dlopen.
        // Also needed to export symbols in wasm lib.
        lib.rdynamic = true;

        try addBuildOptions(b, lib, opts);

        // lib.linkLibC();
        lib.addModule("stdx", stdx);
        if (opts.useMimalloc) {
            mimalloc_lib.addModule(lib, "mimalloc", mimalloc);
            mimalloc_lib.buildAndLink(b, lib, .{});
        }

        if (!target.getCpuArch().isWasm()) {
            tcc_lib.addModule(lib, "tcc", tcc);
            tcc_lib.buildAndLink(b, lib, .{
                .selinux = selinux,
            });
        } else {
            lib.addAnonymousModule("tcc", .{
                .source_file = .{ .path = thisDir() ++ "/src/nopkg.zig" },
            });
            // Disable stack protector since the compiler isn't linking __stack_chk_guard/__stack_chk_fail.
            lib.stack_protector = false;
        }
        step.dependOn(&lib.step);
        step.dependOn(&b.addInstallArtifact(lib, .{}).step);
    }

    {
        const step = b.step("wasm-test", "Build the wasm test runner.");
        const opts = getDefaultOptions(target, optimize);
        const lib = b.addSharedLibrary(.{
            .name = "test",
            .root_source_file = .{ .path = "test/wasm_test.zig" },
            .target = target,
            .optimize = optimize,
            .main_pkg_path = .{ .path = "." },
        });
        if (lib.optimize != .Debug) {
            // Currently there is a limit to @embedFile in wasm_test.zig before strip=true crashes on linux.
            if (builtin.os.tag == .linux) {
                lib.strip = false;
            } else {
                lib.strip = true;
            }
        }
        lib.addIncludePath(.{ .path = thisDir() ++ "/src" });

        // Allow dynamic libraries to be loaded by filename in the cwd.
        // lib.addRPath(".");

        // Allow exported symbols to be visible to dlopen.
        // Also needed to export symbols in wasm lib.
        lib.rdynamic = true;

        try addBuildOptions(b, lib, opts);

        lib.linkLibC();
        lib.addModule("stdx", stdx);

        if (!target.getCpuArch().isWasm()) {
            tcc_lib.addModule(lib, "tcc", tcc);
            tcc_lib.buildAndLink(b, lib, .{
                .selinux = selinux,
            });
        } else {
            lib.addAnonymousModule("tcc", .{
                .source_file = .{ .path = thisDir() ++ "/src/nopkg.zig" },
            });
            // Disable stack protector since the compiler isn't linking __stack_chk_guard/__stack_chk_fail.
            lib.stack_protector = false;
        }
        step.dependOn(&lib.step);
        step.dependOn(&b.addInstallArtifact(lib, .{}).step);
    }

    {
        const mainStep = b.step("test", "Run tests.");

        var opts = getDefaultOptions(target, optimize);
        opts.trackGlobalRc = true;

        var step = b.addTest(.{
            .root_source_file = .{ .path = "./test/main_test.zig" },
            .target = target,
            .optimize = optimize,
            .filter = testFilter,
            .main_pkg_path = .{ .path = "." },
        });
        step.addIncludePath(.{ .path = thisDir() ++ "/src" });

        try addBuildOptions(b, step, opts);
        step.addModule("stdx", stdx);
        step.rdynamic = true;

        if (opts.useMimalloc) {
            mimalloc_lib.addModule(step, "mimalloc", mimalloc);
            mimalloc_lib.buildAndLink(b, step, .{});
        }

        if (vmEngine == .c) {
            try buildCVM(b.allocator, step, opts);
        }

        tcc_lib.addModule(step, "tcc", tcc);
        tcc_lib.buildAndLink(b, step, .{
            .selinux = opts.selinux,
        });
        mainStep.dependOn(&b.addRunArtifact(step).step);

        step = try addTraceTest(b, opts);
        mainStep.dependOn(&b.addRunArtifact(step).step);
    }

    {
        const mainStep = b.step("test-lib", "Run tests.");

        var opts = getDefaultOptions(target, optimize);
        opts.trackGlobalRc = true;

        var step = b.addTest(.{
            .root_source_file = .{ .path = "./test/lib_test.zig" },
            .target = target,
            .optimize = optimize,
            .filter = testFilter,
            .main_pkg_path = .{ .path = "." },
        });
        step.addIncludePath(.{ .path = thisDir() ++ "/src" });

        try addBuildOptions(b, step, opts);
        step.addModule("stdx", stdx);
        step.rdynamic = true;

        if (opts.useMimalloc) {
            mimalloc_lib.addModule(step, "mimalloc", mimalloc);
            mimalloc_lib.buildAndLink(b, step, .{});
        }

        tcc_lib.addModule(step, "tcc", tcc);
        tcc_lib.buildAndLink(b, step, .{
            .selinux = opts.selinux,
        });
        mainStep.dependOn(&b.addRunArtifact(step).step);

        step = try addTraceTest(b, opts);
        mainStep.dependOn(&b.addRunArtifact(step).step);
    }

    {
        // Just trace test.
        const mainStep = b.step("test-trace", "Run trace tests.");
        var opts = getDefaultOptions(target, optimize);
        opts.trackGlobalRc = true;
        const step = try addTraceTest(b, opts);
        mainStep.dependOn(&b.addRunArtifact(step).step);
    }

    const printStep = b.allocator.create(PrintStep) catch unreachable;
    printStep.* = PrintStep.init(b, Version);
    b.step("version", "Get the short version.").dependOn(&printStep.step);
}

const Options = struct {
    selinux: bool,
    trackGlobalRc: bool,
    trace: bool,
    traceObjects: bool,
    target: std.zig.CrossTarget,
    optimize: std.builtin.OptimizeMode,
    useMimalloc: bool,
};

fn getDefaultOptions(target: std.zig.CrossTarget, optimize: std.builtin.OptimizeMode) Options {
    return .{
        .selinux = selinux,
        .trackGlobalRc = optimize != .ReleaseFast,
        .trace = false,
        .traceObjects = optimize == .Debug,
        .target = target,
        .optimize = optimize,

        // Use mimalloc for fast builds.
        .useMimalloc = optimize == .ReleaseFast and !target.getCpuArch().isWasm(),
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
    build_options.addOption(bool, "useMimalloc", opts.useMimalloc);

    build_options.addOption(config.Engine, "vmEngine", vmEngine);
    build_options.addOption(bool, "trace", opts.trace);
    build_options.addOption(bool, "traceObjects", opts.traceObjects);
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
        .filter = testFilter,
        .main_pkg_path = .{ .path = "." },
    });
    step.addIncludePath(.{ .path = thisDir() ++ "/src" });

    var newOpts = opts;
    newOpts.trace = true;
    try addBuildOptions(b, step, newOpts);
    step.addModule("stdx", stdx);

    if (opts.useMimalloc) {
        mimalloc_lib.addModule(step, "mimalloc", mimalloc);
        mimalloc_lib.buildAndLink(b, step, .{});
    }

    tcc_lib.addModule(step, "tcc", tcc);
    tcc_lib.buildAndLink(b, step, .{
        .selinux = opts.selinux,
    });
    if (vmEngine == .c) {
        try buildCVM(b.allocator, step, newOpts);
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
    build: *std.Build,
    str: []const u8,

    pub fn init(build_: *std.Build, str: []const u8) PrintStep {
        return PrintStep{
            .build = build_,
            .step = std.build.Step.init(.{
                .id = .custom,
                .name = "print",
                .owner = build_,
                .makeFn = make,
            }),
            .str = build_.dupe(str),
        };
    }

    fn make(step: *std.build.Step, _: *std.Progress.Node) anyerror!void {
        const self = @fieldParentPtr(PrintStep, "step", step);
        std.io.getStdOut().writer().writeAll(self.str) catch unreachable;
    }
};

pub fn buildCVM(alloc: std.mem.Allocator, step: *std.build.CompileStep, opts: Options) !void {
    var cflags = std.ArrayList([]const u8).init(alloc);
    if (step.optimize == .Debug) {
        try cflags.append("-DDEBUG=1");
    } else {
        try cflags.append("-DDEBUG=0");
    }
    try cflags.append("-DCGOTO=1");
    if (opts.trackGlobalRc) {
        try cflags.append("-DTRACK_GLOBAL_RC=1");
    }
    if (opts.trace) {
        try cflags.append("-DTRACE_ENABLED=1");
    }

    step.addIncludePath(.{ .path = thisDir() ++ "/src"});
    step.addCSourceFile(.{
        .file = .{ .path = thisDir() ++ "/src/vm.c" },
        .flags = cflags.items
    });
}