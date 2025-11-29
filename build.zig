const std = @import("std");
const Build = std.Build;
const builtin = @import("builtin");
const config_ = @import("src/config.zig");
const mimalloc_lib = @import("lib/mimalloc/lib.zig");
const tcc_lib = @import("lib/tcc/lib.zig");

// FIND: v0.4
const Version = "0.4";

var optMalloc: ?Allocator = undefined;
var selinux: bool = undefined;
var vmEngine: config_.Engine = undefined;
var testFilter: ?[]const u8 = undefined;
var test_backend: config_.TestBackend = undefined;
var trace: bool = undefined;
var log_mem: bool = undefined;
var no_cache: bool = undefined;
var optFFI: ?bool = undefined;
var optStatic: ?bool = undefined;
var optJIT: ?bool = undefined;
var strip: bool = undefined;

var rtarget: std.Build.ResolvedTarget = undefined;
var target: std.Target = undefined;
var optimize: std.builtin.OptimizeMode = undefined;
var dev: bool = undefined;
var isystem: []const []const u8 = undefined;

pub const Allocator = enum {
    zig,
    malloc,
    mimalloc,
};

pub fn build(b: *std.Build) !void {
    rtarget = b.standardTargetOptions(.{});
    target = rtarget.result;
    optimize = b.standardOptimizeOption(.{});

    selinux = b.option(bool, "selinux", "Whether you are building on linux distro with selinux. e.g. Fedora.") orelse false;
    testFilter = b.option([]const u8, "test-filter", "Test filter.");
    test_backend = b.option(config_.TestBackend, "test-backend", "Test compiler backend.") orelse .vm;
    vmEngine = b.option(config_.Engine, "vm", "Build with `zig` or `c` VM.") orelse .c;
    optMalloc = b.option(Allocator, "malloc", "Override default allocator: `malloc`, `mimalloc`, `zig`");
    optFFI = b.option(bool, "ffi", "Override default FFI: true, false");
    optStatic = b.option(bool, "static", "Override default lib build type: true=static, false=dynamic");
    trace = b.option(bool, "trace", "Enable tracing features.") orelse (optimize == .Debug);
    optJIT = b.option(bool, "jit", "Build with JIT.");
    log_mem = b.option(bool, "log-mem", "Log memory traces.") orelse false;
    no_cache = b.option(bool, "no-cache", "Disable caching when running tests.") orelse false;
    dev = b.option(bool, "dev", "Marks version as dev.") orelse true;
    isystem = b.option([]const []const u8, "isystem", "System includes.") orelse &.{};
    strip = b.option(bool, "strip", "Override strip.") orelse b: {
        if (optimize != .Debug and !trace) {
            // Sometimes there are issues with strip for ReleaseFast + wasi.
            if (target.os.tag != .wasi) {
                break :b true;
            }
        }
        break :b false;
    };

    const abs_root_path = try std.fs.cwd().realpathAlloc(b.allocator, ".");

    var opts = getDefaultOptions();
    opts.malloc = if (target.cpu.arch.isWasm()) .zig else .malloc;
    opts.applyOverrides();

    const lib_mod = try create_lib_module(b, opts);
    const lib = try buildLib(b, lib_mod, opts);
    const lib_test = try add_lib_test(b, lib_mod);

    // TODO: Remove vmc dep from cli.
    const vmc = try create_vmc_mod(b, opts);

    var cli_config = CliConfig.default();
    if (optimize == .Debug) {
        // Dev build looks at `@root`, release build uses the exe dir as root.
        cli_config = cli_config.with_mod_root(abs_root_path);
    }
    const cli_mod = try create_cli_module(b, vmc, lib, cli_config);

    const cli_test_config = CliConfig.default().with_mod_root(abs_root_path);
    const cli_test_mod = try create_cli_module(b, vmc, lib, cli_test_config);
    const cli_test = try add_cli_test(b, cli_test_mod);

    const trace_test = try addTraceTest(b, opts, cli_test_config);

    const behavior_test = try addBehaviorTest(b, lib, vmc, cli_test_config);

    {
        const step = b.step("cli", "Build main cli.");

        const exe = b.addExecutable(.{
            .name = "cyber",
            .root_module = cli_mod,
        });
        exe.root_module.strip = strip;
        exe.addIncludePath(b.path("src"));

        // Allow dynamic libraries to be loaded by filename in the cwd.
        if (target.os.tag == .linux) {
            exe.addRPath(.{ .cwd_relative = ":$ORIGIN" });
            if (target.cpu.arch == .x86_64) {
                exe.addRPath(.{ .cwd_relative = "/usr/lib/x86_64-linux-gnu" });
            }
        } else if (target.os.tag == .macos) {
            exe.addRPath(.{ .cwd_relative = "@loader_path" });
        }

        // Allow exported symbols in exe to be visible to dlopen.
        exe.rdynamic = true;

        // step.dependOn(&b.addInstallFileWithDir(
        //     exe.getEmittedAsm(), .prefix, "cyber.s",
        // ).step);

        step.dependOn(&b.addInstallArtifact(exe, .{}).step);
        if (optimize != .Debug) {
            step.dependOn(&b.addInstallFile(b.path("src/builtins/core.cy"), "bin/src/builtins/core.cy").step);
            step.dependOn(&b.addInstallFile(b.path("src/builtins/cy.cy"), "bin/src/builtins/cy.cy").step);
            step.dependOn(&b.addInstallFile(b.path("src/builtins/meta.cy"), "bin/src/builtins/meta.cy").step);
            step.dependOn(&b.addInstallFile(b.path("src/builtins/c.cy"), "bin/src/builtins/c.cy").step);
            step.dependOn(&b.addInstallFile(b.path("src/std/math.cy"), "bin/src/std/math.cy").step);
            step.dependOn(&b.addInstallFile(b.path("src/std/os.cy"), "bin/src/std/os.cy").step);
            step.dependOn(&b.addInstallFile(b.path("src/std/io.cy"), "bin/src/std/io.cy").step);
            step.dependOn(&b.addInstallFile(b.path("src/std/cli.cy"), "bin/src/std/cli.cy").step);
            step.dependOn(&b.addInstallFile(b.path("src/std/test.cy"), "bin/src/std/test.cy").step);
            step.dependOn(&b.addInstallFile(b.path("src/std/os.macos.cy"), "bin/src/std/os.macos.cy").step);
            step.dependOn(&b.addInstallFile(b.path("src/std/os.linux.cy"), "bin/src/std/os.linux.cy").step);
            step.dependOn(&b.addInstallFile(b.path("src/std/os.windows.cy"), "bin/src/std/os.windows.cy").step);
            step.dependOn(&b.addInstallFile(b.path("src/std/win32.cy"), "bin/src/std/win32.cy").step);
            step.dependOn(&b.addInstallFile(b.path("src/std/libc.cy"), "bin/src/std/libc.cy").step);
            step.dependOn(&b.addInstallFile(b.path("src/std/libc.macos.cy"), "bin/src/std/libc.macos.cy").step);
            step.dependOn(&b.addInstallFile(b.path("src/std/libc.linux.cy"), "bin/src/std/libc.linux.cy").step);
        }
    }

    // {
    //     const step = b.step("vm-lib", "Build vm as a library.");

    //     const opts = getDefaultOptions();
    //     const obj = try buildCVM(b, opts);

    //     const root = b.createModule(.{
    //         .target = rtarget,
    //         .optimize = optimize,
    //     });
    //     const lib = b.addLibrary(.{
    //         .linkage = .static,
    //         .name = "vm",
    //         .root_module = root,
    //     });
    //     lib.addObject(obj);

    //     step.dependOn(&lib.step);
    //     step.dependOn(&b.addInstallArtifact(lib, .{}).step);
    // }

    {
        const step = b.step("lib", "Build as a library.");
        step.dependOn(&b.addInstallArtifact(lib, .{}).step);
    }

    // {
    //     const main_step = b.step("web-lib", "Build wasm lib for web.");

    //     var opts = getDefaultOptions();
    //     opts.ffi = false;
    //     opts.malloc = if (target.cpu.arch.isWasm()) .zig else .malloc;
    //     opts.cli = false;
    //     opts.applyOverrides();

    //     const root = b.createModule(.{
    //         .root_source_file = b.path("src/web.zig"),
    //         .target = rtarget,
    //         .optimize = optimize,
    //     });
    //     const lib = b.addExecutable(.{
    //         .name = "cyber-web",
    //         .root_module = root,
    //     });
    //     lib.entry = .disabled;
    //     if (optimize != .Debug) {
    //         // Disable to see better symbols when debugging wasm.
    //         lib.root_module.strip = true;
    //     }
    //     lib.addIncludePath(b.path("src"));

    //     if (target.cpu.arch.isWasm()) {
    //         // Export table so non-exported functions can still be invoked from:
    //         // `instance.exports.__indirect_function_table`
    //         lib.export_table = true;
    //     }
    //     lib.rdynamic = true;

    //     const build_options = try create_lib_config(b, opts);
    //     const stdx = b.createModule(.{
    //         .root_source_file = b.path("src/stdx/stdx.zig"),
    //     });

    //     try buildAndLinkDeps(lib.root_module, build_options, stdx, opts);
    //     main_step.dependOn(&b.addInstallArtifact(lib, .{}).step);
    // }

    {
        const main_step = b.step("build-test", "Build tests.");

        var install = b.addInstallArtifact(lib_test, .{});
        main_step.dependOn(&install.step);

        install = b.addInstallArtifact(cli_test, .{});
        main_step.dependOn(&install.step);

        install = b.addInstallArtifact(behavior_test, .{});
        main_step.dependOn(&install.step);

        install = b.addInstallArtifact(trace_test, .{});
        main_step.dependOn(&install.step);
    }

    const behavior_test_cmd = b.step("behavior-test", "Run behavior tests.");
    {
        var run = b.addRunArtifact(behavior_test);
        run.has_side_effects = no_cache;
        behavior_test_cmd.dependOn(&run.step);
    }

    const lib_test_cmd = b.step("lib-test", "Run lib tests.");
    {
        var run = b.addRunArtifact(lib_test);
        run.has_side_effects = no_cache;
        lib_test_cmd.dependOn(&run.step);
    }

    const cli_test_cmd = b.step("cli-test", "Run cli tests.");
    {
        var run = b.addRunArtifact(cli_test);
        run.has_side_effects = no_cache;
        cli_test_cmd.dependOn(&run.step);
    }

    const trace_test_cmd = b.step("trace-test", "Run trace tests.");
    {
        const run = b.addRunArtifact(trace_test);
        run.has_side_effects = no_cache;
        trace_test_cmd.dependOn(&run.step);
    }

    {
        const main_step = b.step("test", "Run all tests.");
        main_step.dependOn(lib_test_cmd);
        main_step.dependOn(cli_test_cmd);
        main_step.dependOn(trace_test_cmd);
        main_step.dependOn(behavior_test_cmd);
    }

    const printStep = b.allocator.create(PrintStep) catch unreachable;
    printStep.* = PrintStep.init(b, Version);
    b.step("version", "Get the short version.").dependOn(&printStep.step);
}

fn link_cli_deps(b: *std.Build, cli: *std.Build.Module, vmc: *std.Build.Module, config: CliConfig) !void {
    cli.addIncludePath(b.path("src"));

    const build_config = try create_cli_config(b, config);
    cli.addImport("build_config", build_config);

    const linenoise = try createLinenoiseModule(b);
    cli.addImport("linenoise", linenoise);

    cli.addImport("vmc", vmc);

    const stdx = b.createModule(.{
        .root_source_file = b.path("src/stdx/stdx.zig"),
    });
    cli.addImport("stdx", stdx);

    if (config.malloc == .mimalloc) {
        const mimalloc = mimalloc_lib.createModule(b);
        cli.addImport("mimalloc", mimalloc);
        try mimalloc_lib.buildAndLink(b, cli, .{
            .target = rtarget,
            .optimize = optimize,
        });
    }
}

pub fn create_vmc_mod(b: *std.Build, opts: Options) !*std.Build.Module {
    const mod = b.addTranslateC(.{
        .root_source_file = b.path("src/vm.h"),
        .target = rtarget,
        .optimize = optimize,
    });
    const rt_safety = optimize == .Debug or optimize == .ReleaseSafe;
    mod.defineCMacro("RT_SAFETY", if (rt_safety) "1" else "0");
    mod.defineCMacro("DEBUG", if (optimize == .Debug) "1" else "0");
    mod.defineCMacro("TRACE", if (opts.trace) "1" else "0");
    mod.defineCMacro("IS_32BIT", if (is32Bit(opts.target)) "1" else "0");
    mod.defineCMacro("HAS_CYC", if (opts.cyc) "1" else "0");
    return b.createModule(.{
        .root_source_file = mod.getOutput(),
    });
}

pub const Options = struct {
    selinux: bool,
    trace: bool,
    log_mem: bool,
    target: std.Target,
    optimize: std.builtin.OptimizeMode,
    malloc: Allocator,
    static: bool,
    cyc: bool,
    ffi: bool,
    cli: bool,
    jit: bool,

    fn applyOverrides(self: *Options) void {
        if (optMalloc) |malloc| {
            self.malloc = malloc;
        }
        if (optFFI) |ffi| {
            self.ffi = ffi;
        }
        if (optStatic) |static| {
            self.static = static;
        }
        if (optJIT) |jit| {
            self.jit = jit;
        }
    }
};

fn getDefaultOptions() Options {
    var malloc: Allocator = undefined;
    // Use mimalloc for fast builds.
    if (target.cpu.arch.isWasm()) {
        malloc = .zig;
    } else {
        if (optimize != .Debug) {
            malloc = .mimalloc;
        } else {
            malloc = .zig;
        }
    }
    return .{
        .selinux = selinux,
        .trace = trace,
        .log_mem = log_mem,
        .target = target,
        .optimize = optimize,
        .cyc = true,
        .malloc = malloc,
        .static = true,
        .ffi = !target.cpu.arch.isWasm(),
        .cli = !target.cpu.arch.isWasm(),
        .jit = false,
    };
}

const TestConfig = struct {
    test_backend: config_.TestBackend,
};

fn create_test_config(b: *std.Build, config: TestConfig) !*std.Build.Module {
    const options = b.addOptions();
    options.addOption(config_.TestBackend, "test_backend", config.test_backend);
    return options.createModule();
}

const CliConfig = struct {
    mod_root: ?[]const u8,
    trace: bool,
    malloc: Allocator,

    fn default() CliConfig {
        var malloc_: Allocator = undefined;
        if (optimize != .Debug) {
            malloc_ = .mimalloc;
        } else {
            malloc_ = .zig;
        }
        if (optMalloc) |malloc| {
            malloc_ = malloc;
        }
        return .{
            .mod_root = null,
            .trace = trace,
            .malloc = malloc_,
        };
    }

    fn with_mod_root(self: *const CliConfig, mod_root: ?[]const u8) CliConfig {
        var new = self.*;
        new.mod_root = mod_root;
        return new;
    }
};

fn create_cli_config(b: *std.Build, opts: CliConfig) !*std.Build.Module {
    const options = b.addOptions();
    options.addOption(?[]const u8, "mod_root", opts.mod_root);
    options.addOption(bool, "trace", opts.trace);
    options.addOption(Allocator, "malloc", opts.malloc);
    return options.createModule();
}

fn create_lib_config(b: *std.Build, opts: Options) !*std.Build.Module {
    const buildTag = std.process.getEnvVarOwned(b.allocator, "BUILD") catch |err| b: {
        if (err == error.EnvironmentVariableNotFound) {
            break :b "0";
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
    const options = b.addOptions();
    options.addOption(bool, "dev", dev);
    if (dev) {
        options.addOption([]const u8, "version", b.fmt("{s}-DEV", .{Version}));
        options.addOption([]const u8, "full_version", b.fmt("Cyber {s}-DEV build-{s}-{s}", .{ Version, buildTag, commitTag }));
    } else {
        options.addOption([]const u8, "version", Version);
        options.addOption([]const u8, "full_version", b.fmt("Cyber {s} build-{s}-{s}", .{ Version, buildTag, commitTag }));
    }
    options.addOption([]const u8, "build", buildTag);
    options.addOption([]const u8, "commit", commitTag);
    options.addOption(Allocator, "malloc", opts.malloc);
    options.addOption(config_.Engine, "vmEngine", vmEngine);
    options.addOption(bool, "trace", opts.trace);
    options.addOption(bool, "log_mem", opts.log_mem);
    options.addOption(bool, "is32Bit", is32Bit(opts.target));
    options.addOption(bool, "cyc", opts.cyc);
    options.addOption(bool, "ffi", opts.ffi);
    options.addOption(bool, "cli", opts.cli);
    options.addOption(bool, "jit", opts.jit);
    return options.createModule();
}

fn add_cli_test(b: *std.Build, cli: *std.Build.Module) !*std.Build.Step.Compile {
    const step = b.addTest(.{
        .name = "cli_test",
        .root_module = cli,
        .filters = &.{testFilter orelse ""},
    });
    return step;
}

fn add_lib_test(b: *std.Build, lib: *std.Build.Module) !*std.Build.Step.Compile {
    const step = b.addTest(.{
        .name = "lib_test",
        .root_module = lib,
        .filters = &.{testFilter orelse ""},
    });
    return step;
}

fn addBehaviorTest(b: *std.Build, lib: *std.Build.Step.Compile, vmc: *std.Build.Module, config: CliConfig) !*std.Build.Step.Compile {
    const root = b.createModule(.{
        .root_source_file = b.path("src/behavior_test.zig"),
        .target = rtarget,
        .optimize = optimize,
    });
    const step = b.addTest(.{
        .name = "behavior_test",
        .root_module = root,
        .filters = &.{testFilter orelse ""},
        .test_runner = .{
            .path = b.path("src/test_runner.zig"),
            .mode = .simple,
        },
    });

    // For linux FFI test.
    step.rdynamic = true;

    try link_cli_deps(b, root, vmc, config);
    step.linkLibrary(lib);

    const test_config = try create_test_config(b, .{
        .test_backend = test_backend,
    });
    step.root_module.addImport("test_config", test_config);
    return step;
}

// Trace test builds its own lib because it requires the trace flag.
fn addTraceTest(b: *std.Build, lib_config_: Options, cli_config_: CliConfig) !*std.Build.Step.Compile {
    var lib_config = lib_config_;
    lib_config.trace = true;
    var cli_config = cli_config_;
    cli_config.trace = true;

    const root = b.createModule(.{
        .root_source_file = b.path("src/trace_test.zig"),
        .target = rtarget,
        .optimize = optimize,
    });

    const vmc = try create_vmc_mod(b, lib_config);
    try link_cli_deps(b, root, vmc, cli_config);

    const lib_mod = try create_lib_module(b, lib_config);
    const lib = try buildLib(b, lib_mod, lib_config);
    root.linkLibrary(lib);

    const step = b.addTest(.{
        .name = "trace_test",
        .root_module = root,
        .filters = &.{testFilter orelse ""},
        .test_runner = .{
            .path = b.path("src/test_runner.zig"),
            .mode = .simple,
        },
    });
    step.addIncludePath(b.path("src"));

    const test_config = try create_test_config(b, .{
        .test_backend = test_backend,
    });
    step.root_module.addImport("test_config", test_config);

    return step;
}

fn is32Bit(target_: std.Target) bool {
    switch (target_.cpu.arch) {
        .wasm32, .x86 => return true,
        else => return false,
    }
}

inline fn thisDir() []const u8 {
    return comptime std.fs.path.dirname(@src().file) orelse ".";
}

pub const PrintStep = struct {
    step: std.Build.Step,
    build: *std.Build,
    str: []const u8,

    pub fn init(build_: *std.Build, str: []const u8) PrintStep {
        return PrintStep{
            .build = build_,
            .step = std.Build.Step.init(.{
                .id = .custom,
                .name = "print",
                .owner = build_,
                .makeFn = make,
            }),
            .str = build_.dupe(str),
        };
    }

    fn make(step: *std.Build.Step, _: std.Build.Step.MakeOptions) anyerror!void {
        const self: *PrintStep = @fieldParentPtr("step", step);
        std.fs.File.stdout().writeAll(self.str) catch unreachable;
    }
};

pub fn buildCVM(b: *std.Build, opts: Options) !*std.Build.Step.Compile {
    const module = b.createModule(.{
        .target = rtarget,
        .optimize = optimize,
    });
    const lib = b.addObject(.{
        .name = "vm",
        .root_module = module,
    });
    lib.linkLibC();
    lib.root_module.strip = strip;

    var cflags = try std.ArrayList([]const u8).initCapacity(b.allocator, 0);
    const rt_safety = optimize == .Debug or optimize == .ReleaseSafe;
    if (rt_safety) {
        try cflags.append(b.allocator, "-DRT_SAFETY=1");
    } else {
        try cflags.append(b.allocator, "-DRT_SAFETY=0");
    }
    if (optimize == .Debug) {
        try cflags.append(b.allocator, "-DDEBUG=1");
    } else {
        try cflags.append(b.allocator, "-DDEBUG=0");
    }
    try cflags.append(b.allocator, "-DCGOTO=1");
    if (opts.trace) {
        try cflags.append(b.allocator, "-DTRACE=1");
    } else {
        try cflags.append(b.allocator, "-DTRACE=0");
    }
    if (opts.log_mem) {
        try cflags.append(b.allocator, "-DLOG_MEM=1");
    } else {
        try cflags.append(b.allocator, "-DLOG_MEM=0");
    }
    if (is32Bit(opts.target)) {
        try cflags.append(b.allocator, "-DIS_32BIT=1");
    } else {
        try cflags.append(b.allocator, "-DIS_32BIT=0");
    }
    if (opts.cyc) {
        try cflags.append(b.allocator, "-DHAS_CYC=1");
    } else {
        try cflags.append(b.allocator, "-DHAS_CYC=0");
    }

    // Disable traps for arithmetic/bitshift overflows.
    // Note that changing this alone doesn't clear the build cache.
    lib.root_module.sanitize_c = .off;

    lib.addIncludePath(b.path("src"));
    lib.addCSourceFile(.{ .file = b.path("src/vm.c"), .flags = cflags.items });
    for (isystem) |path| {
        lib.addSystemIncludePath(.{ .cwd_relative = path });
    }
    return lib;
}

fn create_cli_module(b: *std.Build, vmc: *std.Build.Module, lib: *std.Build.Step.Compile, config: CliConfig) !*std.Build.Module {
    const mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = rtarget,
        .optimize = optimize,
    });
    try link_cli_deps(b, mod, vmc, config);
    mod.linkLibrary(lib);
    return mod;
}

fn create_lib_module(b: *std.Build, opts: Options) !*std.Build.Module {
    const mod = b.createModule(.{
        .root_source_file = b.path("src/lib.zig"),
        .target = rtarget,
        .optimize = optimize,
    });

    mod.strip = strip;

    const build_config = try create_lib_config(b, opts);
    const stdx = b.createModule(.{
        .root_source_file = b.path("src/stdx/stdx.zig"),
    });

    mod.addIncludePath(b.path("src"));
    mod.addImport("build_config", build_config);
    mod.addImport("stdx", stdx);

    const vmc = try create_vmc_mod(b, opts);
    mod.addImport("vmc", vmc);

    mod.link_libc = true;

    if (vmEngine == .c) {
        const lib = try buildCVM(b, opts);
        if (opts.target.cpu.arch.isWasm()) {
            lib.root_module.stack_protector = false;
        }
        mod.addObject(lib);
    }

    if (opts.ffi) {
        const tcc = tcc_lib.createModule(b, rtarget, optimize);
        tcc_lib.addImport(mod, "tcc", tcc);
        try tcc_lib.buildAndLink(b, mod, .{
            .selinux = selinux,
            .target = rtarget,
            .optimize = optimize,
        });
    } else {
        mod.addAnonymousImport("tcc", .{
            .root_source_file = b.path("src/tcc_stub.zig"),
        });
        // Disable stack protector since the compiler isn't linking __stack_chk_guard/__stack_chk_fail.
        mod.stack_protector = false;
    }

    if (opts.jit) {
        // step.addIncludePath(.{ .path = "/opt/homebrew/Cellar/llvm/17.0.1/include" });
        // step.addLibraryPath(.{ .path = "/opt/homebrew/Cellar/llvm/17.0.1/lib" });
        // step.linkSystemLibrary("LLVM-17");
    }
    return mod;
}

fn buildLib(b: *std.Build, lib_mod: *std.Build.Module, opts: Options) !*std.Build.Step.Compile {
    const lib = b.addLibrary(.{
        .linkage = if (opts.static) .static else .dynamic,
        .name = "cyber",
        .root_module = lib_mod,
    });

    if (target.cpu.arch.isWasm()) {
        // Export table so non-exported functions can still be invoked from:
        // `instance.exports.__indirect_function_table`
        lib.export_table = true;
    }

    if (opts.cli) {
        if (target.os.tag == .windows) {
            lib.linkSystemLibrary("ole32");
            lib.linkSystemLibrary("crypt32");
            lib.linkSystemLibrary("ws2_32");

            if (target.abi == .msvc) {
                lib.linkSystemLibrary("advapi32");
                lib.linkSystemLibrary("shell32");
            }
        }
    }

    // Allow exported symbols to be visible to dlopen.
    // Also needed to export symbols in wasm lib.
    lib.rdynamic = true;

    for (isystem) |path| {
        lib.addSystemIncludePath(.{ .cwd_relative = path });
    }

    return lib;
}

pub fn createLinenoiseModule(b: *std.Build) !*std.Build.Module {
    if (target.os.tag != .windows and target.os.tag != .wasi) {
        const linenoise_ = b.addTranslateC(.{
            .root_source_file = b.path("lib/linenoise/linenoise.h"),
            .target = rtarget,
            .optimize = optimize,
        });
        const mod = b.createModule(.{
            .root_source_file = linenoise_.getOutput(),
        });
        mod.addIncludePath(b.path("lib/linenoise"));
        try buildAndLinkLinenoise(b, mod);
        return mod;
    } else {
        const mod = b.createModule(.{
            .root_source_file = b.path("src/ln_stub.zig"),
            .target = rtarget,
            .optimize = optimize,
        });
        mod.addAnonymousImport("linenoise", .{
            .root_source_file = b.path("src/ln_stub.zig"),
        });
        return mod;
    }
}

pub fn buildAndLinkLinenoise(b: *std.Build, mod: *std.Build.Module) !void {
    const root = b.createModule(.{
        .target = rtarget,
        .optimize = optimize,
    });
    const lib = b.addLibrary(.{
        .linkage = .static,
        .name = "linenoise",
        .root_module = root,
    });
    lib.linkLibC();
    // lib.disable_sanitize_c = true;

    const c_flags = try std.ArrayList([]const u8).initCapacity(b.allocator, 0);

    var sources = try std.ArrayList([]const u8).initCapacity(b.allocator, 0);
    sources.appendSlice(b.allocator, &.{
        "lib/linenoise/linenoise.c",
    }) catch @panic("error");
    for (sources.items) |src| {
        lib.addCSourceFile(.{
            .file = b.path(src),
            .flags = c_flags.items,
        });
    }
    mod.linkLibrary(lib);
}
