const std = @import("std");
const Build = std.Build;
const builtin = @import("builtin");
const config = @import("src/config.zig");
const mimalloc_lib = @import("lib/mimalloc/lib.zig");
const tcc_lib = @import("lib/tcc/lib.zig");

// FIND: v0.4
const Version = "0.4";

var optMalloc: ?config.Allocator = undefined;
var selinux: bool = undefined;
var vmEngine: config.Engine = undefined;
var testFilter: ?[]const u8 = undefined;
var test_backend: config.TestBackend = undefined;
var trace: bool = undefined;
var log_mem: bool = undefined;
var no_cache: bool = undefined;
var link_test: bool = undefined;
var optFFI: ?bool = undefined;
var optStatic: ?bool = undefined;
var optJIT: ?bool = undefined;

var rtarget: std.Build.ResolvedTarget = undefined;
var target: std.Target = undefined;
var optimize: std.builtin.OptimizeMode = undefined;
var dev: bool = undefined;
var isystem: []const []const u8 = undefined;

pub fn build(b: *std.Build) !void {
    rtarget = b.standardTargetOptions(.{});
    target = rtarget.result;
    optimize = b.standardOptimizeOption(.{});

    selinux = b.option(bool, "selinux", "Whether you are building on linux distro with selinux. e.g. Fedora.") orelse false;
    testFilter = b.option([]const u8, "test-filter", "Test filter.");
    test_backend = b.option(config.TestBackend, "test-backend", "Test compiler backend.") orelse .vm;
    vmEngine = b.option(config.Engine, "vm", "Build with `zig` or `c` VM.") orelse .c;
    optMalloc = b.option(config.Allocator, "malloc", "Override default allocator: `malloc`, `mimalloc`, `zig`");
    optFFI = b.option(bool, "ffi", "Override default FFI: true, false");
    optStatic = b.option(bool, "static", "Override default lib build type: true=static, false=dynamic");
    trace = b.option(bool, "trace", "Enable tracing features.") orelse (optimize == .Debug);
    optJIT = b.option(bool, "jit", "Build with JIT.");
    link_test = b.option(bool, "link-test", "Build test by linking lib. Disable for better stack traces.") orelse true;
    log_mem = b.option(bool, "log-mem", "Log memory traces.") orelse false;
    no_cache = b.option(bool, "no-cache", "Disable caching when running tests.") orelse false;
    dev = b.option(bool, "dev", "Marks version as dev.") orelse true;
    isystem = b.option([]const []const u8, "isystem", "System includes.") orelse &.{};

    {
        const step = b.step("cli", "Build main cli.");

        var opts = getDefaultOptions();
        opts.applyOverrides();

        const main = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            // .root_source_file = b.path("main.zig"),
            .target = rtarget,
            .optimize = optimize,
        });

        const exe = b.addExecutable(.{
            .name = "cyber",
            .root_module = main,
        });
        if (optimize != .Debug) {
            // Sometimes there are issues with strip for ReleaseFast + wasi.
            if (opts.target.os.tag != .wasi) {
                exe.root_module.strip = true;
            }
        }
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

        const build_options = try createBuildOptions(b, opts);
        const stdx = b.createModule(.{
            .root_source_file = b.path("src/stdx/stdx.zig"),
        });
        try buildAndLinkDeps(exe.root_module, build_options, stdx, opts);  
        step.dependOn(&b.addInstallArtifact(exe, .{}).step);
        // step.dependOn(&b.addInstallFile(b.path("src/builtins/builtins.cy"), "bin/src/builtins/builtins.cy").step);
        // step.dependOn(&b.addInstallFile(b.path("src/builtins/cy.cy"), "bin/src/builtins/cy.cy").step);
        // step.dependOn(&b.addInstallFile(b.path("src/builtins/math.cy"), "bin/src/builtins/math.cy").step);
        // step.dependOn(&b.addInstallFile(b.path("src/builtins/meta.cy"), "bin/src/builtins/meta.cy").step);
        // step.dependOn(&b.addInstallFile(b.path("src/builtins/c.cy"), "bin/src/builtins/c.cy").step);
        // step.dependOn(&b.addInstallFile(b.path("src/std/math.cy"), "bin/src/std/math.cy").step);
        // step.dependOn(&b.addInstallFile(b.path("src/std/os.cy"), "bin/src/std/os.cy").step);
        // step.dependOn(&b.addInstallFile(b.path("src/std/io.cy"), "bin/src/std/io.cy").step);
        // step.dependOn(&b.addInstallFile(b.path("src/std/os.macos.cy"), "bin/src/std/os.macos.cy").step);
        // step.dependOn(&b.addInstallFile(b.path("src/std/libc.cy"), "bin/src/std/libc.cy").step);
        // step.dependOn(&b.addInstallFile(b.path("src/std/libc.macos.cy"), "bin/src/std/libc.macos.cy").step);
    }

    {
        const step = b.step("vm-lib", "Build vm as a library.");

        const opts = getDefaultOptions();
        const obj = try buildCVM(b, opts);

        const root = b.createModule(.{
            .target = rtarget,
            .optimize = optimize,
        });
        const lib = b.addLibrary(.{
            .linkage = .static,
            .name = "vm",
            .root_module = root,
        });
        lib.addObject(obj);

        step.dependOn(&lib.step);
        step.dependOn(&b.addInstallArtifact(lib, .{}).step);
    }

    // {
    //     const step = b.step("rt", "Build runtime for AOT.");

    //     var opts = getDefaultOptions();
    //     opts.ffi = false;
    //     opts.malloc = if (target.cpu.arch.isWasm()) .zig else .malloc;
    //     opts.cli = false;
    //     opts.rt = .pm;
    //     opts.applyOverrides();

    //     const lib = b.addStaticLibrary(.{
    //         .name = "rt",
    //         .root_source_file = .{ .cwd_relative = "src/runtime_static.zig" },
    //         .target = rtarget,
    //         .optimize = optimize,
    //     });
    //     if (optimize != .Debug) {
    //         lib.root_module.strip = true;
    //     }
    //     lib.addIncludePath(b.path("src"));

    //     const build_options = try createBuildOptions(b, opts);
    //     const stdx = b.createModule(.{
    //         .root_source_file = b.path("src/stdx/stdx.zig"),
    //     });

    //     try buildAndLinkDeps(lib.root_module, build_options, stdx, opts);
    //     step.dependOn(&lib.step);
    //     step.dependOn(&b.addInstallArtifact(lib, .{}).step);
    // }

    {
        const step = b.step("lib", "Build as a library.");

        var opts = getDefaultOptions();
        opts.ffi = false;
        opts.malloc = if (target.cpu.arch.isWasm()) .zig else .malloc;
        opts.cli = false;
        opts.applyOverrides();

        const lib = try buildLib(b, opts);
        for (isystem) |path| {
            lib.addSystemIncludePath(.{ .cwd_relative = path });
        }
        step.dependOn(&b.addInstallArtifact(lib, .{}).step);
    }

    {
        const main_step = b.step("web-lib", "Build wasm lib for web.");

        var opts = getDefaultOptions();
        opts.ffi = false;
        opts.malloc = if (target.cpu.arch.isWasm()) .zig else .malloc;
        opts.cli = false;
        opts.applyOverrides();

        const root = b.createModule(.{
            .root_source_file = b.path("src/web.zig"),
            .target = rtarget,
            .optimize = optimize,
        });
        const lib = b.addExecutable(.{
            .name = "cyber-web",
            .root_module = root,
        });
        lib.entry = .disabled;
        if (optimize != .Debug) {
            // Disable to see better symbols when debugging wasm.
            lib.root_module.strip = true;
        }
        lib.addIncludePath(b.path("src"));

        if (target.cpu.arch.isWasm()) {
            // Export table so non-exported functions can still be invoked from:
            // `instance.exports.__indirect_function_table`
            lib.export_table = true;
        }
        lib.rdynamic = true;

        const build_options = try createBuildOptions(b, opts);
        const stdx = b.createModule(.{
            .root_source_file = b.path("src/stdx/stdx.zig"),
        });

        try buildAndLinkDeps(lib.root_module, build_options, stdx, opts);
        main_step.dependOn(&b.addInstallArtifact(lib, .{}).step);
    }

    {
        const main_step = b.step("build-test", "Build tests.");

        var opts = getDefaultOptions();
        opts.trackGlobalRc = true;
        opts.applyOverrides();

        var step = try addUnitTest(b, opts);
        main_step.dependOn(&b.addInstallArtifact(step, .{}).step);

        step = try addBehaviorTest(b, opts);
        main_step.dependOn(&b.addInstallArtifact(step, .{}).step);

        step = try addTraceTest(b, opts);
        main_step.dependOn(&b.addInstallArtifact(step, .{}).step);
    }

    {
        const main_step = b.step("behavior-test", "Run behavior tests.");

        var opts = getDefaultOptions();
        opts.trackGlobalRc = true;
        opts.applyOverrides();

        const step = try addBehaviorTest(b, opts);
        main_step.dependOn(&b.addInstallArtifact(step, .{}).step);
        const run = b.addRunArtifact(step);
        run.has_side_effects = no_cache;
        main_step.dependOn(&run.step);
    }

    {
        const main_step = b.step("unit-test", "Run unit tests.");

        var opts = getDefaultOptions();
        opts.trackGlobalRc = true;
        opts.applyOverrides();

        const step = try addUnitTest(b, opts);
        const run = b.addRunArtifact(step);
        run.has_side_effects = no_cache;
        main_step.dependOn(&run.step);
    }

    {
        const main_step = b.step("trace-test", "Run trace tests.");

        var opts = getDefaultOptions();
        opts.trackGlobalRc = true;
        opts.applyOverrides();

        const step = try addTraceTest(b, opts);
        const run = b.addRunArtifact(step);
        run.has_side_effects = no_cache;
        main_step.dependOn(&run.step);
    }

    {
        const main_step = b.step("test", "Run all tests.");

        var opts = getDefaultOptions();
        opts.trackGlobalRc = true;
        opts.applyOverrides();

        var step = try addUnitTest(b, opts);
        var run = b.addRunArtifact(step);
        run.has_side_effects = no_cache;
        main_step.dependOn(&run.step);

        step = try addBehaviorTest(b, opts);
        run = b.addRunArtifact(step);
        run.has_side_effects = no_cache;
        main_step.dependOn(&run.step);

        step = try addTraceTest(b, opts);
        run = b.addRunArtifact(step);
        run.has_side_effects = no_cache;
        main_step.dependOn(&run.step);
    }

    const printStep = b.allocator.create(PrintStep) catch unreachable;
    printStep.* = PrintStep.init(b, Version);
    b.step("version", "Get the short version.").dependOn(&printStep.step);
}

fn createAllModule(b: *std.Build, build_options: *std.Build.Module, stdx: *std.Build.Module, opts: Options) !*std.Build.Module {
    const mod = b.createModule(.{
        .root_source_file = b.path("src/all.zig"),
        .target = rtarget,
    });
    mod.addIncludePath(b.path("src"));
    try buildAndLinkDeps(mod, build_options, stdx, opts);
    return mod;
}

pub fn buildAndLinkDeps(mod: *std.Build.Module, build_options: *std.Build.Module, stdx: *std.Build.Module, opts: Options) !void {
    const b = mod.owner;

    mod.addIncludePath(b.path("src"));
    mod.addImport("build_options", build_options);
    mod.addImport("stdx", stdx);

    const vmc = b.addTranslateC(.{
        .root_source_file = b.path("src/vm.h"),
        .target = rtarget,
        .optimize = optimize,
    });
    const rt_safety = optimize == .Debug or optimize == .ReleaseSafe;
    vmc.defineCMacro("RT_SAFETY", if (rt_safety) "1" else "0");
    vmc.defineCMacro("DEBUG", if (optimize == .Debug) "1" else "0");
    vmc.defineCMacro("TRACK_GLOBAL_RC", if (opts.trackGlobalRc) "1" else "0");
    vmc.defineCMacro("TRACE", if (opts.trace) "1" else "0");
    vmc.defineCMacro("IS_32BIT", if (is32Bit(opts.target)) "1" else "0");
    vmc.defineCMacro("HAS_CYC", if (opts.cyc) "1" else "0");
    const vmc_mod = b.createModule(.{
        .root_source_file = vmc.getOutput(),
    });
    mod.addImport("vmc", vmc_mod);

    mod.link_libc = true;

    if (opts.malloc == .mimalloc) {
        const mimalloc = mimalloc_lib.createModule(b);
        mod.addImport("mimalloc", mimalloc);
        try mimalloc_lib.buildAndLink(b, mod, .{
            .target = rtarget,
            .optimize = optimize,
        });
    }

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

    if (opts.cli and target.os.tag != .windows and target.os.tag != .wasi) {
        const linenoise = createLinenoiseModule(b);
        mod.addImport("linenoise", linenoise);
        try buildAndLinkLinenoise(b, mod);
    } else {
        mod.addAnonymousImport("linenoise", .{
            .root_source_file = b.path("src/ln_stub.zig"),
        });
    }

    if (opts.jit) {
        // step.addIncludePath(.{ .path = "/opt/homebrew/Cellar/llvm/17.0.1/include" });
        // step.addLibraryPath(.{ .path = "/opt/homebrew/Cellar/llvm/17.0.1/lib" });
        // step.linkSystemLibrary("LLVM-17");
    }
}

pub const Options = struct {
    selinux: bool,
    trackGlobalRc: bool,
    trace: bool,
    log_mem: bool,
    target: std.Target,
    optimize: std.builtin.OptimizeMode,
    malloc: config.Allocator,
    static: bool,
    cyc: bool,
    ffi: bool,
    cli: bool,
    jit: bool,

    /// Link with the lib when building test root.
    /// Disable to see better Zig stack traces.
    link_test: bool,

    export_vmz: bool,

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
    var malloc: config.Allocator = undefined;
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
        .trackGlobalRc = optimize == .Debug,
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
        .link_test = link_test,
        .export_vmz = true,
    };
}

fn createBuildOptions(b: *std.Build, opts: Options) !*std.Build.Module {
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
    options.addOption(config.Allocator, "malloc", opts.malloc);
    options.addOption(config.Engine, "vmEngine", vmEngine);
    options.addOption(bool, "trace", opts.trace);
    options.addOption(bool, "log_mem", opts.log_mem);
    options.addOption(bool, "trackGlobalRC", opts.trackGlobalRc);
    options.addOption(bool, "is32Bit", is32Bit(opts.target));
    options.addOption(bool, "cyc", opts.cyc);
    options.addOption(bool, "ffi", opts.ffi);
    options.addOption(bool, "cli", opts.cli);
    options.addOption(bool, "jit", opts.jit);
    options.addOption(config.TestBackend, "testBackend", test_backend);
    options.addOption(bool, "link_test", opts.link_test);
    options.addOption(bool, "export_vmz", opts.export_vmz);
    return options.createModule();
}

fn addUnitTest(b: *std.Build, opts: Options) !*std.Build.Step.Compile {
    const root = b.createModule(.{
        .root_source_file = b.path("src/unit_test.zig"),
        .target = rtarget,
        .optimize = optimize,
    });
    const step = b.addTest(.{
        .name = "unit_test",
        .root_module = root,
        .filters = &.{testFilter orelse ""},
    });
    const build_options = try createBuildOptions(b, opts);
    const stdx = b.createModule(.{
        .root_source_file = b.path("src/stdx/stdx.zig"),
    });
    try buildAndLinkDeps(step.root_module, build_options, stdx, opts);
    return step;
}

fn addBehaviorTest(b: *std.Build, opts: Options) !*std.Build.Step.Compile {
    const root = b.createModule(.{
        .root_source_file = b.path("test/main_test.zig"),
        .target = rtarget,
        .optimize = optimize,
    });
    const step = b.addTest(.{
        .name = "test",
        .root_module = root,
        .filters = &.{testFilter orelse ""},
        .test_runner = .{
            .path = b.path("test/test_runner.zig"),
            .mode = .simple,
        },
    });

    // For linux FFI test.
    step.rdynamic = true;

    const build_options = try createBuildOptions(b, opts);
    step.root_module.addImport("build_options", build_options);

    const stdx = b.createModule(.{
        .root_source_file = b.path("src/stdx/stdx.zig"),
    });
    step.root_module.addImport("stdx", stdx);

    const all = try createAllModule(b, build_options, stdx, opts);
    step.root_module.addImport("all", all);

    if (opts.link_test) {
        const lib = try buildLib(b, opts);
        step.linkLibrary(lib);
    }
    return step;
}

fn addTraceTest(b: *std.Build, opts: Options) !*std.Build.Step.Compile {
    const root = b.createModule(.{
        .root_source_file = b.path("test/trace_test.zig"),
        .target = rtarget,
        .optimize = optimize,
    });
    const step = b.addTest(.{
        .name = "trace_test",
        .root_module = root,
        .filters = &.{testFilter orelse ""},
    });
    step.addIncludePath(b.path("src"));

    var opts_ = opts;
    opts_.trace = true;
    const lib = try buildLib(b, opts_);
    step.linkLibrary(lib);

    opts_.export_vmz = false;
    const build_options = try createBuildOptions(b, opts_);
    step.root_module.addImport("build_options", build_options);

    const stdx = b.createModule(.{
        .root_source_file = b.path("src/stdx/stdx.zig"),
    });
    step.root_module.addImport("stdx", stdx);

    const all = try createAllModule(b, build_options, stdx, opts_);
    step.root_module.addImport("all", all);

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
    if (optimize != .Debug) {
        lib.root_module.strip = true;
    }

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
    if (opts.trackGlobalRc) {
        try cflags.append(b.allocator, "-DTRACK_GLOBAL_RC=1");
    }
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
    lib.addCSourceFile(.{
        .file = b.path("src/vm.c"),
        .flags = cflags.items
    });
    for (isystem) |path| {
        lib.addSystemIncludePath(.{ .cwd_relative = path });
    }
    return lib;
}

fn buildLib(b: *std.Build, opts: Options) !*std.Build.Step.Compile {
    const root = b.createModule(.{
        .root_source_file = b.path("src/lib.zig"),
        .target = rtarget,
        .optimize = optimize,
    });
    const lib = b.addLibrary(.{
        .linkage = if (opts.static) .static else .dynamic,
        .name = "cyber",
        .root_module = root,
    });

    if (optimize != .Debug) {
        lib.root_module.strip = true;
    }

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

    const build_options = try createBuildOptions(b, opts);
    const stdx = b.createModule(.{
        .root_source_file = b.path("src/stdx/stdx.zig"),
    });
    try buildAndLinkDeps(lib.root_module, build_options, stdx, opts);
    return lib;
}

pub fn createLinenoiseModule(b: *std.Build) *std.Build.Module {
    const linenoise_ = b.addTranslateC(.{
        .root_source_file = b.path("lib/linenoise/linenoise.h"),
        .target = rtarget,
        .optimize = optimize,
    });
    const mod = b.createModule(.{
        .root_source_file = linenoise_.getOutput(),
    });
    mod.addIncludePath(b.path("lib/linenoise"));
    return mod;
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
