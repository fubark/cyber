const std = @import("std");
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
var testBackend: config.TestBackend = undefined;
var trace: bool = undefined;
var log_mem: bool = undefined;
var no_cache: bool = undefined;
var link_test: bool = undefined;
var optFFI: ?bool = undefined; 
var optStatic: ?bool = undefined; 
var optJIT: ?bool = undefined;
var optRT: ?config.Runtime = undefined;

var rtarget: std.Build.ResolvedTarget = undefined;
var target: std.Target = undefined;
var optimize: std.builtin.OptimizeMode = undefined;
var dev: bool = undefined;

pub fn build(b: *std.Build) !void {
    rtarget = b.standardTargetOptions(.{});
    target = rtarget.result;
    optimize = b.standardOptimizeOption(.{});

    selinux = b.option(bool, "selinux", "Whether you are building on linux distro with selinux. eg. Fedora.") orelse false;
    testFilter = b.option([]const u8, "test-filter", "Test filter.");
    testBackend = b.option(config.TestBackend, "test-backend", "Test compiler backend.") orelse .vm;
    vmEngine = b.option(config.Engine, "vm", "Build with `zig` or `c` VM.") orelse .c;
    optMalloc = b.option(config.Allocator, "malloc", "Override default allocator: `malloc`, `mimalloc`, `zig`");
    optFFI = b.option(bool, "ffi", "Override default FFI: true, false");
    optStatic = b.option(bool, "static", "Override default lib build type: true=static, false=dynamic");
    trace = b.option(bool, "trace", "Enable tracing features.") orelse (optimize == .Debug);
    optJIT = b.option(bool, "jit", "Build with JIT.");
    optRT = b.option(config.Runtime, "rt", "Runtime.");
    link_test = b.option(bool, "link-test", "Build test by linking lib. Disable for better stack traces.") orelse true;
    log_mem = b.option(bool, "log-mem", "Log memory traces.") orelse false;
    no_cache = b.option(bool, "no-cache", "Disable caching when running tests.") orelse false;
    dev = b.option(bool, "dev", "Marks version as dev.") orelse true;

    {
        const step = b.step("cli", "Build main cli.");

        var opts = getDefaultOptions();
        opts.applyOverrides();

        const exe = b.addExecutable(.{
            .name = "cyber",
            .root_source_file = .{ .path = "src/main.zig" },
            .target = rtarget,
            .optimize = optimize,
        });
        if (optimize != .Debug) {
            // Sometimes there are issues with strip for ReleaseFast + wasi.
            if (opts.target.os.tag != .wasi) {
                exe.root_module.strip = true;
            }
        }
        exe.addIncludePath(.{ .path = thisDir() ++ "/src" });

        // Allow dynamic libraries to be loaded by filename in the cwd.
        if (target.os.tag == .linux) {
            exe.addRPath(.{ .path = ":$ORIGIN"});
            if (target.cpu.arch == .x86_64) {
                exe.addRPath(.{ .path = "/usr/lib/x86_64-linux-gnu"});
            }
        } else if (target.os.tag == .macos) {
            exe.addRPath(.{ .path = "@loader_path"});
        }

        // Allow exported symbols in exe to be visible to dlopen.
        exe.rdynamic = true;

        // step.dependOn(&b.addInstallFileWithDir(
        //     exe.getEmittedAsm(), .prefix, "cyber.s",
        // ).step);
        
        const build_options = try createBuildOptions(b, opts);
        const stdx = b.createModule(.{
            .root_source_file = .{ .path = thisDir() ++ "/src/stdx/stdx.zig" },
        });
        try buildAndLinkDeps(&exe.root_module, build_options, stdx, opts);  
        step.dependOn(&b.addInstallArtifact(exe, .{}).step);
    }

    {
        const step = b.step("vm-lib", "Build vm as a library.");

        const opts = getDefaultOptions();
        const obj = try buildCVM(b, opts);

        const lib = b.addStaticLibrary(.{
            .name = "vm",
            .target = rtarget,
            .optimize = optimize,
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
    //         .root_source_file = .{ .path = "src/runtime_static.zig" },
    //         .target = rtarget,
    //         .optimize = optimize,
    //     });
    //     if (optimize != .Debug) {
    //         lib.root_module.strip = true;
    //     }
    //     lib.addIncludePath(.{. path = thisDir() ++ "/src" });

    //     try buildAndLinkDeps(&lib.root_module, opts);
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
        step.dependOn(&b.addInstallArtifact(lib, .{}).step);
    }

    {
        const main_step = b.step("web-lib", "Build wasm lib for web.");

        var opts = getDefaultOptions();
        opts.ffi = false;
        opts.malloc = if (target.cpu.arch.isWasm()) .zig else .malloc;
        opts.cli = false;
        opts.applyOverrides();

        const lib = b.addExecutable(.{
            .name = "cyber-web",
            .root_source_file = .{ .path = "src/web.zig" },
            .target = rtarget,
            .optimize = optimize,
        });
        lib.entry = .disabled;
        if (optimize != .Debug) {
            // Disable to see better symbols when debugging wasm.
            lib.root_module.strip = true;
        }
        lib.addIncludePath(.{. path = thisDir() ++ "/src" });

        if (target.cpu.arch.isWasm()) {
            // Export table so non-exported functions can still be invoked from:
            // `instance.exports.__indirect_function_table`
            lib.export_table = true;
        }
        lib.rdynamic = true;

        const build_options = try createBuildOptions(b, opts);
        const stdx = b.createModule(.{
            .root_source_file = .{ .path = thisDir() ++ "/src/stdx/stdx.zig" },
        });

        try buildAndLinkDeps(&lib.root_module, build_options, stdx, opts);
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
        .root_source_file = .{ .path = thisDir() ++ "/src/all.zig" },
        .target = rtarget,
    });
    mod.addIncludePath(.{ .path = thisDir() ++ "/src" });
    try buildAndLinkDeps(mod, build_options, stdx, opts);
    return mod;
}

pub fn buildAndLinkDeps(mod: *std.Build.Module, build_options: *std.Build.Module, stdx: *std.Build.Module, opts: Options) !void {
    const b = mod.owner;

    mod.addIncludePath(.{ .path = thisDir() ++ "/src" });
    mod.addImport("build_options", build_options);
    mod.addImport("stdx", stdx);

    mod.link_libc = true;

    if (opts.malloc == .mimalloc) {
        const mimalloc = mimalloc_lib.createModule(b);
        mod.addImport("mimalloc", mimalloc);
        mimalloc_lib.buildAndLink(b, mod, .{
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
        const tcc = tcc_lib.createModule(b);
        tcc_lib.addImport(mod, "tcc", tcc);
        tcc_lib.buildAndLink(b, mod, .{
            .selinux = selinux,
            .target = rtarget,
            .optimize = optimize,
        });
    } else {
        mod.addAnonymousImport("tcc", .{
            .root_source_file = .{ .path = thisDir() ++ "/src/tcc_stub.zig" },
        });
        // Disable stack protector since the compiler isn't linking __stack_chk_guard/__stack_chk_fail.
        mod.stack_protector = false;
    }

    if (opts.cli and target.os.tag != .windows and target.os.tag != .wasi) {
        const linenoise = createLinenoiseModule(b);
        mod.addImport("linenoise", linenoise);
        buildAndLinkLinenoise(b, mod);
    } else {
        mod.addAnonymousImport("linenoise", .{
            .root_source_file = .{ .path = thisDir() ++ "/src/ln_stub.zig" },
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
    gc: bool,
    ffi: bool,
    cli: bool,
    jit: bool,
    rt: config.Runtime,

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
        .gc = true,
        .malloc = malloc,
        .static = true,
        .ffi = !target.cpu.arch.isWasm(),
        .cli = !target.cpu.arch.isWasm(),
        .jit = false,
        .rt = .vm,
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
        options.addOption([]const u8, "full_version", b.fmt("Cyber {s}-DEV build-{s}-{s}", .{Version, buildTag, commitTag}));
    } else {
        options.addOption([]const u8, "version", Version);
        options.addOption([]const u8, "full_version", b.fmt("Cyber {s} build-{s}-{s}", .{Version, buildTag, commitTag}));
    }
    options.addOption([]const u8, "build", buildTag);
    options.addOption([]const u8, "commit", commitTag);
    options.addOption(config.Allocator, "malloc", opts.malloc);
    options.addOption(config.Engine, "vmEngine", vmEngine);
    options.addOption(bool, "trace", opts.trace);
    options.addOption(bool, "log_mem", opts.log_mem);
    options.addOption(bool, "trackGlobalRC", opts.trackGlobalRc);
    options.addOption(bool, "is32Bit", is32Bit(opts.target));
    options.addOption(bool, "gc", opts.gc);
    options.addOption(bool, "ffi", opts.ffi);
    options.addOption(bool, "cli", opts.cli);
    options.addOption(bool, "jit", opts.jit);
    options.addOption(config.TestBackend, "testBackend", testBackend);
    options.addOption(config.Runtime, "rt", opts.rt);
    options.addOption(bool, "link_test", opts.link_test);
    options.addOption(bool, "export_vmz", opts.export_vmz);
    return options.createModule();
}

fn addUnitTest(b: *std.Build, opts: Options) !*std.Build.Step.Compile {
    const step = b.addTest(.{
        .name = "unit_test",
        .root_source_file = .{ .path = "./src/unit_test.zig" },
        .target = rtarget,
        .optimize = optimize,
        .filter = testFilter,
    });
    const build_options = try createBuildOptions(b, opts);
    const stdx = b.createModule(.{
        .root_source_file = .{ .path = thisDir() ++ "/src/stdx/stdx.zig" },
    });
    try buildAndLinkDeps(&step.root_module, build_options, stdx, opts);
    return step;
}

fn addBehaviorTest(b: *std.Build, opts: Options) !*std.Build.Step.Compile {
    const step = b.addTest(.{
        .name = "test",
        .root_source_file = .{ .path = "./test/main_test.zig" },
        .target = rtarget,
        .optimize = optimize,
        .filter = testFilter,
    });

    // For linux FFI test.
    step.rdynamic = true;

    const build_options = try createBuildOptions(b, opts);
    step.root_module.addImport("build_options", build_options);

    const stdx = b.createModule(.{
        .root_source_file = .{ .path = thisDir() ++ "/src/stdx/stdx.zig" },
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
    const step = b.addTest(.{
        .name = "trace_test",
        .root_source_file = .{ .path = "./test/trace_test.zig" },
        .optimize = optimize,
        .target = rtarget,
        .filter = testFilter,
    });
    step.addIncludePath(.{ .path = thisDir() ++ "/src" });

    var opts_ = opts;
    opts_.trace = true;
    const lib = try buildLib(b, opts_);
    step.linkLibrary(lib);

    opts_.export_vmz = false;
    const build_options = try createBuildOptions(b, opts_);
    step.root_module.addImport("build_options", build_options);

    const stdx = b.createModule(.{
        .root_source_file = .{ .path = thisDir() ++ "/src/stdx/stdx.zig" },
    });
    step.root_module.addImport("stdx", stdx);

    const all = try createAllModule(b, build_options, stdx, opts_);
    step.root_module.addImport("all", all);

    return step;
}

fn is32Bit(target_: std.Target) bool {
    switch (target_.cpu.arch) {
        .wasm32,
        .x86 => return true,
        else => return false,
    }
}

inline fn thisDir() []const u8 {
    return comptime std.fs.path.dirname(@src().file) orelse unreachable;
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

    fn make(step: *std.Build.Step, _: *std.Progress.Node) anyerror!void {
        const self: *PrintStep = @fieldParentPtr("step", step);
        std.io.getStdOut().writer().writeAll(self.str) catch unreachable;
    }
};

pub fn buildCVM(b: *std.Build, opts: Options) !*std.Build.Step.Compile {
    const lib = b.addObject(.{
        .name = "vm",
        .target = rtarget,
        .optimize = optimize,
    });
    lib.linkLibC();
    if (optimize != .Debug) {
        lib.root_module.strip = true;
    }

    var cflags = std.ArrayList([]const u8).init(b.allocator);
    const rt_safety = optimize == .Debug or optimize == .ReleaseSafe;
    if (rt_safety) {
        try cflags.append("-DRT_SAFETY=1");
    } else {
        try cflags.append("-DRT_SAFETY=0");
    }
    if (optimize == .Debug) {
        try cflags.append("-DDEBUG=1");
    } else {
        try cflags.append("-DDEBUG=0");
    }
    try cflags.append("-DCGOTO=1");
    if (opts.trackGlobalRc) {
        try cflags.append("-DTRACK_GLOBAL_RC=1");
    }
    if (opts.trace) {
        try cflags.append("-DTRACE=1");
    } else {
        try cflags.append("-DTRACE=0");
    }
    if (opts.log_mem) {
        try cflags.append("-DLOG_MEM=1");
    } else {
        try cflags.append("-DLOG_MEM=0");
    }
    if (is32Bit(opts.target)) {
        try cflags.append("-DIS_32BIT=1");
    } else {
        try cflags.append("-DIS_32BIT=0");
    }
    if (opts.gc) {
        try cflags.append("-DHAS_GC=1");
    } else {
        try cflags.append("-DHAS_GC=0");
    }

    // Disable traps for arithmetic/bitshift overflows.
    // Note that changing this alone doesn't clear the build cache.
    lib.root_module.sanitize_c = false;

    lib.addIncludePath(.{ .path = thisDir() ++ "/src"});
    lib.addCSourceFile(.{
        .file = .{ .path = thisDir() ++ "/src/vm.c" },
        .flags = cflags.items
    });
    return lib;
}

fn buildLib(b: *std.Build, opts: Options) !*std.Build.Step.Compile {
    var lib: *std.Build.Step.Compile = undefined;
    if (opts.static) {
        lib = b.addStaticLibrary(.{
            .name = "cyber",
            .root_source_file = .{ .path = "src/lib.zig" },
            .target = rtarget,
            .optimize = optimize,
        });
    } else {
        lib = b.addSharedLibrary(.{
            .name = "cyber",
            .root_source_file = .{ .path = "src/lib.zig" },
            .target = rtarget,
            .optimize = optimize,
        });
    }

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
        .root_source_file = .{ .path = thisDir() ++ "/src/stdx/stdx.zig" },
    });
    try buildAndLinkDeps(&lib.root_module, build_options, stdx, opts);
    return lib;
}

pub fn createLinenoiseModule(b: *std.Build) *std.Build.Module {
    const mod = b.createModule(.{
        .root_source_file = .{ .path = thisDir() ++ "/lib/linenoise/linenoise.zig" },
    });
    mod.addIncludePath(.{ .path = thisDir() ++ "/lib/linenoise" });
    return mod;
}

pub fn buildAndLinkLinenoise(b: *std.Build, mod: *std.Build.Module) void {
    const lib = b.addStaticLibrary(.{
        .name = "linenoise",
        .target = rtarget,
        .optimize = optimize,
    });
    lib.linkLibC();
    // lib.disable_sanitize_c = true;

    const c_flags = std.ArrayList([]const u8).init(b.allocator);

    var sources = std.ArrayList([]const u8).init(b.allocator);
    sources.appendSlice(&.{
        "/lib/linenoise/linenoise.c",
    }) catch @panic("error");
    for (sources.items) |src| {
        lib.addCSourceFile(.{
            .file = .{ .path = b.fmt("{s}{s}", .{thisDir(), src}) },
            .flags = c_flags.items,
        });
    }
    mod.linkLibrary(lib);
}
