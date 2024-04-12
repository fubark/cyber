const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const cy = @import("cyber.zig");
const c = @import("capi.zig");
const log = cy.log.scoped(.main);
const cli = @import("cli.zig");
const build_options = @import("build_options");
const fmt = @import("fmt.zig");
comptime {
    const lib = @import("lib.zig");
    std.testing.refAllDecls(lib);
}

var verbose = false;
var reload = false;
var backend: c.Backend = c.BackendVM;
var dumpStats = false; // Only for trace build.
var pc: ?u32 = null;

const CP_UTF8 = 65001;
var prevWinConsoleOutputCP: u32 = undefined;

// Default VM.
var vm: cy.VM = undefined;

pub fn main() !void {
    if (builtin.os.tag == .windows) {
        prevWinConsoleOutputCP = std.os.windows.kernel32.GetConsoleOutputCP();
        _ = std.os.windows.kernel32.SetConsoleOutputCP(CP_UTF8);
    }
    defer {
        if (builtin.os.tag == .windows) {
            _ = std.os.windows.kernel32.SetConsoleOutputCP(prevWinConsoleOutputCP);
        }
    }

    const alloc = cy.heap.getAllocator();
    defer cy.heap.deinitAllocator();

    const args = try std.process.argsAlloc(alloc);
    defer std.process.argsFree(alloc, args);

    var cmd = Command.none;
    var arg0: ?[]const u8 = null;

    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (arg[0] == '-') {
            if (std.mem.eql(u8, arg, "-v")) {
                verbose = true;
            } else if (std.mem.eql(u8, arg, "-r")) {
                reload = true;
            } else if (std.mem.eql(u8, arg, "-vm")) {
                backend = c.BackendVM;
            } else if (std.mem.eql(u8, arg, "-cc")) {
                backend = c.BackendCC;
            } else if (std.mem.eql(u8, arg, "-tcc")) {
                backend = c.BackendTCC;
            } else if (std.mem.eql(u8, arg, "-jit")) {
                backend = c.BackendJIT;
            } else if (std.mem.eql(u8, arg, "-pc")) {
                i += 1;
                if (i < args.len) {
                    pc = try std.fmt.parseInt(u32, args[i], 10);
                } else {
                    std.debug.print("Missing pc arg.\n", .{});
                    exit(1);
                }
            } else {
                if (cy.Trace) {
                    if (std.mem.eql(u8, arg, "-stats")) {
                        dumpStats = true;
                        continue;
                    }
                }
                // Ignore unrecognized options so a script can use them.
            }
        } else {
            if (cmd == .none) {
                if (std.mem.eql(u8, arg, "compile")) {
                    cmd = .compile;
                } else if (std.mem.eql(u8, arg, "version")) {
                    cmd = .version;
                } else if (std.mem.eql(u8, arg, "help")) {
                    cmd = .help;
                } else {
                    cmd = .eval;
                    if (arg0 == null) {
                        arg0 = arg;
                        break;
                    }
                }
            } else {
                if (arg0 == null) {
                    arg0 = arg;
                    break;
                }
            }
        }
    }

    switch (cmd) {
        .eval => {
            if (arg0) |path| {
                try evalPath(alloc, path);
            } else {
                return error.MissingFilePath;
            }
        },
        .compile => {
            if (arg0) |path| {
                try compilePath(alloc, path);
            } else {
                return error.MissingFilePath;
            }
        },
        .help => {
            help();
        },
        .version => {
            version();
        },
        .none => {
            help();
        },
    }
}

fn exit(code: u8) noreturn {
    if (builtin.os.tag == .windows) {
        _ = std.os.windows.kernel32.SetConsoleOutputCP(prevWinConsoleOutputCP);
    }
    std.os.exit(code);
}

const Command = enum {
    eval,
    compile,
    help,
    version,
    none,
};

fn compilePath(alloc: std.mem.Allocator, path: []const u8) !void {
    c.setVerbose(verbose);

    try vm.init(alloc);
    cli.csSetupForCLI(@ptrCast(&vm));
    defer vm.deinit(false);

    var config = c.defaultCompileConfig();
    config.single_run = builtin.mode == .ReleaseFast;
    config.file_modules = true;
    config.gen_debug_func_markers = true;
    config.backend = backend;
    _ = vm.compile(path, null, config) catch |err| {
        if (err == error.CompileError) {
            if (!c.silent()) {
                const report = c.newErrorReportSummary(@ptrCast(&vm));
                defer c.freeStr(@ptrCast(&vm), report);
                cy.rt.writeStderr(c.fromStr(report));
            }
            exit(1);
        } else {
            fmt.panic("unexpected {}\n", &.{fmt.v(err)});
        }
    };
    try cy.debug.dumpBytecode(&vm, .{ .pcContext = pc });
}

fn evalPath(alloc: std.mem.Allocator, path: []const u8) !void {
    c.setVerbose(verbose);

    try vm.init(alloc);
    cli.csSetupForCLI(@ptrCast(&vm));
    defer vm.deinit(false);

    var config = c.defaultEvalConfig();
    config.single_run = builtin.mode == .ReleaseFast;
    config.file_modules = true;
    config.reload = reload;
    config.backend = backend;
    config.spawn_exe = true;
    _ = vm.eval(path, null, config) catch |err| {
        switch (err) {
            error.Panic => {
                if (!c.silent()) {
                    const report = c.newPanicSummary(@ptrCast(&vm));
                    defer c.freeStr(@ptrCast(&vm), report);
                    try std.io.getStdErr().writeAll(c.fromStr(report));
                }
            },
            error.CompileError => {
                if (!c.silent()) {
                    const report = c.newErrorReportSummary(@ptrCast(&vm));
                    defer c.freeStr(@ptrCast(&vm), report);
                    try std.io.getStdErr().writeAll(c.fromStr(report));
                }
            },
            else => {
                std.debug.print("unexpected {}\n", .{err});
            },
        }
        if (builtin.mode == .Debug) {
            // Report error trace.
            return err;
        } else {
            // Exit early.
            exit(1);
        }
    };
    if (verbose) {
        std.debug.print("\n==VM Info==\n", .{});
        try vm.dumpInfo();
    }
    if (cy.Trace and dumpStats) {
        vm.dumpStats();
    }
    if (cy.TrackGlobalRC) {
        vm.deinitRtObjects();
        vm.compiler.deinitModRetained();
        try cy.arc.checkGlobalRC(&vm);
    }
}

fn help() void {
    std.debug.print(
        \\Cyber {s}
        \\
        \\Usage: cyber [command?] [options] [source]
        \\
        \\Commands:
        \\  cyber [source]          Compile and run.
        \\  cyber compile [source]  Compile and dump the code.
        \\  cyber help              Print usage.
        \\  cyber version           Print version number.
        \\
        \\Backend options:
        \\  -vm     Compile to bytecode. (Default)
        \\          Fast build, slow perf.
        \\  -cc     Compile to C with optimizations. Experimental.
        \\          Slow build, fast perf.
        \\  -tcc    Compile to C with builtin TinyC compiler. Experimental.
        \\          Mid build, mid perf.
        \\  -jit    Compile to machine code based on bytecode. Experimental.
        \\          Fast build, mid perf.
        \\
        \\General options:
        \\  -r      Refetch url imports and cached assets.
        \\  -v      Verbose.
        \\                            
        \\`cyber compile` options:
        \\  -pc     Next arg is the pc to dump detailed bytecode at.
        \\
    , .{build_options.version});
}

fn version() void {
    std.debug.print("{s}\n", .{build_options.full_version});
}

pub fn panic(msg: []const u8, errRetTrace: ?*std.builtin.StackTrace, _: ?usize) noreturn {
    const ret = @returnAddress();
    // TODO: Print something useful if caused by script execution.
    std.debug.panicImpl(errRetTrace, ret, msg);
}