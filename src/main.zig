const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const mi = @import("mimalloc");
const cy = @import("cyber.zig");
const log = stdx.log.scoped(.main);
const build_options = @import("build_options");
const TraceEnabled = build_options.trace;
const fmt = @import("fmt.zig");

/// Use mimalloc for fast builds.
const UseMimalloc = builtin.mode == .ReleaseFast;

var gpa: std.heap.GeneralPurposeAllocator(.{
    .enable_memory_limit = false,
    .stack_trace_frames = if (builtin.mode == .Debug) 10 else 0,
}) = .{};
var miAlloc: mi.Allocator = undefined;

var verbose = false;
var reload = false;
var pc: ?u32 = null;

const CP_UTF8 = 65001;
var prevWinConsoleOutputCP: u32 = undefined;

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

    if (UseMimalloc) {
        miAlloc.init();
    }
    const alloc = if (UseMimalloc) miAlloc.allocator() else gpa.allocator();
    defer {
        if (builtin.mode == .Debug) {
            if (UseMimalloc) {
                miAlloc.deinit();
            } else {
                _ = gpa.deinit();
            }
        }
    }

    // var traceAlloc: stdx.heap.TraceAllocator = undefined;
    // traceAlloc.init(miAlloc.allocator());
    // traceAlloc.init(child);
    // defer traceAlloc.dump();
    // const alloc = traceAlloc.allocator();

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
            } else if (std.mem.eql(u8, arg, "-pc")) {
                i += 1;
                if (i < args.len) {
                    pc = try std.fmt.parseInt(u32, args[i], 10);
                } else {
                    std.debug.print("Missing pc arg.\n", .{});
                    exit(1);
                }
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
                    }
                }

            } else {
                if (arg0 == null) {
                    arg0 = arg;
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
            exit(1);
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
    const src = try std.fs.cwd().readFileAlloc(alloc, path, 1e10);
    defer alloc.free(src);

    const vm = cy.getUserVM();
    try vm.init(alloc);
    defer vm.deinit();

    var trace: cy.TraceInfo = undefined;
    vm.setTrace(&trace);
    _ = vm.compile(path, src) catch |err| {
        switch (err) {
            error.TokenError,
            error.ParseError,
            error.CompileError => {
                exit(1);
            },
            else => {
                fmt.panic("unexpected {}\n", &.{fmt.v(err)});
            },
        }
    };
    try cy.debug.dumpBytecode(vm.constInternal(), pc);
}

fn evalPath(alloc: std.mem.Allocator, path: []const u8) !void {
    const src = try std.fs.cwd().readFileAllocOptions(alloc, path, 1e10, 4096, @alignOf(u8), null);
    defer alloc.free(src);

    const vm = cy.getUserVM();
    try vm.init(alloc);
    defer vm.deinit();

    var trace: cy.TraceInfo = undefined;
    vm.setTrace(&trace);
    _ = vm.eval(path, src, .{
        .singleRun = builtin.mode == .ReleaseFast,
        .enableFileModules = true,
        .reload = reload,
    }) catch |err| {
        switch (err) {
            error.Panic => {
                try vm.printLastUserPanicError();
                exit(1);
            },
            error.TokenError,
            error.ParseError,
            error.CompileError => {
                exit(1);
            },
            else => {
                fmt.panic("unexpected {}\n", &.{fmt.v(err)});
            },
        }
    };
    if (verbose) {
        std.debug.print("\n==VM Info==\n", .{});
        vm.dumpInfo();
        if (TraceEnabled) {
            vm.dumpStats();
        }
    }
}

fn help() void {
    std.debug.print(
        \\Cyber {s}
        \\
        \\Usage: cyber [source]
        \\       cyber [command] ...
        \\
        \\Commands:
        \\  cyber [source]            Compile and run a script.
        \\  cyber compile [source]    Compile script and dump the bytecode.
        \\  cyber help                Print usage.
        \\  cyber version             Print version number.
        \\  
        \\General Options:
        \\  -r      Refetch url imports and cached assets.
        \\  -v      Verbose.
        \\                            
        \\cyber compile Options:
        \\  -pc     Next arg is the pc to dump bytecode at.
        \\
    , .{build_options.version});
}

fn version() void {
    std.debug.print("Cyber {s} build-{s}-{s}\n", .{build_options.version, build_options.build, build_options.commit});
}