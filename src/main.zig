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

pub fn main() !void {
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
    var arg0: []const u8 = "";
    var verbose = false;
    for (args[1..]) |arg| {
        if (arg[0] == '-') {
            if (std.mem.eql(u8, arg, "-v")) {
                verbose = true;
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
                    arg0 = arg;
                }

            } else {
                arg0 = arg;
            }
        }
    }

    switch (cmd) {
        .eval => {
            try evalPath(alloc, arg0, verbose);
        },
        .compile => {
            try compilePath(alloc, arg0);
        },
        .help => {
            help();
        },
        .version => {
            version();
        },
        .none => {
            help();
            std.os.exit(1);
        },
    }
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
    const buf = vm.compile(path, src) catch |err| {
        switch (err) {
            error.TokenError,
            error.ParseError,
            error.CompileError => {
                std.os.exit(1);
            },
            else => {
                fmt.panic("unexpected {}\n", &.{fmt.v(err)});
            },
        }
    };
    try buf.dump();
}

fn evalPath(alloc: std.mem.Allocator, path: []const u8, verbose: bool) !void {
    const src = try std.fs.cwd().readFileAllocOptions(alloc, path, 1e10, 4096, @alignOf(u8), null);
    defer alloc.free(src);

    const vm = cy.getUserVM();
    try vm.init(alloc);
    defer vm.deinit();

    var trace: cy.TraceInfo = undefined;
    vm.setTrace(&trace);
    _ = vm.eval(path, src) catch |err| {
        switch (err) {
            error.Panic => {
                try vm.dumpPanicStackTrace();
                std.os.exit(1);
            },
            error.TokenError,
            error.ParseError,
            error.CompileError => {
                std.os.exit(1);
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
        \\  cyber [source]             Compile and run a script.
        \\  cyber compile [source]     Compile script and dump the bytecode.
        \\  cyber help                 Print usage.
        \\  cyber version              Print version number.
        \\  
        \\General Options:
        \\  -v      Verbose.
        \\
    , .{build_options.version});
}

fn version() void {
    std.debug.print("Cyber {s} build-{s}-{s}\n", .{build_options.version, build_options.build, build_options.commit});
}