const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const mi = @import("mimalloc");
const cy = @import("cyber.zig");
const log = stdx.log.scoped(.main);

/// Trace collects debug info.
const UseMimalloc = false;

var gpa: std.heap.GeneralPurposeAllocator(.{ .enable_memory_limit = false }) = .{};
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
        .none => {
            std.debug.print("Missing command\n", .{});
            std.os.exit(1);
        },
    }
}

const Command = enum {
    eval,
    compile,
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
    const buf = vm.compile(src) catch |err| {
        stdx.panicFmt("unexpected {}", .{err});
    };
    try buf.dump();
}

fn evalPath(alloc: std.mem.Allocator, path: []const u8, verbose: bool) !void {
    const src = try std.fs.cwd().readFileAlloc(alloc, path, 1e10);
    defer alloc.free(src);

    const vm = cy.getUserVM();
    try vm.init(alloc);
    defer vm.deinit();

    var trace: cy.TraceInfo = undefined;
    vm.setTrace(&trace);
    _ = vm.eval(src) catch |err| {
        if (err == error.Panic) {
            vm.dumpPanicStackTrace();
            std.os.exit(1);
        } else {
            stdx.panicFmt("unexpected {}", .{err});
        }
    };
    if (verbose) {
        std.debug.print("\n==VM Info==\n", .{});
        vm.dumpInfo();
    }
}