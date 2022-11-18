const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const mi = @import("mimalloc");
const cy = @import("cyber.zig");
const log = stdx.log.scoped(.main);

/// Trace collects debug info.
const Trace = false;
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

    if (args.len > 1) {
        const arg0 = args[1];
        if (std.mem.eql(u8, arg0, "compile")) {
            if (args.len > 2) {
                try compilePath(alloc, args[2]);
            }
        } else {
            try evalPath(alloc, arg0);
        }
    }
}

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

fn evalPath(alloc: std.mem.Allocator, path: []const u8) !void {
    const src = try std.fs.cwd().readFileAlloc(alloc, path, 1e10);
    defer alloc.free(src);

    const vm = cy.getUserVM();
    try vm.init(alloc);
    defer vm.deinit();

    var trace: cy.TraceInfo = undefined;
    vm.setTrace(&trace);
    _ = vm.eval(src, Trace) catch |err| {
        stdx.panicFmt("unexpected {}", .{err});
    };
}