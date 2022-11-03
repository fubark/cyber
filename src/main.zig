const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const mi = @import("mimalloc");
const cy = @import("cyber.zig");
const log = stdx.log.scoped(.main);

/// Trace collects debug info.
const Trace = false;

var gpa: std.heap.GeneralPurposeAllocator(.{ .enable_memory_limit = false }) = .{};

/// It seems having the vm in the .data section outperforms it on the stack at least for release-fast.
/// Since the eval hot loop just stays in the same stack frame, a VM allocated on the stack could be evicted more frequently from the cpu cache.
/// TODO: Check how it would perform if the VM was allocated on the heap right next to the vm stack.
var vm: cy.VM = undefined;

pub fn main() !void {
    // var miAlloc: mi.Allocator = undefined;
    // miAlloc.init();
    // defer miAlloc.deinit();
    // const alloc = miAlloc.allocator();

    const alloc = gpa.allocator();
    defer {
        if (builtin.mode == .Debug) {
            _ = gpa.deinit();
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
        const path = args[1];
        const src = try std.fs.cwd().readFileAlloc(alloc, path, 1e10);
        defer alloc.free(src);

        try vm.init(alloc);
        defer vm.deinit();

        var trace: cy.TraceInfo = undefined;
        vm.trace = &trace;
        const res = vm.eval(src, Trace) catch |err| {
            stdx.panicFmt("unexpected {}", .{err});
        };

        if (cy.Value.floatCanBeInteger(res.asF64())) {
            std.debug.print("{}\n", .{@floatToInt(u64, res.asF64())});
        } else {
            std.debug.print("{d:.10}\n", .{res.asF64()});
        }
    }
}