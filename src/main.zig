const std = @import("std");
const stdx = @import("stdx");
const mi = @import("mimalloc");
const cy = @import("cyber.zig");
const log = stdx.log.scoped(.main);

pub fn main() !void {
    // var miAlloc: mi.Allocator = undefined;
    // miAlloc.init();
    // defer miAlloc.deinit();
    // const alloc = miAlloc.allocator();

    const alloc = stdx.heap.getDefaultAllocator();
    defer stdx.heap.deinitDefaultAllocator();

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

        var vm: cy.VM = undefined;
        try vm.init(alloc);
        defer vm.deinit();

        var trace: cy.TraceInfo = undefined;
        vm.trace = &trace;
        const res = try vm.eval(src, false);

        if (cy.Value.floatCanBeInteger(res.asF64())) {
            std.debug.print("{d:.0}\n", .{@floatToInt(u64, res.asF64())});
        } else {
            std.debug.print("{d:.10}\n", .{res.asF64()});
        }
    }
}