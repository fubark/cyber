// Copyright (c) 2023 Cyber (See LICENSE)

/// Runtime memory management with ARC.

const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const log = stdx.log.scoped(.arc);
const cy = @import("cyber.zig");

pub fn release(vm: *cy.VM, val: cy.Value) linksection(cy.HotSection) void {
    if (cy.TraceEnabled) {
        vm.trace.numReleaseAttempts += 1;
    }
    if (val.isPointer()) {
        const obj = val.asHeapObject();
        if (builtin.mode == .Debug) {
            if (obj.retainedCommon.structId == cy.NullId) {
                log.debug("object already freed. {*}", .{obj});
                cy.debug.dumpObjectTrace(vm, obj) catch stdx.fatal();
                stdx.fatal();
            }
        }
        obj.retainedCommon.rc -= 1;
        log.debug("release {} {}", .{val.getUserTag(), obj.retainedCommon.rc});
        if (cy.TrackGlobalRC) {
            vm.refCounts -= 1;
        }
        if (cy.TraceEnabled) {
            vm.trace.numReleases += 1;
        }
        if (obj.retainedCommon.rc == 0) {
            @call(.never_inline, cy.heap.freeObject, .{vm, obj});
        }
    }
}

pub fn releaseObject(vm: *cy.VM, obj: *cy.HeapObject) linksection(cy.HotSection) void {
    if (builtin.mode == .Debug or builtin.is_test) {
        if (obj.retainedCommon.structId == cy.NullId) {
            stdx.panic("object already freed.");
        }
    }
    obj.retainedCommon.rc -= 1;
    log.debug("release {} {}", .{obj.getUserTag(), obj.retainedCommon.rc});
    if (cy.TrackGlobalRC) {
        vm.refCounts -= 1;
    }
    if (cy.TraceEnabled) {
        vm.trace.numReleases += 1;
        vm.trace.numReleaseAttempts += 1;
    }
    if (obj.retainedCommon.rc == 0) {
        @call(.never_inline, cy.heap.freeObject, .{vm, obj});
    }
}

pub fn runReleaseOps(vm: *cy.VM, stack: []const cy.Value, framePtr: usize, startPc: usize) void {
    var pc = startPc;
    while (vm.ops[pc].code == .release) {
        const local = vm.ops[pc+1].arg;
        // stack[framePtr + local].dump();
        cy.arc.release(vm, stack[framePtr + local]);
        pc += 2;
    }
}

pub inline fn retainObject(self: *cy.VM, obj: *cy.HeapObject) linksection(cy.HotSection) void {
    obj.retainedCommon.rc += 1;
    log.debug("retain {} {}", .{obj.getUserTag(), obj.retainedCommon.rc});
    if (cy.TrackGlobalRC) {
        self.refCounts += 1;
    }
    if (cy.TraceEnabled) {
        self.trace.numRetains += 1;
        self.trace.numRetainAttempts += 1;
    }
}

pub inline fn retain(self: *cy.VM, val: cy.Value) linksection(cy.HotSection) void {
    if (cy.TraceEnabled) {
        self.trace.numRetainAttempts += 1;
    }
    if (val.isPointer()) {
        const obj = val.asHeapObject();
        obj.retainedCommon.rc += 1;
        log.debug("retain {} {}", .{obj.getUserTag(), obj.retainedCommon.rc});
        if (cy.TrackGlobalRC) {
            self.refCounts += 1;
        }
        if (cy.TraceEnabled) {
            self.trace.numRetains += 1;
        }
    }
}

pub inline fn retainInc(self: *cy.VM, val: cy.Value, inc: u32) linksection(cy.HotSection) void {
    if (cy.TraceEnabled) {
        self.trace.numRetainAttempts += inc;
    }
    if (val.isPointer()) {
        const obj = val.asHeapObject();
        obj.retainedCommon.rc += inc;
        log.debug("retain {} {}", .{obj.getUserTag(), obj.retainedCommon.rc});
        if (cy.TrackGlobalRC) {
            self.refCounts += inc;
        }
        if (cy.TraceEnabled) {
            self.trace.numRetains += inc;
        }
    }
}

pub fn forceRelease(self: *cy.VM, obj: *cy.HeapObject) void {
    if (cy.TraceEnabled) {
        self.trace.numForceReleases += 1;
    }
    switch (obj.retainedCommon.structId) {
        cy.ListS => {
            const list = stdx.ptrAlignCast(*cy.List(cy.Value), &obj.list.list);
            list.deinit(self.alloc);
            cy.heap.freePoolObject(self, obj);
            if (cy.TrackGlobalRC) {
                self.refCounts -= obj.retainedCommon.rc;
            }
        },
        cy.MapS => {
            const map = stdx.ptrAlignCast(*cy.MapInner, &obj.map.inner);
            map.deinit(self.alloc);
            cy.heap.freePoolObject(self, obj);
            if (cy.TrackGlobalRC) {
                self.refCounts -= obj.retainedCommon.rc;
            }
        },
        else => {
            return stdx.panic("unsupported struct type");
        },
    }
}

pub fn getGlobalRC(self: *const cy.VM) usize {
    if (cy.TrackGlobalRC) {
        return self.refCounts;
    } else {
        stdx.panic("Enable TrackGlobalRC.");
    }
}

/// Performs an iteration over the heap pages to check whether there are retain cycles.
pub fn checkMemory(self: *cy.VM) !bool {
    var nodes: std.AutoHashMapUnmanaged(*cy.HeapObject, RcNode) = .{};
    defer nodes.deinit(self.alloc);

    var cycleRoots: std.ArrayListUnmanaged(*cy.HeapObject) = .{};
    defer cycleRoots.deinit(self.alloc);

    // No concept of root vars yet. Just report any existing retained objects.
    // First construct the graph.
    for (self.heapPages.items()) |page| {
        for (page.objects[1..]) |*obj| {
            if (obj.common.structId != cy.NullId) {
                try nodes.put(self.alloc, obj, .{
                    .visited = false,
                    .entered = false,
                });
            }
        }
    }
    const S = struct {
        fn visit(alloc: std.mem.Allocator, graph: *std.AutoHashMapUnmanaged(*cy.HeapObject, RcNode), cycleRoots_: *std.ArrayListUnmanaged(*cy.HeapObject), obj: *cy.HeapObject, node: *RcNode) bool {
            if (node.visited) {
                return false;
            }
            if (node.entered) {
                return true;
            }
            node.entered = true;

            switch (obj.retainedCommon.structId) {
                cy.ListS => {
                    const list = stdx.ptrAlignCast(*cy.List(cy.Value), &obj.list.list);
                    for (list.items()) |it| {
                        if (it.isPointer()) {
                            const ptr = it.asHeapObject();
                            if (visit(alloc, graph, cycleRoots_, ptr, graph.getPtr(ptr).?)) {
                                cycleRoots_.append(alloc, obj) catch stdx.fatal();
                                return true;
                            }
                        }
                    }
                },
                else => {
                },
            }
            node.entered = false;
            node.visited = true;
            return false;
        }
    };
    var iter = nodes.iterator();
    while (iter.next()) |*entry| {
        if (S.visit(self.alloc, &nodes, &cycleRoots, entry.key_ptr.*, entry.value_ptr)) {
            if (cy.TraceEnabled) {
                self.trace.numRetainCycles = 1;
                self.trace.numRetainCycleRoots = @intCast(u32, cycleRoots.items.len);
            }
            for (cycleRoots.items) |root| {
                // Force release.
                forceRelease(self, root);
            }
            return false;
        }
    }
    return true;
}

const RcNode = struct {
    visited: bool,
    entered: bool,
};

pub fn checkGlobalRC(vm: *cy.VM) !void {
    const rc = getGlobalRC(vm);
    if (rc != 0) {
        std.debug.print("{} unreleased refcount\n", .{rc});
        var buf: [128]u8 = undefined;

        var iter = vm.objectTraceMap.iterator();
        while (iter.next()) |it| {
            const trace = it.value_ptr.*;
            if (trace.freePc == cy.NullId) {
                const msg = try std.fmt.bufPrint(&buf, "Init alloc: {*} at pc: {}, rc: {}", .{it.key_ptr.*, trace.allocPc, it.key_ptr.*.retainedCommon.rc});
                try cy.debug.printTraceAtPc(vm, trace.allocPc, msg);
            }
        }

        return error.UnreleasedReferences;
    }
}