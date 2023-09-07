// Copyright (c) 2023 Cyber (See LICENSE)

/// Runtime memory management with ARC.

const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const log = cy.log.scoped(.arc);
const cy = @import("cyber.zig");
const rt = cy.rt;

pub fn release(vm: *cy.VM, val: cy.Value) linksection(cy.HotSection) void {
    if (cy.Trace) {
        vm.trace.numReleaseAttempts += 1;
    }
    if (val.isPointer()) {
        const obj = val.asHeapObject();
        if (cy.Trace) {
            checkDoubleFree(vm, obj);
        }
        obj.head.rc -= 1;
        log.tracev("release {} rc={}", .{val.getUserTag(), obj.head.rc});
        if (cy.TrackGlobalRC) {
            if (cy.Trace) {
                if (vm.refCounts == 0) {
                    cy.fmt.printStderr("Double free. {}\n", &.{cy.fmt.v(obj.head.typeId)});
                    cy.fatal();
                }
            }
            vm.refCounts -= 1;
        }
        if (cy.Trace) {
            vm.trace.numReleases += 1;
        }
        if (obj.head.rc == 0) {
            @call(.never_inline, cy.heap.freeObject, .{vm, obj});
        }
    }
}

pub fn isObjectAlreadyFreed(vm: *cy.VM, obj: *cy.HeapObject) bool {
    if (obj.head.typeId == cy.NullId) {
        // Can check structId for pool objects since they are still in memory.
        return true;
    }
    if (vm.objectTraceMap.get(obj)) |trace| {
        // For external objects check for trace entry.
        if (trace.freePc != cy.NullId) {
            return true;
        }
    }
    return false;
}

pub fn checkDoubleFree(vm: *cy.VM, obj: *cy.HeapObject) void {
    if (isObjectAlreadyFreed(vm, obj)) {
        const msg = std.fmt.allocPrint(vm.alloc, "Double free object: {*} at pc: {}({s})", .{
            obj, vm.debugPc, @tagName(vm.ops[vm.debugPc].opcode()),
        }) catch cy.fatal();
        defer vm.alloc.free(msg);
        cy.debug.printTraceAtPc(vm, vm.debugPc, msg) catch cy.fatal();

        cy.debug.dumpObjectTrace(vm, obj) catch cy.fatal();
        cy.fatal();
    }
}

pub fn releaseObject(vm: *cy.VM, obj: *cy.HeapObject) linksection(cy.HotSection) void {
    if (cy.Trace) {
        checkDoubleFree(vm, obj);
    }
    obj.head.rc -= 1;
    log.tracev("release {} rc={}", .{obj.getUserTag(), obj.head.rc});
    if (cy.TrackGlobalRC) {
        vm.refCounts -= 1;
    }
    if (cy.Trace) {
        vm.trace.numReleases += 1;
        vm.trace.numReleaseAttempts += 1;
    }
    if (obj.head.rc == 0) {
        @call(.never_inline, cy.heap.freeObject, .{vm, obj});
    }
}

pub fn runTempReleaseOps(vm: *cy.VM, stack: []const cy.Value, framePtr: usize, startPc: usize) void {
    if (vm.ops[startPc].opcode() == .release) {
        var pc = startPc;
        while (true) {
            const local = vm.ops[pc+1].val;
            // stack[framePtr + local].dump();
            release(vm, stack[framePtr + local]);
            pc += 2;
            if (vm.ops[pc].opcode() != .release) {
                break;
            }
        }
    }
}

pub fn runBlockEndReleaseOps(vm: *cy.VM, stack: []const cy.Value, framePtr: usize, startPc: usize) void {
    if (vm.ops[startPc].opcode() == .releaseN) {
        const numLocals = vm.ops[startPc+1].val;
        for (vm.ops[startPc+2..startPc+2+numLocals]) |local| {
            release(vm, stack[framePtr + local.val]);
        }
    }
}

pub inline fn retainObject(self: *cy.VM, obj: *cy.HeapObject) linksection(cy.HotSection) void {
    obj.head.rc += 1;
    if (cy.Trace) {
        checkRetainDanglingPointer(self, obj);
        if (cy.verbose) {
            log.debug("retain {} rc={}", .{obj.getUserTag(), obj.head.rc});
        }
    }
    if (cy.TrackGlobalRC) {
        self.refCounts += 1;
    }
    if (cy.Trace) {
        self.trace.numRetains += 1;
        self.trace.numRetainAttempts += 1;
    }
}

pub fn checkRetainDanglingPointer(vm: *cy.VM, obj: *cy.HeapObject) void {
    if (isObjectAlreadyFreed(vm, obj)) {
        cy.panic("Retaining dangling pointer.");
    }
}

pub inline fn retain(self: *cy.VM, val: cy.Value) linksection(cy.HotSection) void {
    if (cy.Trace) {
        self.trace.numRetainAttempts += 1;
    }
    if (val.isPointer()) {
        const obj = val.asHeapObject();
        obj.head.rc += 1;
        if (cy.Trace) {
            checkRetainDanglingPointer(self, obj);
            log.tracev("retain {} {}", .{obj.getUserTag(), obj.head.rc});
        }
        if (cy.TrackGlobalRC) {
            self.refCounts += 1;
        }
        if (cy.Trace) {
            self.trace.numRetains += 1;
        }
    }
}

pub inline fn retainInc(self: *cy.VM, val: cy.Value, inc: u32) linksection(cy.HotSection) void {
    if (cy.Trace) {
        self.trace.numRetainAttempts += inc;
    }
    if (val.isPointer()) {
        const obj = val.asHeapObject();
        obj.head.rc += inc;
        if (cy.Trace) {
            checkRetainDanglingPointer(self, obj);
            log.tracev("retain {} {}", .{obj.getUserTag(), obj.head.rc});
        }
        if (cy.TrackGlobalRC) {
            self.refCounts += inc;
        }
        if (cy.Trace) {
            self.trace.numRetains += inc;
        }
    }
}

pub fn forceRelease(self: *cy.VM, obj: *cy.HeapObject) void {
    if (cy.Trace) {
        self.trace.numForceReleases += 1;
    }
    switch (obj.head.typeId) {
        rt.ListT => {
            const list = cy.ptrAlignCast(*cy.List(cy.Value), &obj.list.list);
            list.deinit(self.alloc);
            cy.heap.freePoolObject(self, obj);
            if (cy.TrackGlobalRC) {
                self.refCounts -= obj.head.rc;
            }
        },
        rt.MapT => {
            const map = cy.ptrAlignCast(*cy.MapInner, &obj.map.inner);
            map.deinit(self.alloc);
            cy.heap.freePoolObject(self, obj);
            if (cy.TrackGlobalRC) {
                self.refCounts -= obj.head.rc;
            }
        },
        else => {
            return cy.panic("unsupported struct type");
        },
    }
}

pub fn getGlobalRC(self: *const cy.VM) usize {
    if (cy.TrackGlobalRC) {
        return self.refCounts;
    } else {
        cy.panic("Enable TrackGlobalRC.");
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
            if (obj.head.typeId != cy.NullId) {
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

            switch (obj.head.typeId) {
                rt.ListT => {
                    const list = cy.ptrAlignCast(*cy.List(cy.Value), &obj.list.list);
                    for (list.items()) |it| {
                        if (it.isPointer()) {
                            const ptr = it.asHeapObject();
                            if (visit(alloc, graph, cycleRoots_, ptr, graph.getPtr(ptr).?)) {
                                cycleRoots_.append(alloc, obj) catch cy.fatal();
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
            if (cy.Trace) {
                self.trace.numRetainCycles = 1;
                self.trace.numRetainCycleRoots = @intCast(cycleRoots.items.len);
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
    if (rc != vm.expGlobalRC) {
        std.debug.print("unreleased refcount: {}, expected: {}\n", .{rc, vm.expGlobalRC});

        if (cy.Trace) {
            var buf: [256]u8 = undefined;
            var iter = vm.objectTraceMap.iterator();
            while (iter.next()) |it| {
                const trace = it.value_ptr.*;
                if (trace.freePc == cy.NullId) {
                    const typeName = vm.getTypeName(it.key_ptr.*.head.typeId);
                    const msg = try std.fmt.bufPrint(&buf, "Init alloc: {*}, type: {s}, rc: {} at pc: {}\nval={s}", .{
                        it.key_ptr.*, typeName, it.key_ptr.*.head.rc, trace.allocPc,
                        vm.valueToTempString(cy.Value.initPtr(it.key_ptr.*)),
                    });
                    try cy.debug.printTraceAtPc(vm, trace.allocPc, msg);
                }
            }
        }

        return error.UnreleasedReferences;
    }
}
