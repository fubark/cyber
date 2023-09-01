// Copyright (c) 2023 Cyber (See LICENSE)

/// Runtime memory management with ARC.

const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const log = stdx.log.scoped(.arc);
const cy = @import("cyber.zig");
const rt = cy.rt;

pub fn release(vm: *cy.VM, val: cy.Value) linksection(cy.HotSection) void {
    if (cy.TraceEnabled) {
        vm.trace.numReleaseAttempts += 1;
    }
    if (val.isPointer()) {
        const obj = val.asHeapObject();
        if (cy.TraceObjects) {
            checkDoubleFree(vm, obj);
        }
        obj.head.rc -= 1;
        if (builtin.mode == .Debug) {
            if (cy.verbose) {
                log.debug("release {} rc={}", .{val.getUserTag(), obj.head.rc});
            }
        }
        if (cy.TrackGlobalRC) {
            vm.refCounts -= 1;
        }
        if (cy.TraceEnabled) {
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

fn checkDoubleFree(vm: *cy.VM, obj: *cy.HeapObject) void {
    if (isObjectAlreadyFreed(vm, obj)) {
        const msg = std.fmt.allocPrint(vm.alloc, "Double free object: {*} at pc: {}({s})", .{
            obj, vm.debugPc, @tagName(vm.ops[vm.debugPc].opcode()),
        }) catch stdx.fatal();
        defer vm.alloc.free(msg);
        cy.debug.printTraceAtPc(vm, vm.debugPc, msg) catch stdx.fatal();

        cy.debug.dumpObjectTrace(vm, obj) catch stdx.fatal();
        stdx.fatal();
    }
}

pub fn releaseObject(vm: *cy.VM, obj: *cy.HeapObject) linksection(cy.HotSection) void {
    if (cy.TraceObjects) {
        checkDoubleFree(vm, obj);
    }
    obj.head.rc -= 1;
    if (builtin.mode == .Debug) {
        if (cy.verbose) {
            log.debug("release {} rc={}", .{obj.getUserTag(), obj.head.rc});
        }
    }
    if (cy.TrackGlobalRC) {
        vm.refCounts -= 1;
    }
    if (cy.TraceEnabled) {
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
    if (builtin.mode == .Debug) {
        checkRetainDanglingPointer(self, obj);
        if (cy.verbose) {
            log.debug("retain {} {}", .{obj.getUserTag(), obj.head.rc});
        }
    }
    if (cy.TrackGlobalRC) {
        self.refCounts += 1;
    }
    if (cy.TraceEnabled) {
        self.trace.numRetains += 1;
        self.trace.numRetainAttempts += 1;
    }
}

fn checkRetainDanglingPointer(vm: *cy.VM, obj: *cy.HeapObject) void {
    if (isObjectAlreadyFreed(vm, obj)) {
        stdx.panic("Retaining dangling pointer.");
    }
}

pub inline fn retain(self: *cy.VM, val: cy.Value) linksection(cy.HotSection) void {
    if (cy.TraceEnabled) {
        self.trace.numRetainAttempts += 1;
    }
    if (val.isPointer()) {
        const obj = val.asHeapObject();
        obj.head.rc += 1;
        if (builtin.mode == .Debug) {
            checkRetainDanglingPointer(self, obj);
            if (cy.verbose) {
                log.debug("retain {} {}", .{obj.getUserTag(), obj.head.rc});
            }
        }
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
        obj.head.rc += inc;
        if (builtin.mode == .Debug) {
            checkRetainDanglingPointer(self, obj);
            if (cy.verbose) {
                log.debug("retain {} {}", .{obj.getUserTag(), obj.head.rc});
            }
        }
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
    switch (obj.head.typeId) {
        rt.ListT => {
            const list = stdx.ptrAlignCast(*cy.List(cy.Value), &obj.list.list);
            list.deinit(self.alloc);
            cy.heap.freePoolObject(self, obj);
            if (cy.TrackGlobalRC) {
                self.refCounts -= obj.head.rc;
            }
        },
        rt.MapT => {
            const map = stdx.ptrAlignCast(*cy.MapInner, &obj.map.inner);
            map.deinit(self.alloc);
            cy.heap.freePoolObject(self, obj);
            if (cy.TrackGlobalRC) {
                self.refCounts -= obj.head.rc;
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

        if (builtin.mode == .Debug) {
            var buf: [256]u8 = undefined;
            var iter = vm.objectTraceMap.iterator();
            while (iter.next()) |it| {
                const trace = it.value_ptr.*;
                if (trace.freePc == cy.NullId) {
                    const typeName = vm.types.buf[it.key_ptr.*.head.typeId].name;
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
