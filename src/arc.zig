// Copyright (c) 2023 Cyber (See LICENSE)

/// Runtime memory management with ARC.

const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
pub const log = cy.log.scoped(.arc);
const cy = @import("cyber.zig");
const c = @import("capi.zig");
const vmc = cy.vmc;
const rt = cy.rt;
const bt = cy.types.BuiltinTypes;
const build_options = @import("build_options");
const log_mem = build_options.log_mem;

pub fn release(vm: *cy.VM, val: cy.Value) void {
    if (cy.Trace) {
        vm.c.trace.numReleaseAttempts += 1;
    }
    if (val.isPointer()) {
        const obj = val.asHeapObject();
        if (cy.TraceRC) {
            log.tracevIf(log_mem, "{} -1 release: {s}, {*}", .{obj.head.rc, vm.getTypeName(val.getTypeId()), obj});
        }
        if (cy.Trace) {
            checkDoubleFree(vm, obj);
        }
        obj.head.rc -= 1;
        if (cy.TrackGlobalRC) {
            if (cy.Trace) {
                if (vm.c.refCounts == 0) {
                    rt.errFmt(vm, "Double free. {}\n", &.{cy.fmt.v(obj.getTypeId())});
                    cy.fatal();
                }
            }
            vm.c.refCounts -= 1;
        }
        if (cy.Trace) {
            vm.c.trace.numReleases += 1;
        }
        if (obj.head.rc == 0) {
            // Free children and the object.
            @call(.never_inline, cy.heap.freeObject, .{vm, obj, false});

            if (cy.Trace) {
                if (vm.countFrees) {
                    vm.numFreed += 1;
                }
            }
        }
    } else {
        log.tracevIf(log_mem, "release: {s}, nop", .{vm.getTypeName(val.getTypeId())});
    }
}

pub fn isObjectAlreadyFreed(vm: *cy.VM, obj: *cy.HeapObject) bool {
    if (obj.isFreed()) {
        // Can check structId for pool objects since they are still in memory.
        return true;
    }
    if (vm.objectTraceMap.get(obj)) |trace| {
        // For external objects check for trace entry.
        if (trace.free_pc != null) {
            return true;
        }
    }
    return false;
}

pub fn checkDoubleFree(vm: *cy.VM, obj: *cy.HeapObject) void {
    if (isObjectAlreadyFreed(vm, obj)) {
        const msg = std.fmt.allocPrint(vm.alloc, "{*} at pc: {}({s})", .{
            obj, vm.c.debugPc, @tagName(vm.c.ops[vm.c.debugPc].opcode()),
        }) catch cy.fatal();
        defer vm.alloc.free(msg);
        cy.debug.printTraceAtPc(vm, vm.c.debugPc, "double free", msg) catch cy.fatal();

        cy.debug.dumpObjectTrace(vm, obj) catch cy.fatal();
        cy.fatal();
    }
}

pub fn releaseObject(vm: *cy.VM, obj: *cy.HeapObject) void {
    if (cy.Trace) {
        checkDoubleFree(vm, obj);
    }
    if (cy.TraceRC) {
        log.tracevIf(log_mem, "{} -1 release: {s}", .{obj.head.rc, vm.getTypeName(obj.getTypeId())});
    }
    obj.head.rc -= 1;
    if (cy.TrackGlobalRC) {
        vm.c.refCounts -= 1;
    }
    if (cy.Trace) {
        vm.c.trace.numReleases += 1;
        vm.c.trace.numReleaseAttempts += 1;
    }
    if (obj.head.rc == 0) {
        @call(.never_inline, cy.heap.freeObject, .{vm, obj, false});
    }
}

pub fn runUnwindReleases(vm: *cy.VM, fp: [*]const cy.Value, key: cy.fiber.UnwindKey) void {
    log.tracev("unwind releases", .{});
    var cur = key;
    while (!cur.is_null) {
        if (cur.is_try) {
            const try_e = vm.unwind_trys[cur.idx];
            cur = try_e.prev;
        } else {
            log.tracev("unwind release: {}", .{vm.unwind_slots[cur.idx]});
            release(vm, fp[vm.unwind_slots[cur.idx]]);
            cur = vm.unwind_slot_prevs[cur.idx];
        }
    }
}

pub fn runUnwindReleasesUntilCatch(vm: *cy.VM, fp: [*]const cy.Value, key: cy.fiber.UnwindKey) ?u32 {
    log.tracev("unwind releases until catch", .{});
    var cur = key;
    while (!cur.is_null) {
        if (cur.is_try) {
            const try_e = vm.unwind_trys[cur.idx];
            return try_e.catch_pc;
        } else {
            log.tracev("unwind release: {}", .{vm.unwind_slots[cur.idx]});
            release(vm, fp[vm.unwind_slots[cur.idx]]);
            cur = vm.unwind_slot_prevs[cur.idx];
        }
    }
    return null;
}

pub inline fn retainObject(self: *cy.VM, obj: *cy.HeapObject) void {
    obj.head.rc += 1;
    if (cy.Trace) {
        checkRetainDanglingPointer(self, obj);
        log.tracevIf(log_mem, "{} +1 retain: {s}", .{obj.head.rc, self.getTypeName(obj.getTypeId())});
    }
    if (cy.TrackGlobalRC) {
        self.c.refCounts += 1;
    }
    if (cy.Trace) {
        self.c.trace.numRetains += 1;
        self.c.trace.numRetainAttempts += 1;
    }
}

const Root = @This();

pub const VmExt = struct {
    pub const retainObject = Root.retainObject;
    pub const retain = Root.retain;
    pub const releaseObject = Root.releaseObject;
    pub const release = Root.release;
};

pub fn checkRetainDanglingPointer(vm: *cy.VM, obj: *cy.HeapObject) void {
    if (isObjectAlreadyFreed(vm, obj)) {
        const msg = std.fmt.allocPrint(vm.alloc, "{*} at pc: {}({s})", .{
            obj, vm.c.debugPc, @tagName(vm.c.ops[vm.c.debugPc].opcode()),
        }) catch cy.fatal();
        defer vm.alloc.free(msg);
        cy.debug.printTraceAtPc(vm, vm.c.debugPc, "retain dangling ptr", msg) catch cy.fatal();

        if (cy.Trace) {
            cy.debug.dumpObjectTrace(vm, obj) catch cy.fatal();
        }
        cy.fatal();
    }
}

pub inline fn retain(self: *cy.VM, val: cy.Value) void {
    if (cy.Trace) {
        self.c.trace.numRetainAttempts += 1;
    }
    if (val.isPointer()) {
        const obj = val.asHeapObject();
        if (cy.Trace) {
            checkRetainDanglingPointer(self, obj);
            log.tracevIf(log_mem, "{} +1 retain: {s}", .{obj.head.rc, self.getTypeName(obj.getTypeId())});
        }
        obj.head.rc += 1;
        if (cy.TrackGlobalRC) {
            self.c.refCounts += 1;
        }
        if (cy.Trace) {
            self.c.trace.numRetains += 1;
        }
    }
}

pub inline fn retainInc(self: *cy.VM, val: cy.Value, inc: u32) void {
    if (cy.Trace) {
        self.c.trace.numRetainAttempts += inc;
    }
    if (val.isPointer()) {
        const obj = val.asHeapObject();
        if (cy.Trace) {
            checkRetainDanglingPointer(self, obj);
            log.tracevIf(log_mem, "{} +{} retain: {s}", .{obj.head.rc, inc, self.getTypeName(obj.getTypeId())});
        }
        obj.head.rc += inc;
        if (cy.TrackGlobalRC) {
            self.c.refCounts += inc;
        }
        if (cy.Trace) {
            self.c.trace.numRetains += inc;
        }
    }
}

pub fn getGlobalRC(self: *const cy.VM) usize {
    if (cy.TrackGlobalRC) {
        return self.c.refCounts;
    } else {
        cy.panic("Enable TrackGlobalRC.");
    }
}

/// Mark-sweep leveraging refcounts and deals only with cyclable objects.
/// 1. Looks at all root nodes from the stack and globals.
///    Traverse the children and sets the mark flag to true.
///    Only cyclable objects that aren't marked are visited.
/// 2. Sweep iterates all cyclable objects.
///    If the mark flag is not set:
///       - the object's children are released excluding confirmed child cyc objects.
///       - the object is queued to be freed later.
///    If the mark flag is set, reset the flag for the next gc run.
///    TODO: Allocate using separate pages for cyclable and non-cyclable objects,
///          so only cyclable objects are iterated.
pub fn performGC(vm: *cy.VM) !c.GCResult {
    log.tracev("Run gc.", .{});
    try performMark(vm);

    // Make sure dummy node has mark bit.
    cy.vm.dummyCyclableHead.typeId = vmc.GC_MARK_MASK | bt.Void;

    return try performSweep(vm);
}

fn performMark(vm: *cy.VM) !void {
    log.tracev("Perform mark.", .{});
    try markMainStackRoots(vm);

    // Mark globals.
    for (vm.c.getVarSyms().items()) |sym| {
        if (sym.value.isCycPointer()) {
            markValue(vm, sym.value);
        }
    }
}

fn performSweep(vm: *cy.VM) !c.GCResult {
    log.tracev("Perform sweep.", .{});
    // Collect cyc nodes and release their children (child cyc nodes are skipped).
    // TODO: Report `num_freed` after flattening recursive release.
    const num_freed: u32 = 0;
    var num_cyc_freed: u32 = 0;

    log.tracev("Sweep heap pages.", .{});
    for (vm.heapPages.items()) |page| {
        var i: u32 = 1;
        while (i < page.objects.len) {
            const obj = &page.objects[i];
            if (obj.freeSpan.typeId != cy.NullId) {
                if (obj.isNoMarkCyc()) {
                    log.tracev("gc free: {s}, rc={}", .{vm.getTypeName(obj.getTypeId()), obj.head.rc});
                    if (cy.Trace) {
                        checkDoubleFree(vm, obj);
                    }
                    if (cy.TrackGlobalRC) {
                        vm.c.refCounts -= obj.head.rc;
                    }
                    cy.heap.freeObject(vm, obj, true);
                    num_cyc_freed += 1;
                } else if (obj.isGcMarked()) {
                    obj.resetGcMarked();
                }
                i += 1;
            } else {
                // Freespan, skip to end.
                i += obj.freeSpan.len;
            }
        }
    }

    // Traverse non-pool cyc nodes.
    log.tracev("Sweep non-pool cyc nodes.", .{});
    var mbNode: ?*cy.heap.DListNode = vm.cyclableHead;
    while (mbNode) |node| {
        // Obtain next before node is freed.
        mbNode = node.next;

        const obj = node.getHeapObject();
        if (obj.isNoMarkCyc()) {
            log.tracev("gc free: {s}, rc={}", .{vm.getTypeName(obj.getTypeId()), obj.head.rc});
            if (cy.Trace) {
                checkDoubleFree(vm, obj);
            }
            if (cy.TrackGlobalRC) {
                vm.c.refCounts -= obj.head.rc;
            }
            cy.heap.freeObject(vm, obj, true);
            num_cyc_freed += 1;
        } else if (obj.isGcMarked()) {
            obj.resetGcMarked();
        }
    }

    if (cy.Trace) {
        vm.c.trace.numCycFrees += num_cyc_freed;
    }

    const res = c.GCResult{
        .numCycFreed = num_cyc_freed,
        .numObjFreed = num_freed,
    };
    log.tracev("gc result: num cyc {}, num obj {}", .{res.numCycFreed, res.numObjFreed});
    return res;
}

fn markMainStackRoots(vm: *cy.VM) !void {
    if (vm.c.pc[0].opcode() == .end) {
        return;
    }

    var pcOff = cy.fiber.getInstOffset(vm.c.ops, vm.c.pc);
    var fpOff = cy.fiber.getStackOffset(vm.c.stack, vm.c.framePtr);

    while (true) {
        const symIdx = try cy.debug.indexOfDebugSym(vm, pcOff);
        const key = cy.debug.getUnwindKey(vm, symIdx);
        log.tracev("mark frame: pc={} {s} fp={} unwind={},{}", .{pcOff, @tagName(vm.c.ops[pcOff].opcode()), fpOff, key.is_null, key.idx});

        if (!key.is_null) {
            const fp = vm.c.stack + fpOff;
            log.tracev("mark temps", .{});
            var cur = key;
            while (!cur.is_null) {
                if (cur.is_try) {
                    const try_e = vm.unwind_trys[cur.idx];
                    cur = try_e.prev;
                } else {
                    log.tracev("mark slot: {}", .{vm.unwind_slots[cur.idx]});
                    const v = fp[vm.unwind_slots[cur.idx]];
                    if (v.isCycPointer()) {
                        markValue(vm, v);
                    }
                    cur = vm.unwind_slot_prevs[cur.idx];
                }
            }
        }

        if (fpOff == 0) {
            // Done, at main block.
            return;
        } else {
            // Unwind.
            pcOff = cy.fiber.getInstOffset(vm.c.ops, vm.c.stack[fpOff + 2].retPcPtr) - vm.c.stack[fpOff + 1].call_info.call_inst_off;
            fpOff = cy.fiber.getStackOffset(vm.c.stack, vm.c.stack[fpOff + 3].retFramePtr);
        }
    }
}

/// Assumes `v` is a cyclable pointer.
fn markValue(vm: *cy.VM, v: cy.Value) void {
    const obj = v.asHeapObject();
    if (!obj.isGcMarked()) {
        obj.setGcMarked();
    } else {
        // Already marked.
        return;
    }
    // Visit children.
    const typeId = obj.getTypeId();
    switch (typeId) {
        bt.Map => {
            const map = obj.map.map();
            var iter = map.iterator();
            while (iter.next()) |entry| {
                if (entry.key.isCycPointer()) {
                    markValue(vm, entry.key);
                }
                if (entry.value.isCycPointer()) {
                    markValue(vm, entry.value);
                }
            }
        },
        bt.MapIter => {
            markValue(vm, obj.mapIter.map);
        },
        bt.Closure => {
            const vals = obj.closure.getCapturedValuesPtr()[0..obj.closure.numCaptured];
            for (vals) |val| {
                // TODO: Can this be assumed to always be a Box value?
                if (val.isCycPointer()) {
                    markValue(vm, val);
                }
            }
        },
        bt.UpValue => {
            if (obj.up.val.isCycPointer()) {
                markValue(vm, obj.up.val);
            }
        },
        bt.Fiber => {
            // TODO: Visit other fiber stacks.
        },
        else => {
            // Assume caller used isCycPointer and obj is cyclable.
            const entry = vm.c.types[typeId];
            if (entry.kind == .object) {
                // User type.
                const members = obj.object.getValuesConstPtr()[0..entry.data.object.numFields];
                for (members) |m| {
                    if (m.isCycPointer()) {
                        markValue(vm, m);
                    }
                }
            } else {
                // Custom object type.
                if (entry.sym.cast(.custom_t).getChildrenFn) |getChildren| {
                    const children = getChildren(@ptrCast(vm), @ptrFromInt(@intFromPtr(obj) + 8));
                    for (cy.Value.fromSliceC(children)) |child| {
                        if (child.isCycPointer()) {
                            markValue(vm, child);
                        }
                    }
                }
            }
        },
    }
}

pub fn countObjects(vm: *cy.VM) usize {
    var count: usize = 0;
    for (vm.heapPages.items()) |page| {
        var i: u32 = 1;
        while (i < page.objects.len) {
            const obj = &page.objects[i];
            if (obj.freeSpan.typeId != cy.NullId) {
                count += 1;
                i += 1;
            } else {
                // Freespan, skip to end.
                i += obj.freeSpan.len;
            }
        }
    }

    // Traverse non-pool cyc nodes.
    var mbNode: ?*cy.heap.DListNode = vm.cyclableHead;
    while (mbNode) |node| {
        count += 1;
        mbNode = node.next;
    }

    // Account for the dummy cyclable node.
    count -= 1;

    return count;
}

pub fn checkGlobalRC(vm: *cy.VM) !void {
    const rc = getGlobalRC(vm);
    if (rc != 0) {
        std.debug.print("unreleased refcount: {}\n", .{rc});
        c.traceDumpLiveObjects(@ptrCast(vm));

        // var iter = cy.vm.traceObjRetains.iterator();
        // while (iter.next()) |e| {
        //     const msg = std.fmt.bufPrint(&cy.tempBuf, "pc={} retains={}", .{e.key_ptr.*, e.value_ptr.*}) catch cy.fatal();
        //     cy.debug.printTraceAtPc(vm, e.key_ptr.*, "retain", msg) catch cy.fatal();
        // }

        return error.UnreleasedReferences;
    }
}

pub fn dumpObjectAllocTrace(vm: *cy.VM, obj: *cy.HeapObject, allocPc: u32) !void {
    var buf: [256]u8 = undefined;
    const typeId = obj.getTypeId();
    const typeName = vm.getTypeName(typeId);
    const valStr = try vm.bufPrintValueShortStr(&vm.tempBuf, cy.Value.initNoCycPtr(obj));
    const msg = try std.fmt.bufPrint(&buf, "{*}, type: {s}, rc: {} at pc: {}\nval={s}", .{
        obj, typeName, obj.head.rc, allocPc, valStr,
    });
    try cy.debug.printTraceAtPc(vm, allocPc, "alloced", msg);
}
