// Copyright (c) 2023 Cyber (See LICENSE)

/// Runtime memory management with ARC.

const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const log = cy.log.scoped(.arc);
const cy = @import("cyber.zig");
const vmc = cy.vmc;
const rt = cy.rt;

pub fn release(vm: *cy.VM, val: cy.Value) linksection(cy.HotSection) void {
    if (cy.Trace) {
        vm.trace.numReleaseAttempts += 1;
    }
    if (val.isPointer()) {
        const obj = val.asHeapObject();
        log.tracev("release obj: {}, rc={}", .{val.getUserTag(), obj.head.rc});
        if (cy.Trace) {
            checkDoubleFree(vm, obj);
        }
        obj.head.rc -= 1;
        if (cy.TrackGlobalRC) {
            if (cy.Trace) {
                if (vm.refCounts == 0) {
                    cy.fmt.printStderr("Double free. {}\n", &.{cy.fmt.v(obj.getTypeId())});
                    cy.fatal();
                }
            }
            vm.refCounts -= 1;
        }
        if (cy.Trace) {
            vm.trace.numReleases += 1;
        }
        if (obj.head.rc == 0) {
            // Free children and the object.
            @call(.never_inline, cy.heap.freeObject, .{vm, obj, true, false, true});

            if (cy.Trace) {
                if (vm.countFrees) {
                    vm.numFreed += 1;
                }
            }
        }
    } else {
        log.tracev("release: {}, nop", .{val.getUserTag()});
    }
}

pub fn isObjectAlreadyFreed(vm: *cy.VM, obj: *cy.HeapObject) bool {
    if (obj.isFreed()) {
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
        @call(.never_inline, cy.heap.freeObject, .{vm, obj, true, false, true});
        if (cy.Trace) {
            if (vm.countFrees) {
                vm.numFreed += 1;
            }
        }
    }
}

pub fn runTempReleaseOps(vm: *cy.VM, fp: [*]const cy.Value, tempIdx: u32) void {
    log.tracev("unwind release temps", .{});
    var curIdx = tempIdx;
    while (curIdx != cy.NullId) {
        log.tracev("unwind release: {}", .{vm.unwindTempRegs[curIdx]});
        release(vm, fp[vm.unwindTempRegs[curIdx]]);
        curIdx = vm.unwindTempPrevIndexes[curIdx];
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

pub fn getGlobalRC(self: *const cy.VM) usize {
    if (cy.TrackGlobalRC) {
        return self.refCounts;
    } else {
        cy.panic("Enable TrackGlobalRC.");
    }
}

const GCResult = struct {
    numCycFreed: u32,
    numObjFreed: u32,
};

/// Mark-sweep leveraging refcounts and deals only with cyclable objects.
/// 1. Looks at all root nodes from the stack and globals.
///    Traverse the children and sets the mark flag to true.
///    Only cyclable objects that aren't marked are visited.
/// 2. Sweep iterates all cyclable objects.
///    If the mark flag is not set:
///       - the object's children are released exlcuding confirmed child cyc objects.
///       - the object is queued to be freed later.
///    If the mark flag is set, reset the flag for the next gc run.
///    TODO: Allocate using separate pages for cyclable and non-cyclable objects,
///          so only cyclable objects are iterated.
pub fn performGC(vm: *cy.VM) !GCResult {
    log.tracev("Run gc.", .{});
    try performMark(vm);

    // Make sure dummy node has mark bit.
    cy.vm.dummyCyclableHead.typeId = vmc.GC_MARK_MASK | rt.NoneT;

    return try performSweep(vm);
}

fn performMark(vm: *cy.VM) !void {
    try markMainStackRoots(vm);

    // Mark globals.
    for (vm.varSyms.items()) |sym| {
        if (sym.value.isCycPointer()) {
            markValue(vm, sym.value);
        }
    }
}

fn performSweep(vm: *cy.VM) !GCResult {
    // Collect cyc nodes and release their children (child cyc nodes are skipped).
    if (cy.Trace) {
        vm.countFrees = true;
        vm.numFreed = 0;
    }
    defer {
        if (cy.Trace) {
            vm.countFrees = false;
        }
    }
    var cycObjs: std.ArrayListUnmanaged(*cy.HeapObject) = .{};
    defer cycObjs.deinit(vm.alloc);
    for (vm.heapPages.items()) |page| {
        var i: u32 = 1;
        while (i < page.objects.len) {
            const obj = &page.objects[i];
            if (obj.freeSpan.typeId != cy.NullId) {
                if (obj.isGcConfirmedCyc()) {
                    try cycObjs.append(vm.alloc, obj);
                    cy.heap.freeObject(vm, obj, true, true, false);
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
    var mbNode: ?*cy.heap.DListNode = vm.cyclableHead;
    while (mbNode) |node| {
        const obj = node.getHeapObject();
        if (obj.isGcConfirmedCyc()) {
            try cycObjs.append(vm.alloc, obj);
            cy.heap.freeObject(vm, obj, true, true, false);
        } else if (obj.isGcMarked()) {
            obj.resetGcMarked();
        }
        mbNode = node.next;
    }

    // Free cyc nodes.
    for (cycObjs.items) |obj| {
        log.tracev("cyc free: {s}, rc={}", .{vm.getTypeName(obj.getTypeId()), obj.head.rc});
        if (cy.Trace) {
            checkDoubleFree(vm, obj);
        }
        if (cy.TrackGlobalRC) {
            vm.refCounts -= obj.head.rc;
        }
        // No need to bother with their refcounts.
        cy.heap.freeObject(vm, obj, false, false, true);
    }

    if (cy.Trace) {
        vm.trace.numCycFrees += @intCast(cycObjs.items.len);
        vm.numFreed += @intCast(cycObjs.items.len);
    }

    return GCResult{
        .numCycFreed = @intCast(cycObjs.items.len),
        .numObjFreed = if (cy.Trace) vm.numFreed else 0,
    };
}

fn markMainStackRoots(vm: *cy.VM) !void {
    if (vm.pc[0].opcode() == .end) {
        return;
    }

    var pcOff = cy.fiber.getInstOffset(vm.ops.ptr, vm.pc);
    var fpOff = cy.fiber.getStackOffset(vm.stack.ptr, vm.framePtr);

    while (true) {
        const symIdx = cy.debug.indexOfDebugSym(vm, pcOff) orelse return error.NoDebugSym;
        const sym = cy.debug.getDebugSymByIndex(vm, symIdx);
        const tempIdx = cy.debug.getDebugTempIndex(vm, symIdx);
        const endLocalsPc = cy.debug.debugSymToEndLocalsPc(vm, sym);
        log.debug("mark frame: {} {} {} {}", .{pcOff, vm.ops[pcOff].opcode(), tempIdx, endLocalsPc});

        if (tempIdx != cy.NullId) {
            const fp = vm.stack.ptr + fpOff;
            log.tracev("mark temps", .{});
            var curIdx = tempIdx;
            while (curIdx != cy.NullId) {
                log.tracev("mark reg: {}", .{vm.unwindTempRegs[curIdx]});
                const v = fp[vm.unwindTempRegs[curIdx]];
                if (v.isCycPointer()) {
                    markValue(vm, v);
                }
                curIdx = vm.unwindTempPrevIndexes[curIdx];
            }
        }

        if (endLocalsPc != cy.NullId) {
            if (vm.ops[endLocalsPc].opcode() == .releaseN) {
                const numLocals = vm.ops[endLocalsPc+1].val;
                for (vm.ops[endLocalsPc+2..endLocalsPc+2+numLocals]) |local| {
                    const v = vm.stack[fpOff + local.val];
                    if (v.isCycPointer()) {
                        markValue(vm, v);
                    }
                }
            }
        }
        if (fpOff == 0) {
            // Done, at main block.
            return;
        } else {
            // Unwind.
            pcOff = cy.fiber.getInstOffset(vm.ops.ptr, vm.stack[fpOff + 2].retPcPtr) - vm.stack[fpOff + 1].retInfoCallInstOffset();
            fpOff = cy.fiber.getStackOffset(vm.stack.ptr, vm.stack[fpOff + 3].retFramePtr);
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
        rt.ListT => {
            const items = obj.list.items();
            for (items) |it| {
                if (it.isCycPointer()) {
                    markValue(vm, it);
                }
            }
        },
        rt.MapT => {
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
        rt.ListIteratorT => {
            markValue(vm, cy.Value.initPtr(obj.listIter.list));
        },
        rt.MapIteratorT => {
            markValue(vm, cy.Value.initPtr(obj.mapIter.map));
        },
        rt.ClosureT => {
            const vals = obj.closure.getCapturedValuesPtr()[0..obj.closure.numCaptured];
            for (vals) |val| {
                // TODO: Can this be assumed to always be a Box value?
                if (val.isCycPointer()) {
                    markValue(vm, val);
                }
            }
        },
        rt.BoxT => {
            if (obj.box.val.isCycPointer()) {
                markValue(vm, obj.box.val);
            }
        },
        rt.FiberT => {
            // TODO: Visit other fiber stacks.
        },
        else => {
            // Assume caller used isCycPointer and obj is cyclable.
            const entry = &vm.types.buf[typeId];
            if (!entry.isHostObject) {
                // User type.
                const members = obj.object.getValuesConstPtr()[0..entry.data.numFields];
                for (members) |m| {
                    if (m.isCycPointer()) {
                        markValue(vm, m);
                    }
                }
            } else {
                // Host type.
                if (entry.data.hostObject.getChildren) |getChildren| {
                    const children = @as(cy.ObjectGetChildrenFn, @ptrCast(@alignCast(getChildren)))(@ptrCast(vm), @ptrFromInt(@intFromPtr(obj) + 8));
                    for (children.slice()) |child| {
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
    if (rc != vm.expGlobalRC) {
        std.debug.print("unreleased refcount: {}, expected: {}\n", .{rc, vm.expGlobalRC});

        if (cy.Trace) {
            var buf: [256]u8 = undefined;
            var iter = vm.objectTraceMap.iterator();
            while (iter.next()) |it| {
                const trace = it.value_ptr.*;
                if (trace.freePc == cy.NullId) {
                    const typeName = vm.getTypeName(it.key_ptr.*.getTypeId());
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
