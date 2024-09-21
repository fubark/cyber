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
    release2(vm, val, false, {});
}

pub fn release2(vm: *cy.VM, val: cy.Value, comptime gc: bool, res: if (gc) *c.GCResult else void) void {
    if (cy.Trace) {
        vm.c.trace.numReleaseAttempts += 1;
    }
    if (val.isPointer()) {
        const obj = val.asHeapObject();
        if (cy.TraceRC) {
            log.tracevIf(log_mem, "{} -1, {s}{s}, {*}", .{obj.head.rc, obj.getRtTypePrefix(), vm.getTypeName(val.getTypeId()), obj});
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
            @call(.never_inline, cy.heap.freeObject, .{vm, obj, gc, res});

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
    releaseObject2(vm, obj, false, {});
}

pub fn releaseObject2(vm: *cy.VM, obj: *cy.HeapObject, comptime gc: bool, res: if (gc) *c.GCResult else void) void {
    if (cy.Trace) {
        checkDoubleFree(vm, obj);
    }
    if (cy.TraceRC) {
        log.tracevIf(log_mem, "{} -1 release: {s}, {*}", .{obj.head.rc, vm.getTypeName(obj.getTypeId()), obj});
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
        @call(.never_inline, cy.heap.freeObject, .{vm, obj, gc, res});
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
        log.tracevIf(log_mem, "{} +1, {s}, {*}", .{obj.head.rc, self.getTypeName(obj.getTypeId()), obj});
    }
    if (cy.TrackGlobalRC) {
        self.c.refCounts += 1;
    }
    if (cy.Trace) {
        self.c.trace.numRetains += 1;
        self.c.trace.numRetainAttempts += 1;
    }
}

pub fn releaseLayout(vm: *cy.VM, dst: [*]const cy.Value, base_idx: usize, layout: *cy.types.RetainLayout, comptime gc: bool, res: if (gc) *c.GCResult else void) void {
    switch (layout.kind) {
        .struct_k => {
            for (layout.data.struct_k.ptr[0..layout.data.struct_k.len]) |e| {
                switch (e.kind) {
                    .refs => {
                        for (e.data.refs.ptr[0..e.data.refs.len]) |i| {
                            vm.release2(dst[base_idx + e.offset + i], gc, res);
                        }
                    },
                    .layout => {
                        vm.releaseLayout(dst, base_idx + e.offset, e.data.layout, gc, res);
                    },
                }
            }
        },
        .array => {
            for (0..layout.data.array.n) |i| {
                if (layout.data.array.layout) |elem_layout| {
                    vm.releaseLayout(dst, base_idx + i * layout.data.array.elem_size, elem_layout, gc, res);
                } else {
                    vm.release2(dst[base_idx + i], gc, res);
                }
            }
        },
        .option => {
            const tag = dst[base_idx].val;
            if (tag == 1) {
                if (layout.data.option.layout) |child_layout| {
                    vm.releaseLayout(dst, base_idx + 1, child_layout, gc, res);
                } else {
                    vm.release2(dst[base_idx + 1], gc, res);
                }
            }
        },
        .union_k => {
            const tag = dst[base_idx].val;
            if (layout.data.union_k.ptr[tag]) |case| {
                if (case.layout) |payload_layout| {
                    vm.releaseLayout(dst, base_idx + 1, payload_layout, gc, res);
                } else {
                    vm.release2(dst[base_idx + 1], gc, res);
                }
            }
        },
    }
}

pub fn retainLayout(vm: *cy.VM, dst: [*]cy.Value, base_idx: usize, layout: *cy.types.RetainLayout) void {
    switch (layout.kind) {
        .struct_k => {
            for (layout.data.struct_k.ptr[0..layout.data.struct_k.len]) |e| {
                switch (e.kind) {
                    .refs => {
                        for (e.data.refs.ptr[0..e.data.refs.len]) |i| {
                            vm.retain(dst[base_idx + e.offset + i]);
                        }
                    },
                    .layout => {
                        vm.retainLayout(dst, base_idx + e.offset, e.data.layout);
                    },
                }
            }
        },
        .array => {
            for (0..layout.data.array.n) |i| {
                if (layout.data.array.layout) |elem_layout| {
                    vm.retainLayout(dst, base_idx + i * layout.data.array.elem_size, elem_layout);
                } else {
                    vm.retain(dst[base_idx + i]);
                }
            }
        },
        .option => {
            const tag = dst[base_idx].val;
            if (tag == 1) {
                if (layout.data.option.layout) |child_layout| {
                    vm.retainLayout(dst, base_idx + 1, child_layout);
                } else {
                    vm.retain(dst[base_idx + 1]);
                }
            }
        },
        .union_k => {
            const tag = dst[base_idx].val;
            if (layout.data.union_k.ptr[tag]) |case| {
                if (case.layout) |payload_layout| {
                    vm.retainLayout(dst, base_idx + 1, payload_layout);
                } else {
                    vm.retain(dst[base_idx + 1]);
                }
            }
        },
    }
}

const Root = @This();

pub const VmExt = struct {
    pub const retainObject = Root.retainObject;
    pub const retain = Root.retain;
    pub const releaseObject = Root.releaseObject;
    pub const retainLayout = Root.retainLayout;
    pub const releaseLayout = Root.releaseLayout;
    pub const release = Root.release;
    pub const release2 = Root.release2;
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
            log.tracevIf(log_mem, "{} +1, {s}{s}, {*}", .{obj.head.rc, obj.getRtTypePrefix(), self.getTypeName(obj.getTypeId()), obj});
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
            log.tracevIf(log_mem, "{} +{}, {s}, {*}", .{obj.head.rc, inc, self.getTypeName(obj.getTypeId()), obj});
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
pub fn collectCycles(vm: *cy.VM) !c.GCResult {
    log.tracev("Run gc.", .{});
    try performMark(vm);

    // Make sure dummy node has mark bit.
    cy.vm.dummyCyclableHead.typeId = vmc.GC_MARK_BIT | bt.Void;

    return try performSweep(vm);
}

fn performMark(vm: *cy.VM) !void {
    log.tracev("Perform mark.", .{});
    try markMainStackRoots(vm);

    // Mark globals.
    for (vm.c.getVarSyms().items()) |sym| {
        if (sym.value.isPointer()) {
            markValue(vm, sym.value);
        }
    }
    for (vm.c.getContextVars().items()) |context_var| {
        if (context_var.value.isPointer()) {
            markValue(vm, context_var.value);
        }
    }

    // Mark compile-time template args values.
    // TODO: Perhaps template args should be converted to unmanaged data to avoid marking.
    for (vm.compiler.chunks.items) |chunk| {
        for (chunk.syms.items) |sym| {
            if (sym.type == .template) {
                const template = sym.cast(.template);
                for (template.variants.items) |variant| {
                    for (variant.args) |arg| {
                        if (arg.isPointer()) {
                            markValue(vm, arg);
                        }
                    }
                    if (variant.type == .ct_val) {
                        if (variant.data.ct_val.ct_val.isPointer()) {
                            markValue(vm, variant.data.ct_val.ct_val);
                        }
                    }
                }
            } else if (sym.type == .func_template) {
                const template = sym.cast(.func_template);
                for (template.variants.items) |variant| {
                    for (variant.args) |arg| {
                        if (arg.isPointer()) {
                            markValue(vm, arg);
                        }
                    }
                }
            }
        }
    }

    for (vm.staticObjects.items()) |obj| {
        markValue(vm, obj);
    }
}

// TODO: This should only be looking at cyclable objects.
//       Cyclable objects should be allocated from separate heap pages.
fn performSweep(vm: *cy.VM) !c.GCResult {
    log.tracev("Perform sweep.", .{});
    // Collect cyc nodes and release their children.
    var res = c.GCResult{
        .num_obj_freed = 0,
    };

    log.tracev("Sweep heap pages.", .{});
    for (vm.heapPages.items()) |page| {
        var i: u32 = 1;
        while (i < page.objects.len) {
            const obj = &page.objects[i];
            if (obj.freeSpan.typeId != cy.NullId) {
                if (!obj.isGcMarked()) {
                    log.tracev("gc free: {s}, rc={}", .{vm.getTypeName(obj.getTypeId()), obj.head.rc});
                    if (cy.Trace) {
                        checkDoubleFree(vm, obj);
                    }
                    if (cy.TrackGlobalRC) {
                        vm.c.refCounts -= obj.head.rc;
                    }

                    // Inc ref-count to ensure cycles do not free it again.
                    obj.head.rc += 1;
                    if (cy.Trace) vm.c.refCounts += 1;
                    cy.heap.freeObject(vm, obj, true, &res);
                } else {
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
        // Obtain next before this node is freed.
        mbNode = node.next;

        const obj = node.getHeapObject();
        if (obj.head.typeId & vmc.TYPE_MASK == cy.NullId & vmc.TYPE_MASK) {
            // Marked free from a previous `freeObject`.
            // Actually free it this time without corrupting the list.
            cy.heap.freeExternalObject(vm, obj, obj.head.rc, false);
            continue;
        }
        if (!obj.isGcMarked()) {
            log.tracev("gc free: {s}, rc={}", .{vm.getTypeName(obj.getTypeId()), obj.head.rc});
            if (cy.Trace) {
                checkDoubleFree(vm, obj);
            }
            if (cy.TrackGlobalRC) {
                vm.c.refCounts -= obj.head.rc;
            }
            obj.head.rc += 1;
            if (cy.Trace) vm.c.refCounts += 1;
            cy.heap.freeObject(vm, obj, true, &res);
            cy.heap.freeExternalObject(vm, obj, obj.head.rc, false);
        } else {
            obj.resetGcMarked();
        }
    }

    log.tracev("gc result: num_obj_freed={}", .{res.num_obj_freed});
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
                    if (v.isPointer()) {
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
                if (entry.key.isPointer()) {
                    markValue(vm, entry.key);
                }
                if (entry.value.isPointer()) {
                    markValue(vm, entry.value);
                }
            }
        },
        bt.MapIter => {
            markValue(vm, obj.mapIter.map);
        },
        bt.Fiber => {
            // TODO: Visit other fiber stacks.
        },
        else => {
            const type_ = vm.getType(typeId);
            switch (type_.kind()) {
                .struct_t => {
                    const size = type_.cast(.struct_t).size;
                    // User type.
                    const members = obj.object.getValuesConstPtr()[0..size];
                    for (members) |m| {
                        if (m.isPointer()) {
                            markValue(vm, m);
                        }
                    }
                },
                .trait => {
                    markValue(vm, obj.trait.impl);
                },
                .func_union => {
                    const vals = obj.func_union.getCapturedValuesPtr()[0..obj.func_union.data.closure.numCaptured];
                    for (vals) |val| {
                        // TODO: Can this be assumed to always be a Box value?
                        if (val.isPointer()) {
                            markValue(vm, val);
                        }
                    }
                },
                .hostobj => {
                    // Custom object type.
                    const hostobj_t = type_.cast(.hostobj);
                    if (hostobj_t.getChildrenFn) |getChildren| {
                        const children = getChildren(@ptrCast(vm), @ptrFromInt(@intFromPtr(obj) + 8));
                        for (cy.Value.fromSliceC(children)) |child| {
                            if (child.isPointer()) {
                                markValue(vm, child);
                            }
                        }
                    }
                },
                else => {},
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

pub fn checkGlobalRC(ivm: *cy.VM) !void {
    const vm: *c.ZVM = @ptrCast(ivm);
    const rc = getGlobalRC(ivm);
    if (rc != 0) {
        std.debug.print("unreleased refcount: {}\n", .{rc});
        vm.traceDumpLiveObjects();

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
    const valStr = try vm.bufPrintValueShortStr(&vm.tempBuf, cy.Value.initPtr(obj), true);
    const msg = try std.fmt.bufPrint(&buf, "{*}, type: {s}, rc: {} at pc: {}\nval={s}", .{
        obj, typeName, obj.head.rc, allocPc, valStr,
    });
    try cy.debug.printTraceAtPc(vm, allocPc, "alloced", msg);
}
