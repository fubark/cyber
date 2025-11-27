const std = @import("std");
const builtin = @import("builtin");
const build_config = @import("build_config");
const cy = @import("cyber.zig");
const vmc = @import("vmc");
const c = @import("capi.zig");
const logger = cy.log.scoped(.thread);
const stdx = @import("stdx");
const zt = stdx.testing;
const fmt = @import("fmt.zig");
const Value = cy.Value;

pub const CallArgStart: u8 = vmc.CALL_ARG_START;
pub const CalleeStart: u8 = vmc.CALLEE_START;

/// Inits `Generator` in the return registers.
pub fn ret_generator(heap: *cy.Heap, type_id: cy.TypeId, ret_size: u16, frame: []cy.Value, resume_pc: ?*cy.Inst, deinit_pc: ?*cy.Inst) !void {
    // Only persists the locals. Assumes generator is 1 reg return.
    const locals = frame[ret_size + 4..];
    const obj: *vmc.Generator = @ptrCast(try heap.allocBigObject(type_id, @sizeOf(vmc.Generator)));
    const locals_dup = try heap.alloc.dupe(cy.Value, locals);
    obj.* = .{
        .frame_ptr = @ptrCast(locals_dup.ptr),
        .frame_len = @intCast(locals_dup.len),
        .resume_pc = @ptrCast(resume_pc),
        .deinit_pc = @ptrCast(deinit_pc),
        .prev_fp = undefined,
        .running = false,
        .done = false,
    };
    frame[0] = Value.initPtr(obj);
}

/// Panic. Unwinds to record the stack trace. Then releases all memory tied to the thread.
pub fn panic(t: *Thread, err: cy.Value) !void {
    _ = err;

    logger.tracev("Begin thread panic.", .{});

    // TODO: Run stack error destructors.

    var cx = t.getFiberContext();

    if (cy.Trace and c.verbose()) {
        if (cx.pc) |pc| {
            const sym = cy.debug.getDebugSymByPc(t.c.vm, pc) orelse {
                logger.trace("at pc: {*}", .{cx.pc});
                return error.NoDebugSym;
            };
            if (!c.silent()) {
                t.log("panic pc: {}:{}", .{sym.file, sym.pc});
                cy.thread.print_thread_inst(t, pc) catch @panic("unexpected");
            }
        } else {
            @panic("TODO");
        }
    }
    cx = try t.unwindStack(t.c.stack(), cx);

    // Build detailed stack trace from compact trace.
    const frames = try cy.debug.allocStackTrace(t.c.vm, t.c.stack(), t.compact_trace.items);
    t.stack_trace.deinit(t.alloc);
    t.stack_trace.frames = frames;

    _ = try freeHeapPages(t);
}

pub fn patch_frame_ptrs(t: *cy.Thread, fp: usize, layout: []bool, new_stack: [*]cy.Value) void {
    for (layout, 0..) |is_ptr, i| {
        if (!is_ptr) continue;

        const ptr: usize = @intCast(t.c.stack_ptr[fp + i].val);
        const stack_start = @intFromPtr(t.c.stack_ptr);
        if (ptr >= stack_start and ptr < stack_start + t.c.stack_len) {
            const byte_off = ptr - stack_start;
            new_stack[fp + i] = cy.Value.initRaw(@intFromPtr(new_stack) + byte_off);
        }
    }
}

pub fn get_ptr_layout(vm: *cy.VM, pc: [*]cy.Inst) ?[]bool {
    const table_idx = cy.debug.indexOfTableEntry(vm.debug_tables, pc) orelse return null;
    const entry = vm.debug_tables[table_idx];
    const table = entry.chunk.buf.ptr_table.items;
    const pc_off = pc - entry.ptr;
    const S = struct {
        fn cmp(cx: usize, o: cy.bytecode.PcPtrLayout) std.math.Order {
            if (cx == o.pc) {
                return .eq;
            }
            if (cx < o.pc) {
                return .lt;
            } else {
                return .gt;
            }
        }
    };
    const idx = std.sort.binarySearch(cy.bytecode.PcPtrLayout, table, pc_off, S.cmp) orelse return null;
    return entry.chunk.buf.ptr_layouts.items[table[idx].layout];
}

const FreeHeapResult = struct {
    num_freed: u32,
};

/// Sweep frees each heap object disregarding any dependencies. Triggered from fatal error.
/// Only critical destructors should be invoked.
pub fn freeHeapPages(t: *cy.Thread) !FreeHeapResult {
    const res = FreeHeapResult{
        .num_freed = 0,
    };

    logger.tracev("Free heap pages.", .{});
    for (t.heap.heapPages.items, 0..) |page, j| {
        _ = j;
    
        var i: u32 = 0;
        while (i < page.objects.len) {
            const pobj = &page.objects[i];
            if (pobj.head.free_span.meta != cy.NullId) {
                var obj: *cy.HeapObject = @ptrCast(&pobj.data);
                if (pobj.head.free_span.meta == cy.heap.BigObjectPtrType) {
                    obj = @ptrCast(@alignCast(obj.pointer.ptr));
                }

                logger.tracev("free: {s}, rc={} {*}", .{t.heap.getType(obj.getTypeId()).name(), obj.rc(), obj});
                if (cy.Trace) {
                    if (t.heap.checkDoubleFree(obj)) {
                        var buf: [128]u8 = undefined;
                        const pc: [*]cy.Inst = @ptrFromInt(t.heap.c.ctx);
                        const msg = std.fmt.bufPrint(&buf, "{*} at pc: {}({s})", .{
                            obj, t.heap.c.ctx, @tagName(pc[0].opcode()),
                        }) catch @panic("error");

                        var w = std.Io.Writer.Allocating.init(t.alloc);
                        defer w.deinit();
                        cy.debug.write_trace_at_pc(t.c.vm, &w.writer, @ptrFromInt(t.heap.c.ctx), "double free", msg) catch @panic("error");
                        cy.debug.write_object_trace(t, &w.writer, obj) catch @panic("error");
                        t.c.vm.log(w.written());
                        cy.fatal();
                    }
                    t.heap.c.refCounts -= obj.rc();
                }

                t.heap.destroyObject(obj, true);
            }
            i += 1;
        }
    }

    var iter = t.heap.buffers.iterator();
    while (iter.next()) |e| {
        const slice = e.key_ptr.*[0..e.value_ptr.*];
        t.heap.alloc.free(slice);
    }
    t.heap.buffers.clearRetainingCapacity();

    logger.tracev("free result: num_freed={}", .{res.num_freed});
    return res;
}

test "thread internals." {
    try zt.eq(8, @alignOf(Thread));
    try zt.eq(@sizeOf(ThreadC), @sizeOf(vmc.Thread));

    inline for (std.meta.fields(ThreadC)) |field| {
        try zt.eq(@offsetOf(ThreadC, field.name), @offsetOf(vmc.Thread, field.name));
    }

    // Check Zig/C structs.
    try zt.eq(@offsetOf(Thread, "c"), @offsetOf(vmc.ZThread, "c"));
    try zt.eq(@offsetOf(Thread, "heap"), @offsetOf(vmc.ZThread, "heap"));
}

/// Mimic layout but with zig types.
pub const ThreadC = extern struct {
    id: usize,
    vm: *cy.VM,

    pc: [*]cy.Inst,
    fp: [*]cy.Value,
    fp_end: ?[*]cy.Value,

    stack_ptr: [*]cy.Value,
    stack_len: usize,
    stack_end: [*]cy.Value,

    panic_payload: u64,
    panic_type: u8,

    trace: *vmc.TraceInfo,
    trace_pc: [*]cy.Inst,
    trace_fp: [*]cy.Value,

    pub fn stack(self: *ThreadC) []Value {
        return self.stack_ptr[0..self.stack_len];
    }
};

pub const Thread = struct {
    alloc: std.mem.Allocator,
    c: ThreadC,
    heap: cy.Heap,

    unwinding: bool,

    /// Records a minimal trace when walking the stack.
    /// Stack frames are then constructed from them.
    compact_trace: std.ArrayList(vmc.CompactFrame),

    stack_trace: cy.StackTrace,

    pub fn init(self: *Thread, id: usize, alloc: std.mem.Allocator, vm: *cy.VM) !void {
        self.* = .{
            .heap = try cy.Heap.init(alloc, vm.sema),
            .alloc = alloc,
            .c = .{
                .id = id,
                .vm = vm,
                .pc = undefined,
                .fp = undefined,
                .fp_end = null,
                .stack_ptr = undefined,
                .stack_len = 0,
                .stack_end = undefined,
                .panic_payload = undefined,
                .panic_type = vmc.PANIC_NONE,
                .trace = undefined,
                .trace_pc = undefined,
                .trace_fp = undefined,
            },

            .unwinding = false,

            .compact_trace = .{},
            .stack_trace = .{},
        };

        // 4KB default stack size. 
        try self.stackEnsureTotalCapacityPrecise(511);
        self.c.fp = self.c.stack_ptr;

        if (cy.Trace) {
            self.c.trace = try alloc.create(vmc.TraceInfo);
        }
    }

    pub fn deinit(self: *Thread, reset: bool) void {
        self.free_panic();
        self.heap.deinit(reset);
        self.stack_trace.deinit(self.alloc);

        if (reset) {
            self.compact_trace.clearRetainingCapacity();
        } else {
            self.compact_trace.deinit(self.alloc);
        }

        if (cy.Trace) {
            if (!reset) {
                self.alloc.destroy(self.c.trace);
            }
        }

        if (!reset) {
            self.alloc.free(self.c.stack());
            self.c.stack_len = 0;
        }
    }

    pub fn checkGlobalRC(self: *cy.Thread) !void {
        const rc = getGlobalRC(self);
        if (rc != 0) {
            std.debug.print("unreleased refcount: {}\n", .{rc});
            c.thread_dump_live_objects(@ptrCast(self));

            // var iter = cy.vm.traceObjRetains.iterator();
            // while (iter.next()) |e| {
            //     const msg = std.fmt.bufPrint(&cy.tempBuf, "pc={} retains={}", .{e.key_ptr.*, e.value_ptr.*}) catch cy.fatal();
            //     cy.debug.printTraceAtPc(vm, e.key_ptr.*, "retain", msg) catch cy.fatal();
            // }

            return error.UnreleasedReferences;
        }
    }

    pub fn getGlobalRC(t: *const cy.Thread) usize {
        if (cy.Trace) {
            return t.heap.c.refCounts;
        } else {
            cy.panic("Enable Trace.");
        }
    }

    /// Assumes `func` is a FuncPtr value.
    /// Assumes arguments have correct types that match the function's parameters.
    pub fn callFunc(self: *Thread, ret_: [*]Value, ret_size: u16, func_ptr: Value, args: []const Value) ![*]Value {
        const fpOff = getStackOffset(self.c.stack_ptr, ret_);

        // Since the compiler isn't aware of a re-entry call, it can not predetermine the stack size required
        // for the arguments.
        try ensureTotalStackCapacity(self, fpOff + CallArgStart + args.len);

        // Copy callee + args to a new blank frame.
        @memcpy(ret_[ret_size+CallArgStart..ret_size+CallArgStart+args.len], args);

        return self.callFunc2(ret_, ret_size, func_ptr);
    }

    pub fn callFunc2(self: *Thread, ret_: [*]Value, ret_size: u16, func_ptr: Value) ![*]Value {
        // Save context to restore afterwards.
        // In particular, `fp_end` is altered by host functions by invoking `param()` to consume parameters.
        // Without a restore, the stack can continue to grow without bounds. 
        // Also preserve the stack offset since the stack could have been relocated.
        const cur_pc: [*]cy.Inst = @ptrCast(self.c.pc);
        const fp_save: usize = self.c.fp - self.c.stack_ptr;
        const fp_end_save: usize = self.c.fp_end.? - self.c.stack_ptr;

        // Stack may have been relocated after the call.
        const ret_off = ret_ - self.c.stack_ptr;

        // This is similar to `callSym` except it does not patch any bytecode.
        const host_func = (func_ptr.val >> 63) != 0;
        if (host_func) {
            self.c.fp = ret_ + ret_size;
            ret_[ret_size] = buildHostCallInfo(false);
            ret_[ret_size + 1] = Value{ .retPcPtr = cur_pc };
            ret_[ret_size + 2] = Value{ .retFramePtr = self.c.fp };
            const ptr: vmc.HostFn = @ptrFromInt(func_ptr.val & ~(@as(u64, 1) << 63));
            const res = ptr.?(@ptrCast(self));
            if (res != 0) {
                return error.Panic;
            }

            // Restore context upon success.
            self.c.pc = @ptrCast(cur_pc);
            self.c.fp = self.c.stack_ptr + fp_save;
            self.c.fp_end = self.c.stack_ptr + fp_end_save;
        } else {
            var end = [1]cy.Inst{.initOpCode(.end)};
            ret_[ret_size] = buildCallInfo(false, 0, 0);
            ret_[ret_size + 1] = Value{ .retPcPtr = &end };
            ret_[ret_size + 2] = Value{ .retFramePtr = self.c.fp };

            // Load context.
            const ptr: [*]cy.Inst = @ptrFromInt(func_ptr.val);
            self.c.pc = ptr;
            self.c.fp = ret_ + ret_size;

            try exec_auto(self, true);

            // Restore context upon success.
            self.c.pc = @ptrCast(cur_pc);
            self.c.fp = self.c.stack_ptr + fp_save;
            self.c.fp_end = self.c.stack_ptr + fp_end_save;
        }
        return self.c.stack_ptr + ret_off;
    }

    pub fn getFiberContext(self: *const cy.Thread) PcFpOff {
        return .{
            .pc = self.c.pc,
            .fp = getStackOffset(self.c.stack_ptr, self.c.fp),
        };
    }

    pub fn getTraceFiberContext(self: *const cy.Thread) PcFpOff {
        return .{
            .pc = @ptrCast(self.c.trace_pc),
            .fp = getStackOffset(@ptrCast(self.c.stack_ptr), @ptrCast(self.c.trace_fp)),
        };
    }

    fn panic(self: *Thread, msg: []const u8) error{Panic, OutOfMemory} {
        @branchHint(.cold);
        const dupe = try self.alloc.dupe(u8, msg);
        self.c.panic_payload = @as(u64, @intFromPtr(dupe.ptr)) | (@as(u64, dupe.len) << 48);
        self.c.panic_type = vmc.PANIC_MSG;
        logger.tracev("{s}", .{dupe});
        return error.Panic;
    }

    fn panicFmt(self: *Thread, format: []const u8, args: []const fmt.FmtValue) error{Panic} {
        @branchHint(.cold);
        const msg = fmt.allocFormat(self.alloc, format, args) catch |err| {
            if (err == error.OutOfMemory) {
                self.c.curFiber.panicType = vmc.PANIC_INFLIGHT_OOM;
                return error.Panic;
            } else {
                cy.panic("unexpected");
            }
        };
        self.panic_payload = @as(u64, @intFromPtr(msg.ptr)) | (@as(u64, msg.len) << 48);
        self.panic_type = vmc.PANIC_MSG;
        logger.tracev("{s}", .{msg});
        return error.Panic;
    }

    pub fn handleExecResult(self: *Thread, res: vmc.ResultCode) !void {
        logger.tracev("handle exec error: {}", .{res});
        if (res == vmc.RES_PANIC) {
            try handleInterrupt(self);
        } else if (res == vmc.RES_UNKNOWN) {
            logger.tracev("Unknown error code.", .{});
            try handleInterrupt(self);
        }
    }

    /// If successful, execution should continue.
    pub fn handleInterrupt(self: *Thread) !void {
        if (self.unwinding) {
            // Abort.
            std.debug.panic("Panic during unwinding. TODO: Abort.", .{});
        } else {
            self.unwinding = true;
        }
        try @call(.never_inline, cy.thread.panic, .{ self, Value.initRaw(self.c.panic_payload) });
        self.unwinding = false;
        return error.Panic;
    }

    /// To reduce the amount of code inlined in the hot loop, handle StackOverflow at the top and resume execution.
    /// This is also the entry way for native code to call into the VM, assuming pc, framePtr, and virtual registers are already set.
    pub fn exec_auto(self: *Thread, handle_panic: bool) error{OutOfMemory, Panic, NoDebugSym, Unexpected, Await, End}!void {
        logger.tracev("begin eval loop", .{});

        if (comptime build_config.vmEngine == .zig) {
            return error.Unsupported;
        } else if (comptime build_config.vmEngine == .c) {
            vmc.cur_thread = @ptrCast(self);
            while (true) {
                const res = vmc.execBytecode(@ptrCast(self));

                if (res == vmc.RES_SUCCESS) {
                    break;
                }

                if (res == vmc.RES_AWAIT) {
                    return error.Await;
                }

                if (res == vmc.RES_STACK_OVERFLOW) {
                    try @call(.never_inline, cy.Thread.growStackAuto, .{self});
                    continue;
                }

                if (handle_panic) {
                    try @call(.never_inline, handleExecResult, .{self, res});
                } else {
                    return error.Panic;
                }
            }
        } else {
            @compileError("Unsupported engine.");
        }
    }

    pub fn getByte(self: *Thread, idx: u32) u8 {
        return self.getValue(idx).asByte();
    }

    pub fn getI8(self: *Thread, idx: u32) i8 {
        return self.getValue(idx).asI8();
    }

    pub fn getU8(self: *Thread, idx: u32) u8 {
        return self.getValue(idx).asU8();
    }

    pub fn getU16(self: *Thread, idx: u32) u16 {
        return self.getValue(idx).asU16();
    }

    pub fn getI16(self: *Thread, idx: u32) i16 {
        return self.getValue(idx).asI16();
    }

    pub fn getI32(self: *Thread, idx: u32) i32 {
        return self.c.fp[CallArgStart + idx].asI32();
    }

    pub fn getInt(self: *Thread, idx: u32) i64 {
        return self.getValue(idx).asInt();
    }

    pub fn getR64(self: *Thread, idx: u32) u64 {
        return self.getValue(idx).val;
    }

    pub fn setInt(self: *Thread, idx: u32, i: i64) void {
        self.c.fp[CallArgStart + idx] = Value.initInt(i);
    }    

    pub fn ret(self: *Thread, comptime Ret: type) *Ret {
        return @ptrCast(@alignCast(self.ret_sized(@sizeOf(Ret))));
    }

    pub fn ret_sized(self: *Thread, byte_size: usize) *anyopaque {
        const reg_size = (byte_size + 7) >> 3;
        // Need to advance `fp_end` so that an extern calling back knows where push a new frame.
        self.c.fp_end = self.c.fp + 4;
        return self.c.fp - reg_size;
    }

    pub fn param_sized(self: *Thread, byte_size: usize) []Value {
        const res = @as([*]Value, @ptrCast(self.c.fp_end));
        const reg_size = (byte_size + 7) >> 3;
        self.c.fp_end = self.c.fp_end.? + reg_size;
        return res[0..reg_size];
    }

    pub fn param(self: *Thread, comptime A: type) A {
        const res = @as(*A, @ptrCast(self.c.fp_end)).*;
        const arg_size = (@sizeOf(A) + 7) >> 3;
        self.c.fp_end = self.c.fp_end.? + arg_size;
        return res;
    }

    pub fn getPtr(self: *Thread, comptime Ptr: type, idx: u32) Ptr {
        return self.getValue(idx).asPtr(Ptr);
    }

    pub fn getAnyPtr(self: *Thread, idx: u32) ?*anyopaque {
        return self.getValue(idx).asAnyPtr();
    }

    pub fn getFloat(self: *Thread, idx: u32) f64 {
        return self.getValue(idx).asF64();
    }

    pub fn getF32(self: *Thread, idx: u32) f32 {
        return self.getValue(idx).asF32();
    }

    pub fn getString(self: *Thread, idx: u32) []const u8 {
        return self.getValue(idx).asString();
    }

    pub fn getBool(self: *Thread, idx: u32) bool {
        return self.getValue(idx).asBool();
    }

    pub fn setBool(self: *Thread, idx: u32, b: bool) void {
        self.c.fp[CallArgStart + idx] = Value.initBool(b);
    }    

    pub fn getSymbol(self: *Thread, idx: u32) u64 {
        return self.getValue(idx).asSymbol();
    }

    pub fn setSymbol(self: *Thread, idx: u32, id: u64) void {
        self.c.fp[CallArgStart + idx] = Value.initSymbol(id);
    }    

    /// This can be used to get boxed values.
    pub fn getValue(self: *Thread, idx: u32) Value {
        return @bitCast(self.c.fp[CallArgStart + idx]);
    }

    pub fn callUnion(self: *Thread, callee: Value, args: []const Value) !cy.Value {
        {
            return error.TODO;
        }
        const ret_ = self.getNewCallFuncRet();
        if (ret_ == 0) {
            self.c.framePtr = self.c.stack;
        }

        const cur_pc = self.c.pc;
        const cur_fp = self.c.framePtr;

        // Copy callee + args to a new blank frame.
        @memcpy(cur_fp[ret_+CallArgStart..ret_+CallArgStart+args.len], args);

        const func = callee.asPtr(*cy.heap.Func);
        switch (func.kind) {
            .pinned_closure,
            .closure => {
                if (@intFromPtr(cur_fp + ret_ + func.stack_size) >= @intFromPtr(self.c.stackEndPtr)) {
                    return error.StackOverflow;
                }

                const new_fp = cur_fp + ret_;
                // Copy closure to local.
                new_fp[1] = buildCallInfo(false, 0, @intCast(func.stack_size));
                new_fp[2] = Value{ .retPcPtr = cur_pc };
                new_fp[3] = Value{ .retFramePtr = cur_fp };
                new_fp[4] = callee;

                self.c.pc = func.data.closure.pc;
                self.c.framePtr = new_fp;
            },
            .bc => {
                if (@intFromPtr(cur_fp + ret_ + func.stack_size) >= @intFromPtr(self.c.stackEndPtr)) {
                    return error.StackOverflow;
                }

                const new_fp = cur_fp + ret_;
                new_fp[1] = buildCallInfo(false, 0, @intCast(func.stack_size));
                new_fp[2] = Value{ .retPcPtr = cur_pc };
                new_fp[3] = Value{ .retFramePtr = cur_fp };

                self.c.pc = func.data.bc.pc;
                self.c.framePtr = new_fp;
            },
            .host => {
                self.c.pc = cur_pc;
                self.c.framePtr = cur_fp + ret_;
                cur_fp[ret_ + 1] = buildHostCallInfo(false);
                cur_fp[ret_ + 2] = Value{ .retPcPtr = cur_pc };
                cur_fp[ret_ + 3] = Value{ .retFramePtr = cur_fp };
                const res: Value = @bitCast(func.data.host.ptr.?(@ptrCast(self)));
                if (res.isInterrupt()) {
                    return error.Panic;
                }
                return @bitCast(res);
            },
        }

        self.main_thread.evalLoopGrowStack(false) catch |err| {
            if (err == error.Await) {
                return error.Await;
            } else if (err == error.Panic) {
                return error.Panic;
            } else {
                @panic("Unsupported result code.");
            }
        };
        // Restore context upon success.
        self.c.pc = cur_pc;
        self.c.framePtr = cur_fp;
        return cur_fp[ret];
    }

    pub fn callPtrInst(self: *Thread, pc: [*]cy.Inst, fp: [*]Value, base: u16) !PcFp {
        const base_ptr = fp + base;
        const callee = base_ptr[3];
        const ptr: *anyopaque = @ptrFromInt(callee.val & ~(@as(u64, 1) << 63));
        const host_func = (callee.val >> 63) != 0;
        if (host_func) {
            self.c.pc = @ptrCast(pc);
            self.c.fp = @ptrCast(fp + base);
            base_ptr[0] = buildHostCallInfo(false);
            base_ptr[1] = Value{ .retPcPtr = pc };
            base_ptr[2] = Value{ .retFramePtr = fp };
            const host_fn: vmc.HostFn = @ptrCast(@alignCast(ptr));
            const res: c.Ret = host_fn.?(@ptrCast(self));
            if (res != 0) {
                return error.Panic;
            }
            return PcFp{
                .pc = pc + cy.bytecode.CallPtrInstLen,
                .fp = fp,
            };
        } else {
            base_ptr[0] = buildCallInfo(true, cy.bytecode.CallPtrInstLen, 0);
            base_ptr[1] = Value{ .retPcPtr = pc + cy.bytecode.CallPtrInstLen };
            base_ptr[2] = Value{ .retFramePtr = fp };
            return PcFp{
                .pc = @ptrCast(ptr),
                .fp = base_ptr,
            };
        }
    }

    pub fn callTraitSymInst(
        self: *Thread, pc: [*]cy.Inst, fp: [*]Value, func_ptr: *anyopaque, host_func: bool, base: u16,
    ) !PcFp {
        if (host_func) {
            // // Optimize.
            // pc[0] = cy.Inst.initOpCode(.call_host_ic);
            // @as(*align(1) u48, @ptrCast(pc + 4)).* = @intCast(@intFromPtr(func.data.host_func));

            self.c.pc = @ptrCast(pc);
            self.c.fp = @ptrCast(fp + base);
            fp[base] = buildHostCallInfo(false);
            fp[base+1] = Value{ .retPcPtr = pc };
            fp[base+2] = Value{ .retFramePtr = fp };

            const ptr: vmc.HostFn = @ptrCast(@alignCast(func_ptr));
            const res: c.Ret = @bitCast(ptr.?(@ptrCast(self)));
            if (res != 0) {
                return error.Panic;
            }
            return PcFp{
                .pc = pc + cy.bytecode.CallTraitInstLen,
                .fp = fp,
            };
        } else {
            // // Optimize.
            // pc[0] = cy.Inst.initOpCode(.call_ic);
            // pc[10] = cy.Inst{ .val = @intCast(func.data.func.stackSize) };
            // @as(*align(1) u48, @ptrCast(pc + 4)).* = @intCast(@intFromPtr(func.data.func.pc));

            const base_ptr = fp + base;
            base_ptr[0] = buildCallInfo(true, cy.bytecode.CallTraitInstLen, 0);
            base_ptr[1] = Value{ .retPcPtr = pc + cy.bytecode.CallTraitInstLen };
            base_ptr[2] = Value{ .retFramePtr = fp };
            return PcFp{
                .pc = @ptrCast(func_ptr),
                .fp = base_ptr,
            };
        }
    }

    pub fn callUnionInst(self: *Thread, pc: [*]cy.Inst, fp: [*]Value, base: u16) !PcFp {
        const base_ptr = fp + base;
        const callee = base_ptr[3];
        const func = callee.asPtr(*cy.heap.Func);
        switch (func.kind) {
            .pinned_closure,
            .closure => {
                // Copy closure to local.
                base_ptr[0] = buildCallInfo(true, cy.bytecode.CallUnionInstLen, 0);
                base_ptr[1] = Value{ .retPcPtr = pc + cy.bytecode.CallUnionInstLen };
                base_ptr[2] = Value{ .retFramePtr = fp };
                return PcFp{
                    .pc = func.data.closure.pc,
                    .fp = base_ptr,
                };
            },
            .bc => {
                base_ptr[0] = buildCallInfo(true, cy.bytecode.CallUnionInstLen, 0);
                base_ptr[1] = Value{ .retPcPtr = pc + cy.bytecode.CallUnionInstLen };
                base_ptr[2] = Value{ .retFramePtr = fp };
                return PcFp{
                    .pc = func.data.bc.pc,
                    .fp = base_ptr,
                };
            },
            .host => {
                self.c.pc = @ptrCast(pc);
                self.c.fp = @ptrCast(fp + base);
                fp[base + 0] = buildHostCallInfo(false);
                fp[base + 1] = Value{ .retPcPtr = pc };
                fp[base + 2] = Value{ .retFramePtr = fp };
                const res: c.Ret = @bitCast(func.data.host.ptr.?(@ptrCast(self)));
                if (res != 0) {
                    return error.Panic;
                }
                return PcFp{
                    .pc = pc + cy.bytecode.CallUnionInstLen,
                    .fp = fp,
                };
            },
        }
    }

    pub fn free_panic(self: *const Thread) void {
        const panicT: PanicType = @enumFromInt(self.c.panic_type);
        if (panicT == .msg) {
            const ptr: usize = @intCast(self.c.panic_payload & ((1 << 48) - 1));
            const len: usize = @intCast(self.c.panic_payload >> 48);
            self.alloc.free(@as([*]const u8, @ptrFromInt(ptr))[0..len]);
        }
    }

    pub fn ret_panic(self: *cy.Thread, msg: []const u8) c.Ret {
        @branchHint(.cold);
        const dupe = self.alloc.dupe(u8, msg) catch cy.fatal();
        self.c.panic_payload = (@as(u64, @intCast(@intFromPtr(dupe.ptr))) & 0xffffffffffff) | (@as(u64, dupe.len) << 48);
        self.c.panic_type = vmc.PANIC_MSG;
        return c.RetInterrupt;
    }

    pub inline fn stackEnsureTotalCapacity(self: *cy.Thread, newCap: usize) !void {
        if (newCap > self.c.stack_len) {
            try stackGrowTotalCapacity(self, newCap);
        }
    }

    pub fn stackEnsureTotalCapacityPrecise(self: *cy.Thread, newCap: usize) !void {
        if (newCap > self.c.stack_len) {
            try stackGrowTotalCapacityPrecise(self, newCap);
        }
    }

    pub fn stackGrowTotalCapacity(self: *cy.Thread, newCap: usize) !void {
        var betterCap = self.c.stack_len;
        while (true) {
            betterCap +|= betterCap / 2 + 8;
            if (betterCap >= newCap) {
                break;
            }
        }
        try stackGrowTotalCapacityPrecise(self, betterCap);
    }

    pub fn stackGrowTotalCapacityPrecise(self: *cy.Thread, newCap: usize) !void {
        if (self.alloc.resize(self.c.stack(), newCap)) {
            self.c.stack_len = newCap;
            self.c.stack_end = self.c.stack_ptr + newCap;
        } else {
            const new_stack = try self.alloc.realloc(self.c.stack(), newCap);
            self.c.stack_ptr = new_stack.ptr;
            self.c.stack_len = new_stack.len;
            self.c.stack_end = self.c.stack_ptr + newCap;

            if (builtin.is_test or cy.Trace) {
                // Fill the stack with null heap objects to surface undefined access better.
                @memset(self.c.stack(), Value.initPtr(&DummyPoolObject.data));
            }
        }
    }

    // Performs stackGrowTotalCapacityPrecise in addition to patching the frame pointers.
    pub fn growStackAuto(self: *Thread) !void {
        @branchHint(.cold);
        // Grow by 50% with minimum of 16.
        var growSize = self.c.stack_len / 2;
        if (growSize < 16) {
            growSize = 16;
        }
        logger.tracev("grow stack to size={}", .{self.c.stack_len + growSize});
        try self.growStackPrecise(self.c.stack_len + growSize);
    }

    fn growStackPrecise(self: *Thread, newCap: usize) !void {
        if (self.alloc.resize(self.c.stack(), newCap)) {
            self.c.stack_len = newCap;
            self.c.stack_end = self.c.stack_ptr + newCap;
            // `alloc.resize` has pointer stability. No more work left.

            // Revert to frame base ptr.
            // This assumes that pc is at a `CHK_STK` inst which will set the ret frame ptr.
            std.debug.assert(self.c.pc[0].opcode() == .chk_stk);
            const ret_size = @as(*const align(1) u16, @ptrCast(self.c.pc + 1)).*;
            self.c.fp += ret_size;
        } else {
            const newStack = try self.alloc.alloc(Value, newCap);

            // Copy to new stack.
            @memcpy(newStack[0..self.c.stack_len], self.c.stack());

            // Patch frame ptrs and ptr values.
            var pc: ?[*]cy.Inst = self.c.pc;
            var fp = getStackOffset(self.c.stack_ptr, self.c.fp);
            var frame = getFrame(self, self.c.stack(), fp, pc);
            const top_base_off = getStackOffset(self.c.stack_ptr, frame.base.?);
            while (frame.frame_type() != .init) {
                if (get_ptr_layout(self.c.vm, pc.?)) |layout| {
                    patch_frame_ptrs(self, fp, layout, newStack.ptr);
                }
                const prev = frame.prev_context();
                const base_off = getStackOffset(self.c.stack_ptr, frame.base.?);
                const prev_fp_off = getStackOffset(self.c.stack_ptr, prev.fp);
                newStack[base_off + 2].retFramePtr = newStack.ptr + prev_fp_off;
                pc = prev.pc;
                fp = prev_fp_off;
                frame = getFrame(self, self.c.stack(), fp, pc);
            }

            // Free old stack.
            self.alloc.free(self.c.stack());

            // Update to new frame base ptr.
            // This assumes that pc is at a `CHK_STK` inst which will set the ret frame ptr.
            std.debug.assert(self.c.pc[0].opcode() == .chk_stk);
            self.c.fp = newStack.ptr + top_base_off;
            self.c.stack_ptr = newStack.ptr;
            self.c.stack_len = newStack.len;
            self.c.stack_end = self.c.stack_ptr + newCap;
        }
    }

    pub fn ensureTotalStackCapacity(self: *cy.Thread, newCap: usize) !void {
        if (newCap > self.c.stack_len) {
            var betterCap = self.c.stack_len;
            while (true) {
                betterCap +|= betterCap / 2 + 8;
                if (betterCap >= newCap) {
                    break;
                }
            }
            try self.growStackPrecise(betterCap);
        }
    }

    /// Walks the stack and records each frame.
    pub fn recordCurFrames(self: *cy.Thread) !void {
        @branchHint(.cold);
        logger.tracev("recordCompactFrames", .{});

        var fp = getStackOffset(self.c.stack_ptr, self.c.fp);
        var pc: ?[*]cy.Inst = self.c.pc;
        while (true) {
            const frame = getFrame(self, self.c.stack(), fp, pc);
            const frame_t = frame.frame_type();
            logger.tracev("record on pc={*}, fp={}, type={}", .{pc, fp, frame_t});

            if (fp == 0 or frame_t == .vm) {
                try self.compact_trace.append(self.alloc, .{
                    .pc = @ptrCast(pc),
                    .fpOffset = fp,
                });
                if (fp == 0) {
                    // Main.
                    break;
                } else {
                    const prev = frame.prev_context();
                    fp = getStackOffset(self.c.stack_ptr, prev.fp);
                    pc = prev.pc;
                }
            } else {
                if (frame_t == .host) {
                    try self.compact_trace.append(self.alloc, .{
                        .pc = null,
                        .fpOffset = fp,
                    });
                }
                const prev = frame.prev_context();
                fp = getStackOffset(self.c.stack_ptr, prev.fp);
                pc = prev.pc;
            }
        }
    }

    /// Unwind from `ctx` and release each frame.
    /// TODO: See if releaseFiberStack can resuse the same code.
    pub fn unwindStack(self: *cy.Thread, tstack: []Value, cx: PcFpOff) !PcFpOff {
        logger.tracev("panic unwind {*}", .{tstack.ptr + cx.fp});
        var pc = cx.pc;
        var fp = cx.fp;

        self.compact_trace.clearRetainingCapacity();

        while (true) {
            const frame = getFrame(self, tstack, fp, pc);
            const frame_t = frame.frame_type();
            logger.tracev("unwind on frame pc={*}, fp={}, type={}", .{pc, fp, frame_t});
            if (frame_t == .init) {
                try self.compact_trace.append(self.alloc, .{
                    .pc = @ptrCast(pc),
                    .fpOffset = fp,
                });
                // Done, at main block.
                return PcFpOff{ .pc = pc, .fp = fp };
            }
            if (frame_t == .vm) {
                try self.compact_trace.append(self.alloc, .{
                    .pc = @ptrCast(pc),
                    .fpOffset = fp,
                });
            } else if (frame_t == .host) {
                // Record host frames.
                try self.compact_trace.append(self.alloc, .{
                    .pc = null,
                    .fpOffset = fp,
                });
            } else if (frame_t == .gen) {
                try self.compact_trace.append(self.alloc, .{
                    .pc = @ptrCast(pc),
                    .fpOffset = fp,
                });
            }
            if (frame_t == .gen) {
                const gen = tstack[fp+3].asPtr(*vmc.Generator);
                pc = tstack[fp + 2].retPcPtr - tstack[fp + 1].call_info.call_inst_off;
                fp = getStackOffset(tstack.ptr, @ptrCast(gen.prev_fp));
            } else {
                const prev = frame.prev_context();
                pc = prev.pc;
                fp = getStackOffset(tstack.ptr, prev.fp);
            }
        }
    }

    pub fn check_memory(self: *cy.Thread) !c.MemoryCheck {
        self.log_tracev("check memory.", .{});

        // TODO: Detect cycles by iterating heap.
        // Mark-sweep isn't a good fit since in unsafe sections of code, stack pointers might not point to a rc object.
        // TODO: Optimize by only looking at cyclable objects.
        return c.MemoryCheck{
            .num_cyc_objects = 0,
        };
    }

    pub fn dump_stats(self: *const Thread) !void {
        const S = struct {
            fn opCountLess(_: void, a: vmc.OpCount, b: vmc.OpCount) bool {
                return a.count > b.count;
            }
        };
        var wa = std.Io.Writer.Allocating.init(self.alloc);
        defer wa.deinit();
        var w = &wa.writer;

        try w.print("main ops evaled: {}\n", .{self.c.trace.totalOpCounts});
        std.sort.pdq(vmc.OpCount, &self.c.trace.opCounts, {}, S.opCountLess);
        var i: u32 = 0;

        while (i < vmc.NumCodes) : (i += 1) {
            if (self.c.trace.opCounts[i].count > 0) {
                const op = std.meta.intToEnum(cy.OpCode, self.c.trace.opCounts[i].code) catch continue;
                try w.print("\t{s} {}\n", .{@tagName(op), self.c.trace.opCounts[i].count});
            }
        }
        self.c.vm.log(wa.written());
    }

    pub fn log(self: *cy.Thread, comptime format: []const u8, args: anytype) void {
        const msg = std.fmt.allocPrint(self.alloc, format, args) catch @panic("error");
        defer self.alloc.free(msg);
        self.c.vm.log(msg);
    }

    pub fn log_tracev(self: *cy.Thread, comptime format: []const u8, args: anytype) void {
        if (cy.Trace and c.verbose()) {
            self.log(format, args);
        }
    }
};

const FrameInfo = struct {
    base: ?[*]cy.Value,

    pub fn frame_type(self: *const FrameInfo) FrameType {
        if (self.base) |base| {
            return base[0].call_info.type;
        } else {
            return .init;
        }
    }

    pub fn prev_context(self: *const FrameInfo) PcFp {
        const base = self.base.?;
        const ret_pc: ?[*]cy.Inst = base[1].retPcPtr;
        if (ret_pc) |pc| {
            return PcFp{ .pc = pc - base[0].call_info.call_inst_off, .fp = base[2].retFramePtr };
        } else {
            return PcFp{ .pc = null, .fp = base[2].retFramePtr };
        }
    }
};

// Determine whether it's a vm or host frame.
pub fn getFrame(t: *cy.Thread, stack: []Value, fp_off: u32, opt_pc: ?[*]cy.Inst) FrameInfo {
    if (opt_pc) |pc| {
        if (fp_off == 0) {
            return .{
                .base = null,
            };
        }
        // Recover frame base by obtaining the current function's return address.
        const metadata = cy.debug.get_pc_func_metadata(t.c.vm, pc);
        return .{
            .base = stack.ptr + fp_off + metadata.ret_size,
        };
    } else {
        // Assume `fp_off` is at frame base.
        return .{
            .base = stack.ptr + fp_off,
        };
    }
}

pub const FrameType = enum(u2) {
    vm,
    host,
    init,
    gen,
};

pub const PcFp = struct {
    pc: ?[*]cy.Inst,
    fp: [*]Value,
};

pub const PcFpOff = struct {
    pc: ?[*]cy.Inst,
    fp: u32,
};

pub inline fn getStackOffset(from: [*]const Value, to: [*]const Value) u32 {
    // Divide by eight.
    return @intCast((@intFromPtr(to) - @intFromPtr(from)) >> 3);
}

pub inline fn buildHostCallInfo(comptime cont: bool) Value {
    return .{ .call_info = .{
        .ret_flag = !cont,
        .call_inst_off = 0,
        .stack_size = 4,
        .type = .host,
    }};
}

pub const CallInfo = packed struct {
    /// Whether a ret op should return from the VM loop.
    ret_flag: bool,

    /// Since there are different call insts with varying lengths,
    /// the call convention prefers to advance the pc before saving it so
    /// stepping over the call will already have the correct pc.
    /// An offset is stored to the original call inst for stack unwinding.
    call_inst_off: u7,

    /// Stack size of the function.
    /// Used to dynamically push stack frames when calling into the VM.
    stack_size: u8,

    type: FrameType,
    padding: u14 = undefined,
    payload: u32 = undefined,
};

pub inline fn buildCallInfo2(cont: bool, comptime callInstOffset: u8, stack_size: u8) Value {
    return .{ .call_info = .{
        .ret_flag = !cont,
        .call_inst_off = callInstOffset,
        .stack_size = stack_size,
        .type = .vm,
    }};
}

pub inline fn buildCallInfo(comptime cont: bool, comptime callInstOffset: u8, stack_size: u8) Value {
    return .{ .call_info = .{
        .ret_flag = !cont,
        .call_inst_off = callInstOffset,
        .stack_size = stack_size,
        .type = .vm,
    }};
}

pub const PanicPayload = u64;

pub const PanicType = enum(u8) {
    err = vmc.PANIC_ERROR,
    staticMsg = vmc.PANIC_STATIC_MSG,
    msg = vmc.PANIC_MSG,
    inflightOom = vmc.PANIC_INFLIGHT_OOM,
    none = vmc.PANIC_NONE,
};

var DummyPoolObject = cy.heap.PoolObject{
    .head = .{
        .object = .{
            .meta = cy.NullId,
            .rc = 0,
        },
    },
    .data = undefined,
};

/// Assumes `v` is a cyclable pointer.
fn markValue(t: *cy.Thread, v: cy.Value) void {
    const obj = v.asHeapObject();
    if (!obj.isGcMarked()) {
        // obj.setGcMarked();
    } else {
        // Already marked.
        return;
    }
    // Visit children.
    const typeId = obj.getTypeId();
    switch (typeId) {
        else => {
            const type_ = t.c.vm.sema.types.items[typeId];
            switch (type_.kind()) {
                .struct_t => {
                    // User type.
                    const fields = type_.cast(.struct_t).fields();
                    for (fields, 0..) |field, i| {
                        if (fields[i].type.isObject()) {
                            const member_ptr: *cy.Value = @ptrCast(@alignCast(v.asPtr([*]u8) + field.offset));
                            markValue(t, member_ptr.*);
                        }
                    }
                },
                .ref_trait => {
                    markValue(t, obj.trait.impl);
                },
                .func => {
                    const vals = obj.func.getCapturedValuesPtr()[0..obj.func.data.closure.numCaptured];
                    for (vals) |val| {
                        // TODO: Can this be assumed to always be a Box value?
                        markValue(t, val);
                    }
                },
                else => {},
            }
        },
    }
}

fn HostFrame(comptime T: type) type {
    return extern struct {
        ret: *T,
        arg_ptr: [*]Value,
    };
}

pub fn print_thread_inst(t: *cy.Thread, pc: [*]const cy.Inst) !void {
    var buf: [256]u8 = undefined;
    var w = std.Io.Writer.fixed(&buf);
    cy.thread.write_thread_inst(t, &w, pc) catch @panic("error");
    t.c.vm.log(w.buffered());
}

pub fn write_thread_inst(t: *cy.Thread, w: *std.Io.Writer, pc: [*]const cy.Inst) !void {
    var buf: [16]u8 = undefined;
    const prefix = try std.fmt.bufPrint(&buf, "t{}", .{t.c.id});
    try cy.bytecode.write_inst(t.c.vm, w, pc[0].opcode(), null, null, pc, .{
        .prefix = prefix,
        .extra = null,
    });
}
