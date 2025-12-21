const builtin = @import("builtin");
const std = @import("std");
const cy = @import("../cyber.zig");
const bc = @import("../bc_gen.zig");
pub const stencils = switch (builtin.cpu.arch) {
    .aarch64 => @import("a64_stencils.zig"),
    .x86_64 => @import("x64_stencils.zig"),
    else => void,
};
const log = cy.log.scoped(.jit_gen);
const bt = cy.types.BuiltinTypes;
const cc = @import("../capi.zig");
const v = cy.fmt.v;
const rt = cy.rt;
const ir = cy.ir;
const ast = cy.ast;
pub const as = @import("assembler.zig");
const a64 = @import("a64_assembler.zig");
const A64 = @import("a64.zig");
const x64 = @import("x64_assembler.zig");
const X64 = @import("x64.zig");
const is_wasm = builtin.cpu.arch.isWasm();

const Cstr = bc.Cstr;
const GenValue = bc.GenValue;
const SlotId = bc.SlotId;
const regValue = bc.regValue;

const CallHoleLen = switch (builtin.cpu.arch) {
    .aarch64 => 4,
    .x86_64 => 5,
    else => 0,
};

/// When verbose=true, a debug dump call is generated for each IR expression.
/// The chunk and irIdx dumped can then be set to `GenBreakpointAtIr` so the next run
/// can print the relevant source location and generate a breakpoint.
// const GenBreakpointAtIr: ?ChunkIr = .{ .chunkId = 0, .irIdx = 633 };
const GenBreakpointAtIr: ?ChunkIr = null;
var DumpCodeFrom: ?usize = null;

const ChunkIr = struct {
    chunkId: cy.ChunkId,
    irIdx: u32,
};

pub const RelocType = enum {
    jumpToFunc,
    jump_f,
    jump_cond,

    // BC entering JIT.
    jit_entry,
};

pub const Reloc = struct {
    type: RelocType,
    data: union {
        jumpToFunc: struct {
            func: *cy.Func,
            pc: u32,
        },
        jump_f: struct {
            pc: u32,
            chunk: *cy.Chunk,
            bc_target_pc: u32,
        },
        jump_cond: struct {
            pc: u32,
            chunk: *cy.Chunk,
            bc_target_pc: u32,
        },
        jit_entry: struct {
            chunk: *cy.Chunk,
            bc_pc: u32,
            func: *cy.Func
        },
    },
};

const JitFunc = struct {
    pc: usize,
};

const LabelKey = struct {
    chunk: *cy.Chunk,
    bc_pc: usize,
};

//extern "kernel32" fn FlushInstructionCache(
//    hProcess: std.os.windows.HANDLE,
//    lpBaseAddress: ?*const anyopaque,
//    dwSize: std.os.windows.SIZE_T,
//) callconv(.winapi) std.os.windows.BOOL;

pub const CodeBuffer = struct {
    gpa: std.mem.Allocator,

    executable: bool,
    buf: std.ArrayListAligned(u8, std.mem.Alignment.fromByteUnits(std.heap.page_size_min)),

    /// Forced abstraction by Windows needing a buffer allocated with VirtualAlloc.
    final_buf: []align(std.heap.page_size_min) u8,

    /// Where main begins. Currently only jit code uses this.
    mainPc: u32,

    /// Relocation entries.
    relocs: std.ArrayListUnmanaged(Reloc),

    funcs: std.AutoHashMapUnmanaged(*cy.Func, JitFunc),

    labels: std.AutoHashMapUnmanaged(LabelKey, usize),

    const_slots: std.ArrayList(?[*]const cy.Inst),

    pub fn init(gpa: std.mem.Allocator) CodeBuffer {
        return .{
            .gpa = gpa,
            .buf = .{},
            .final_buf = &.{},
            .mainPc = 0,
            .relocs = .{},
            .funcs = .{},
            .labels = .{},
            .executable = false,
            .const_slots = .{},
        };
    }

    pub fn clear(self: *CodeBuffer) void {
        self.buf.clearRetainingCapacity();
        self.relocs.clearRetainingCapacity();
        self.funcs.clearRetainingCapacity();
        self.labels.clearRetainingCapacity();
        self.const_slots.clearRetainingCapacity();
    }

    pub fn deinit(self: *CodeBuffer) void {
        {
            // Memory must be writable to be freed.
            if (self.executable) {
                self.unset_executable() catch @panic("error");
            }
            self.buf.deinit(self.gpa);
        }

        self.relocs.deinit(self.gpa);
        self.funcs.deinit(self.gpa);
        self.labels.deinit(self.gpa);
        self.const_slots.deinit(self.gpa);
    }

    pub fn unset_executable(self: *CodeBuffer) !void {
        if (cy.isWasm) {
            return error.Unsupported;
        }
        if (!self.executable) {
            return;
        }
        if (builtin.os.tag == .windows) {
            try std.posix.mprotect(self.final_buf, std.c.PROT.READ | std.c.PROT.WRITE);
            std.os.windows.VirtualFree(self.final_buf.ptr, 0, std.os.windows.MEM_RELEASE);
        } else {
            try std.posix.mprotect(self.buf.items.ptr[0..self.buf.capacity], std.c.PROT.WRITE);
        }
        self.executable = false;
    }

    pub fn set_executable(self: *CodeBuffer) !void {
        if (cy.isWasm) {
            return error.Unsupported;
        }

        // Mark code executable.
        try std.posix.mprotect(self.final_buf, std.c.PROT.READ | std.c.PROT.EXEC);
        if (builtin.os.tag == .windows) {
            // const flush_res = FlushInstructionCache(std.os.windows.GetCurrentProcess(), self.exe_buf.ptr, self.exe_buf.len);
            // var info: std.os.windows.MEMORY_BASIC_INFORMATION = undefined;
            // const res = try std.os.windows.VirtualQuery(self.exe_buf.ptr, &info, @sizeOf(std.os.windows.MEMORY_BASIC_INFORMATION));
        }
        self.executable = true;

        // if (jitRes.buf.items.len > 500*4) {
        //     logger.tracev("jit code (size: {}) {}...", .{jitRes.buf.items.len, std.fmt.fmtSliceHexLower(jitRes.buf.items[0..100*4])});
        // } else {
        //     logger.tracev("jit code (size: {}) {}", .{jitRes.buf.items.len, std.fmt.fmtSliceHexLower(jitRes.buf.items)});
        // }
    }

    pub fn ensure_label(self: *CodeBuffer, key: LabelKey) !void {
        if (!self.labels.contains(key)) {
            try self.labels.putNoClobber(self.gpa, key, 0);
        }
    }

    pub fn raw(self: *CodeBuffer) []const u8 {
        return self.buf.items;
    }

    pub fn push(self: *CodeBuffer, code: []const u8) !void {
        const start = try self.reserve(code.len);
        const dst: []u8 = @ptrCast(self.buf.items.ptr[start..start+code.len]);
        @memcpy(dst, code);
    }

    pub fn set_u32(self: *CodeBuffer, pc: usize, code: u32) void {
        const dst: []u8 = @ptrCast(self.buf.items.ptr[pc..pc+@sizeOf(u32)]);
        @memcpy(dst, std.mem.asBytes(&code));
    }

    pub fn push_u32(self: *CodeBuffer, code: u32) !void {
        const start = try self.ensureUnusedCap(@sizeOf(u32));
        self.buf.items.len += @sizeOf(u32);
        self.set_u32(start, code);
    }

    pub fn reserve(self: *CodeBuffer, size: usize) !usize {
        const start = try self.ensureUnusedCap(size);
        self.buf.items.len += size;
        return start;
    }

    pub fn reserve_slice(self: *CodeBuffer, size: usize) ![]u8 {
        const start = try self.ensureUnusedCap(size);
        self.buf.items.len += size;
        return self.buf.items[start..];
    }

    pub fn ensureUnusedCap(self: *CodeBuffer, size: usize) !usize {
        if (self.buf.items.len + size > self.buf.capacity) {
            var inc = self.buf.capacity / 2;
            if (inc <= std.heap.page_size_min) {
                inc = std.heap.page_size_min;
            }
            try self.buf.ensureTotalCapacityPrecise(self.gpa, self.buf.capacity + inc);
        }
        return self.buf.items.len;
    }

    pub fn pos(self: *CodeBuffer) usize {
        return self.buf.items.len;
    }
};

pub const ChunkExt = struct {
    pub const jitCopyAdvance = copyAdvance;
    pub const jitPushU64 = pushU64;
    pub const jitPushStencil = pushStencil;
};

pub fn copyAdvance(c: *cy.Chunk, dst: usize, src: []const u8) void {
    const to: []u8 = @ptrCast(c.jit.buf.items.ptr[dst..dst+src.len]);
    @memcpy(to, src);
    c.jit.buf.items.len = dst+src.len;
}


pub fn pushU64(c: *cy.Chunk, code: u64) !void {
    const start = try c.jitEnsureUnusedCap(@sizeOf(u64));
    c.jit.buf.items.len += @sizeOf(u64);
    const dst: []u8 = @ptrCast(c.jit.buf.items.ptr[start..start+@sizeOf(u64)]);
    @memcpy(dst, std.mem.asBytes(&code));
}

pub fn pushStencil(c: *cy.Chunk, code: []const u8) !usize {
    const start = try c.jitEnsureUnusedCap(code.len);
    c.jit.buf.items.len += code.len;
    const dst: []u8 = @ptrCast(c.jit.buf.items.ptr[start..start+code.len]);
    @memcpy(dst, code);
    return start;
}

fn genStmt(c: *cy.Chunk, idx: u32) anyerror!void {
    const code = c.ir.getStmtCode(idx);
    const node = c.ir.getNode(idx);
    c.curNode = node;

    var dumpEndPc: usize = undefined;
    if (cy.Trace) {
        const contextStr = try c.encoder.format(node, &cy.tempBuf);
        log.tracev("----{s}: {{{s}}}", .{@tagName(code), contextStr});

        if (cc.verbose()) {
            dumpEndPc = try genCallDumpJitSection(c, idx, true);
        }

        if (GenBreakpointAtIr) |chunkIr| {
            if (c.id == chunkIr.chunkId and idx == chunkIr.irIdx) {
                try as.genBreakpoint(c.jit);
            }
        }
    }

    var tempRetainedStart: usize = undefined;
    var slot_count: usize = undefined;
    if (cy.Trace and c.proc_stack.items.len > 0) {
        tempRetainedStart = @intCast(c.unwind_entry_stack.items.len);
        slot_count = bc.numSlots(c);
    }

    switch (code) {
        // .breakStmt          => try breakStmt(c, nodeId),
        // .contStmt           => try contStmt(c, nodeId),
        .declareLocal       => try declareLocal(c, idx, node),
        // .destrElemsStmt     => try destrElemsStmt(c, idx, nodeId),
        .exprStmt           => try exprStmt(c, idx, node),
        // .forIterStmt        => try forIterStmt(c, idx, nodeId),
        // .forRangeStmt       => try forRangeStmt(c, idx, nodeId),
        .funcBlock          => try funcBlock(c, idx, node),
        .mainBlock          => try mainBlock(c, idx, node),
        // .opSet              => try opSet(c, idx, nodeId),
        // .pushDebugLabel     => try pushDebugLabel(c, idx),
        .retExprStmt        => try retExprStmt(c, idx, node),
        // .retStmt            => try retStmt(c),
        // .setCaptured        => try setCaptured(c, idx, nodeId),
        // .setField           => try setField(c, idx, .{}, nodeId),
        // .setFuncSym         => try setFuncSym(c, idx, nodeId),
        // .setIndex           => try setIndex(c, idx, nodeId),
        // .setLocal           => try irSetLocal(c, idx, nodeId),
        // .setObjectField     => try setObjectField(c, idx, .{}, nodeId),
        // .setVarSym          => try setVarSym(c, idx, nodeId),
        // .setLocalType       => try setLocalType(c, idx),
        // .switchStmt         => try switchStmt(c, idx, nodeId),
        // .tryStmt            => try tryStmt(c, idx, nodeId),
        .verbose            => try verbose(c, idx, node),
        // .whileCondStmt      => try whileCondStmt(c, idx, nodeId),
        // .whileInfStmt       => try whileInfStmt(c, idx, nodeId),
        // .whileOptStmt       => try whileOptStmt(c, idx, nodeId),
        else => {
            return error.TODO;
        }
    }

    // Check stack after statement.
    if (cy.Trace and c.proc_stack.items.len > 0) {
        if (c.unwind_entry_stack.items.len != tempRetainedStart) {
            return c.reportErrorFmt("Expected {} unwindable retained temps, got {}",
                &.{v(tempRetainedStart), v(c.unwind_entry_stack.items.len)}, node);
        }

        if (bc.numSlots(c) != slot_count) {
            return c.reportErrorFmt("Expected {} slots, got {}",
                &.{v(slot_count), v(bc.numSlots(c))}, node);
        }
    }
    log.tracev("----{s}: end", .{@tagName(code)});

    if (cy.Trace) {
        if (cc.verbose()) {
            as.patchMovPcRelTo(c, dumpEndPc, c.jitGetPos());
        }
    }
}

fn exprStmt(c: *cy.Chunk, idx: usize, nodeId: *ast.Node) !void {
    _ = nodeId;
    const data = c.ir.getStmtData(idx, .exprStmt);
    const expr = c.ir.advanceStmt(idx, .exprStmt);

    if (data.isBlockResult) {
        const inMain = c.cur_proc.block_depth == 0;
        if (inMain) {
            const exprv = try genExpr(c, expr, Cstr.simpleOwn);
            c.cur_proc.endLocal = exprv.reg;
            try bc.popTempAndUnwind(c, exprv);
        } else {
            // Return from block expression.
            const b = c.blocks.getLast();
            _ = try genExpr(c, expr, b.blockExprCstr);
        }
    } else {
        const exprv = try genExpr(c, expr, Cstr.simple);

        try bc.popTempAndUnwind(c, exprv);

        // TODO: Merge with previous release inst.
        // try bc.releaseTempValue(c, exprv, nodeId);
    }
}

pub fn prepareFunc(c: *cy.Compiler, func: *cy.Func) !void {
    if (func.type == .userLambda) {
        return;
    }
    if (cy.Trace) {
        const symPath = try cy.sym.allocSymName(&c.sema, c.alloc, @ptrCast(func.sym.?), .{});
        defer c.alloc.free(symPath);
        log.tracev("jit prepare func: {s}", .{symPath});
    }
    if (func.type == .hostFunc) {
        // const funcSig = c.sema.getFuncSig(func.funcSigId);
        // const rtFunc = rt.FuncSymbol.initHostFunc(@ptrCast(func.data.hostFunc.ptr), funcSig.reqCallTypeCheck, funcSig.numParams(), func.funcSigId);

        // try c.genSymMap.putNoClobber(c.alloc, func, .{ .hostFuncSym = .{ .ptr = func.data.hostFunc }});
        // if (func.isMethod) {
        //     const parentT = func.sym.?.head.parent.?.getStaticType().?;
        //     const name = func.name();
        //     const mgId = try c.vm.ensureMethod(name);
        //     if (funcSig.reqCallTypeCheck) {
        //         const m = rt.MethodInit.initHostTyped(func.funcSigId, @ptrCast(func.data.hostFunc.ptr), func.numParams);
        //         try c.vm.addMethod(parentT, mgId, m);
        //     } else {
        //         const m = rt.MethodInit.initHostUntyped(func.funcSigId, @ptrCast(func.data.hostFunc.ptr), func.numParams);
        //         try c.vm.addMethod(parentT, mgId, m);
        //     }
        // }
    } else if (func.type == .userFunc) {
        // Func is patched later once funcPc and stackSize is obtained.
        // Method entry is also added later.
    } else {
        log.tracev("{}", .{func.type});
        return error.Unsupported;
    }
}

fn genCallFuncSym(c: *cy.Chunk, idx: usize, cstr: Cstr, nodeId: *ast.Node) !GenValue {
    _ = c;
    _ = idx;
    _ = cstr;
    _ = nodeId;
    // if (data.func.type == .hostFunc) {
    //     // Populate callHost stencil args.
    //     try as.genAddImm(c, .arg0, .fp, 8 * (inst.ret + cy.vm.CallArgStart));
    //     try as.genMovImm(c, .arg1, data.numArgs);

    //     try c.jitPush(stencils.callHost[0..stencils.callHost_hostFunc]);
    //     try as.genCallFuncPtr(c, data.func.data.hostFunc.ptr);
    //     try c.jitPush(stencils.callHost[stencils.callHost_hostFunc+CallHoleLen..]);

    //     // Copy result to ret.
    //     // TODO: Copy directly to final dst.
    //     try as.genStoreSlot(c, inst.ret, .arg2);
    // } else if (data.func.type == .userFunc) {
    //     try as.genCallFunc(c, inst.ret, data.func);
    // } else return error.TODO;
    return undefined;
}

fn zDumpJitStmtSection(vm: *cy.VM, fp: [*]const cy.Value, chunkId: u64, irIdx: u64, startPc: [*]const u8, endPc: [*]const u8) void {
    const c = vm.compiler.chunks.items[@intCast(chunkId)];
    const code = c.ir.getStmtCode(@intCast(irIdx));
    const nodeId = c.ir.getNode(@intCast(irIdx));

    const mc = startPc[0..@intFromPtr(endPc)-@intFromPtr(startPc)];
    const contextStr = c.encoder.format(nodeId, &cy.tempBuf) catch cy.fatal();
    log.tracev("{s} {{{s}}} {*} {} ({}:{})", .{@tagName(code), contextStr, fp, std.fmt.fmtSliceHexLower(mc), chunkId, irIdx});
}

fn zDumpJitExprSection(vm: *cy.VM, fp: [*]const cy.Value, chunkId: u64, irIdx: u64, startPc: [*]const u8, endPc: [*]const u8) void {
    const c = vm.compiler.chunks.items[@intCast(chunkId)];
    const code = c.ir.getExprCode(@intCast(irIdx));
    const nodeId = c.ir.getNode(@intCast(irIdx));

    const mc = startPc[0..@intFromPtr(endPc)-@intFromPtr(startPc)];
    const contextStr = c.encoder.format(nodeId, &cy.tempBuf) catch cy.fatal();
    log.tracev("{s} {{{s}}} {*} {} ({}:{})", .{@tagName(code), contextStr, fp, std.fmt.fmtSliceHexLower(mc), chunkId, irIdx});
}

fn genCallDumpJitSection(c: *cy.Chunk, idx: usize, isStmt: bool) !usize {
    try as.gen_imm(c, .arg0, c.id);
    try as.gen_imm(c, .arg1, idx);
    const dumpStartPc = c.jitGetPos();
    try as.genPatchableMovPcRel(c, .arg2);

    const dumpEndPc = c.jitGetPos();
    try as.genPatchableMovPcRel(c, .arg3);
    try c.jitPush(stencils.dumpJitSection[0..stencils.dumpJitSection_zDumpJitSection]);
    if (isStmt) {
        try as.genCallFuncPtr(c, &zDumpJitStmtSection);
    } else {
        try as.genCallFuncPtr(c, &zDumpJitExprSection);
    }
    try c.jitPush(stencils.dumpJitSection[stencils.dumpJitSection_zDumpJitSection+CallHoleLen..]);
    as.patchMovPcRelTo(c, dumpStartPc, c.jitGetPos());
    return dumpEndPc;
}

fn genExpr(c: *cy.Chunk, idx: usize, cstr: Cstr) anyerror!GenValue {
    const code = c.ir.getExprCode(idx);
    const nodeId = c.ir.getNode(idx);

    var dumpEndPc: usize = undefined;
    if (cy.Trace) {
        const contextStr = try c.encoder.format(nodeId, &cy.tempBuf);
        log.tracev("{s}: {{{s}}} {s}", .{@tagName(code), contextStr, @tagName(cstr.type)});

        if (cc.verbose()) {
            dumpEndPc = try genCallDumpJitSection(c, idx, false);
        }

        if (GenBreakpointAtIr) |chunkIr| {
            if (c.id == chunkIr.chunkId and idx == chunkIr.irIdx) {
                try as.genBreakpoint(c);
            }
        }
    }
    const res = try switch (code) {
        .preCallFuncSym     => genCallFuncSym(c, idx, cstr, nodeId),
        else => return error.TODO,
    };
    log.tracev("{s}: end", .{@tagName(code)});

    if (cy.Trace) {
        if (cc.verbose()) {
            as.patchMovPcRelTo(c, dumpEndPc, c.jitGetPos());
        }
    }

    return res;
}

const OperandKind = enum(u8) {
    reg,
    constant,
};

const Operand = struct {
    kind: OperandKind,
    data: union {
        reg: as.LRegister,
        constant: u64,
    },

    pub fn constant(x: u64) Operand {
        return .{
            .kind = .constant,
            .data = .{
                .constant = x,
            },
        };
    }

    pub fn reg(r: as.LRegister) Operand {
        return .{
            .kind = .reg,
            .data = .{
                .reg = r,
            },
        };
    }
};

fn gen_slot(buf: *CodeBuffer, reg: as.LRegister, slot: u16) !Operand {
    if (buf.const_slots.items[slot]) |pc| {
        // Contains inline constant.
        switch (pc[0].opcode()) {
            .const_16s => {
                const val: i64 = @as(i16, @bitCast(pc[2]));
                buf.const_slots.items[slot] = null;
                return Operand.constant(@bitCast(val));
            },
            else => {
                std.debug.print("jit: Unsupported const inst: {}\n", .{pc[0].opcode()});
                return error.Unsupported;
            },
        }
    } else {
        try as.genLoadSlot(buf, reg, slot);
        return Operand.reg(reg);
    }
}

fn gen_slot_inline(buf: *CodeBuffer, reg: as.LRegister, slot: u16) !void {
    const operand = try gen_slot(buf, reg, slot);
    switch (operand.kind) {
        .reg => {},
        .constant => {
            try as.gen_imm(buf, reg, operand.data.constant);
        },
    }
}

fn gen_cmp_jmp(buf: *CodeBuffer, cond: as.LCond, left: u16, right: u16, c: *cy.Chunk, bc_target_pc: usize) !void {
    try gen_slot_inline(buf, .temp, left);
    try gen_slot_inline(buf, .temp2, right);

    try as.genCmp(buf, .temp, .temp2);
    const pos = buf.pos();
    try as.genJumpCond(buf, cond, 0);

    try buf.relocs.append(buf.gpa, .{ .type = .jump_cond, .data = .{ .jump_cond = .{
        .chunk = c,
        .pc = @intCast(pos),
        .bc_target_pc = @intCast(bc_target_pc),
    }}});
    try buf.ensure_label(.{ .chunk=c, .bc_pc=@intCast(bc_target_pc) });
}

fn gen_func(c: *cy.Chunk, func: *cy.Func) !void {
    const sym = c.compiler.genSymMap.get(func).?;
    const pc_len = sym.func.end_pc - sym.func.pc;

    const buf = c.jit;

    var pc = sym.func.static_pc;
    const end = sym.func.static_pc + pc_len;

    // Skip `enter_jit` inst.
    std.debug.assert(pc[0].opcode() == .enter_jit);
    pc += cy.bytecode.getInstLenAt(pc);

    std.debug.assert(pc[0].opcode() == .chk_stk);
    const ret_size = pc[1].val;
    const frame_size = pc[2].val;
    try c.jit.const_slots.resize(c.alloc, @intCast(frame_size));
    const const_slots = c.jit.const_slots.items;
    @memset(const_slots, null);

    // Load all BC backward labels.
    for (c.buf.labels.items) |bc_pc| {
        try c.jit.labels.putNoClobber(c.alloc, .{.chunk=c, .bc_pc=bc_pc}, 0);
    }

    // Record pc.
    try c.jit.funcs.put(c.alloc, func, .{ .pc = c.jit.pos() });

    // Emit VM/JIT -> JIT prologue.     

    // Persist return addr reg into stack.
    if (builtin.cpu.arch == .aarch64) {
        try buf.push_u32(A64.LoadStore.strImmOff(a64.FpReg, @intCast(ret_size+1), .x30).bitCast());
    } else if (builtin.cpu.arch == .x86_64) {
        // try c.x64Enc.int3();
        // TODO: Currently still pass return address from [rsp] but the i2c allows a dedicated register.
        try x64.push_load(buf, .rax, .sibBase(.initReg(.rsp), 0));
        try x64.push_store(buf, .sibBase(.initReg(x64.FpReg), (ret_size+1)*8), .rax);
    }

    while (@intFromPtr(pc) < @intFromPtr(end)) {
        // TODO: A better way to map jump labels is for each function to hold the start idx to `Chunk.buf.labels`.
        //       As JIT iterates the BC, keep a rolling next label to match with. This would avoid a hashtable lookup for every inst.
        const pc_off = pc - c.buf.ops.items.ptr;
        if (c.jit.labels.contains(.{.chunk=c, .bc_pc=pc_off})) {
            try c.jit.labels.put(c.alloc, .{.chunk=c, .bc_pc=pc_off}, c.jit.pos());
        }

        // var msg_buf: [1024]u8 = undefined;
        // const msg = try std.fmt.bufPrint(&msg_buf, "{}", .{pc[0].opcode()});
        // const heap_msg = try c.buf.getOrPushConstString(msg);
        // try as.gen_nop(buf); // Indicates skipped bytecode generation if this is first inst at breakpoint.
        // try as.gen_log(c.jit, heap_msg.slice);
        // try as.genBreakpoint(buf);

        const inst_len = cy.bytecode.getInstLenAt(pc);
        switch (pc[0].opcode()) {
            .ret => {
                try as.gen_func_ret(buf, 1);
            },
            .mov => {
                try as.genLoadSlot(buf, .temp, pc[2].val);
                try as.genStoreSlot(buf, pc[1].val, .temp);
            },
            .jump_f => {
                try as.gen_imm(buf, .arg0, pc[1].val);
                const pos = buf.pos();
                // TODO: Push up to the cond jump and patch that instead.
                try buf.push(&stencils.jump_f);
                const jump_off = pc[2].val;
                try buf.relocs.append(c.alloc, .{ .type = .jump_f, .data = .{ .jump_f = .{
                    .chunk = c,
                    .pc = @intCast(pos + stencils.jump_f_br3),
                    .bc_target_pc = @intCast(pc_off + jump_off),
                }}});
                try buf.ensure_label(.{ .chunk=c, .bc_pc=pc_off + jump_off });
            },
            .call => {
                const base = pc[1].val;
                const func_vm_pc: usize = std.mem.readInt(u48, @ptrCast(pc + 2), .little);
                const func_id: u32 = @as(*const align(2) u32, @ptrFromInt(func_vm_pc - 4)).*;
                const details = &c.vm.funcSymDetails.items[func_id];
                const callee = details.func.?;
                if (!callee.info.jit) {
                    return c.reportError("Unsupported JIT -> VM call.", func.decl);
                }

                // Can omit for now.
                // fp[0] = JIT_CALLINFO(0, 0);

                // return addr saved in callee func prologue.

                // Save current fp.
                // Fewer ops when generating imm base and base+offset than to a stencil variable.
                try as.genStoreSlot(buf, base + 2, .fp);

                // Advance fp. Unlike BC, skips base and goes directly to the frame start.
                try as.gen_add_imm(buf, .fp, .fp, 8 * (base - details.ret_size));

                try as.genCallFunc(buf, callee);
            },
            .trap => {
                try as.genBreakpoint(buf);
            },
            .add => {
                const dst = pc[1].val;
                const left = try gen_slot(buf, .temp, pc[2].val);
                var right: Operand = undefined;
                if (left.kind == .constant) {
                    try gen_slot_inline(buf, .temp, pc[3].val);
                    right = left;
                } else {
                    right = try gen_slot(buf, .temp2, pc[3].val);
                }
                switch (right.kind) {
                    .constant => {
                        try as.gen_add_imm(buf, .temp, .temp, right.data.constant);
                    },
                    .reg => {
                        try as.gen_add(buf, .temp, right.data.reg);
                    },
                }
                try as.genStoreSlot(buf, dst, .temp);
            },
            .sub => {
                const dst = pc[1].val;
                try gen_slot_inline(buf, .temp, pc[2].val);
                const right = try gen_slot(buf, .temp2, pc[3].val);
                switch (right.kind) {
                    .constant => {
                        try as.gen_sub_imm(buf, .temp, .temp, right.data.constant);
                    },
                    .reg => {
                        try as.gen_sub(buf, .temp, right.data.reg);
                    },
                }
                try as.genStoreSlot(buf, dst, .temp);
            },
            .lt => {
                const dst = pc[1].val;
                if (pc[inst_len].opcode() == .jump_f) {
                    if (pc[0].temp_dst() and pc[inst_len + 1].val == dst) {
                        const bc_target_pc = pc_off + inst_len + pc[inst_len + 2].val;
                        try gen_cmp_jmp(buf, .ge, pc[2].val, pc[3].val, c, bc_target_pc);
                        pc += inst_len;
                        pc += cy.bytecode.getInstLenAt(pc);
                        continue;
                    }
                }
                try as.gen_imm(buf, .arg0, dst);
                try gen_slot_inline(buf, .arg1, pc[2].val);
                try gen_slot_inline(buf, .arg2, pc[3].val);
                try buf.push(&stencils.lt);
            },
            .le => {
                const dst = pc[1].val;
                if (pc[inst_len].opcode() == .jump_f) {
                    if (pc[0].temp_dst() and pc[inst_len + 1].val == dst) {
                        const bc_target_pc = pc_off + inst_len + pc[inst_len + 2].val;
                        try gen_cmp_jmp(buf, .le, pc[2].val, pc[3].val, c, bc_target_pc);
                        pc += inst_len;
                        pc += cy.bytecode.getInstLenAt(pc);
                        continue;
                    }
                }
                try as.gen_imm(buf, .arg0, dst);
                try gen_slot_inline(buf, .arg1, pc[2].val);
                try gen_slot_inline(buf, .arg2, pc[3].val);
                try buf.push(&stencils.le);
            },
            .gt => {
                const dst = pc[1].val;
                if (pc[inst_len].opcode() == .jump_f) {
                    if (pc[0].temp_dst() and pc[inst_len + 1].val == dst) {
                        const bc_target_pc = pc_off + inst_len + pc[inst_len + 2].val;
                        try gen_cmp_jmp(buf, .gt, pc[2].val, pc[3].val, c, bc_target_pc);
                        pc += inst_len;
                        pc += cy.bytecode.getInstLenAt(pc);
                        continue;
                    }
                }
                try as.gen_imm(buf, .arg0, dst);
                try gen_slot_inline(buf, .arg1, pc[2].val);
                try gen_slot_inline(buf, .arg2, pc[3].val);
                try buf.push(&stencils.gt);
            },
            .ge => {
                const dst = pc[1].val;
                if (pc[inst_len].opcode() == .jump_f) {
                    if (pc[0].temp_dst() and pc[inst_len + 1].val == dst) {
                        const bc_target_pc = pc_off + inst_len + pc[inst_len + 2].val;
                        try gen_cmp_jmp(buf, .ge, pc[2].val, pc[3].val, c, bc_target_pc);
                        pc += inst_len;
                        pc += cy.bytecode.getInstLenAt(pc);
                        continue;
                    }
                }
                try as.gen_imm(buf, .arg0, dst);
                try gen_slot_inline(buf, .arg1, pc[2].val);
                try gen_slot_inline(buf, .arg2, pc[3].val);
                try buf.push(&stencils.ge);
            },
            .const_16s => {
                if (pc[0].temp_dst()) {
                    const dst = pc[1].val;
                    const_slots[dst] = pc;
                    pc += inst_len;
                    continue;
                }
                const dst = pc[1].val;
                try as.gen_imm(buf, .arg0, dst);
                const val: i64 = @as(i16, @bitCast(pc[2]));
                try as.gen_imm(buf, .arg1, @bitCast(val));
                try buf.push(&stencils.store_const);
            },
            .chk_stk => {
                try as.gen_imm(buf, .arg0, frame_size);
                if (builtin.cpu.arch == .aarch64) {
                    try buf.push(&stencils.chk_stk);
                } else {
                    const start = buf.pos();
                    try buf.push(&stencils.chk_stk);
                    as.patchJumpCond(buf.buf.items, start + stencils.chk_stk_cont3, buf.pos());
                }
            },
            else => {
                std.debug.print("jit: Unsupported bytecode instruction: {}\n", .{pc[0].opcode()});
                return error.Unsupported;
            },
        }
        pc += inst_len;
    }

    if (cy.Trace) {
        for (const_slots) |slot| {
            if (slot != null) {
                std.debug.print("jit: Unconsumed inline const.\n", .{});
                return error.Unexpected;
            }
        }
    }
}

fn mainBlock(c: *cy.Chunk, idx: usize, nodeId: *ast.Node) !void {
    const data = c.ir.getStmtData(idx, .mainBlock);
    log.tracev("main block: {}", .{data.maxLocals});

    try bc.pushProc(c, .main, "main", nodeId);

    try bc.reserveMainRegs(c, data.maxLocals);

    c.jit.mainPc = @intCast(c.jitGetPos());

    // Spill return addr to slot 0.
    if (builtin.cpu.arch == .aarch64) {
        try c.jitPushU32(A64.LoadStore.strImmOff(a64.FpReg, 0, .x30).bitCast());
    } else if (builtin.cpu.arch == .x86_64) {
        // try c.x64Enc.int3();
        try c.x64Enc.movMem(.rax, x64.MemSibBase(x64.BaseReg(.rsp), 0));
        try c.x64Enc.movToMem(x64.MemSibBase(x64.BaseReg(x64.FpReg), 0), .rax);
    }

    var child = data.bodyHead;
    while (child != cy.NullId) {
        try genStmt(c, child);
        child = c.ir.getStmtNext(child);
    }

    if (bc.shouldGenMainScopeReleaseOps(c.compiler)) {
        // try genBlockReleaseLocals(c);
    }
    if (c.cur_proc.endLocal != cy.NullU8) {
        try mainEnd(c, c.cur_proc.endLocal);
    } else {
        try mainEnd(c, null);
    }
    try bc.popProc(c);

    c.buf.mainStackSize = c.getMaxUsedRegisters();

    // Pop boundary index.
    try bc.popUnwind(c, cy.NullU8);
}

fn mainEnd(c: *cy.Chunk, optReg: ?u8) !void {
    const retSlot = optReg orelse cy.NullU8;

    try as.gen_imm(c, .arg0, retSlot);
    try c.jitPush(&stencils.end);
    try as.genMainReturn(c);
}

fn pushReleaseVals(c: *cy.Chunk, vals: []const GenValue, debugNodeId: *ast.Node) !void {
    _ = debugNodeId;
    if (vals.len > 1) {
        // try self.pushOptionalDebugSym(debugNodeId);
        // try self.buf.pushOp1(.releaseN, @intCast(vals.len));

        // const start = self.buf.ops.items.len;
        // try self.buf.ops.resize(self.alloc, self.buf.ops.items.len + vals.len);
        // for (vals, 0..) |val, i| {
        //     self.buf.ops.items[start+i] = .{ .val = val.local };
        // }
        return error.TODO;
    } else if (vals.len == 1) {
        // try pushRelease(self, vals[0].local, debugNodeId);
        try c.jitPush(stencils.release[0..stencils.release_zFreeObject]);
        try as.genCallFuncPtr(c, &cy.vm.zFreeObject);
        try c.jitPush(stencils.release[stencils.release_zFreeObject+CallHoleLen..]);
    }
}

fn declareLocalInit(c: *cy.Chunk, idx: u32, nodeId: *ast.Node) !void {
    const data = c.ir.getStmtData(idx, .declareLocalInit);

    // Don't advance nextLocalReg yet since the rhs hasn't generated so the
    // alive locals should not include this declaration.
    const reg = try bc.reserveLocalReg(c, data.id, data.declType, data.lifted, nodeId, false);

    const val = try genExpr(c, data.init, Cstr.toLocal(reg, false));

    const local = bc.getLocalInfoPtr(c, reg);

    // if (local.some.boxed) {
    //     try c.pushOptionalDebugSym(nodeId);
    //     try c.buf.pushOp2(.box, reg, reg);
    // }
    local.some.rcCandidate = val.tracked;

    // rhs has generated, increase `nextLocalReg`.
    c.cur_proc.nextLocalReg += 1;
    log.tracev("declare {}, rced: {} ", .{val.local, local.some.rcCandidate});
}

fn declareLocal(c: *cy.Chunk, idx: u32, node: *ast.Node) !void {
    const data = c.ir.getStmtData(idx, .declareLocal);
    const slot = try bc.reserveLocal(c, data.declType, data.lifted, node);
    c.genIrLocalMapStack.items[c.cur_proc.irLocalMapStart + data.id] = slot;
}

fn funcBlock(c: *cy.Chunk, idx: usize, nodeId: *ast.Node) !void {
    const data = c.ir.getStmtData(idx, .funcBlock);
    const func = data.func;
    const paramsIdx = c.ir.advanceStmt(idx, .funcBlock);
    const params = c.ir.getArray(paramsIdx, ir.FuncParam, func.numParams);

    const funcPc = c.jitGetPos();

    // A64 relies on bl to obtain the return addr in x30.
    // Ideally, the return addr shouldn't be spilled until the first function call.
    if (builtin.cpu.arch == .aarch64) {
        try c.jitPushU32(A64.LoadStore.strImmOff(a64.FpReg, 2, .x30).bitCast());
    } else if (builtin.cpu.arch == .x86_64) {
        // Save rax to ret addr slot.
        try as.genStoreSlot(c, 2, .temp);
    }

    try c.compiler.genSymMap.putNoClobber(c.alloc, func, .{ .func = .{ .id = 0, .pc = @intCast(funcPc) }});

    try bc.pushFuncBlock(c, data, params, nodeId);

    // try genStmts(c, data.bodyHead);

    // Get stack size.
    const stackSize = c.getMaxUsedRegisters();
    _ = stackSize;

    // // Add method entry.
    // if (func.isMethod) {
    //     const mgId = try c.compiler.vm.ensureMethod(func.name());
    //     const funcSig = c.compiler.sema.getFuncSig(func.funcSigId);
    //     if (funcSig.reqCallTypeCheck) {
    //         const m = rt.MethodInit.initTyped(func.funcSigId, funcPc, stackSize, func.numParams);
    //         try c.compiler.vm.addMethod(data.parentType, mgId, m);
    //     } else {
    //         const m = rt.MethodInit.initUntyped(func.funcSigId, funcPc, stackSize, func.numParams);
    //         try c.compiler.vm.addMethod(data.parentType, mgId, m);
    //     }
    // }

    try bc.popFuncBlockCommon(c, func);
}

pub fn relocate(buf: *CodeBuffer) !void {
    if (builtin.os.tag == .windows) {
        const code_ptr = try std.os.windows.VirtualAlloc(null, buf.buf.capacity, std.os.windows.MEM_COMMIT | std.os.windows.MEM_RESERVE, std.os.windows.PAGE_READWRITE);
        const code_buf = @as([*]align(std.heap.page_size_min) u8, @ptrCast(@alignCast(code_ptr)))[0..buf.buf.capacity];
        @memcpy(code_buf[0..buf.buf.items.len], buf.buf.items);
        buf.final_buf = code_buf;
    } else {
        buf.final_buf = buf.buf.items.ptr[0..buf.buf.capacity];
    }

    // From this point on, only reference final_buf for relocations.

    // Ensure enough scratch capacity for patching imm64.
    if (builtin.cpu.arch == .aarch64) {
        _ = try buf.ensureUnusedCap(4 * 4);
    }

    // Perform relocation.
    for (buf.relocs.items) |reloc| {
        switch (reloc.type) {
            .jumpToFunc => {
                const data = reloc.data.jumpToFunc;
                if (data.func.type == .hostFunc) {
                    return error.Unexpected;
                }
                // const func_pc = buf.buf.items.ptr + buf.funcs.get(data.func).?.pc;
                // as.patch_imm64(buf, data.temp_pc, .temp, @intFromPtr(func_pc));
                const func_pc = buf.funcs.get(data.func).?.pc;
                as.patch_jump_rel(buf.final_buf, data.pc, func_pc);
            },
            .jump_f => {
                const data = reloc.data.jump_f;
                const target_pc = buf.labels.get(.{.chunk=data.chunk, .bc_pc=data.bc_target_pc}).?;
                std.debug.assert(target_pc != 0);
                as.patch_jump_rel(buf.final_buf, data.pc, target_pc);
            },
            .jump_cond => {
                const data = reloc.data.jump_cond;
                const target_pc = buf.labels.get(.{.chunk=data.chunk, .bc_pc=data.bc_target_pc}).?;
                std.debug.assert(target_pc != 0);
                as.patchJumpCond(buf.final_buf, data.pc, target_pc);
            },
            .jit_entry => {
                const data = reloc.data.jit_entry;
                const func_pc = buf.final_buf.ptr + buf.funcs.get(data.func).?.pc;
                const addr: usize = @intFromPtr(func_pc);
                data.chunk.buf.setOpArgU48(data.bc_pc, @intCast(addr));
            },
        }
    }
}

pub fn gen_funcs(c: *cy.Chunk, funcs: []const *cy.Func) !void {
    if (is_wasm) {
        @panic("unsupported");
    }
    // // Prepare host funcs.
    // for (chunk.funcs.items) |func| {
    //     try prepareFunc(self, func);
    // }

    // c.buf = &c.compiler.buf;
    c.jit = &c.compiler.jitBuf;

    c.listDataStack.clearRetainingCapacity();

    for (funcs) |func| {
        gen_func(c, func) catch |err| {
            return c.reportErrorFmt("jit: {}", &.{v(err)}, func.decl);
        };
    }

    if (cy.Trace and !cy.isWasm) {
        if (DumpCodeFrom) |start| {
            var codeBuf = c.jit.buf.items[start..];
            if (codeBuf.len > 4 * 20) {
                codeBuf = codeBuf[0..4*20];
            }
            log.trace("Dump at marker: {x}\n", .{codeBuf});
            log.trace("Dump all: {x}\n", .{c.jit.buf.items});
            DumpCodeFrom = null;
        }
    }
}

fn retExprStmt(c: *cy.Chunk, idx: usize, nodeId: *ast.Node) !void {
    _ = nodeId;
    const childIdx = c.ir.advanceStmt(idx, .retExprStmt);

    // TODO: If the returned expr is a local, consume the local after copying to reg 0.
    var childv: GenValue = undefined;
    if (c.cur_proc.type == .main) {
        // Main block.
        childv = try genExpr(c, childIdx, Cstr.simpleOwn);
    } else {
        childv = try genExpr(c, childIdx, Cstr.ret);
    }

    try bc.popTempAndUnwind(c, childv);

    // try genBlockReleaseLocals(c);
    if (c.cur_proc.type == .main) {
        // try c.buf.pushOp1(.end, @intCast(childv.local));
        return error.TODO;
    } else {
        try as.gen_func_ret(c);
    }
}

fn verbose(c: *cy.Chunk, idx: usize, nodeId: *ast.Node) !void {
    _ = nodeId;
    const data = c.ir.getStmtData(idx, .verbose);
    cc.setVerbose(data.verbose);
}
