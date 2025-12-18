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

    // BC entering JIT.
    jit_entry,
};

pub const Reloc = struct {
    type: RelocType,
    data: union {
        jumpToFunc: struct {
            func: *cy.Func,
            temp_pc: u32,
        },
        jump_f: struct {
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

pub const CodeBuffer = struct {
    gpa: std.mem.Allocator,

    executable: bool,
    buf: std.ArrayListAligned(u8, std.mem.Alignment.fromByteUnits(std.heap.page_size_min)),

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
        const PROT_WRITE = 2;
        try std.posix.mprotect(self.buf.items.ptr[0..self.buf.capacity], PROT_WRITE);
        self.executable = false;
    }

    pub fn set_executable(self: *CodeBuffer) !void {
        if (cy.isWasm) {
            return error.Unsupported;
        }

        // Mark code executable.
        const PROT_READ = 1;
        const PROT_EXEC = 4;
        try std.posix.mprotect(self.buf.items.ptr[0..self.buf.capacity], PROT_READ | PROT_EXEC);
        self.executable = true;

        // if (jitRes.buf.items.len > 500*4) {
        //     logger.tracev("jit code (size: {}) {}...", .{jitRes.buf.items.len, std.fmt.fmtSliceHexLower(jitRes.buf.items[0..100*4])});
        // } else {
        //     logger.tracev("jit code (size: {}) {}", .{jitRes.buf.items.len, std.fmt.fmtSliceHexLower(jitRes.buf.items)});
        // }
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
    pub const jitGetA64Inst = getA64Inst;
};

pub fn getA64Inst(c: *cy.Chunk, pos: usize, comptime T: type) *align(4)T {
    if (cy.Trace) {
        if (pos % 4 != 0) {
            cy.panic("Unaligned inst access.");
        }
    }
    return @ptrCast(@alignCast(&c.jit.buf.items[pos]));
}

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
                try as.genBreakpoint(c);
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
        .ifStmt             => try ifStmt(c, idx, node),
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

const BinOpOptions = struct {
    left: ?GenValue = null,
};

fn genBinOp(c: *cy.Chunk, idx: usize, cstr: Cstr, opts: BinOpOptions, nodeId: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .preBinOp).binOp;
    const ret_t = c.ir.getExprType(idx).id;
    log.tracev("binop {} {}", .{data.op, data.leftT});

    if (data.op == .and_op) {
        // return genAndOp(c, idx, data, cstr, nodeId, jit);
        return error.TODO;
    } else if (data.op == .or_op) {
        // return genOr(c, idx, data, cstr, nodeId, jit);
        return error.TODO;
    }

    // Most builtin binOps do not retain.
    var willRetain = false;
    switch (data.op) {
        .index => {
            willRetain = true;
        },
        else => {},
    }
    const inst = try bc.selectForDstInst(c, cstr, ret_t, willRetain, nodeId);

    var prefer = bc.PreferDst{
        .dst = inst.dst,
        .canUseDst = !bc.isParamOrLocalVar(c, inst.dst),
    };

    // Lhs.
    var leftv: GenValue = undefined;
    if (opts.left) |left| {
        leftv = left;
    } else {
        var lcstr = Cstr.preferVolatileIf(prefer.canUseDst, prefer.dst);
        lcstr.jitPreferConstant = true;
        leftv = try genExpr(c, data.left, lcstr);
    }

    // Rhs.
    var rcstr = prefer.nextCstr(leftv);
    rcstr.jitPreferConstant = true;
    const rightv = try genExpr(c, data.right, rcstr);

    var optCondFlag: ?JitCondFlagType = null;
    const retained = false;
    switch (data.op) {
        // .index => {
        //     if (data.leftT == bt.List) {
        //         try pushInlineBinExpr(c, .indexList, leftv.local, rightv.local, inst.dst, nodeId);
        //     } else if (data.leftT == bt.Tuple) {
        //         try pushInlineBinExpr(c, .indexTuple, leftv.local, rightv.local, inst.dst, nodeId);
        //     } else if (data.leftT == bt.Map) {
        //         try pushInlineBinExpr(c, .indexMap, leftv.local, rightv.local, inst.dst, nodeId);
        //     } else return error.Unexpected;
        //     retained = true;
        // },
        // .bitwiseAnd,
        // .bitwiseOr,
        // .bitwiseXor,
        // .bitwiseLeftShift,
        // .bitwiseRightShift => {
        //     if (data.leftT == bt.Integer) {
        //         try pushInlineBinExpr(c, getIntOpCode(data.op), leftv.local, rightv.local, inst.dst, nodeId);
        //     } else return error.Unexpected;
        // },
        .greater,
        .greater_equal,
        .less,
        .less_equal => {
            if (data.leftT == bt.Float) {
                // try pushInlineBinExpr(c, getFloatOpCode(data.op), leftv.local, rightv.local, inst.dst, nodeId);
                return error.TODO;
            } else if (data.leftT == bt.Integer) {
                // try pushInlineBinExpr(c, getIntOpCode(data.op), leftv.local, rightv.local, inst.dst, nodeId);
                if (cstr.type == .simple and cstr.jitPreferCondFlag)  {
                    // Load operands.
                    if (leftv.type == .constant) {
                        try as.genMovImm(c, .arg0, leftv.data.constant.val.val);
                    } else {
                        try as.genLoadSlot(c, .arg0, leftv.reg);
                    }
                    if (rightv.type == .constant) {
                        try as.genMovImm(c, .arg1, rightv.data.constant.val.val);
                    } else {
                        try as.genLoadSlot(c, .arg1, rightv.reg);
                    }
                    try c.jitPush(&stencils.intPair);

                    // Compare.
                    try as.genCmp(c, .arg0, .arg1);
                    optCondFlag = .lt;
                } else {
                    return error.TODO;
                }
            } else return error.Unexpected;
        },
        .star,
        .slash,
        .percent,
        .caret,
        .plus,
        .minus => {
            if (data.leftT == bt.Float) {
                // try pushInlineBinExpr(c, getFloatOpCode(data.op), leftv.local, rightv.local, inst.dst, nodeId);

                // Load operands.
                if (leftv.type == .constant) {
                    try as.genMovImm(c, .arg0, leftv.data.constant.val.val);
                } else {
                    try as.genLoadSlot(c, .arg0, leftv.reg);
                }

                if (rightv.type == .constant) {
                    try as.genMovImm(c, .arg1, rightv.data.constant.val.val);
                } else {
                    try as.genLoadSlot(c, .arg1, rightv.reg);
                }

                if (data.op == .minus) {
                    try c.jitPush(&stencils.subFloat);
                } else if (data.op == .plus) {
                    try c.jitPush(&stencils.addFloat);
                } else if (data.op == .star) {
                    try c.jitPush(&stencils.mulFloat);
                } else if (data.op == .slash) {
                    try c.jitPush(&stencils.divFloat);
                } else {
                    return error.TODO;
                }

                // Save result.
                try as.genStoreSlot(c, inst.dst, .arg0);
            } else if (data.leftT == bt.Integer) {
                // try pushInlineBinExpr(c, getIntOpCode(data.op), leftv.reg, rightv.reg, inst.dst, nodeId);

                // Load operands.
                if (leftv.type == .constant) {
                    try as.genMovImm(c, .arg0, leftv.data.constant.val.val);
                } else {
                    try as.genLoadSlot(c, .arg0, leftv.reg);
                }

                if (rightv.type == .constant) {
                    try as.genMovImm(c, .arg1, rightv.data.constant.val.val);
                } else {
                    try as.genLoadSlot(c, .arg1, rightv.reg);
                }

                if (data.op == .minus) {
                    try c.jitPush(&stencils.subInt);
                } else if (data.op == .plus) {
                    try c.jitPush(&stencils.addInt);
                } else if (data.op == .star) {
                    try c.jitPush(&stencils.mulInt);
                } else if (data.op == .slash) {
                    try c.jitPush(&stencils.divInt);
                } else {
                    return error.TODO;
                }

                // Save result.
                try as.genStoreSlot(c, inst.dst, .arg0);
            } else return error.Unexpected;
        },
        // .equal_equal => {
        //     try c.pushOptionalDebugSym(nodeId);
        //     try c.buf.pushOp3Ext(.compare, leftv.reg, rightv.reg, inst.dst, c.desc(nodeId));
        // },
        // .bang_equal => {
        //     try c.pushOptionalDebugSym(nodeId);
        //     try c.buf.pushOp3Ext(.compareNot, leftv.reg, rightv.reg, inst.dst, c.desc(nodeId));
        // },
        else => {
            return c.reportErrorFmt("Unsupported op: {}", &.{v(data.op)}, nodeId);
        },
    }

    // const leftRetained = if (opts.left == null) bc.unwindTempKeepDst(c, leftv, inst.dst) else false;
    // const rightRetained = bc.unwindTempKeepDst(c, rightv, inst.dst);

    // ARC cleanup.
    // _ = leftRetained;
    // _ = rightRetained;
    // try pushReleaseOpt2(c, leftRetained, leftv.reg, rightRetained, rightv.reg, nodeId);

    var val = regValue(c, inst.dst, retained);
    if (optCondFlag) |condFlag| {
        val.type = .jitCondFlag;
        val.data.jitCondFlag.type = condFlag;
    }
    return finishInst(c, val, inst.finalDst);
}

pub const JitCondFlagType = enum {
    lt,
};

fn ifStmt(c: *cy.Chunk, idx: usize, nodeId: *ast.Node) !void {
    // const data = c.ir.getStmtData(idx, .ifStmt);
    const bodyEndJumpsStart = c.listDataStack.items.len;

    const condIdx = c.ir.advanceStmt(idx, .ifStmt);
    // var condNodeId = c.ir.getNode(condIdx);

    var cstr = Cstr.simple;
    cstr.jitPreferCondFlag = true;
    const condv = try genExpr(c, condIdx, cstr);

    var prevCaseMissJump: usize = undefined;
    var prevCaseMissJumpFromCondFlag: bool = undefined;
    if (condv.type == .jitCondFlag) {
        prevCaseMissJump = c.jitGetPos();
        prevCaseMissJumpFromCondFlag = true;
        if (condv.data.jitCondFlag.type == .lt) {
            try as.genJumpCond(c, .ge, 0);
        } else {
            return error.TODO;
        }
    } else {
        return error.TODO;
    }

    // ARC cleanup for true case.
    try bc.popTempAndUnwind(c, condv);
    // try bc.releaseTempValue(c, condv, condNodeId);

    try bc.pushBlock(c, false, nodeId);
    // try genStmts(c, data.body_head);
    try bc.popBlock(c);

    const hasElse = false;

    // if (data.numElseBlocks > 0) {
    //     const elseBlocks = c.ir.getArray(data.elseBlocks, u32, data.numElseBlocks);

    //     for (elseBlocks) |elseIdx| {
    //         const elseBlockNodeId = c.ir.getNode(elseIdx);
    //         const elseBlock = c.ir.getExprData(elseIdx, .elseBlock);

    //         const bodyEndJump = try c.pushEmptyJump();
    //         try c.listDataStack.append(c.alloc, .{ .pc = bodyEndJump });

    //         // Jump here from prev case miss.
    //         c.patchJumpNotCondToCurPc(prevCaseMissJump);

    //         if (!elseBlock.isElse) {
    //             condIdx = c.ir.advanceExpr(elseIdx, .elseBlock);
    //             condNodeId = c.ir.getNode(condIdx);
    //             condv = try genExpr(c, condIdx, RegisterCstr.localOrTemp, jit);
    //             prevCaseMissJump = try c.pushEmptyJumpNotCond(condv.reg);

    //             // ARC cleanup for true case.
    //             if (unwindAndFreeTemp(c, condv)) {
    //                 try pushRelease(c, condv.reg, condNodeId);
    //             }
    //         } else {
    //             hasElse = true;
    //         }

    //         try pushSubBlock(c, false, elseBlockNodeId);
    //         try genStmts(c, elseBlock.bodyHead, jit);
    //         try popSubBlock(c);
    //     }
    // }

    // Jump here from all body ends.
    const bodyEndJumps = c.listDataStack.items[bodyEndJumpsStart..];
    for (bodyEndJumps) |jump| {
        c.patchJumpToCurPc(jump.pc);
    }
    c.listDataStack.items.len = bodyEndJumpsStart;

    if (!hasElse) {
        // Jump here from prev case miss.
        // c.patchJumpNotCondToCurPc(prevCaseMissJump);

        if (prevCaseMissJumpFromCondFlag) {
            as.patchJumpCond(c, prevCaseMissJump, c.jitGetPos());
        } else {
            return error.TODO;
        }
    }
}

fn genFloat(c: *cy.Chunk, idx: usize, cstr: Cstr, nodeId: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .float);
    const inst = try bc.selectForNoErrNoDepInst(c, cstr, bt.Float, false, nodeId);
    if (inst.requiresPreRelease) {
        // try pushRelease(c, inst.dst, nodeId);
    }

    const val = cy.Value.initFloat64(data.val);
    try as.genStoreSlotValue(c, inst.dst, val);

    const value = regValue(c, inst.dst, false);
    return finishInst(c, value, inst.finalDst);
}

fn genInt(c: *cy.Chunk, idx: usize, cstr: Cstr, nodeId: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .int);

    const val = cy.Value.initInt(@intCast(data.val));
    if (cstr.jitPreferConstant) {
        return GenValue.initConstant(val);
    }

    const inst = try bc.selectForNoErrNoDepInst(c, cstr, bt.Integer, false, nodeId);
    if (inst.requiresPreRelease) {
        // try pushRelease(c, inst.dst, nodeId);
    }

    try as.genStoreSlotValue(c, inst.dst, val);

    const value = regValue(c, inst.dst, false);
    return finishInst(c, value, inst.finalDst);
}

fn finishInst(c: *cy.Chunk, val: GenValue, optDst: ?Cstr, node: *ast.Node) !GenValue {
    if (optDst) |dst| {
        const final = try genToFinalDst(c, val, dst, node);
        try bc.pushUnwindValue(c, final);
        return final;
    } else {
        try bc.pushUnwindValue(c, val);
        return val;
    }
}

fn genToFinalDst(c: *cy.Chunk, val: GenValue, dst: Cstr, node: *ast.Node) !GenValue {
    log.tracev("genToFinalDst src: {} dst: {s}", .{val.reg, @tagName(dst.type)});

    const res = try genToExact(c, val, dst, null);

    // Check to remove the temp that is used to move to final dst.
    if (val.isTemp()) try bc.popTemp(c, val.reg, node);
    return res;
}

fn genToExact(c: *cy.Chunk, val: GenValue, dst: Cstr, desc: ?u32) !GenValue {
    _ = desc;
    switch (dst.type) {
        .localReg => {
            const reg = dst.data.slot;
            if (val.reg == reg.dst) return error.Unexpected;
            if (reg.releaseDst) {
                // try c.buf.pushOp2Ext(.copyReleaseDst, val.reg, local.reg, desc);
                return error.TODO;
            } else {
                // try c.buf.pushOp2Ext(.copy, val.reg, local.reg, desc);
                try as.genLoadSlot(c, .temp, val.reg);
                try as.genStoreSlot(c, reg.dst, .temp);
            }
            // Parent only cares about the retained property.
            return GenValue.initOwned(val.tracked);
        },
        // .boxedLocal => {
        //     const boxed = dst.data.boxedLocal;
        //     if (val.reg == boxed.reg) return error.Unexpected;
        //     if (boxed.retained) {
        //         try c.buf.pushOp2Ext(.setBoxValueRelease, boxed.reg, val.reg, desc);
        //     } else {
        //         try c.buf.pushOp2Ext(.setBoxValue, boxed.reg, val.reg, desc);
        //     }
        //     return GenValue.initRetained(val.retained);
        // },
        // .varSym => {
        //     // Set var assumes retained src.
        //     const pc = c.buf.len();
        //     try c.buf.pushOp3(.setStaticVar, 0, 0, val.reg);
        //     c.buf.setOpArgU16(pc + 1, @intCast(dst.data.varSym));
        //     return GenValue.initRetained(val.retained);
        // },
        // .captured => {
        //     const captured = dst.data.captured;
        //     try c.buf.pushOp3Ext(.setCaptured, c.curBlock.closureLocal, captured.idx, val.reg, desc);
        //     return GenValue.initRetained(val.retained);
        // },
        // .exact => {
        //     if (val.reg == dst.data.exact) return error.Unexpected;
        //     try c.buf.pushOp2(.copy, val.reg, dst.data.exact);
        //     return genValue(c, dst.data.exact, val.retained);
        // },
        else => {
            log.tracev("{}", .{dst.type});
            return error.TODO;
        },
    }
}

fn genCallFuncSym(c: *cy.Chunk, idx: usize, cstr: Cstr, nodeId: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .preCallFuncSym).callFuncSym;
    const ret_t = c.ir.getExprType(idx).id;
    const inst = try bc.beginCall(c, cstr, ret_t, false, nodeId);

    const args = c.ir.getArray(data.args, u32, data.numArgs);

    const argStart = bc.numSlots(c);
    for (args, 0..) |argIdx, i| {
        const arg_t = c.ir.getExprType(argIdx).id;
        const temp = try bc.reserveTemp(c, arg_t);
        if (cy.Trace and temp != argStart + i) return error.Unexpected;
        _ = try genExpr(c, argIdx, Cstr.toTemp(temp));
    }

    if (data.func.type == .hostFunc) {
        // Populate callHost stencil args.
        try as.genAddImm(c, .arg0, .fp, 8 * (inst.ret + cy.vm.CallArgStart));
        try as.genMovImm(c, .arg1, data.numArgs);

        try c.jitPush(stencils.callHost[0..stencils.callHost_hostFunc]);
        try as.genCallFuncPtr(c, data.func.data.hostFunc.ptr);
        try c.jitPush(stencils.callHost[stencils.callHost_hostFunc+CallHoleLen..]);

        // Copy result to ret.
        // TODO: Copy directly to final dst.
        try as.genStoreSlot(c, inst.ret, .arg2);
    } else if (data.func.type == .userFunc) {
        try as.genCallFunc(c, inst.ret, data.func);
    } else return error.TODO;

    try bc.popTemps(c, data.numArgs, nodeId);

    const retRetained = c.sema.isTrackedType(data.func.retType);
    return endCall(c, inst, retRetained);
}

fn endCall(c: *cy.Chunk, inst: bc.CallInst, retained: bool) !GenValue {
    bc.popNullSlots(c, inst.numPreludeTemps);
    const val = regValue(c, inst.ret, retained);
    return finishInst(c, val, inst.finalDst, inst.node);
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
    try as.genMovImm(c, .arg0, c.id);
    try as.genMovImm(c, .arg1, idx);
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
        // .captured           => genCaptured(c, idx, cstr, nodeId),
        // .cast               => genCast(c, idx, cstr, nodeId),
        // .coinitCall         => genCoinitCall(c, idx, cstr, nodeId),
        // .condExpr           => genCondExpr(c, idx, cstr, nodeId),
        // .coresume           => genCoresume(c, idx, cstr, nodeId),
        // .coyield            => genCoyield(c, idx, cstr, nodeId),
        // .enumMemberSym      => genEnumMemberSym(c, idx, cstr, nodeId),
        // .errorv             => genError(c, idx, cstr, nodeId),
        // .falsev             => genFalse(c, cstr, nodeId),
        // .fieldDynamic       => genFieldDynamic(c, idx, cstr, .{}, nodeId),
        // .fieldStatic        => genFieldStatic(c, idx, cstr, .{}, nodeId),
        .float              => genFloat(c, idx, cstr, nodeId),
        // .funcSym            => genFuncSym(c, idx, cstr, nodeId),
        .int                => genInt(c, idx, cstr, nodeId),
        // .lambda             => genLambda(c, idx, cstr, nodeId),
        // .list               => genList(c, idx, cstr, nodeId),
        .local              => genLocal(c, idx, cstr, nodeId),
        // .map                => genMap(c, idx, cstr, nodeId),
        // .none               => genNone(c, cstr, nodeId),
        // .objectInit         => genObjectInit(c, idx, cstr, nodeId),
        // .pre                => return error.Unexpected,
        .preBinOp           => genBinOp(c, idx, cstr, .{}, nodeId),
        // .preCall            => genCall(c, idx, cstr, nodeId),
        .preCallFuncSym     => genCallFuncSym(c, idx, cstr, nodeId),
        // .preCallObjSym      => genCallObjSym(c, idx, cstr, nodeId),
        // .preCallObjSymBinOp => genCallObjSymBinOp(c, idx, cstr, nodeId),
        // .preCallObjSymUnOp  => genCallObjSymUnOp(c, idx, cstr, nodeId),
        // .preUnOp            => genUnOp(c, idx, cstr, nodeId),
        // .string             => genString(c, idx, cstr, nodeId),
        // .switchBlock        => genSwitchBlock(c, idx, cstr, nodeId),
        // .symbol             => genSymbol(c, idx, cstr, nodeId),
        // .throw              => genThrow(c, idx, nodeId),
        // .truev              => genTrue(c, cstr, nodeId),
        // .tryExpr            => genTryExpr(c, idx, cstr, nodeId),
        // .typeSym            => genTypeSym(c, idx, cstr, nodeId),
        // .varSym             => genVarSym(c, idx, cstr, nodeId),
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

fn gen_slot_inline(buf: *CodeBuffer, reg: as.LRegister, slot: u16) !void {
    if (buf.const_slots.items[slot]) |pc| {
        // Contains inline constant.
        switch (pc[0].opcode()) {
            .const_16si => {
                const val: i64 = @as(i16, @bitCast(pc[2]));
                try as.genMovImm(buf, reg, @bitCast(val));
            },
            else => {
                std.debug.print("jit: Unsupported const inst: {}\n", .{pc[0].opcode()});
                return error.Unsupported;
            },
        }
        buf.const_slots.items[slot] = null;
    } else {
        try as.genLoadSlot(buf, reg, slot);
    }
}

fn gen_func(c: *cy.Chunk, func: *cy.Func) !void {
    const sym = c.compiler.genSymMap.get(func).?;
    const pc_len = sym.func.end_pc - sym.func.pc;

    const buf = c.jit;

    var pc = sym.func.static_pc;
    const end = sym.func.static_pc + pc_len;

    // Record pc.
    try c.jit.funcs.put(c.alloc, func, .{ .pc = c.jit.pos() });

    // Emit VM/JIT -> JIT prologue.     
    try c.jit.push(&stencils.func_prologue);

    // Skip `enter_jit` inst.
    std.debug.assert(pc[0].opcode() == .enter_jit);
    pc += cy.bytecode.getInstLenAt(pc);

    std.debug.assert(pc[0].opcode() == .chk_stk);
    const frame_size = pc[2].val;
    try c.jit.const_slots.resize(c.alloc, @intCast(frame_size));
    const const_slots = c.jit.const_slots.items;
    @memset(const_slots, null);

    while (@intFromPtr(pc) < @intFromPtr(end)) {
        // TODO: A better way to map jump labels is for each function to hold the start idx to `Chunk.buf.labels`.
        //       As JIT iterates the BC, keep a rolling next label to match with. This would avoid a hashtable lookup for every inst.
        const pc_off = pc - c.buf.ops.items.ptr;
        if (c.jit.labels.contains(.{.chunk=c, .bc_pc=pc_off})) {
            try c.jit.labels.put(c.alloc, .{.chunk=c, .bc_pc=pc_off}, c.jit.pos());
        }

        // var buf: [1024]u8 = undefined;
        // const msg = try std.fmt.bufPrint(&buf, "{}", .{pc[0].opcode()});
        // const heap_msg = try c.buf.getOrPushConstString(msg);
        // try as.gen_log(c.jit, heap_msg.slice);
        // try as.genBreakpoint(c);

        switch (pc[0].opcode()) {
            .ret => {
                try buf.push(&stencils.pre_ret);
                try buf.push_u32(A64.Br.ret().bitCast());
            },
            .mov => {
                try as.genMovImm(buf, .arg0, pc[1].val);
                try as.genMovImm(buf, .arg1, pc[2].val);
                try buf.push(&stencils.mov);
            },
            .jump_f => {
                try as.genMovImm(buf, .arg0, pc[1].val);
                const pos = buf.pos();
                try buf.push(&stencils.jump_f);
                try buf.relocs.append(c.alloc, .{ .type = .jump_f, .data = .{ .jump_f = .{
                    .chunk = c,
                    .pc = @intCast(pos + stencils.jump_f_br3),
                    .bc_target_pc = @intCast(pc_off + pc[2].val),
                }}});
            },
            .call => {
                try as.genMovImm(buf, .arg0, pc[1].val);
                try buf.push(&stencils.pre_call);
                const func_vm_pc: usize = @intCast(@as(*const align(2) u48, @ptrCast(pc + 2)).*);
                const func_id: u32 = @as(*const align(2) u32, @ptrFromInt(func_vm_pc - 4)).*;
                const callee = c.vm.funcSymDetails.items[func_id].func.?;
                if (!callee.info.jit) {
                    return c.reportError("Unsupported JIT -> VM call.", func.decl);
                }
                try as.genCallFunc(c, callee);
            },
            .trap => {
                try as.genBreakpoint(c);
            },
            .add => {
                try as.genMovImm(buf, .arg0, pc[1].val);
                try gen_slot_inline(buf, .arg1, pc[2].val);
                try gen_slot_inline(buf, .arg2, pc[3].val);
                try buf.push(&stencils.add);
            },
            .sub => {
                try as.genMovImm(buf, .arg0, pc[1].val);
                try gen_slot_inline(buf, .arg1, pc[2].val);
                try gen_slot_inline(buf, .arg2, pc[3].val);
                try buf.push(&stencils.sub);
            },
            .lt => {
                try as.genMovImm(buf, .arg0, pc[1].val);
                try gen_slot_inline(buf, .arg1, pc[2].val);
                try gen_slot_inline(buf, .arg2, pc[3].val);
                try buf.push(&stencils.lt);
            },
            .const_16s => {
                const dst = pc[1].val;
                try as.genMovImm(buf, .arg0, dst);
                const val: i64 = @as(i16, @bitCast(pc[2]));
                try as.genMovImm(buf, .arg1, @bitCast(val));
                try buf.push(&stencils.store_const);
            },
            .const_16si => {
                const dst = pc[1].val;
                const_slots[dst] = pc;
            },
            .chk_stk => {
                try as.genMovImm(buf, .arg0, pc[1].val);
                try as.genMovImm(buf, .arg1, pc[2].val);
                try buf.push(&stencils.chk_stk);
            },
            else => {
                std.debug.print("jit: Unsupported bytecode instruction: {}\n", .{pc[0].opcode()});
                return error.Unsupported;
            },
        }
        pc += cy.bytecode.getInstLenAt(pc);
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

    try as.genMovImm(c, .arg0, retSlot);
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
                const func_pc = buf.buf.items.ptr + buf.funcs.get(data.func).?.pc;
                as.patch_imm64(buf, data.temp_pc, .temp, @intFromPtr(func_pc));
            },
            .jump_f => {
                const data = reloc.data.jump_f;
                const target_pc = buf.labels.get(.{.chunk=data.chunk, .bc_pc=data.bc_target_pc}).?;
                as.patch_jump_rel(buf, data.pc, target_pc);
            },
            .jit_entry => {
                const data = reloc.data.jit_entry;
                const func_pc = buf.buf.items.ptr + buf.funcs.get(data.func).?.pc;
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

    if (builtin.cpu.arch == .x86_64) {
        c.x64Enc = X64.Encoder{ .buf = c.jit, .alloc = c.alloc };
    }

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

fn genLocalReg(c: *cy.Chunk, reg: SlotId, cstr: Cstr, nodeId: *ast.Node) !GenValue {
    _ = nodeId;

    const local = bc.getSlot(c, reg);

    if (!local.boxed_up) {
        const srcv = regValue(c, reg, false);
        var exact_cstr = cstr;
        switch (cstr.type) {
            .preferVolatile => {
                exact_cstr = Cstr.toLocal(reg, false);
                exact_cstr.data.slot.own = false;
            },
            .simple => {
                exact_cstr = Cstr.toLocal(reg, false);
                exact_cstr.data.slot.own = local.isBoxed() and cstr.data.simple.own;
            },
            else => {},
        }
        return genToExact(c, srcv, exact_cstr, null);
    } else {
        // // Special case when src local is boxed.
        // const retainSrc = local.some.rcCandidate and (cstr.mustRetain or cstr.type == .local or cstr.type == .boxedLocal);
        // const inst = try c.rega.selectForDstInst(cstr, retainSrc);

        // if (retainSrc) {
        //     try c.buf.pushOp2Ext(.boxValueRetain, reg, inst.dst, c.desc(nodeId));
        // } else {
        //     try c.buf.pushOp2Ext(.boxValue, reg, inst.dst, c.desc(nodeId));
        // }

        // const val = genValue(c, inst.dst, retainSrc);
        // return finishInst(c, val, inst.finalDst);
        return error.TODO;
    }
}

fn genLocal(c: *cy.Chunk, idx: usize, cstr: Cstr, nodeId: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .local);
    const reg = bc.toLocalReg(c, data.id);
    return genLocalReg(c, reg, cstr, nodeId);
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
        try as.genFuncReturn(c);
    }
}

fn verbose(c: *cy.Chunk, idx: usize, nodeId: *ast.Node) !void {
    _ = nodeId;
    const data = c.ir.getStmtData(idx, .verbose);
    cc.setVerbose(data.verbose);
}
