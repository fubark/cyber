const builtin = @import("builtin");
const std = @import("std");
const cy = @import("../cyber.zig");
const bcgen = @import("../bc_gen.zig");
const stencils = switch (builtin.cpu.arch) {
    .aarch64 => @import("a64_stencils.zig"),
    .x86_64 => @import("x64_stencils.zig"),
    else => void,
};
const log = cy.log.scoped(.jit_gen);
const bt = cy.types.BuiltinTypes;
const v = cy.fmt.v;
const rt = cy.rt;
const ir = cy.ir;
const assm = @import("assembler.zig");
const a64 = @import("a64_assembler.zig");
const A64 = @import("a64.zig");
const x64 = @import("x64_assembler.zig");
const X64 = @import("x64.zig");

const GenValue = bcgen.GenValue;
const Cstr = cy.register.Cstr;
const RegisterId = cy.register.RegisterId;
const regValue = bcgen.regValue;

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
};

pub const Reloc = struct {
    type: RelocType,
    data: union {
        jumpToFunc: struct {
            func: *cy.Func,
            pc: u32,
        },
    },
};

pub const CodeBuffer = struct {
    buf: std.ArrayListAlignedUnmanaged(u8, std.mem.page_size),

    /// Where main begins. Currently only jit code uses this.
    mainPc: u32,

    /// Relocation entries.
    relocs: std.ArrayListUnmanaged(Reloc),

    pub fn init() CodeBuffer {
        return .{
            .buf = .{},
            .mainPc = 0,
            .relocs = .{},
        };
    }

    pub fn clear(self: *CodeBuffer) void {
        self.buf.clearRetainingCapacity();
        self.relocs.clearRetainingCapacity();
    }

    pub fn deinit(self: *CodeBuffer, alloc: std.mem.Allocator) void {
        self.buf.deinit(alloc);
        self.relocs.deinit(alloc);
    }
};

pub const ChunkExt = struct {
    pub fn jitEnsureUnusedCap(c: *cy.Chunk, size: usize) !usize {
        return ensureUnusedCap(&c.jitBuf.buf, c.alloc, size);
    }
    pub const jitCopyAdvance = copyAdvance;
    pub const jitPush = push;
    pub const jitPushU32 = pushU32;
    pub const jitPushU64 = pushU64;
    pub const jitPushStencil = pushStencil;
    pub const jitGetPos = getPos;
    pub const jitGetA64Inst = getA64Inst;
};

pub fn getA64Inst(c: *cy.Chunk, pos: usize, comptime T: type) *align(4)T {
    if (cy.Trace) {
        if (pos % 4 != 0) {
            cy.panic("Unaligned inst access.");
        }
    }
    return @ptrCast(@alignCast(&c.jitBuf.buf.items[pos]));
}

pub fn getPos(c: *cy.Chunk) usize {
    return c.jitBuf.buf.items.len;
}

pub fn ensureUnusedCap(buf: *std.ArrayListAlignedUnmanaged(u8, std.mem.page_size), alloc: std.mem.Allocator, size: usize) !usize {
    if (buf.items.len + size > buf.capacity) {
        var inc = buf.capacity / 2;
        if (inc <= std.mem.page_size) {
            inc = std.mem.page_size;
        }
        try buf.ensureTotalCapacityPrecise(alloc, buf.capacity + inc);
    }
    return buf.items.len;
}

pub fn copyAdvance(c: *cy.Chunk, dst: usize, src: []const u8) void {
    const to: []u8 = @ptrCast(c.jitBuf.buf.items.ptr[dst..dst+src.len]);
    @memcpy(to, src);
    c.jitBuf.buf.items.len = dst+src.len;
}

pub fn pushU32(c: *cy.Chunk, code: u32) !void {
    const start = try c.jitEnsureUnusedCap(@sizeOf(u32));
    c.jitBuf.buf.items.len += @sizeOf(u32);
    const dst: []u8 = @ptrCast(c.jitBuf.buf.items.ptr[start..start+@sizeOf(u32)]);
    @memcpy(dst, std.mem.asBytes(&code));
}

pub fn pushU64(c: *cy.Chunk, code: u64) !void {
    const start = try c.jitEnsureUnusedCap(@sizeOf(u64));
    c.jitBuf.buf.items.len += @sizeOf(u64);
    const dst: []u8 = @ptrCast(c.jitBuf.buf.items.ptr[start..start+@sizeOf(u64)]);
    @memcpy(dst, std.mem.asBytes(&code));
}

pub fn push(c: *cy.Chunk, code: []const u8) !void {
    const start = try c.jitEnsureUnusedCap(code.len);
    c.jitBuf.buf.items.len += code.len;
    const dst: []u8 = @ptrCast(c.jitBuf.buf.items.ptr[start..start+code.len]);
    @memcpy(dst, code);
}

pub fn pushStencil(c: *cy.Chunk, code: []const u8) !usize {
    const start = try c.jitEnsureUnusedCap(code.len);
    c.jitBuf.buf.items.len += code.len;
    const dst: []u8 = @ptrCast(c.jitBuf.buf.items.ptr[start..start+code.len]);
    @memcpy(dst, code);
    return start;
}

fn genStmt(c: *cy.Chunk, idx: u32) anyerror!void {
    const code = c.ir.getStmtCode(idx);
    const nodeId = c.ir.getNode(idx);
    c.curNodeId = nodeId;

    var dumpEndPc: usize = undefined;
    if (cy.Trace) {
        const contextStr = try c.encoder.formatNode(nodeId, &cy.tempBuf);
        log.tracev("----{s}: {{{s}}}", .{@tagName(code), contextStr});

        if (cy.verbose) {
            dumpEndPc = try genCallDumpJitSection(c, idx, true);
        }

        if (GenBreakpointAtIr) |chunkIr| {
            if (c.id == chunkIr.chunkId and idx == chunkIr.irIdx) {
                try assm.genBreakpoint(c);
            }
        }
    }

    var tempRetainedStart: usize = undefined;
    var tempStart: usize = undefined;
    if (cy.Trace and c.procs.items.len > 0) {
        tempRetainedStart = @intCast(c.unwindTempIndexStack.items.len);
        tempStart = c.rega.nextTemp;
    }

    switch (code) {
        // .breakStmt          => try breakStmt(c, nodeId),
        // .contStmt           => try contStmt(c, nodeId),
        .declareLocal       => try declareLocal(c, idx, nodeId),
        // .destrElemsStmt     => try destrElemsStmt(c, idx, nodeId),
        .exprStmt           => try exprStmt(c, idx, nodeId),
        // .forIterStmt        => try forIterStmt(c, idx, nodeId),
        // .forRangeStmt       => try forRangeStmt(c, idx, nodeId),
        .funcBlock          => try funcBlock(c, idx, nodeId),
        .ifStmt             => try ifStmt(c, idx, nodeId),
        .mainBlock          => try mainBlock(c, idx, nodeId),
        // .opSet              => try opSet(c, idx, nodeId),
        // .pushDebugLabel     => try pushDebugLabel(c, idx),
        .retExprStmt        => try retExprStmt(c, idx, nodeId),
        // .retStmt            => try retStmt(c),
        // .setCallObjSymTern  => try setCallObjSymTern(c, idx, nodeId),
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
        .verbose            => try verbose(c, idx, nodeId),
        // .whileCondStmt      => try whileCondStmt(c, idx, nodeId),
        // .whileInfStmt       => try whileInfStmt(c, idx, nodeId),
        // .whileOptStmt       => try whileOptStmt(c, idx, nodeId),
        else => {
            return error.TODO;
        }
    }

    // Check stack after statement.
    if (cy.Trace and c.procs.items.len > 0) {
        if (c.unwindTempIndexStack.items.len != tempRetainedStart) {
            return c.reportErrorAt("Expected {} unwindable retained temps, got {}",
                &.{v(tempRetainedStart), v(c.unwindTempIndexStack.items.len)}, nodeId);
        }

        if (c.rega.nextTemp != tempStart) {
            return c.reportErrorAt("Expected {} temp start, got {}",
                &.{v(tempStart), v(c.rega.nextTemp)}, nodeId);
        }
    }
    log.tracev("----{s}: end", .{@tagName(code)});

    if (cy.Trace) {
        if (cy.verbose) {
            assm.patchMovPcRelTo(c, dumpEndPc, c.jitGetPos());
        }
    }
}

fn exprStmt(c: *cy.Chunk, idx: usize, nodeId: cy.NodeId) !void {
    _ = nodeId;
    const data = c.ir.getStmtData(idx, .exprStmt);
    const expr = c.ir.advanceStmt(idx, .exprStmt);

    if (data.isBlockResult) {
        const inMain = c.curBlock.sBlockDepth == 0;
        if (inMain) {
            const exprv = try genExpr(c, expr, Cstr.simpleRetain);
            c.curBlock.endLocal = exprv.reg;
            try bcgen.popTempAndUnwind(c, exprv);
        } else {
            // Return from block expression.
            const b = c.blocks.getLast();
            _ = try genExpr(c, expr, b.blockExprCstr);
        }
    } else {
        const exprv = try genExpr(c, expr, Cstr.simple);

        try bcgen.popTempAndUnwind(c, exprv);

        // TODO: Merge with previous release inst.
        // try bcgen.releaseTempValue(c, exprv, nodeId);
    }
}

pub fn prepareFunc(c: *cy.VMcompiler, func: *cy.Func) !void {
    if (func.type == .userLambda) {
        return;
    }
    if (cy.Trace) {
        const symPath = try func.sym.?.head.allocAbsPath(c.alloc);
        defer c.alloc.free(symPath);
        log.tracev("jit prepare func: {s}", .{symPath});
    }
    if (func.type == .hostFunc) {
        const funcSig = c.sema.getFuncSig(func.funcSigId);
        // const rtFunc = rt.FuncSymbol.initHostFunc(@ptrCast(func.data.hostFunc.ptr), funcSig.reqCallTypeCheck, funcSig.numParams(), func.funcSigId);

        // try c.genSymMap.putNoClobber(c.alloc, func, .{ .hostFuncSym = .{ .ptr = func.data.hostFunc }});
        if (func.isMethod) {
            const parentT = func.sym.?.head.parent.?.getStaticType().?;
            const name = func.name();
            const mgId = try c.vm.ensureMethodGroup(name);
            if (funcSig.reqCallTypeCheck) {
                const m = rt.MethodInit.initHostTyped(func.funcSigId, @ptrCast(func.data.hostFunc.ptr), func.numParams);
                try c.vm.addMethod(parentT, mgId, m);
            } else {
                const m = rt.MethodInit.initHostUntyped(func.funcSigId, @ptrCast(func.data.hostFunc.ptr), func.numParams);
                try c.vm.addMethod(parentT, mgId, m);
            }
        }
    } else if (func.type == .hostInlineFunc) {
        // return error.TODO;
        // const funcSig = c.sema.getFuncSig(func.funcSigId);
        // const rtFunc = rt.FuncSymbol.initHostInlineFunc(@ptrCast(func.data.hostInlineFunc.ptr), funcSig.reqCallTypeCheck, funcSig.numParams(), func.funcSigId);
        // _ = try addVmFunc(c, func, rtFunc);
        // if (func.isMethod) {
        //     log.tracev("ismethod", .{});
        //     const name = func.name();
        //     const mgId = try c.vm.ensureMethodGroup(name);
        //     const parentT = func.sym.?.head.parent.?.getStaticType().?;
        //     log.tracev("host inline method: {s}.{s} {} {}", .{c.sema.getTypeName(parentT), name, parentT, mgId});
        //     const m = rt.MethodInit.initHostInline(func.funcSigId, func.data.hostInlineFunc.ptr, func.numParams);
        //     try c.vm.addMethod(parentT, mgId, m);
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

fn genBinOp(c: *cy.Chunk, idx: usize, cstr: Cstr, opts: BinOpOptions, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .preBinOp).binOp;
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
    const inst = try c.rega.selectForDstInst(cstr, willRetain, nodeId);

    var prefer = bcgen.PreferDst{
        .dst = inst.dst,
        .canUseDst = !c.isParamOrLocalVar(inst.dst),
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
                        try assm.genMovImm(c, .arg0, leftv.data.constant.val.val);
                    } else {
                        try assm.genLoadSlot(c, .arg0, leftv.reg);
                    }
                    if (rightv.type == .constant) {
                        try assm.genMovImm(c, .arg1, rightv.data.constant.val.val);
                    } else {
                        try assm.genLoadSlot(c, .arg1, rightv.reg);
                    }
                    try c.jitPush(&stencils.intPair);

                    // Compare.
                    try assm.genCmp(c, .arg0, .arg1);
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
                    try assm.genMovImm(c, .arg0, leftv.data.constant.val.val);
                } else {
                    try assm.genLoadSlot(c, .arg0, leftv.reg);
                }

                if (rightv.type == .constant) {
                    try assm.genMovImm(c, .arg1, rightv.data.constant.val.val);
                } else {
                    try assm.genLoadSlot(c, .arg1, rightv.reg);
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
                try assm.genStoreSlot(c, inst.dst, .arg0);
            } else if (data.leftT == bt.Integer) {
                // try pushInlineBinExpr(c, getIntOpCode(data.op), leftv.reg, rightv.reg, inst.dst, nodeId);

                // Load operands.
                if (leftv.type == .constant) {
                    try assm.genMovImm(c, .arg0, leftv.data.constant.val.val);
                } else {
                    try assm.genLoadSlot(c, .arg0, leftv.reg);
                }

                if (rightv.type == .constant) {
                    try assm.genMovImm(c, .arg1, rightv.data.constant.val.val);
                } else {
                    try assm.genLoadSlot(c, .arg1, rightv.reg);
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
                try assm.genStoreSlot(c, inst.dst, .arg0);
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
            return c.reportErrorAt("Unsupported op: {}", &.{v(data.op)}, nodeId);
        },
    }

    // const leftRetained = if (opts.left == null) bcgen.unwindTempKeepDst(c, leftv, inst.dst) else false;
    // const rightRetained = bcgen.unwindTempKeepDst(c, rightv, inst.dst);

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

fn ifStmt(c: *cy.Chunk, idx: usize, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(idx, .ifStmt);
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
            try assm.genJumpCond(c, .ge, 0);
        } else {
            return error.TODO;
        }
    } else {
        return error.TODO;
    }

    // ARC cleanup for true case.
    try bcgen.popTempAndUnwind(c, condv);
    // try bcgen.releaseTempValue(c, condv, condNodeId);

    try bcgen.pushBlock(c, false, nodeId);
    try genStmts(c, data.bodyHead);
    try bcgen.popBlock(c);

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
            assm.patchJumpCond(c, prevCaseMissJump, c.jitGetPos());
        } else {
            return error.TODO;
        }
    }
}

fn genFloat(c: *cy.Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .float);
    const inst = try c.rega.selectForNoErrNoDepInst(cstr, false, nodeId);
    if (inst.requiresPreRelease) {
        // try pushRelease(c, inst.dst, nodeId);
    }

    const val = cy.Value.initF64(data.val);
    try assm.genStoreSlotValue(c, inst.dst, val);

    const value = regValue(c, inst.dst, false);
    return finishInst(c, value, inst.finalDst);
}

fn genInt(c: *cy.Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .int);

    const val = cy.Value.initInt(@intCast(data.val));
    if (cstr.jitPreferConstant) {
        return GenValue.initConstant(val);
    }

    const inst = try c.rega.selectForNoErrNoDepInst(cstr, false, nodeId);
    if (inst.requiresPreRelease) {
        // try pushRelease(c, inst.dst, nodeId);
    }

    try assm.genStoreSlotValue(c, inst.dst, val);

    const value = regValue(c, inst.dst, false);
    return finishInst(c, value, inst.finalDst);
}

fn finishInst(c: *cy.Chunk, val: GenValue, optDst: ?Cstr) !GenValue {
    if (optDst) |dst| {
        const final = try genToFinalDst(c, val, dst);
        try bcgen.pushUnwindValue(c, final);
        return final;
    } else {
        try bcgen.pushUnwindValue(c, val);
        return val;
    }
}

fn genToFinalDst(c: *cy.Chunk, val: GenValue, dst: Cstr) !GenValue {
    log.tracev("genToFinalDst src: {} dst: {s}", .{val.reg, @tagName(dst.type)});

    const desc = cy.bytecode.InstDesc{};
    const res = try genToExact(c, val, dst, desc);

    // Check to remove the temp that is used to move to final dst.
    if (val.isTemp()) try bcgen.popTemp(c, val.reg);
    return res;
}

fn genToExact(c: *cy.Chunk, val: GenValue, dst: Cstr, desc: cy.bytecode.InstDesc) !GenValue {
    _ = desc;
    switch (dst.type) {
        .localReg => {
            const reg = dst.data.reg;
            if (val.reg == reg.dst) return error.Unexpected;
            if (reg.releaseDst) {
                // try c.buf.pushOp2Ext(.copyReleaseDst, val.reg, local.reg, desc);
                return error.TODO;
            } else {
                // try c.buf.pushOp2Ext(.copy, val.reg, local.reg, desc);
                try assm.genLoadSlot(c, .temp, val.reg);
                try assm.genStoreSlot(c, reg.dst, .temp);
            }
            // Parent only cares about the retained property.
            return GenValue.initRetained(val.retained);
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

fn genCallFuncSym(c: *cy.Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .preCallFuncSym).callFuncSym;
    const inst = try bcgen.beginCall(c, cstr, false, nodeId);

    const argsIdx = c.ir.advanceExpr(idx, .preCallFuncSym);
    const args = c.ir.getArray(argsIdx, u32, data.numArgs);

    const argStart = c.rega.nextTemp;
    for (args, 0..) |argIdx, i| {
        const temp = try c.rega.consumeNextTemp();
        if (cy.Trace and temp != argStart + i) return error.Unexpected;
        try genAndPushExpr(c, argIdx, Cstr.toTemp(temp));
    }

    if (data.hasDynamicArg) {
        // try genCallTypeCheck(c, inst.ret + cy.vm.CallArgStart, data.numArgs, data.func.funcSigId, nodeId);
    }

    if (data.func.type == .hostFunc) {
        // Populate callHost stencil args.
        try assm.genAddImm(c, .arg0, .fp, 8 * (inst.ret + cy.vm.CallArgStart));
        try assm.genMovImm(c, .arg1, data.numArgs);

        try c.jitPush(stencils.callHost[0..stencils.callHost_hostFunc]);
        try assm.genCallFuncPtr(c, data.func.data.hostFunc.ptr);
        try c.jitPush(stencils.callHost[stencils.callHost_hostFunc+CallHoleLen..]);

        // Copy result to ret.
        // TODO: Copy directly to final dst.
        try assm.genStoreSlot(c, inst.ret, .arg2);
    } else if (data.func.type == .userFunc) {
        try assm.genCallFunc(c, inst.ret, data.func);
    } else return error.TODO;

    const argvs = bcgen.popValues(c, data.numArgs);
    try bcgen.checkArgs(argStart, argvs);
    const retained = try bcgen.popTempAndUnwinds2(c, argvs);
    _ = retained;
    // try pushReleaseVals(c, retained, nodeId);

    const retRetained = c.sema.isRcCandidateType(data.func.retType);
    return endCall(c, inst, retRetained);
}

fn endCall(c: *cy.Chunk, inst: bcgen.CallInst, retained: bool) !GenValue {
    c.rega.freeTemps(inst.numPreludeTemps);
    const val = regValue(c, inst.ret, retained);
    return finishInst(c, val, inst.finalDst);
}

fn genAndPushExpr(c: *cy.Chunk, idx: usize, cstr: Cstr) !void {
    const val = try genExpr(c, idx, cstr);
    try c.genValueStack.append(c.alloc, val);
}

fn zDumpJitStmtSection(vm: *cy.VM, fp: [*]const cy.Value, chunkId: u64, irIdx: u64, startPc: [*]const u8, endPc: [*]const u8) void {
    const c = vm.compiler.chunks.items[@intCast(chunkId)];
    const code = c.ir.getStmtCode(@intCast(irIdx));
    const nodeId = c.ir.getNode(@intCast(irIdx));

    const mc = startPc[0..@intFromPtr(endPc)-@intFromPtr(startPc)];
    const contextStr = c.encoder.formatNode(nodeId, &cy.tempBuf) catch cy.fatal();
    log.tracev("{s} {{{s}}} {*} {} ({}:{})", .{@tagName(code), contextStr, fp, std.fmt.fmtSliceHexLower(mc), chunkId, irIdx});
}

fn zDumpJitExprSection(vm: *cy.VM, fp: [*]const cy.Value, chunkId: u64, irIdx: u64, startPc: [*]const u8, endPc: [*]const u8) void {
    const c = vm.compiler.chunks.items[@intCast(chunkId)];
    const code = c.ir.getExprCode(@intCast(irIdx));
    const nodeId = c.ir.getNode(@intCast(irIdx));

    const mc = startPc[0..@intFromPtr(endPc)-@intFromPtr(startPc)];
    const contextStr = c.encoder.formatNode(nodeId, &cy.tempBuf) catch cy.fatal();
    log.tracev("{s} {{{s}}} {*} {} ({}:{})", .{@tagName(code), contextStr, fp, std.fmt.fmtSliceHexLower(mc), chunkId, irIdx});
}

fn genCallDumpJitSection(c: *cy.Chunk, idx: usize, isStmt: bool) !usize {
    try assm.genMovImm(c, .arg0, c.id);
    try assm.genMovImm(c, .arg1, idx);
    const dumpStartPc = c.jitGetPos();
    try assm.genPatchableMovPcRel(c, .arg2);

    const dumpEndPc = c.jitGetPos();
    try assm.genPatchableMovPcRel(c, .arg3);
    try c.jitPush(stencils.dumpJitSection[0..stencils.dumpJitSection_zDumpJitSection]);
    if (isStmt) {
        try assm.genCallFuncPtr(c, &zDumpJitStmtSection);
    } else {
        try assm.genCallFuncPtr(c, &zDumpJitExprSection);
    }
    try c.jitPush(stencils.dumpJitSection[stencils.dumpJitSection_zDumpJitSection+CallHoleLen..]);
    assm.patchMovPcRelTo(c, dumpStartPc, c.jitGetPos());
    return dumpEndPc;
}

fn genExpr(c: *cy.Chunk, idx: usize, cstr: Cstr) anyerror!GenValue {
    const code = c.ir.getExprCode(idx);
    const nodeId = c.ir.getNode(idx);

    var dumpEndPc: usize = undefined;
    if (cy.Trace) {
        const contextStr = try c.encoder.formatNode(nodeId, &cy.tempBuf);
        log.tracev("{s}: {{{s}}} {s}", .{@tagName(code), contextStr, @tagName(cstr.type)});

        if (cy.verbose) {
            dumpEndPc = try genCallDumpJitSection(c, idx, false);
        }

        if (GenBreakpointAtIr) |chunkIr| {
            if (c.id == chunkIr.chunkId and idx == chunkIr.irIdx) {
                try assm.genBreakpoint(c);
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
        // .preSlice           => genSlice(c, idx, cstr, nodeId),
        // .preUnOp            => genUnOp(c, idx, cstr, nodeId),
        // .string             => genString(c, idx, cstr, nodeId),
        .stringTemplate     => genStringTemplate(c, idx, cstr, nodeId),
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
        if (cy.verbose) {
            assm.patchMovPcRelTo(c, dumpEndPc, c.jitGetPos());
        }
    }

    return res;
}

fn genStmts(c: *cy.Chunk, idx: u32) !void {
    var stmt = idx;
    while (stmt != cy.NullId) {
        try genStmt(c, stmt);
        stmt = c.ir.getStmtNext(stmt);
    }
}

fn mainBlock(c: *cy.Chunk, idx: usize, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(idx, .mainBlock);
    log.tracev("main block: {}", .{data.maxLocals});

    try bcgen.pushProc(c, .main, nodeId);
    c.curBlock.frameLoc = 0;

    try bcgen.reserveMainRegs(c, data.maxLocals);

    c.jitBuf.mainPc = @intCast(c.jitGetPos());

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

    if (bcgen.shouldGenMainScopeReleaseOps(c.compiler)) {
        // try genBlockReleaseLocals(c);
    }
    if (c.curBlock.endLocal != cy.NullU8) {
        try mainEnd(c, c.curBlock.endLocal);
    } else {
        try mainEnd(c, null);
    }
    try bcgen.popProc(c);

    c.buf.mainStackSize = c.getMaxUsedRegisters();

    // Pop boundary index.
    try bcgen.popUnwind(c, cy.NullU8);
}

fn mainEnd(c: *cy.Chunk, optReg: ?u8) !void {
    const retSlot = optReg orelse cy.NullU8;

    try assm.genMovImm(c, .arg0, retSlot);
    try c.jitPush(&stencils.end);
    try assm.genMainReturn(c);
}

fn genStringTemplate(c: *cy.Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .stringTemplate);
    const strsIdx = c.ir.advanceExpr(idx, .stringTemplate);
    const strs = c.ir.getArray(strsIdx, []const u8, data.numExprs+1);
    const args = c.ir.getArray(data.args, u32, data.numExprs);

    const inst = try c.rega.selectForDstInst(cstr, true, nodeId); 
    const argStart = c.rega.getNextTemp();

    for (args, 0..) |argIdx, i| {
        const temp = try c.rega.consumeNextTemp();
        if (cy.Trace and temp != argStart + i) return error.Unexpected;
        try genAndPushExpr(c, argIdx, Cstr.toTemp(temp));
    }
    if (cy.Trace and c.rega.nextTemp != argStart + data.numExprs) return error.Unexpected;

    // Inline const strings.
    const skipPc = c.jitGetPos();
    try assm.genPatchableJumpRel(c);

    // Forward align.
    const advanceLen = std.mem.alignForward(usize, c.jitGetPos(), 8) - c.jitGetPos();
    _ = try c.jitEnsureUnusedCap(advanceLen);
    c.jitBuf.buf.items.len += advanceLen;

    const strsPc = c.jitGetPos();
    for (strs) |str| {
        const ustr = try c.unescapeString(str);
        const constIdx = try c.buf.getOrPushStaticStringConst(ustr);
        const constStr = c.buf.consts.items[constIdx];
        try c.jitPushU64(@bitCast(constStr));
    }

    assm.patchJumpRel(c, skipPc, c.jitGetPos());

    // try c.pushOptionalDebugSym(nodeId);
    // try c.buf.pushOp3(.stringTemplate, argStart, data.numExprs, inst.dst);

    // Load strs.
    try assm.genMovPcRel(c, .arg0, strsPc);

    // Load exprs.
    try assm.genAddImm(c, .arg1, .fp, 8 * argStart);

    // Load expr count.
    try assm.genMovImm(c, .arg2, data.numExprs);

    try c.jitPush(stencils.stringTemplate[0..stencils.stringTemplate_zAllocStringTemplate2]);
    try assm.genCallFuncPtr(c, &cy.vm.zAllocStringTemplate2);
    try c.jitPush(stencils.stringTemplate[stencils.stringTemplate_zAllocStringTemplate2+CallHoleLen..]);

    // Save result.
    try assm.genStoreSlot(c, inst.dst, .arg0);

    const argvs = bcgen.popValues(c, data.numExprs);
    try bcgen.checkArgs(argStart, argvs);
    const retained = try bcgen.popTempAndUnwinds2(c, argvs);
    try pushReleaseVals(c, retained, nodeId);

    const val = regValue(c, inst.dst, true);
    return finishInst(c, val, inst.finalDst);
}

fn pushReleaseVals(c: *cy.Chunk, vals: []const GenValue, debugNodeId: cy.NodeId) !void {
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
        try assm.genCallFuncPtr(c, &cy.vm.zFreeObject);
        try c.jitPush(stencils.release[stencils.release_zFreeObject+CallHoleLen..]);
    }
}

fn declareLocalInit(c: *cy.Chunk, idx: u32, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(idx, .declareLocalInit);

    // Don't advance nextLocalReg yet since the rhs hasn't generated so the
    // alive locals should not include this declaration.
    const reg = try bcgen.reserveLocalReg(c, data.id, data.declType, data.lifted, nodeId, false);

    const val = try genExpr(c, data.init, Cstr.toLocal(reg, false));

    const local = bcgen.getLocalInfoPtr(c, reg);

    // if (local.some.boxed) {
    //     try c.pushOptionalDebugSym(nodeId);
    //     try c.buf.pushOp2(.box, reg, reg);
    // }
    local.some.rcCandidate = val.retained;

    // rhs has generated, increase `nextLocalReg`.
    c.curBlock.nextLocalReg += 1;
    log.tracev("declare {}, rced: {} ", .{val.local, local.some.rcCandidate});
}

fn declareLocal(c: *cy.Chunk, idx: u32, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(idx, .declareLocal);

    const reg = try bcgen.reserveLocalReg(c, data.id, data.declType, data.lifted, nodeId, true);

    // Not yet initialized, so it does not have a refcount.
    bcgen.getLocalInfoPtr(c, reg).some.rcCandidate = false;
}

fn funcBlock(c: *cy.Chunk, idx: usize, nodeId: cy.NodeId) !void {
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
        try assm.genStoreSlot(c, 2, .temp);
    }

    try c.compiler.genSymMap.putNoClobber(c.alloc, func, .{ .funcSym = .{ .id = 0, .pc = @intCast(funcPc) }});

    try bcgen.pushFuncBlock(c, data, params, nodeId);

    try genStmts(c, data.bodyHead);

    // Get stack size.
    const stackSize = c.getMaxUsedRegisters();

    // Add method entry.
    if (func.isMethod) {
        const mgId = try c.compiler.vm.ensureMethodGroup(func.name());
        const funcSig = c.compiler.sema.getFuncSig(func.funcSigId);
        if (funcSig.reqCallTypeCheck) {
            const m = rt.MethodInit.initTyped(func.funcSigId, funcPc, stackSize, func.numParams);
            try c.compiler.vm.addMethod(data.parentType, mgId, m);
        } else {
            const m = rt.MethodInit.initUntyped(func.funcSigId, funcPc, stackSize, func.numParams);
            try c.compiler.vm.addMethod(data.parentType, mgId, m);
        }
    }

    try bcgen.popFuncBlockCommon(c, func);
}

pub fn gen(self: *cy.VMcompiler) !void {
    // Prepare host funcs.
    for (self.chunks.items) |chunk| {
        const mod = chunk.sym.getMod();
        for (mod.funcs.items) |func| {
            try prepareFunc(self, func);
        }

        for (chunk.modSyms.items) |modSym| {
            const mod2 = modSym.getMod().?;
            for (mod2.funcs.items) |func| {
                try prepareFunc(self, func);
            }
        }
    }

    for (self.chunks.items) |chunk| {
        if (chunk.id != 0) {
            // Skip other chunks for now.
            continue;
        }

        log.tracev("Perform codegen for chunk{}: {s}", .{chunk.id, chunk.srcUri});
        chunk.buf = &self.buf;
        chunk.jitBuf = &self.jitBuf;
        try genChunk(chunk);
        log.tracev("Done. performChunkCodegen {s}", .{chunk.srcUri});
    }

    // Perform relocation.
    const mainChunk = self.chunks.items[0];
    for (self.jitBuf.relocs.items) |reloc| {
        switch (reloc.type) {
            .jumpToFunc => {
                const jumpPc = reloc.data.jumpToFunc.pc;
                const func = reloc.data.jumpToFunc.func;
                if (func.type == .hostFunc) {
                    return error.Unexpected;
                } else {
                    const targetPc = self.genSymMap.get(func).?.funcSym.pc;
                    assm.patchJumpRel(mainChunk, jumpPc, targetPc);
                }
            }
        }
    }
}

fn genChunk(c: *cy.Chunk) !void {
    if (builtin.cpu.arch == .x86_64) {
        c.x64Enc = X64.Encoder{ .buf = &c.jitBuf.buf, .alloc = c.alloc };
    }

    genChunkInner(c) catch |err| {
        if (err != error.CompileError) {
            // Wrap all other errors as a CompileError.
            try c.setErrorFmtAt("error.{}", &.{v(err)}, c.curNodeId);
            return error.CompileError;
        } else return err;
    };
}

fn genChunkInner(c: *cy.Chunk) !void {
    c.dataStack.clearRetainingCapacity();
    c.dataU8Stack.clearRetainingCapacity();
    c.listDataStack.clearRetainingCapacity();

    c.genValueStack.clearRetainingCapacity();

    const code = c.ir.getStmtCode(0);
    if (code != .root) return error.Unexpected;

    const data = c.ir.getStmtData(0, .root);
    try genStmts(c, data.bodyHead);

    // Ensure that all cstr and values were accounted for.
    if (c.genValueStack.items.len > 0) {
        return c.reportErrorAt("Remaining gen values: {}", &.{v(c.genValueStack.items.len)}, cy.NullId);
    }
    if (c.unwindTempIndexStack.items.len > 0) {
        return c.reportErrorAt("Remaining unwind temp index: {}", &.{v(c.unwindTempIndexStack.items.len)}, cy.NullId);
    }
    if (c.unwindTempRegStack.items.len > 0) {
        return c.reportErrorAt("Remaining unwind temp reg: {}", &.{v(c.unwindTempRegStack.items.len)}, cy.NullId);
    }

    if (cy.Trace and !cy.isWasm) {
        if (DumpCodeFrom) |start| {
            var codeBuf = c.jitBuf.buf.items[start..];
            if (codeBuf.len > 4 * 20) {
                codeBuf = codeBuf[0..4*20];
            }
            log.trace("Dump at marker: {}\n", .{std.fmt.fmtSliceHexLower(codeBuf)});
            log.trace("Dump all: {}\n", .{std.fmt.fmtSliceHexLower(c.jitBuf.buf.items)});
            DumpCodeFrom = null;
        }
    }
}

fn genLocalReg(c: *cy.Chunk, reg: RegisterId, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const local = bcgen.getLocalInfo(c, reg);

    if (!local.some.lifted) {
        const inst = try c.rega.selectForLocalInst(cstr, reg, local.some.rcCandidate, nodeId);
        if (inst.dst != reg) {
            if (inst.retainSrc) {
                if (inst.releaseDst) {
                    // try c.buf.pushOp2Ext(.copyRetainRelease, reg, inst.dst, c.desc(nodeId));
                    return error.TODO;
                } else {
                    // try c.buf.pushOp2Ext(.copyRetainSrc, reg, inst.dst, c.desc(nodeId));
                    return error.TODO;
                }
            } else {
                if (inst.releaseDst) {
                    // try c.buf.pushOp2Ext(.copyReleaseDst, reg, inst.dst, c.desc(nodeId));
                    return error.TODO;
                } else {
                    // try c.buf.pushOp2Ext(.copy, reg, inst.dst, c.desc(nodeId));
                    try assm.genLoadSlot(c, .temp, reg);
                    try assm.genStoreSlot(c, inst.dst, .temp);
                }
            }
        } else {
            // Nop. When the cstr allows returning the local itself.
            if (inst.retainSrc) {
                // try c.buf.pushOp1Ext(.retain, reg, c.desc(nodeId));
                return error.TODO;
            } else {
                // Nop.
            }
        }
        const val = regValue(c, inst.dst, inst.retainSrc);
        return finishInst(c, val, inst.finalDst);
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

fn genLocal(c: *cy.Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .local);
    const reg = bcgen.toLocalReg(c, data.id);
    return genLocalReg(c, reg, cstr, nodeId);
}

fn retExprStmt(c: *cy.Chunk, idx: usize, nodeId: cy.NodeId) !void {
    _ = nodeId;
    const childIdx = c.ir.advanceStmt(idx, .retExprStmt);

    // TODO: If the returned expr is a local, consume the local after copying to reg 0.
    var childv: GenValue = undefined;
    if (c.curBlock.type == .main) {
        // Main block.
        childv = try genExpr(c, childIdx, Cstr.simpleRetain);
    } else {
        childv = try genExpr(c, childIdx, Cstr.ret);
    }

    try bcgen.popTempAndUnwind(c, childv);

    // try genBlockReleaseLocals(c);
    if (c.curBlock.type == .main) {
        // try c.buf.pushOp1(.end, @intCast(childv.local));
        return error.TODO;
    } else {
        try assm.genFuncReturn(c);
    }
}

fn verbose(c: *cy.Chunk, idx: usize, nodeId: cy.NodeId) !void {
    _ = nodeId;
    const data = c.ir.getStmtData(idx, .verbose);
    cy.verbose = data.verbose;
}