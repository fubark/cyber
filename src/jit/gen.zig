const std = @import("std");
const cy = @import("../cyber.zig");
const bcgen = @import("../bc_gen.zig");
const stencils = @import("stencils.zig");
const log = cy.log.scoped(.jit_gen);
const bt = cy.types.BuiltinTypes;
const v = cy.fmt.v;
const rt = cy.rt;
const ir = cy.ir;
const assembler = @import("assembler.zig");
const A64 = assembler.A64;

const GenValue = bcgen.GenValue;
const RegisterCstr = cy.register.RegisterCstr;
const RegisterId = cy.register.RegisterId;
const genValue = bcgen.genValue;

const FpReg: A64.Register = .x1;

/// Nop insts with an immediate id are generated before each IR node visit.
/// The id can then be set to `GenBreakpointAtMarker` so the next run
/// can print the relevant source location and generate a breakpoint.
const GenNopDebugMarkers = cy.Trace and true;
const GenBreakpointAtMarker: ?u64 = null;// 9;
var GenNextMarkerId: u64 = 1;
var DumpCodeFrom: ?usize = null;

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
    pub const jitEnsureCap = ensureCap;
    pub const jitCopyAdvance = copyAdvance;
    pub const jitPush = push;
    pub const jitPushU32 = pushU32;
    pub const jitPushU64 = pushU64;
    pub const jitPushStencil = pushStencil;
    pub const jitPushCopyImmToSlot = pushCopyImmToSlot;
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

pub fn ensureCap(c: *cy.Chunk, size: usize) !usize {
    if (c.jitBuf.buf.items.len + size > c.jitBuf.buf.capacity) {
        var inc = c.jitBuf.buf.capacity / 2;
        if (inc == 0) {
            inc = std.mem.page_size;
        }
        try c.jitBuf.buf.ensureTotalCapacityPrecise(c.alloc, c.jitBuf.buf.capacity + inc);
    }
    return c.jitBuf.buf.items.len;
}

pub fn copyAdvance(c: *cy.Chunk, dst: usize, src: []const u8) void {
    const to: []u8 = @ptrCast(c.jitBuf.buf.items.ptr[dst..dst+src.len]);
    @memcpy(to, src);
    c.jitBuf.buf.items.len = dst+src.len;
}

pub fn pushU32(c: *cy.Chunk, code: u32) !void {
    const start = try ensureCap(c, @sizeOf(u32));
    c.jitBuf.buf.items.len += @sizeOf(u32);
    const dst: []u8 = @ptrCast(c.jitBuf.buf.items.ptr[start..start+@sizeOf(u32)]);
    @memcpy(dst, std.mem.asBytes(&code));
}

pub fn pushU64(c: *cy.Chunk, code: u64) !void {
    const start = try ensureCap(c, @sizeOf(u64));
    c.jitBuf.buf.items.len += @sizeOf(u64);
    const dst: []u8 = @ptrCast(c.jitBuf.buf.items.ptr[start..start+@sizeOf(u64)]);
    @memcpy(dst, std.mem.asBytes(&code));
}

pub fn push(c: *cy.Chunk, code: []const u8) !void {
    const start = try ensureCap(c, code.len);
    c.jitBuf.buf.items.len += code.len;
    const dst: []u8 = @ptrCast(c.jitBuf.buf.items.ptr[start..start+code.len]);
    @memcpy(dst, code);
}

pub fn pushStencil(c: *cy.Chunk, code: []const u8) !usize {
    const start = try ensureCap(c, code.len);
    c.jitBuf.buf.items.len += code.len;
    const dst: []u8 = @ptrCast(c.jitBuf.buf.items.ptr[start..start+code.len]);
    @memcpy(dst, code);
    return start;
}

fn genStmt(c: *cy.Chunk, idx: u32) anyerror!void {
    const code = c.irGetStmtCode(idx);
    const nodeId = c.irGetNode(idx);
    c.curNodeId = nodeId;
    if (cy.Trace) {
        const contextStr = try c.encoder.formatNode(nodeId, &cy.tempBuf);
        log.tracev("----{s}: {{{s}}}", .{@tagName(code), contextStr});

        if (GenNopDebugMarkers) {
            try A64.copyImm64(c, .xzr, GenNextMarkerId);
            if (GenBreakpointAtMarker) |id| {
                if (id == GenNextMarkerId) {
                    DumpCodeFrom = c.jitGetPos();
                    try cy.debug.printTraceAtNode(c, nodeId);
                    try A64.breakpoint(c);
                }
            }
            GenNextMarkerId += 1;
        }
    }
    switch (code) {
        // .breakStmt          => try breakStmt(c, nodeId),
        // .contStmt           => try contStmt(c, nodeId),
        .declareLocal       => try declareLocal(c, idx, nodeId),
        // .destrElemsStmt     => try destrElemsStmt(c, idx, nodeId),
        .exprStmt           => try exprStmt(c, idx, nodeId),
        // .forIterStmt        => try forIterStmt(c, idx, nodeId),
        // .forRangeStmt       => try forRangeStmt(c, idx, nodeId),
        .funcDecl           => try funcDecl(c, idx, nodeId),
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
        .verbose            => {
            if (cy.Trace and !cy.verbose) {
                cy.verbose = true;
                c.curBlock.resetVerboseOnBlockEnd = true;
            }
        },
        // .whileCondStmt      => try whileCondStmt(c, idx, nodeId),
        // .whileInfStmt       => try whileInfStmt(c, idx, nodeId),
        // .whileOptStmt       => try whileOptStmt(c, idx, nodeId),
        else => {
            return error.TODO;
        }
    }

    if (c.blocks.items.len > 0) {
        // Must have a block to check against expected stack starts.
        try bcgen.checkStack(c, nodeId);
    }
    log.tracev("----{s}: end", .{@tagName(code)});
}

fn exprStmt(c: *cy.Chunk, idx: usize, nodeId: cy.NodeId) !void {
    _ = nodeId;
    const data = c.irGetStmtData(idx, .exprStmt);

    const cstr = RegisterCstr.initSimple(data.returnMain);

    const expr = c.irAdvanceStmt(idx, .exprStmt);
    const exprv = try genExpr(c, expr, cstr);
    if (bcgen.unwindAndFreeTemp(c, exprv)) {
        // ARC cleanup.
        if (!data.returnMain) {
            // TODO: Merge with previous release inst.
            // try pushRelease(c, exprv.local, nodeId);
        }
    }

    if (data.returnMain) {
        c.curBlock.endLocal = exprv.local;
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

fn genBinOp(c: *cy.Chunk, idx: usize, cstr: RegisterCstr, opts: BinOpOptions, nodeId: cy.NodeId) !GenValue {
    const data = c.irGetExprData(idx, .preBinOp).binOp;
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
    const inst = try c.rega.selectForDstInst(cstr, willRetain);

    var prefer = bcgen.PreferDst{
        .dst = inst.dst,
        .canUseDst = !c.isParamOrLocalVar(inst.dst),
    };

    // Lhs.
    var leftv: GenValue = undefined;
    if (opts.left) |left| {
        leftv = left;
    } else {
        const leftIdx = c.irAdvanceExpr(idx, .preBinOp);
        leftv = try genExpr(c, leftIdx, RegisterCstr.preferIf(prefer.dst, prefer.canUseDst));
    }

    // Rhs.
    const rightv = try genExpr(c, data.right, prefer.nextCstr(leftv));

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
                if (cstr.type == .simple and cstr.data.simple.jitPreferCondFlag)  {
                    // Load operands.
                    try c.jitPushU32(A64.LoadStore.ldrImmOff(FpReg, leftv.local, .x2).bitCast());
                    try c.jitPushU32(A64.LoadStore.ldrImmOff(FpReg, rightv.local, .x3).bitCast());
                    try c.jitPush(&stencils.intPair);

                    // Compare.
                    try c.jitPushU32(A64.AddSubShifted.cmp(.x2, .x3).bitCast());
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
                try c.jitPushU32(A64.LoadStore.ldrImmOff(FpReg, leftv.local, .x2).bitCast());
                try c.jitPushU32(A64.LoadStore.ldrImmOff(FpReg, rightv.local, .x3).bitCast());

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
                try c.jitPushU32(A64.LoadStore.strImmOff(FpReg, inst.dst, .x2).bitCast());
            } else if (data.leftT == bt.Integer) {
                // try pushInlineBinExpr(c, getIntOpCode(data.op), leftv.local, rightv.local, inst.dst, nodeId);

                // Load operands.
                try c.jitPushU32(A64.LoadStore.ldrImmOff(FpReg, leftv.local, .x2).bitCast());
                try c.jitPushU32(A64.LoadStore.ldrImmOff(FpReg, rightv.local, .x3).bitCast());

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
                try c.jitPushU32(A64.LoadStore.strImmOff(FpReg, inst.dst, .x2).bitCast());
            } else return error.Unexpected;
        },
        // .equal_equal => {
        //     try c.pushOptionalDebugSym(nodeId);
        //     try c.buf.pushOp3Ext(.compare, leftv.local, rightv.local, inst.dst, c.desc(nodeId));
        // },
        // .bang_equal => {
        //     try c.pushOptionalDebugSym(nodeId);
        //     try c.buf.pushOp3Ext(.compareNot, leftv.local, rightv.local, inst.dst, c.desc(nodeId));
        // },
        else => {
            return c.reportErrorAt("Unsupported op: {}", &.{v(data.op)}, nodeId);
        },
    }

    const leftRetained = if (opts.left == null) bcgen.unwindTempKeepDst(c, leftv, inst.dst) else false;
    const rightRetained = bcgen.unwindTempKeepDst(c, rightv, inst.dst);

    // ARC cleanup.
    _ = leftRetained;
    _ = rightRetained;
    // try pushReleaseOpt2(c, leftRetained, leftv.local, rightRetained, rightv.local, nodeId);

    var val = genValue(c, inst.dst, retained);
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
    const data = c.irGetStmtData(idx, .ifStmt);
    const bodyEndJumpsStart = c.listDataStack.items.len;

    const condIdx = c.irAdvanceStmt(idx, .ifStmt);
    // var condNodeId = c.irGetNode(condIdx);

    var cstr = RegisterCstr.simple;
    cstr.data.simple.jitPreferCondFlag = true;
    const condv = try genExpr(c, condIdx, cstr);

    var prevCaseMissJump: usize = undefined;
    var prevCaseMissJumpFromCondFlag: bool = undefined;
    if (condv.type == .jitCondFlag) {
        prevCaseMissJump = c.jitGetPos();
        prevCaseMissJumpFromCondFlag = true;
        if (condv.data.jitCondFlag.type == .lt) {
            try c.jitPushU32(A64.BrCond.init(.ge, 0).bitCast());
        } else {
            return error.TODO;
        }
    } else {
        return error.TODO;
    }

    // ARC cleanup for true case.
    if (bcgen.unwindAndFreeTemp(c, condv)) {
        // try pushRelease(c, condv.local, condNodeId);
    }

    try bcgen.pushSubBlock(c, false, nodeId);
    try genStmts(c, data.bodyHead);
    try bcgen.popSubBlock(c);

    const hasElse = false;

    // if (data.numElseBlocks > 0) {
    //     const elseBlocks = c.irGetArray(data.elseBlocks, u32, data.numElseBlocks);

    //     for (elseBlocks) |elseIdx| {
    //         const elseBlockNodeId = c.irGetNode(elseIdx);
    //         const elseBlock = c.irGetExprData(elseIdx, .elseBlock);

    //         const bodyEndJump = try c.pushEmptyJump();
    //         try c.listDataStack.append(c.alloc, .{ .pc = bodyEndJump });

    //         // Jump here from prev case miss.
    //         c.patchJumpNotCondToCurPc(prevCaseMissJump);

    //         if (!elseBlock.isElse) {
    //             condIdx = c.irAdvanceExpr(elseIdx, .elseBlock);
    //             condNodeId = c.irGetNode(condIdx);
    //             condv = try genExpr(c, condIdx, RegisterCstr.simple, jit);
    //             prevCaseMissJump = try c.pushEmptyJumpNotCond(condv.local);

    //             // ARC cleanup for true case.
    //             if (unwindAndFreeTemp(c, condv)) {
    //                 try pushRelease(c, condv.local, condNodeId);
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
            const inst = c.jitGetA64Inst(prevCaseMissJump, A64.BrCond);
            inst.imm19 = @intCast((c.jitBuf.buf.items.len - prevCaseMissJump) >> 2);
        } else {
            return error.TODO;
        }
    }
}

fn genFloat(c: *cy.Chunk, idx: usize, cstr: RegisterCstr, nodeId: cy.NodeId) !GenValue {
    _ = nodeId;
    const data = c.irGetExprData(idx, .float);
    const inst = try c.rega.selectForNoErrInst(cstr, false);
    if (inst.requiresPreRelease) {
        // try pushRelease(c, inst.dst, nodeId);
    }

    const val = cy.Value.initF64(data.val);
    try c.jitPushCopyImmToSlot(inst.dst, val);

    const value = genValue(c, inst.dst, false);
    return finishInst(c, value, inst.finalDst);
}

fn genInt(c: *cy.Chunk, idx: usize, cstr: RegisterCstr, nodeId: cy.NodeId) !GenValue {
    _ = nodeId;
    const data = c.irGetExprData(idx, .int);

    const inst = try c.rega.selectForNoErrInst(cstr, false);
    if (inst.requiresPreRelease) {
        // try pushRelease(c, inst.dst, nodeId);
    }

    const val = cy.Value.initInt(@intCast(data.val));
    try c.jitPushCopyImmToSlot(inst.dst, val);

    const value = genValue(c, inst.dst, false);
    return finishInst(c, value, inst.finalDst);
}

fn finishInst(c: *cy.Chunk, val: GenValue, optDst: ?RegisterCstr) !GenValue {
    if (optDst) |dst| {
        const final = try genToFinalDst(c, val, dst);
        try bcgen.pushOptUnwindableTemp(c, final);
        return final;
    } else {
        try bcgen.pushOptUnwindableTemp(c, val);
        return val;
    }
}

fn genToFinalDst(c: *cy.Chunk, val: GenValue, dst: RegisterCstr) !GenValue {
    log.tracev("genToFinalDst src: {} dst: {s}", .{val.local, @tagName(dst.type)});

    const desc = cy.bytecode.InstDesc{};
    const res = try genToDst(c, val, dst, desc);

    // Check to remove the temp that is used to move to final dst.
    if (val.isTempLocal) c.rega.freeTemps(1);
    return res;
}

fn genToDst(c: *cy.Chunk, val: GenValue, dst: RegisterCstr, desc: cy.bytecode.InstDesc) !GenValue {
    _ = desc;
    switch (dst.type) {
        .local => {
            const local = dst.data.local;
            if (val.local == local.reg) return error.Unexpected;
            if (local.retained) {
                // try c.buf.pushOp2Ext(.copyReleaseDst, val.local, local.reg, desc);
                return error.TODO;
            } else {
                // try c.buf.pushOp2Ext(.copy, val.local, local.reg, desc);
                try c.jitPushU32(A64.LoadStore.ldrImmOff(FpReg, val.local, .x8).bitCast());
                try c.jitPushU32(A64.LoadStore.strImmOff(FpReg, local.reg, .x8).bitCast());
            }
            // Parent only cares about the retained property.
            return GenValue.initRetained(val.retained);
        },
        // .boxedLocal => {
        //     const boxed = dst.data.boxedLocal;
        //     if (val.local == boxed.reg) return error.Unexpected;
        //     if (boxed.retained) {
        //         try c.buf.pushOp2Ext(.setBoxValueRelease, boxed.reg, val.local, desc);
        //     } else {
        //         try c.buf.pushOp2Ext(.setBoxValue, boxed.reg, val.local, desc);
        //     }
        //     return GenValue.initRetained(val.retained);
        // },
        // .varSym => {
        //     // Set var assumes retained src.
        //     const pc = c.buf.len();
        //     try c.buf.pushOp3(.setStaticVar, 0, 0, val.local);
        //     c.buf.setOpArgU16(pc + 1, @intCast(dst.data.varSym));
        //     return GenValue.initRetained(val.retained);
        // },
        // .captured => {
        //     const captured = dst.data.captured;
        //     try c.buf.pushOp3Ext(.setCaptured, c.curBlock.closureLocal, captured.idx, val.local, desc);
        //     return GenValue.initRetained(val.retained);
        // },
        // .exact => {
        //     if (val.local == dst.data.exact) return error.Unexpected;
        //     try c.buf.pushOp2(.copy, val.local, dst.data.exact);
        //     return genValue(c, dst.data.exact, val.retained);
        // },
        else => {
            log.tracev("{}", .{dst.type});
            return error.TODO;
        },
    }
}

fn genCallFuncSym(c: *cy.Chunk, idx: usize, cstr: RegisterCstr, nodeId: cy.NodeId) !GenValue {
    _ = nodeId;
    const data = c.irGetExprData(idx, .preCallFuncSym).callFuncSym;
    const inst = try bcgen.beginCall(c, cstr, false);

    const argsIdx = c.irAdvanceExpr(idx, .preCallFuncSym);
    const args = c.irGetArray(argsIdx, u32, data.numArgs);

    const argStart = c.rega.nextTemp;
    for (args, 0..) |argIdx, i| {
        const temp = try c.rega.consumeNextTemp();
        if (cy.Trace and temp != argStart + i) return error.Unexpected;
        try genAndPushExpr(c, argIdx, RegisterCstr.exact(temp));
    }

    if (data.hasDynamicArg) {
        // try genCallTypeCheck(c, inst.ret + cy.vm.CallArgStart, data.numArgs, data.func.funcSigId, nodeId);
    }

    if (data.func.type == .hostFunc) {
        // Populate callHost stencil args.
        try c.jitPushU32(A64.AddSubImm.add(.x2, FpReg, 8 * (inst.ret + cy.vm.CallArgStart)).bitCast());

        try A64.copyImm64(c, .x3, data.numArgs);

        try c.jitPush(stencils.callHost[0..stencils.callHost_hostFunc]);
        // No reloc needed, copy address to x30 (since it's already spilled) and invoke with blr.
        try A64.copyImm64(c, .x30, @intFromPtr(data.func.data.hostFunc.ptr));
        try c.jitPushU32(A64.Br.blr(.x30).bitCast());

        try c.jitPush(stencils.callHost[stencils.callHost_hostFunc+4..]);

        // Copy result to ret.
        // TODO: Copy directly to final dst.
        try c.jitPushU32(A64.LoadStore.strImmOff(FpReg, inst.ret, .x4).bitCast());
    } else if (data.func.type == .userFunc) {
        // Skip ret info.
        // Skip bc pc slot.
        try c.jitPushU32(A64.LoadStore.strImmOff(FpReg, inst.ret + 3, FpReg).bitCast());

        // Advance fp.
        try c.jitPushU32(A64.AddSubImm.add(FpReg, FpReg, 8 * inst.ret).bitCast());

        // Push empty branch.
        const jumpPc = c.jitGetPos();
        try c.jitBuf.relocs.append(c.alloc, .{ .type = .jumpToFunc, .data = .{ .jumpToFunc = .{
            .func = data.func,
            .pc = @intCast(jumpPc),
        }}});
        try c.jitPushU32(A64.BrImm.bl(0).bitCast());
    } else return error.TODO;

    const argvs = bcgen.popValues(c, data.numArgs);
    try bcgen.checkArgs(argStart, argvs);
    const retained = bcgen.unwindTemps(c, argvs);
    _ = retained;
    // try pushReleaseVals(c, retained, nodeId);

    const retRetained = c.sema.isRcCandidateType(data.func.retType);
    return endCall(c, inst, retRetained);
}

fn endCall(c: *cy.Chunk, inst: bcgen.CallInst, retained: bool) !GenValue {
    c.rega.freeTemps(inst.numPreludeTemps);
    const val = genValue(c, inst.ret, retained);
    return finishInst(c, val, inst.finalDst);
}

fn genAndPushExpr(c: *cy.Chunk, idx: usize, cstr: RegisterCstr) !void {
    const val = try genExpr(c, idx, cstr);
    try c.genValueStack.append(c.alloc, val);
}

fn zDumpJitSection(vm: *cy.VM, fp: [*]const cy.Value, chunkId: u64, irIdx: u64, startPc: [*]const u8, endPc: [*]const u8) void {
    const c = vm.compiler.chunks.items[@intCast(chunkId)];
    const code = c.irGetExprCode(@intCast(irIdx));
    const nodeId = c.irGetNode(@intCast(irIdx));

    const mc = startPc[0..@intFromPtr(endPc)-@intFromPtr(startPc)];
    const contextStr = c.encoder.formatNode(nodeId, &cy.tempBuf) catch cy.fatal();
    log.tracev("{s} {{{s}}} {*} {}", .{@tagName(code), contextStr, fp, std.fmt.fmtSliceHexLower(mc)});
}

fn genExpr(c: *cy.Chunk, idx: usize, cstr: RegisterCstr) anyerror!GenValue {
    const code = c.irGetExprCode(idx);
    const nodeId = c.irGetNode(idx);

    var dumpEndPc: usize = undefined;
    if (cy.Trace) {
        const contextStr = try c.encoder.formatNode(nodeId, &cy.tempBuf);
        log.tracev("{s}: {{{s}}} {s}", .{@tagName(code), contextStr, @tagName(cstr.type)});

        if (GenNopDebugMarkers) {
            try A64.copyImm64(c, .xzr, GenNextMarkerId);
            if (GenBreakpointAtMarker) |id| {
                if (id == GenNextMarkerId) {
                    DumpCodeFrom = c.jitGetPos();
                    try cy.debug.printTraceAtNode(c, nodeId);
                    try A64.breakpoint(c);
                }
            }
            GenNextMarkerId += 1;
        }

        if (cy.verbose) {
            try A64.copyImm64(c, .x2, c.id);
            try A64.copyImm64(c, .x3, idx);
            const dumpStartPc = c.jitGetPos();
            try c.jitPushU32(A64.PcRelAddr.adr(.x4, 0).bitCast());
            dumpEndPc = c.jitGetPos();
            try c.jitPushU32(A64.PcRelAddr.adr(.x5, 0).bitCast());
            try c.jitPush(stencils.dumpJitSection[0..stencils.dumpJitSection_zDumpJitSection]);
            try A64.copyImm64(c, .x30, @intFromPtr(&zDumpJitSection));
            try c.jitPushU32(A64.Br.blr(.x30).bitCast());
            try c.jitPush(stencils.dumpJitSection[stencils.dumpJitSection_zDumpJitSection+4..]);

            const adr = c.jitGetA64Inst(dumpStartPc, A64.PcRelAddr);
            adr.setOffsetFrom(dumpStartPc, c.jitGetPos());
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
        // .tagSym             => genTagSym(c, idx, cstr, nodeId),
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
            const adr = c.jitGetA64Inst(dumpEndPc, A64.PcRelAddr);
            adr.setOffsetFrom(dumpEndPc, c.jitGetPos());
        }
    }

    return res;
}

fn genStmts(c: *cy.Chunk, idx: u32) !void {
    var stmt = idx;
    while (stmt != cy.NullId) {
        try genStmt(c, stmt);
        stmt = c.irGetNextStmt(stmt);
    }
}

fn mainBlock(c: *cy.Chunk, idx: usize, nodeId: cy.NodeId) !void {
    const data = c.irGetStmtData(idx, .mainBlock);
    log.tracev("main block: {}", .{data.maxLocals});

    try bcgen.pushBlock(c, .main, nodeId);
    c.curBlock.frameLoc = 0;

    try bcgen.reserveMainRegs(c, data.maxLocals);

    c.jitBuf.mainPc = @intCast(c.jitGetPos());

    // Spill return addr to slot 0.
    try c.jitPushU32(A64.LoadStore.strImmOff(FpReg, 0, .x30).bitCast());

    var child = data.bodyHead;
    while (child != cy.NullId) {
        try genStmt(c, child);
        child = c.irGetNextStmt(child);
    }

    if (bcgen.shouldGenMainScopeReleaseOps(c.compiler)) {
        // try genBlockReleaseLocals(c);
    }
    if (c.curBlock.endLocal != cy.NullU8) {
        try mainEnd(c, c.curBlock.endLocal);
    } else {
        try mainEnd(c, null);
    }
    try bcgen.popBlock(c);

    c.buf.mainStackSize = c.getMaxUsedRegisters();

    // Pop boundary index.
    _ = c.popRetainedTemp();
}

fn mainEnd(c: *cy.Chunk, optReg: ?u8) !void {
    const retSlot = optReg orelse cy.NullU8;

    try A64.copyImm64(c, .x2, retSlot);
    try c.jitPush(&stencils.end);

    // Load ret addr back into x30.
    try c.jitPushU32(A64.LoadStore.ldrImmOff(FpReg, 0, .x30).bitCast());

    // Return.
    try c.jitPushU32(A64.Br.ret().bitCast());
}

fn genStringTemplate(c: *cy.Chunk, idx: usize, cstr: RegisterCstr, nodeId: cy.NodeId) !GenValue {
    const data = c.irGetExprData(idx, .stringTemplate);
    const strsIdx = c.irAdvanceExpr(idx, .stringTemplate);
    const strs = c.irGetArray(strsIdx, []const u8, data.numExprs+1);
    const args = c.irGetArray(data.args, u32, data.numExprs);

    const inst = try c.rega.selectForDstInst(cstr, true); 
    const argStart = c.rega.getNextTemp();

    for (args, 0..) |argIdx, i| {
        const temp = try c.rega.consumeNextTemp();
        if (cy.Trace and temp != argStart + i) return error.Unexpected;
        try genAndPushExpr(c, argIdx, RegisterCstr.exact(temp));
    }
    if (cy.Trace and c.rega.nextTemp != argStart + data.numExprs) return error.Unexpected;

    // Inline const strings.
    const skipPc = c.jitGetPos();
    try c.jitPushU32(@bitCast(A64.BrImm.b(0)));

    const strsPc = c.jitGetPos();
    for (strs) |str| {
        const ustr = try c.unescapeString(str);
        const constIdx = try c.buf.getOrPushStaticStringConst(ustr);
        const constStr = c.buf.consts.items[constIdx];
        try c.jitPushU64(@bitCast(constStr));
    }

    const skipi = c.jitGetA64Inst(skipPc, A64.BrImm);
    skipi.setOffsetFrom(skipPc, c.jitGetPos());

    // try c.pushOptionalDebugSym(nodeId);
    // try c.buf.pushOp3(.stringTemplate, argStart, data.numExprs, inst.dst);

    // Load strs.
    try c.jitPushU32(A64.PcRelAddr.adrFrom(.x2, c.jitGetPos(), strsPc).bitCast());

    // Load exprs.
    try c.jitPushU32(A64.AddSubImm.add(.x3, FpReg, 8 * argStart).bitCast());

    // Load expr count.
    try A64.copyImm64(c, .x4, data.numExprs);

    try c.jitPush(stencils.stringTemplate[0..stencils.stringTemplate_zAllocStringTemplate2]);
    // No reloc needed, copy address to x30 (since it's already spilled) and invoke with blr.
    try A64.copyImm64(c, .x30, @intFromPtr(&cy.vm.zAllocStringTemplate2));
    try c.jitPushU32(A64.Br.blr(.x30).bitCast());
    try c.jitPush(stencils.stringTemplate[stencils.stringTemplate_zAllocStringTemplate2+4..]);

    // Save result.
    try c.jitPushU32(A64.LoadStore.strImmOff(FpReg, inst.dst, .x5).bitCast());

    const argvs = bcgen.popValues(c, data.numExprs);
    try bcgen.checkArgs(argStart, argvs);
    const retained = bcgen.unwindTemps(c, argvs);
    try pushReleaseVals(c, retained, nodeId);

    const val = genValue(c, inst.dst, true);
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
        try A64.copyImm64(c, .x30, @intFromPtr(&cy.vm.zFreeObject));
        try c.jitPushU32(A64.Br.blr(.x30).bitCast());
        try c.jitPush(stencils.release[stencils.release_zFreeObject+4..]);
    }
}

fn declareLocal(c: *cy.Chunk, idx: u32, nodeId: cy.NodeId) !void {
    const data = c.irGetStmtData(idx, .declareLocal);
    if (data.assign) {
        // Don't advance nextLocalReg yet since the rhs hasn't generated so the
        // alive locals should not include this declaration.
        const reg = try bcgen.reserveLocalReg(c, data.id, data.declType, data.isBoxed, nodeId, false);

        const exprIdx = c.irAdvanceStmt(idx, .declareLocal);
        const val = try genExpr(c, exprIdx, RegisterCstr.toLocal(reg, false));

        const local = bcgen.getLocalInfoPtr(c, reg);

        // if (local.some.boxed) {
        //     try c.pushOptionalDebugSym(nodeId);
        //     try c.buf.pushOp2(.box, reg, reg);
        // }
        local.some.rcCandidate = val.retained;

        // rhs has generated, increase `nextLocalReg`.
        c.curBlock.nextLocalReg += 1;
        log.tracev("declare {}, rced: {} ", .{val.local, local.some.rcCandidate});
    } else {
        const reg = try bcgen.reserveLocalReg(c, data.id, data.declType, data.isBoxed, nodeId, true);

        // Not yet initialized, so it does not have a refcount.
        bcgen.getLocalInfoPtr(c, reg).some.rcCandidate = false;
    }
}

fn funcDecl(c: *cy.Chunk, idx: usize, nodeId: cy.NodeId) !void {
    const data = c.irGetStmtData(idx, .funcDecl);
    const func = data.func;
    const paramsIdx = c.irAdvanceStmt(idx, .funcDecl);
    const params = c.irGetArray(paramsIdx, ir.FuncParam, func.numParams);

    const funcPc = c.jitGetPos();

    // A64 relies on bl to obtain the return addr in x30.
    // Ideally, the return addr shouldn't be spilled until the first function call.
    const inst = A64.LoadStore.strImmOff(FpReg, 2, .x30);
    try c.jitPush(std.mem.asBytes(&inst));

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

pub fn genChunk(c: *cy.Chunk) !void {
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

    const code = c.irGetStmtCode(0);
    if (code != .root) return error.Unexpected;

    const data = c.irGetStmtData(0, .root);
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
            std.debug.print("Dump at marker: {}\n", .{std.fmt.fmtSliceHexLower(codeBuf)});
            std.debug.print("Dump all: {}\n", .{std.fmt.fmtSliceHexLower(c.jitBuf.buf.items)});
            DumpCodeFrom = null;
        }
    }
}

fn genLocalReg(c: *cy.Chunk, reg: RegisterId, cstr: RegisterCstr, nodeId: cy.NodeId) !GenValue {
    _ = nodeId;
    const local = bcgen.getLocalInfo(c, reg);

    if (!local.some.boxed) {
        const inst = try c.rega.selectForLocalInst(cstr, reg, local.some.rcCandidate);
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
                    try c.jitPushU32(A64.LoadStore.ldrImmOff(FpReg, reg, .x8).bitCast());
                    try c.jitPushU32(A64.LoadStore.strImmOff(FpReg, inst.dst, .x8).bitCast());
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
        const val = genValue(c, inst.dst, inst.retainSrc);
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

fn genLocal(c: *cy.Chunk, idx: usize, cstr: RegisterCstr, nodeId: cy.NodeId) !GenValue {
    const data = c.irGetExprData(idx, .local);
    const reg = bcgen.toLocalReg(c, data.id);
    return genLocalReg(c, reg, cstr, nodeId);
}

fn retExprStmt(c: *cy.Chunk, idx: usize, nodeId: cy.NodeId) !void {
    _ = nodeId;
    const childIdx = c.irAdvanceStmt(idx, .retExprStmt);

    // TODO: If the returned expr is a local, consume the local after copying to reg 0.
    var childv: GenValue = undefined;
    if (c.curBlock.type == .main) {
        // Main block.
        childv = try genExpr(c, childIdx, RegisterCstr.simpleMustRetain);
    } else {
        childv = try genExpr(c, childIdx, RegisterCstr.exactMustRetain(0));
    }

    _ = bcgen.unwindAndFreeTemp(c, childv);

    // try genBlockReleaseLocals(c);
    if (c.curBlock.type == .main) {
        // try c.buf.pushOp1(.end, @intCast(childv.local));
        return error.TODO;
    } else {
        // Load ret addr back into x30.
        const loadRet = A64.LoadStore.ldrImmOff(FpReg, 2, .x30);
        try c.jitPush(std.mem.asBytes(&loadRet));

        // Load prev fp.
        const loadFp = A64.LoadStore.ldrImmOff(FpReg, 3, FpReg);
        try c.jitPush(std.mem.asBytes(&loadFp));

        // Return.
        const reti = A64.Br.ret();
        try c.jitPush(std.mem.asBytes(&reti));
    }
}

fn pushCopyImmToSlot(c: *cy.Chunk, dst: RegisterId, val: cy.Value) !void {
    try A64.copyImm64(c, .x8, val.val);
    try A64.copyToPtrImmOff(c, FpReg, dst, .x8);
}
