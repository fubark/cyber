const std = @import("std");
const cy = @import("cyber.zig");
const jitgen = @import("jit/gen.zig");
const log = cy.log.scoped(.bc_gen);
const ir = cy.ir;
const rt = cy.rt;
const sema = cy.sema;
const types = cy.types;
const bt = types.BuiltinTypes;
const v = cy.fmt.v;
const vmc = cy.vmc;

const RegisterCstr = cy.register.RegisterCstr;
const RegisterId = cy.register.RegisterId;
const TypeId = types.TypeId;

const Chunk = cy.chunk.Chunk;

pub fn unwindTemp(c: *cy.Chunk, val: GenValue) bool {
    if (val.isRetainedTemp()) {
        _ = c.popRetainedTemp();
        return true;
    }
    return false;
}

/// Returns whether value is a retained temp.
pub fn unwindAndFreeTemp(c: *cy.Chunk, val: GenValue) bool {
    if (val.isTempLocal) {
        c.rega.freeTemps(1);
        if (val.retained) {
            _ = c.popRetainedTemp();
            return true;
        }
    }
    return false;
}

pub fn unwindTempKeepDst(c: *cy.Chunk, val: GenValue, dst: RegisterId) bool {
    if (val.isTempLocal) {
        if (val.local != dst) {
            c.rega.freeTemps(1);
        }
        if (val.retained) {
            _ = c.popRetainedTemp();
            return true;
        }
    }
    return false;
}

/// Returns modified slice of retained temps.
pub fn unwindTemps(c: *cy.Chunk, vals: []GenValue) []GenValue {
    var i: u32 = 0;
    for (vals) |val| {
        if (val.isTempLocal) {
            c.rega.freeTemps(1);
            if (val.retained) {
                vals[i] = val;
                i += 1;
            }
        }
    }
    _ = c.popRetainedTemps(i);
    return vals[0..i];
}

pub fn popValues(c: *Chunk, n: u32) []GenValue {
    const vals = c.genValueStack.items[c.genValueStack.items.len-n..];
    c.genValueStack.items.len = c.genValueStack.items.len-n;
    return vals;
}

pub fn genChunk(c: *Chunk) !void {
    genChunkInner(c) catch |err| {
        if (err != error.CompileError) {
            // Wrap all other errors as a CompileError.
            try c.setErrorFmtAt("error.{}", &.{v(err)}, c.curNodeId);
            return error.CompileError;
        } else return err;
    };
}

fn genStmts(c: *Chunk, idx: u32) !void {
    var stmt = idx;
    while (stmt != cy.NullId) {
        try genStmt(c, stmt);
        stmt = c.irGetNextStmt(stmt);
    }
}

fn genStmt(c: *Chunk, idx: u32) anyerror!void {
    const code = c.irGetStmtCode(idx);
    const nodeId = c.irGetNode(idx);
    c.curNodeId = nodeId;
    if (cy.Trace) {
        const contextStr = try c.encoder.formatNode(nodeId, &cy.tempBuf);
        log.tracev("----{s}: {{{s}}}", .{@tagName(code), contextStr});
    }
    switch (code) {
        .breakStmt          => try breakStmt(c, nodeId),
        .contStmt           => try contStmt(c, nodeId),
        .declareLocal       => try declareLocal(c, idx, nodeId),
        .destrElemsStmt     => try destrElemsStmt(c, idx, nodeId),
        .exprStmt           => try exprStmt(c, idx, nodeId),
        .forIterStmt        => try forIterStmt(c, idx, nodeId),
        .forRangeStmt       => try forRangeStmt(c, idx, nodeId),
        .funcDecl           => try funcDecl(c, idx, nodeId),
        .ifStmt             => try ifStmt(c, idx, nodeId),
        .mainBlock          => try mainBlock(c, idx, nodeId),
        .opSet              => try opSet(c, idx, nodeId),
        .pushDebugLabel     => try pushDebugLabel(c, idx),
        .retExprStmt        => try retExprStmt(c, idx, nodeId),
        .retStmt            => try retStmt(c),
        .setCallObjSymTern  => try setCallObjSymTern(c, idx, nodeId),
        .setCaptured        => try setCaptured(c, idx, nodeId),
        .setField           => try setField(c, idx, .{}, nodeId),
        .setFuncSym         => try setFuncSym(c, idx, nodeId),
        .setIndex           => try setIndex(c, idx, nodeId),
        .setLocal           => try irSetLocal(c, idx, nodeId),
        .setObjectField     => try setObjectField(c, idx, .{}, nodeId),
        .setVarSym          => try setVarSym(c, idx, nodeId),
        .setLocalType       => try setLocalType(c, idx),
        .switchStmt         => try switchStmt(c, idx, nodeId),
        .tryStmt            => try tryStmt(c, idx, nodeId),
        .verbose            => {
            if (cy.Trace and !cy.verbose) {
                cy.verbose = true;
                c.curBlock.resetVerboseOnBlockEnd = true;
            }
        },
        .whileCondStmt      => try whileCondStmt(c, idx, nodeId),
        .whileInfStmt       => try whileInfStmt(c, idx, nodeId),
        .whileOptStmt       => try whileOptStmt(c, idx, nodeId),
        // TODO: Specialize op assign.
        // .opSetLocal => try opSetLocal(c, getData(pc, .opSetLocal), nodeId),
        // .opSetObjectField => try opSetObjectField(c, getData(pc, .opSetObjectField), nodeId),
        // .opSetField => try opSetField(c, getData(pc, .opSetField), nodeId),
        //         .dumpBytecode => {
        //             try cy.debug.dumpBytecode(c.compiler.vm, null);
        //         },
        else => {
            return error.TODO;
        }
    }

    if (c.blocks.items.len > 0) {
        // Must have a block to check against expected stack starts.
        try checkStack(c, nodeId);
    }
    log.tracev("----{s}: end", .{@tagName(code)});
}

fn genChunkInner(c: *Chunk) !void {
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
}

fn genAndPushExpr(c: *Chunk, idx: usize, cstr: RegisterCstr) !void {
    const val = try genExpr(c, idx, cstr);
    try c.genValueStack.append(c.alloc, val);
}

fn genExpr(c: *Chunk, idx: usize, cstr: RegisterCstr) anyerror!GenValue {
    const code = c.irGetExprCode(idx);
    const nodeId = c.irGetNode(idx);
    if (cy.Trace) {
        const contextStr = try c.encoder.formatNode(nodeId, &cy.tempBuf);
        log.tracev("{s}: {{{s}}} {s}", .{@tagName(code), contextStr, @tagName(cstr.type)});
    }
    const res = try switch (code) {
        .captured           => genCaptured(c, idx, cstr, nodeId),
        .cast               => genCast(c, idx, cstr, nodeId),
        .coinitCall         => genCoinitCall(c, idx, cstr, nodeId),
        .condExpr           => genCondExpr(c, idx, cstr, nodeId),
        .coresume           => genCoresume(c, idx, cstr, nodeId),
        .coyield            => genCoyield(c, idx, cstr, nodeId),
        .enumMemberSym      => genEnumMemberSym(c, idx, cstr, nodeId),
        .errorv             => genError(c, idx, cstr, nodeId),
        .falsev             => genFalse(c, cstr, nodeId),
        .fieldDynamic       => genFieldDynamic(c, idx, cstr, .{}, nodeId),
        .fieldStatic        => genFieldStatic(c, idx, cstr, .{}, nodeId),
        .float              => genFloat(c, idx, cstr, nodeId),
        .funcSym            => genFuncSym(c, idx, cstr, nodeId),
        .int                => genInt(c, idx, cstr, nodeId),
        .lambda             => genLambda(c, idx, cstr, nodeId),
        .list               => genList(c, idx, cstr, nodeId),
        .local              => genLocal(c, idx, cstr, nodeId),
        .map                => genMap(c, idx, cstr, nodeId),
        .none               => genNone(c, cstr, nodeId),
        .objectInit         => genObjectInit(c, idx, cstr, nodeId),
        .pre                => return error.Unexpected,
        .preBinOp           => genBinOp(c, idx, cstr, .{}, nodeId),
        .preCall            => genCall(c, idx, cstr, nodeId),
        .preCallFuncSym     => genCallFuncSym(c, idx, cstr, nodeId),
        .preCallObjSym      => genCallObjSym(c, idx, cstr, nodeId),
        .preCallObjSymBinOp => genCallObjSymBinOp(c, idx, cstr, nodeId),
        .preCallObjSymUnOp  => genCallObjSymUnOp(c, idx, cstr, nodeId),
        .preSlice           => genSlice(c, idx, cstr, nodeId),
        .preUnOp            => genUnOp(c, idx, cstr, nodeId),
        .string             => genString(c, idx, cstr, nodeId),
        .stringTemplate     => genStringTemplate(c, idx, cstr, nodeId),
        .switchBlock        => genSwitchBlock(c, idx, cstr, nodeId),
        .tagSym             => genTagSym(c, idx, cstr, nodeId),
        .throw              => genThrow(c, idx, nodeId),
        .truev              => genTrue(c, cstr, nodeId),
        .tryExpr            => genTryExpr(c, idx, cstr, nodeId),
        .typeSym            => genTypeSym(c, idx, cstr, nodeId),
        .varSym             => genVarSym(c, idx, cstr, nodeId),
        else => return error.TODO,
    };
    log.tracev("{s}: end", .{@tagName(code)});
    return res;
}

fn pushDebugLabel(c: *Chunk, idx: usize) !void {
    const data = c.irGetStmtData(idx, .pushDebugLabel);
    try c.buf.pushDebugLabel(data.name);
}

fn mainBlock(c: *Chunk, idx: usize, nodeId: cy.NodeId) !void {
    const data = c.irGetStmtData(idx, .mainBlock);
    log.tracev("main block: {}", .{data.maxLocals});

    try pushBlock(c, .main, nodeId);
    c.curBlock.frameLoc = 0;

    try reserveMainRegs(c, data.maxLocals);

    var child = data.bodyHead;
    while (child != cy.NullId) {
        try genStmt(c, child);
        child = c.irGetNextStmt(child);
    }

    if (shouldGenMainScopeReleaseOps(c.compiler)) {
        try genBlockReleaseLocals(c);
    }
    if (c.curBlock.endLocal != cy.NullU8) {
        try mainEnd(c, c.curBlock.endLocal);
    } else {
        try mainEnd(c, null);
    }
    try popBlock(c);

    c.buf.mainStackSize = c.getMaxUsedRegisters();

    // Pop boundary index.
    _ = c.popRetainedTemp();
}

fn funcDecl(c: *Chunk, idx: usize, nodeId: cy.NodeId) !void {
    const data = c.irGetStmtData(idx, .funcDecl);
    const func = data.func;
    const paramsIdx = c.irAdvanceStmt(idx, .funcDecl);
    const params = c.irGetArray(paramsIdx, ir.FuncParam, func.numParams);

    // Reserve jump to skip the body.
    const skipJump = try c.pushEmptyJump();

    const funcPc = c.buf.ops.items.len;

    try pushFuncBlock(c, data, params, nodeId);

    try genStmts(c, data.bodyHead);

    // Get stack size.
    const stackSize = c.getMaxUsedRegisters();

    // Patch empty func sym slot.
    const rtId = c.compiler.genSymMap.get(func).?.funcSym.id;
    const rtFunc = rt.FuncSymbol.initFunc(funcPc, stackSize, func.numParams, func.funcSigId);
    c.compiler.vm.funcSyms.buf[rtId] = rtFunc;

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

    try popFuncBlockCommon(c, func);
    c.patchJumpToCurPc(skipJump);
}

fn genCoresume(c: *Chunk, idx: usize, cstr: RegisterCstr, nodeId: cy.NodeId) !GenValue {
    _ = nodeId;
    const inst = try c.rega.selectForDstInst(cstr, true);

    const childIdx = c.irAdvanceExpr(idx, .coresume);
    const childv = try genExpr(c, childIdx, RegisterCstr.tempMustRetain);

    try c.buf.pushOp2(.coresume, childv.local, inst.dst);

    _ = unwindAndFreeTemp(c, childv);

    const val = genValue(c, inst.dst, true);
    return finishInst(c, val, inst.finalDst);
}

fn genCoyield(c: *Chunk, idx: usize, cstr: RegisterCstr, nodeId: cy.NodeId) !GenValue {
    _ = idx;
    try c.buf.pushOp2(.coyield, c.curBlock.startLocalReg, c.curBlock.nextLocalReg);
    // TODO: return coyield expression.
    return genNone(c, cstr, nodeId);
}

fn genCoinitCall(c: *Chunk, idx: usize, cstr: RegisterCstr, nodeId: cy.NodeId) !GenValue {
    const callIdx = c.irAdvanceExpr(idx, .coinitCall);
    const callCode: ir.ExprCode = @enumFromInt(c.irBuf.items[callIdx]);
    const data = c.irGetExprData(callIdx, .pre);

    const inst = try c.rega.selectForDstInst(cstr, true);

    const tempStart = c.rega.nextTemp;
    var numArgs: u32 = 0;
    var args: []align(1) const u32 = undefined;
    if (callCode == .preCallFuncSym) {
        numArgs = data.callFuncSym.numArgs;

        const argsIdx = c.irAdvanceExpr(callIdx, .preCallFuncSym);
        args = c.irGetArray(argsIdx, u32, numArgs);
    } else if (callCode == .preCall) {
        numArgs = data.call.numArgs;
        args = c.irGetArray(data.call.args, u32, numArgs);

        const calleeIdx = c.irAdvanceExpr(callIdx, .preCall);
        const temp = try c.rega.consumeNextTemp();
        _ = try genAndPushExpr(c, calleeIdx, RegisterCstr.exactMustRetain(temp));
    } else return error.Unexpected;

    for (args) |argIdx| {
        const temp = try c.rega.consumeNextTemp();
        try genAndPushExpr(c, argIdx, RegisterCstr.exactMustRetain(temp));
    }

    const node = c.nodes[nodeId];
    const callExprId = node.head.child_head;

    var numTotalArgs = numArgs;
    var argDst: u8 = undefined;
    if (callCode == .preCallFuncSym) {
        argDst = 1 + cy.vm.CallArgStart;
    } else if (callCode == .preCall) {
        numTotalArgs += 1;
        argDst = 1 + cy.vm.CallArgStart - 1;
    } else return error.Unexpected;

    // Compute initial stack size.
    // 1 + (min call ret) + 4 (prelude) + 1 (callee) + numArgs
    var initialStackSize = 1 + 4 + 1 + numArgs;
    if (initialStackSize < 16) {
        initialStackSize = 16;
    }
    const coinitPc = c.buf.ops.items.len;
    try c.pushOptionalDebugSym(nodeId);
    try c.buf.pushOpSlice(.coinit, &[_]u8{
        tempStart, @intCast(numTotalArgs), argDst, 0, @intCast(initialStackSize), inst.dst });

    try pushFiberBlock(c, @intCast(numTotalArgs), nodeId);

    // Gen func call.
    const callRet: u8 = 1;
    if (callCode == .preCallFuncSym) {
        const rtId = c.compiler.genSymMap.get(data.callFuncSym.func).?.funcSym.id;
        try pushCallSym(c, callRet, numArgs, 1, rtId, callExprId);
    } else if (callCode == .preCall) {
        try c.pushFailableDebugSym(nodeId);
        try c.buf.pushOp3Ext(.call, callRet, @intCast(numArgs), 1, c.desc(callExprId));
    } else return error.Unexpected;

    try c.buf.pushOp(.coreturn);
    c.buf.setOpArgs1(coinitPc + 4, @intCast(c.buf.ops.items.len - coinitPc));

    try popFiberBlock(c);

    const argvs = popValues(c, numTotalArgs);
    _ = unwindTemps(c, argvs);

    const val = genValue(c, inst.dst, true);
    return finishInst(c, val, inst.finalDst);
}

fn genCast(c: *Chunk, idx: usize, cstr: RegisterCstr, nodeId: cy.NodeId) !GenValue {
    const data = c.irGetExprData(idx, .cast);

    const inst = try c.rega.selectForDstInst(cstr, true);

    const childIdx = c.irAdvanceExpr(idx, .cast);
    // TODO: If inst.dst is a temp, this should have a cstr of localOrExact.
    const childv = try genExpr(c, childIdx, RegisterCstr.initSimple(cstr.mustRetain));

    const sym = c.sema.getTypeSym(data.typeId);
    if (sym.type == .object) {
        try c.pushFailableDebugSym(nodeId);
        const pc = c.buf.ops.items.len;
        try c.buf.pushOpSlice(.cast, &.{ childv.local, 0, 0, inst.dst });
        c.buf.setOpArgU16(pc + 2, @intCast(data.typeId));
    } else if (sym.type == .predefinedType) {
        if (types.toRtConcreteType(data.typeId)) |tId| {
            try c.pushFailableDebugSym(nodeId);
            const pc = c.buf.ops.items.len;
            try c.buf.pushOpSlice(.cast, &.{ childv.local, 0, 0, inst.dst });
            c.buf.setOpArgU16(pc + 2, @intCast(tId));
        } else {
            // Cast to abstract type.
            try c.pushFailableDebugSym(nodeId);
            const pc = c.buf.ops.items.len;
            try c.buf.pushOpSlice(.castAbstract, &.{ childv.local, 0, 0, inst.dst });
            c.buf.setOpArgU16(pc + 2, @intCast(data.typeId));
        }
    } else {
        return error.TODO;
    }

    _ = unwindAndFreeTemp(c, childv);

    const val = genValue(c, inst.dst, childv.retained);
    return finishInst(c, val, inst.finalDst);
} 

const FieldOptions = struct {
    recv: ?GenValue = null,
};

fn genFieldDynamic(c: *Chunk, idx: usize, cstr: RegisterCstr, opts: FieldOptions, nodeId: cy.NodeId) !GenValue {
    const data = c.irGetExprData(idx, .fieldDynamic).dynamic;
    const recIdx = c.irAdvanceExpr(idx, .fieldDynamic);

    const inst = try c.rega.selectForDstInst(cstr, true);
    const ownRecv = opts.recv == null;

    var recv: GenValue = undefined;
    if (opts.recv) |recv_| {
        recv = recv_;
    } else {
        recv = try genExpr(c, recIdx, RegisterCstr.simple);
    }

    const fieldId = try c.compiler.vm.ensureFieldSym(data.name);
    try pushField(c, recv.local, inst.dst, @intCast(fieldId), nodeId);

    if (ownRecv) {
        if (unwindAndFreeTemp(c, recv)) {
            try pushRelease(c, recv.local, nodeId);
        }
    }

    const val = genValue(c, inst.dst, true);
    return finishInst(c, val, inst.finalDst);
}

fn genFieldStatic(c: *Chunk, idx: usize, cstr: RegisterCstr, opts: FieldOptions, nodeId: cy.NodeId) !GenValue {
    const data = c.irGetExprData(idx, .fieldStatic).static;
    const recIdx = c.irAdvanceExpr(idx, .fieldStatic);

    const inst = try c.rega.selectForNoErrInst(cstr, true);
    const ownRecv = opts.recv == null;

    var recv: GenValue = undefined;
    if (opts.recv) |recv_| {
        recv = recv_;
    } else {
        recv = try genExpr(c, recIdx, RegisterCstr.simple);
    }

    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, nodeId);
    }
    try pushObjectField(c, recv.local, data.idx, inst.dst, nodeId);

    if (ownRecv) {
        if (unwindAndFreeTemp(c, recv)) {
            // ARC cleanup.
            try pushRelease(c, recv.local, nodeId);
        }
    }

    const willRetain = c.sema.isRcCandidateType(data.typeId);
    const val = genValue(c, inst.dst, willRetain);
    return finishInst(c, val, inst.finalDst);
}

fn genObjectInit(c: *Chunk, idx: usize, cstr: RegisterCstr, nodeId: cy.NodeId) !GenValue {
    const data = c.irGetExprData(idx, .objectInit);

    const inst = try c.rega.selectForDstInst(cstr, true);
    const argStart = c.rega.nextTemp;

    // TODO: Would it be faster/efficient to copy the fields into contiguous registers
    //       and copy all at once to heap or pass locals into the operands and iterate each and copy to heap?
    //       The current implementation is the former.
    const argsIdx = c.irAdvanceExpr(idx, .objectInit);
    const args = c.irGetArray(argsIdx, u32, data.numArgs);
    for (args) |argIdx| {
        try genAndPushExpr(c, argIdx, RegisterCstr.tempMustRetain);
    }

    const sym = c.sema.getTypeSym(data.typeId).cast(.object);

    if (data.numFieldsToCheck > 0) {
        try c.pushFailableDebugSym(nodeId);
        try c.buf.pushOp2(.objectTypeCheck, argStart, @intCast(data.numFieldsToCheck));

        const checkFields = c.irGetArray(data.fieldsToCheck, u8, data.numFieldsToCheck);

        for (checkFields) |fidx| {
            const start = c.buf.ops.items.len;
            try c.buf.pushOperands(&.{ @as(u8, @intCast(fidx)), 0, 0, 0, 0 });
            c.buf.setOpArgU32(start + 1, sym.fields[fidx].type);
        }
    }

    try pushObjectInit(c, data.typeId, argStart, @intCast(sym.numFields), inst.dst, nodeId);

    const argvs = popValues(c, data.numArgs);
    _ = unwindTemps(c, argvs);

    const val = genValue(c, inst.dst, true);
    return finishInst(c, val, inst.finalDst);
}

fn breakStmt(c: *Chunk, nodeId: cy.NodeId) !void {
    // Release from startLocal of the first parent loop block.
    var idx = c.subBlocks.items.len-1;
    while (true) {
        const sblock = c.subBlocks.items[idx];
        if (sblock.isLoopBlock) {
            try genReleaseLocals(c, sblock.nextLocalReg, nodeId);
            break;
        }
        idx -= 1;
    }

    const pc = try c.pushEmptyJumpExt(c.desc(nodeId));
    try c.subBlockJumpStack.append(c.alloc, .{ .jumpT = .brk, .pc = pc });
}

fn contStmt(c: *Chunk, nodeId: cy.NodeId) !void {
    // Release from startLocal of the first parent loop block.
    var idx = c.subBlocks.items.len-1;
    while (true) {
        const sblock = c.subBlocks.items[idx];
        if (sblock.isLoopBlock) {
            try genReleaseLocals(c, sblock.nextLocalReg, nodeId);
            break;
        }
        idx -= 1;
    }

    const pc = try c.pushEmptyJumpExt(c.desc(nodeId));
    try c.subBlockJumpStack.append(c.alloc, .{ .jumpT = .cont, .pc = pc });
}

fn genThrow(c: *Chunk, idx: usize, nodeId: cy.NodeId) !GenValue {
    const childIdx = c.irAdvanceExpr(idx, .throw);
    const childv = try genExpr(c, childIdx, RegisterCstr.simple);

    try c.pushFailableDebugSym(nodeId);
    try c.buf.pushOp1(.throw, childv.local);

    _ = unwindAndFreeTemp(c, childv);
    return GenValue.initNoValue();
}

fn genNone(c: *Chunk, cstr: RegisterCstr, nodeId: cy.NodeId) !GenValue {
    const res = try c.rega.selectForNoErrInst(cstr, false);
    if (res.requiresPreRelease) {
        try pushRelease(c, res.dst, nodeId);
    }
    try pushNone(c, res.dst);
    const val = genValue(c, res.dst, false);
    return finishInst(c, val, res.finalDst);
}

fn pushNone(c: *Chunk, dst: LocalId) !void {
    try c.buf.pushOp1(.none, dst);
}

fn setCallObjSymTern(c: *Chunk, idx: usize, nodeId: cy.NodeId) !void {
    const data = c.irGetStmtData(idx, .setCallObjSymTern).callObjSymTern;

    const inst = try beginCall(c, RegisterCstr.none, false);

    var args: [4]GenValue = undefined;
    const recIdx = c.irAdvanceStmt(idx, .setCallObjSymTern);
    var temp = try c.rega.consumeNextTemp();
    args[0] = try genExpr(c, recIdx, RegisterCstr.exact(temp));
    temp = try c.rega.consumeNextTemp();
    args[1] = try genExpr(c, data.index, RegisterCstr.exact(temp));
    temp = try c.rega.consumeNextTemp();
    args[2] = try genExpr(c, data.right, RegisterCstr.exact(temp));

    const mgId = try c.compiler.vm.ensureMethodGroup(data.name);
    try pushCallObjSym(c, inst.ret, 3,
        @intCast(mgId), @intCast(data.funcSigId), nodeId);

    var retained = unwindTemps(c, args[0..3]);

    // Include ret value for release.
    retained.len += 1;
    retained[retained.len-1] = GenValue.initTempValue(inst.ret, true);

    try pushReleaseVals(c, retained, nodeId);

    // Unwind return temp as well.
    c.rega.freeTemps(inst.numPreludeTemps + 1);
}

fn setIndex(c: *Chunk, idx: usize, nodeId: cy.NodeId) !void {
    const data = c.irGetStmtData(idx, .setIndex).index;
    if (data.recvT != bt.List and data.recvT != bt.Map) {
        return error.Unexpected;
    }

    // None of the operands force a retain since it must mimic the generic $setIndex.
    const valsStart = c.genValueStack.items.len;

    const noneRet = try c.rega.consumeNextTemp();

    // Recv.
    const recIdx = c.irAdvanceStmt(idx, .setIndex);
    try genAndPushExpr(c, recIdx, RegisterCstr.simple);

    // Index
    try genAndPushExpr(c, data.index, RegisterCstr.simple);

    // RHS
    try genAndPushExpr(c, data.right, RegisterCstr.simple);

    const argvs = c.genValueStack.items[valsStart..];
    c.genValueStack.items.len = valsStart;

    const recv = argvs[0];
    const indexv = argvs[1];
    const rightv = argvs[2];

    if (data.recvT == bt.List) {
        try pushInlineTernExpr(c, .setIndexList, recv.local, indexv.local, rightv.local, noneRet, nodeId);
    } else if (data.recvT == bt.Map) {
        try pushInlineTernExpr(c, .setIndexMap, recv.local, indexv.local, rightv.local, noneRet, nodeId);
    } else return error.Unexpected;

    const retained = unwindTemps(c, argvs);
    c.rega.freeTemps(1);

    // ARC cleanup.
    try pushReleaseVals(c, retained, nodeId);
}

const SetFieldOptions = struct {
    recv: ?GenValue = null,
    right: ?GenValue = null,
};

fn setField(c: *Chunk, idx: usize, opts: SetFieldOptions, nodeId: cy.NodeId) !void {
    const setData = c.irGetStmtData(idx, .setField).generic;
    const fieldIdx = c.irAdvanceStmt(idx, .setField);
    const data = c.irGetExprData(fieldIdx, .fieldDynamic).dynamic;

    const fieldId = try c.compiler.vm.ensureFieldSym(data.name);
    const ownRecv = opts.recv == null;

    // LHS
    var recv: GenValue = undefined;
    if (opts.recv) |recv_| {
        recv = recv_;
    } else {
        const recIdx = c.irAdvanceExpr(fieldIdx, .fieldDynamic);
        recv = try genExpr(c, recIdx, RegisterCstr.simple);
    }

    // RHS
    var rightv: GenValue = undefined;
    if (opts.right) |right| {
        rightv = right;
    } else {
        rightv = try genExpr(c, setData.right, RegisterCstr.simpleMustRetain);
    }

    // Performs runtime type check.
    const pc = c.buf.ops.items.len;
    try c.pushFailableDebugSym(nodeId);
    try c.buf.pushOpSlice(.setField, &.{ recv.local, 0, 0, rightv.local, 0, 0, 0, 0, 0 });
    c.buf.setOpArgU16(pc + 2, @intCast(fieldId));

    _ = unwindAndFreeTemp(c, rightv);
    if (ownRecv) {
        if (unwindAndFreeTemp(c, recv)) {
            // ARC cleanup. Right is not released since it's being assigned to the field.
            try pushRelease(c, recv.local, nodeId);
        }
    }
}

const SetObjectFieldOptions = struct {
    recv: ?GenValue = null,
    right: ?GenValue = null,
};

fn setObjectField(c: *Chunk, idx: usize, opts: SetObjectFieldOptions, nodeId: cy.NodeId) !void {
    const data = c.irGetStmtData(idx, .setObjectField).generic;
    const requireTypeCheck = data.leftT.id != bt.Any and data.rightT.dynamic;
    const fieldIdx = c.irAdvanceStmt(idx, .setObjectField);
    const fieldData = c.irGetExprData(fieldIdx, .fieldStatic).static;

    // Receiver.
    var recv: GenValue = undefined;
    if (opts.recv) |recv_| {
        recv = recv_;
    } else {
        const recIdx = c.irAdvanceExpr(fieldIdx, .fieldStatic);
        recv = try genExpr(c, recIdx, RegisterCstr.simple);
    }

    // Right.
    var rightv: GenValue = undefined;
    if (opts.right) |right| {
        rightv = right;
    } else {
        rightv = try genExpr(c, data.right, RegisterCstr.simpleMustRetain);
    }

    const ownRecv = opts.recv == null;
    if (requireTypeCheck) {
        const pc = c.buf.ops.items.len;
        try c.pushFailableDebugSym(nodeId);
        try c.buf.pushOpSlice(.setObjectFieldCheck, &.{ recv.local, 0, 0, rightv.local, fieldData.idx });
        c.buf.setOpArgU16(pc + 2, @intCast(fieldData.typeId));
    } else {
        try c.buf.pushOpSlice(.setObjectField, &.{ recv.local, fieldData.idx, rightv.local });
    }

    _ = unwindAndFreeTemp(c, rightv);
    if (ownRecv) {
        if (unwindAndFreeTemp(c, recv)) {
            // ARC cleanup. Right is not released since it's being assigned to the field.
            try pushRelease(c, recv.local, nodeId);
        }
    }
}

fn genError(c: *Chunk, idx: usize, cstr: RegisterCstr, nodeId: cy.NodeId) !GenValue {
    const data = c.irGetExprData(idx, .errorv);

    const symId = try c.compiler.vm.ensureSymbol(data.name);
    const errval = cy.Value.initErrorSymbol(@intCast(symId));
    const constIdx = try c.buf.getOrPushConst(errval);

    const inst = try c.rega.selectForNoErrInst(cstr, false);
    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, nodeId);
    }
    try genConst(c, constIdx, inst.dst, false, nodeId);
    const val = genValue(c, inst.dst, false);
    return finishInst(c, val, inst.finalDst);
}

fn genTagSym(c: *Chunk, idx: usize, cstr: RegisterCstr, nodeId: cy.NodeId) !GenValue {
    const data = c.irGetExprData(idx, .tagSym);

    const symId = try c.compiler.vm.ensureSymbol(data.name);
    const inst = try c.rega.selectForNoErrInst(cstr, false);
    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, nodeId);
    }
    try c.buf.pushOp2(.tagLiteral, @intCast(symId), inst.dst);
    const val = genValue(c, inst.dst, false);
    return finishInst(c, val, inst.finalDst);
}

fn genTrue(c: *Chunk, cstr: RegisterCstr, nodeId: cy.NodeId) !GenValue {
    const inst = try c.rega.selectForNoErrInst(cstr, false);
    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, nodeId);
    }
    try c.buf.pushOp1(.true, inst.dst);
    const val = genValue(c, inst.dst, false);
    return finishInst(c, val, inst.finalDst);
}

fn genFalse(c: *Chunk, cstr: RegisterCstr, nodeId: cy.NodeId) !GenValue {
    const inst = try c.rega.selectForNoErrInst(cstr, false);
    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, nodeId);
    }
    try c.buf.pushOp1(.false, inst.dst);
    const val = genValue(c, inst.dst, false);
    return finishInst(c, val, inst.finalDst);
}

fn genInt(c: *Chunk, idx: usize, cstr: RegisterCstr, nodeId: cy.NodeId) !GenValue {
    const data = c.irGetExprData(idx, .int);

    const inst = try c.rega.selectForNoErrInst(cstr, false);
    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, nodeId);
    }
    const value = try genConstIntExt(c, data.val, inst.dst, c.desc(nodeId));
    return finishInst(c, value, inst.finalDst);
}

fn genFloat(c: *Chunk, idx: usize, cstr: RegisterCstr, nodeId: cy.NodeId) !GenValue {
    const data = c.irGetExprData(idx, .float);
    const inst = try c.rega.selectForNoErrInst(cstr, false);
    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, nodeId);
    }
    const val = try genConstFloat(c, data.val, inst.dst, nodeId);
    return finishInst(c, val, inst.finalDst);
}

fn genStringTemplate(c: *Chunk, idx: usize, cstr: RegisterCstr, nodeId: cy.NodeId) !GenValue {
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

    try c.pushOptionalDebugSym(nodeId);
    try c.buf.pushOp3(.stringTemplate, argStart, data.numExprs, inst.dst);

    // Append const str indexes.
    const start = try c.buf.reserveData(strs.len);
    for (strs, 0..) |str, i| {
        const ustr = try c.unescapeString(str);
        const constIdx = try c.buf.getOrPushStaticStringConst(ustr);
        c.buf.ops.items[start + i].val = @intCast(constIdx);
    }

    const argvs = popValues(c, data.numExprs);
    try checkArgs(argStart, argvs);
    const retained = unwindTemps(c, argvs);
    try pushReleaseVals(c, retained, nodeId);

    const val = genValue(c, inst.dst, true);
    return finishInst(c, val, inst.finalDst);
}

fn genString(c: *Chunk, idx: usize, cstr: RegisterCstr, nodeId: cy.NodeId) !GenValue {
    const data = c.irGetExprData(idx, .string);
    const str = try c.unescapeString(data.literal);
    const inst = try c.rega.selectForNoErrInst(cstr, true);
    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, nodeId);
    }
    const val = try string(c, str, inst.dst, nodeId);
    return finishInst(c, val, inst.finalDst);
}

fn string(c: *Chunk, str: []const u8, dst: RegisterId, nodeId: cy.NodeId) !GenValue {
    const idx = try c.buf.getOrPushStaticStringConst(str);
    try genConst(c, idx, dst, true, nodeId);
    return genValue(c, dst, true);
}

fn genSlice(c: *Chunk, idx: usize, cstr: RegisterCstr, nodeId: cy.NodeId) !GenValue {
    const data = c.irGetExprData(idx, .preSlice).slice;
    const inst = try c.rega.selectForDstInst(cstr, true);

    if (data.recvT != bt.List) {
        return error.Unexpected;
    }

    var args: [3]GenValue = undefined;
    const recIdx = c.irAdvanceExpr(idx, .preSlice);
    args[0] = try genExpr(c, recIdx, RegisterCstr.simple);
    args[1] = try genExpr(c, data.left, RegisterCstr.simple);
    args[2] = try genExpr(c, data.right, RegisterCstr.simple);

    try pushInlineTernExpr(c, .sliceList, args[0].local, args[1].local, args[2].local, inst.dst, nodeId);

    const retained = unwindTemps(c, &args);

    // ARC cleanup.
    try pushReleaseVals(c, retained, nodeId);

    const val = genValue(c, inst.dst, true);
    return finishInst(c, val, inst.finalDst);
}

fn genUnOp(c: *Chunk, idx: usize, cstr: RegisterCstr, nodeId: cy.NodeId) !GenValue {
    const data = c.irGetExprData(idx, .preUnOp).unOp;
    const childIdx = c.irAdvanceExpr(idx, .preUnOp);
    const inst = try c.rega.selectForDstInst(cstr, false);

    const canUseDst = !c.isParamOrLocalVar(inst.dst);
    const childv = try genExpr(c, childIdx, RegisterCstr.preferIf(inst.dst, canUseDst));

    switch (data.op) {
        .not => {
            try c.buf.pushOp2(.not, childv.local, inst.dst);
        },
        .minus,
        .bitwiseNot => {
            if (data.childT == bt.Integer) {
                try pushInlineUnExpr(c, getIntUnaryOpCode(data.op), childv.local, inst.dst, nodeId);
            } else if (data.childT == bt.Float) {
                try pushInlineUnExpr(c, getFloatUnaryOpCode(data.op), childv.local, inst.dst, nodeId);
            } else return error.Unexpected;
            // Builtin unary expr do not have retained child.
        },
        else => {
            return c.reportErrorAt("Unsupported op: {}", &.{v(data.op)}, nodeId);
        }
    }

    if (unwindTempKeepDst(c, childv, inst.dst)) {
        try pushRelease(c, childv.local, nodeId);
    }

    const val = genValue(c, inst.dst, false);
    return finishInst(c, val, inst.finalDst);
}

fn genCallObjSym(c: *Chunk, idx: usize, cstr: RegisterCstr, nodeId: cy.NodeId) !GenValue {
    const data = c.irGetExprData(idx, .preCallObjSym).callObjSym;
    const inst = try beginCall(c, cstr, false);

    // Receiver.
    const recIdx = c.irAdvanceExpr(idx, .preCallObjSym);
    const argStart = c.rega.nextTemp;
    var temp = try c.rega.consumeNextTemp();
    const recv = try genExpr(c, recIdx, RegisterCstr.exact(temp));
    try c.genValueStack.append(c.alloc, recv);

    const args = c.irGetArray(data.args, u32, data.numArgs);
    for (args, 0..) |argIdx, i| {
        temp = try c.rega.consumeNextTemp();
        if (cy.Trace and temp != argStart + 1 + i) return error.Unexpected;
        try genAndPushExpr(c, argIdx, RegisterCstr.exact(temp));
    }

    const methodSymId = try c.compiler.vm.ensureMethodGroup(data.name);
    try pushCallObjSym(c, inst.ret, data.numArgs + 1,
        @intCast(methodSymId), @intCast(data.funcSigId), nodeId);

    const argvs = popValues(c, data.numArgs+1);
    try checkArgs(argStart, argvs);
    const retained = unwindTemps(c, argvs);
    try pushReleaseVals(c, retained, nodeId);

    return endCall(c, inst, true);
}

fn genCallFuncSym(c: *Chunk, idx: usize, cstr: RegisterCstr, nodeId: cy.NodeId) !GenValue {
    const data = c.irGetExprData(idx, .preCallFuncSym).callFuncSym;
    const inst = try beginCall(c, cstr, false);

    const argsIdx = c.irAdvanceExpr(idx, .preCallFuncSym);
    const args = c.irGetArray(argsIdx, u32, data.numArgs);

    const argStart = c.rega.nextTemp;
    for (args, 0..) |argIdx, i| {
        const temp = try c.rega.consumeNextTemp();
        if (cy.Trace and temp != argStart + i) return error.Unexpected;
        try genAndPushExpr(c, argIdx, RegisterCstr.exact(temp));
    }

    if (data.hasDynamicArg) {
        try genCallTypeCheck(c, inst.ret + cy.vm.CallArgStart, data.numArgs, data.func.funcSigId, nodeId);
    }

    const rtId = c.compiler.genSymMap.get(data.func).?.funcSym.id;
    try pushCallSym(c, inst.ret, data.numArgs, 1, rtId, nodeId);

    const argvs = popValues(c, data.numArgs);
    try checkArgs(argStart, argvs);

    const retained = unwindTemps(c, argvs);
    try pushReleaseVals(c, retained, nodeId);

    const retRetained = c.sema.isRcCandidateType(data.func.retType);
    return endCall(c, inst, retRetained);
}

fn genCall(c: *Chunk, idx: usize, cstr: RegisterCstr, nodeId: cy.NodeId) !GenValue {
    const data = c.irGetExprData(idx, .preCall).call;
    const inst = try beginCall(c, cstr, true);

    // Callee.
    const calleeIdx = c.irAdvanceExpr(idx, .preCall);
    const argStart = c.rega.nextTemp;
    var temp = try c.rega.consumeNextTemp();
    const calleev = try genExpr(c, calleeIdx, RegisterCstr.exact(temp));
    try c.genValueStack.append(c.alloc, calleev);

    // Args.
    const args = c.irGetArray(data.args, u32, data.numArgs);
    for (args, 0..) |argIdx, i| {
        temp = try c.rega.consumeNextTemp();
        if (cy.Trace and temp != argStart + 1 + i) return error.Unexpected;
        try genAndPushExpr(c, argIdx, RegisterCstr.exact(temp));
    }

    const calleeAndArgvs = popValues(c, data.numArgs+1);
    try checkArgs(argStart, calleeAndArgvs);

    try c.pushFailableDebugSym(nodeId);
    try c.buf.pushOp3Ext(.call, inst.ret, data.numArgs, 1, c.desc(nodeId));

    const retained = unwindTemps(c, calleeAndArgvs);
    try pushReleaseVals(c, retained, nodeId);

    return endCall(c, inst, true);
}

const CopyInstSave = struct {
    src: RegisterId,
    dst: RegisterId,
    desc: if (cy.Trace) cy.bytecode.InstDesc else void, 
};

fn extractIfCopyInst(c: *Chunk, leftPc: usize) ?CopyInstSave {
    if (c.buf.ops.items.len - 3 == leftPc) {
        if (c.buf.ops.items[leftPc].opcode() == .copy) {
            var save = CopyInstSave{
                .src = c.buf.ops.items[leftPc+1].val,
                .dst = c.buf.ops.items[leftPc+2].val,
                .desc = undefined,
            };
            if (cy.Trace) {
                save.desc = c.buf.instDescs.items[c.buf.instDescs.items.len-1];
                c.buf.instDescs.items.len -= 1;
            }
            c.buf.ops.items.len = leftPc;
            return save;
        }
    }
    return null;
}

fn genCallObjSymUnOp(c: *Chunk, idx: usize, cstr: RegisterCstr, nodeId: cy.NodeId) !GenValue {
    const data = c.irGetExprData(idx, .preCallObjSymUnOp).callObjSymUnOp;
    const inst = try beginCall(c, cstr, false);

    const childIdx = c.irAdvanceExpr(idx, .preCallObjSymUnOp);

    var temp = try c.rega.consumeNextTemp();
    const childv = try genExpr(c, childIdx, RegisterCstr.exact(temp));

    const mgId = try getUnMGID(c, data.op);

    try pushCallObjSym(c, inst.ret, 1, @intCast(mgId), @intCast(data.funcSigId), nodeId);

    if (unwindAndFreeTemp(c, childv)) {
        try pushRelease(c, childv.local, nodeId);
    }

    return endCall(c, inst, true);
}

fn genCallObjSymBinOp(c: *Chunk, idx: usize, cstr: RegisterCstr, nodeId: cy.NodeId) !GenValue {
    const data = c.irGetExprData(idx, .preCallObjSymBinOp).callObjSymBinOp;
    const inst = try beginCall(c, cstr, false);

    const leftPc = c.buf.ops.items.len;
    // copy inst doesn't create a debug symbol, otherwise that needs to be moved as well.
    const leftIdx = c.irAdvanceExpr(idx, .preCallObjSymBinOp);

    var temp = try c.rega.consumeNextTemp();
    const leftv = try genExpr(c, leftIdx, RegisterCstr.exact(temp));
    const leftCopySave = extractIfCopyInst(c, leftPc);
    const hasLeftCopy = leftCopySave != null;

    const rightPc = c.buf.ops.items.len;
    temp = try c.rega.consumeNextTemp();
    const rightv = try genExpr(c, data.right, RegisterCstr.exact(temp));

    var hasRightCopy = false;
    if (c.buf.ops.items.len - 3 == rightPc) {
        if (c.buf.ops.items[rightPc].opcode() == .copy) {
            hasRightCopy = true;
        }
    }

    if (leftCopySave) |save| {
        var desc = cy.bytecode.InstDesc{};
        if (cy.Trace) desc = save.desc;
        try c.buf.pushOp2Ext(.copy, save.src, save.dst, desc);
    }

    const mgId = try getInfixMGID(c, data.op);

    const start = c.buf.len();
    try pushCallObjSym(c, inst.ret, 2, @intCast(mgId), @intCast(data.funcSigId), nodeId);
    // Provide hint to inlining that one or both args were copies.
    if (hasLeftCopy or hasRightCopy) {
        if (hasLeftCopy and hasRightCopy) {
            c.buf.setOpArgs1(start + 7, 2);
        } else {
            c.buf.setOpArgs1(start + 7, 1);
        }
    }

    const leftRetained = unwindAndFreeTemp(c, leftv);
    const rightRetained = unwindAndFreeTemp(c, rightv);
    try pushReleaseOpt2(c, leftRetained, leftv.local, rightRetained, rightv.local, nodeId);

    return endCall(c, inst, true);
}

fn getUnMGID(c: *Chunk, op: cy.UnaryOp) !vmc.MethodGroupId {
    return switch (op) {
        .minus => c.compiler.@"prefix-MGID",
        .bitwiseNot => c.compiler.@"prefix~MGID",
        else => return error.Unexpected,
    };
}

fn getInfixMGID(c: *Chunk, op: cy.BinaryExprOp) !vmc.MethodGroupId {
    return switch (op) {
        .index => c.compiler.indexMGID,
        .less => c.compiler.@"infix<MGID",
        .greater => c.compiler.@"infix>MGID",
        .less_equal => c.compiler.@"infix<=MGID",
        .greater_equal => c.compiler.@"infix>=MGID",
        .minus => c.compiler.@"infix-MGID",
        .plus => c.compiler.@"infix+MGID",
        .star => c.compiler.@"infix*MGID",
        .slash => c.compiler.@"infix/MGID",
        .percent => c.compiler.@"infix%MGID",
        .caret => c.compiler.@"infix^MGID",
        .bitwiseAnd => c.compiler.@"infix&MGID",
        .bitwiseOr => c.compiler.@"infix|MGID",
        .bitwiseXor => c.compiler.@"infix||MGID",
        .bitwiseLeftShift => c.compiler.@"infix<<MGID",
        .bitwiseRightShift => c.compiler.@"infix>>MGID",
        else => return error.Unexpected,
    };
}

const BinOpOptions = struct {
    left: ?GenValue = null,
};

fn genBinOp(c: *Chunk, idx: usize, cstr: RegisterCstr, opts: BinOpOptions, nodeId: cy.NodeId) !GenValue {
    const data = c.irGetExprData(idx, .preBinOp).binOp;
    log.tracev("binop {} {}", .{data.op, data.leftT});

    if (data.op == .and_op) {
        return genAndOp(c, idx, data, cstr, nodeId);
    } else if (data.op == .or_op) {
        return genOr(c, idx, data, cstr, nodeId);
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

    var prefer = PreferDst{
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

    var retained = false;
    switch (data.op) {
        .index => {
            if (data.leftT == bt.List) {
                try pushInlineBinExpr(c, .indexList, leftv.local, rightv.local, inst.dst, nodeId);
            } else if (data.leftT == bt.Tuple) {
                try pushInlineBinExpr(c, .indexTuple, leftv.local, rightv.local, inst.dst, nodeId);
            } else if (data.leftT == bt.Map) {
                try pushInlineBinExpr(c, .indexMap, leftv.local, rightv.local, inst.dst, nodeId);
            } else return error.Unexpected;
            retained = true;
        },
        .bitwiseAnd,
        .bitwiseOr,
        .bitwiseXor,
        .bitwiseLeftShift,
        .bitwiseRightShift => {
            if (data.leftT == bt.Integer) {
                try pushInlineBinExpr(c, getIntOpCode(data.op), leftv.local, rightv.local, inst.dst, nodeId);
            } else return error.Unexpected;
        },
        .greater,
        .greater_equal,
        .less,
        .less_equal,
        .star,
        .slash,
        .percent,
        .caret,
        .plus,
        .minus => {
            if (data.leftT == bt.Float) {
                try pushInlineBinExpr(c, getFloatOpCode(data.op), leftv.local, rightv.local, inst.dst, nodeId);
            } else if (data.leftT == bt.Integer) {
                try pushInlineBinExpr(c, getIntOpCode(data.op), leftv.local, rightv.local, inst.dst, nodeId);
            } else return error.Unexpected;
        },
        .equal_equal => {
            try c.pushOptionalDebugSym(nodeId);
            try c.buf.pushOp3Ext(.compare, leftv.local, rightv.local, inst.dst, c.desc(nodeId));
        },
        .bang_equal => {
            try c.pushOptionalDebugSym(nodeId);
            try c.buf.pushOp3Ext(.compareNot, leftv.local, rightv.local, inst.dst, c.desc(nodeId));
        },
        else => {
            return c.reportErrorAt("Unsupported op: {}", &.{v(data.op)}, nodeId);
        },
    }

    const leftRetained = if (opts.left == null) unwindTempKeepDst(c, leftv, inst.dst) else false;
    const rightRetained = unwindTempKeepDst(c, rightv, inst.dst);

    // ARC cleanup.
    try pushReleaseOpt2(c, leftRetained, leftv.local, rightRetained, rightv.local, nodeId);

    const val = genValue(c, inst.dst, retained);
    return finishInst(c, val, inst.finalDst);
}

fn genCaptured(c: *Chunk, idx: usize, cstr: RegisterCstr, nodeId: cy.NodeId) !GenValue {
    const data = c.irGetExprData(idx, .captured);

    const inst = try c.rega.selectForNoErrInst(cstr, true);
    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, nodeId);
    }
    try c.buf.pushOp3(.captured, c.curBlock.closureLocal, data.idx, inst.dst);

    const val = genValue(c, inst.dst, true);
    return finishInst(c, val, inst.finalDst);
}

pub fn getLocalInfo(c: *Chunk, reg: u8) Local {
    return c.genLocalStack.items[c.curBlock.localStart + reg];
}

pub fn getLocalInfoPtr(c: *Chunk, reg: u8) *Local {
    return &c.genLocalStack.items[c.curBlock.localStart + reg];
}

pub fn toLocalReg(c: *Chunk, irVarId: u8) RegisterId {
    return c.genIrLocalMapStack.items[c.curBlock.irLocalMapStart + irVarId];
}

fn genLocalReg(c: *Chunk, reg: RegisterId, cstr: RegisterCstr, nodeId: cy.NodeId) !GenValue {
    const local = getLocalInfo(c, reg);

    if (!local.some.boxed) {
        const inst = try c.rega.selectForLocalInst(cstr, reg, local.some.rcCandidate);
        if (inst.dst != reg) {
            if (inst.retainSrc) {
                if (inst.releaseDst) {
                    try c.buf.pushOp2Ext(.copyRetainRelease, reg, inst.dst, c.desc(nodeId));
                } else {
                    try c.buf.pushOp2Ext(.copyRetainSrc, reg, inst.dst, c.desc(nodeId));
                }
            } else {
                if (inst.releaseDst) {
                    try c.buf.pushOp2Ext(.copyReleaseDst, reg, inst.dst, c.desc(nodeId));
                } else {
                    try c.buf.pushOp2Ext(.copy, reg, inst.dst, c.desc(nodeId));
                }
            }
        } else {
            // Nop. When the cstr allows returning the local itself.
            if (inst.retainSrc) {
                try c.buf.pushOp1Ext(.retain, reg, c.desc(nodeId));
            } else {
                // Nop.
            }
        }
        const val = genValue(c, inst.dst, inst.retainSrc);
        return finishInst(c, val, inst.finalDst);
    } else {
        // Special case when src local is boxed.
        const retainSrc = local.some.rcCandidate and (cstr.mustRetain or cstr.type == .local or cstr.type == .boxedLocal);
        const inst = try c.rega.selectForDstInst(cstr, retainSrc);

        if (retainSrc) {
            try c.buf.pushOp2Ext(.boxValueRetain, reg, inst.dst, c.desc(nodeId));
        } else {
            try c.buf.pushOp2Ext(.boxValue, reg, inst.dst, c.desc(nodeId));
        }

        const val = genValue(c, inst.dst, retainSrc);
        return finishInst(c, val, inst.finalDst);
    }
}

fn genLocal(c: *Chunk, idx: usize, cstr: RegisterCstr, nodeId: cy.NodeId) !GenValue {
    const data = c.irGetExprData(idx, .local);
    const reg = toLocalReg(c, data.id);
    return genLocalReg(c, reg, cstr, nodeId);
}

fn genEnumMemberSym(c: *Chunk, idx: usize, cstr: RegisterCstr, nodeId: cy.NodeId) !GenValue {
    const data = c.irGetExprData(idx, .enumMemberSym);

    const inst = try c.rega.selectForNoErrInst(cstr, false);
    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, nodeId);
    }

    const val = cy.Value.initEnum(@intCast(data.type), @intCast(data.val));
    const constIdx = try c.buf.getOrPushConst(val);
    try genConst(c, constIdx, inst.dst, false, nodeId);

    const resv = genValue(c, inst.dst, false);
    return finishInst(c, resv, inst.finalDst);
}

fn genVarSym(c: *Chunk, idx: usize, cstr: RegisterCstr, nodeId: cy.NodeId) !GenValue {
    const data = c.irGetExprData(idx, .varSym);

    const varId = c.compiler.genSymMap.get(data.sym).?.varSym.id;

    const inst = try c.rega.selectForDstInst(cstr, true);
    try c.pushOptionalDebugSym(nodeId);       
    const pc = c.buf.len();
    try c.buf.pushOp3(.staticVar, 0, 0, inst.dst);
    c.buf.setOpArgU16(pc + 1, @intCast(varId));

    const value = genValue(c, inst.dst, true);
    return finishInst(c, value, inst.finalDst);
}

fn genFuncSym(c: *Chunk, idx: usize, cstr: RegisterCstr, nodeId: cy.NodeId) !GenValue {
    const data = c.irGetExprData(idx, .funcSym);

    const inst = try c.rega.selectForDstInst(cstr, true);

    const pc = c.buf.len();
    try c.pushOptionalDebugSym(nodeId);
    try c.buf.pushOp3(.staticFunc, 0, 0, inst.dst);
    const rtId = c.compiler.genSymMap.get(data.func).?.funcSym.id;
    c.buf.setOpArgU16(pc + 1, @intCast(rtId));

    const value = genValue(c, inst.dst, true);
    return finishInst(c, value, inst.finalDst);
}

fn genTypeSym(c: *Chunk, idx: usize, cstr: RegisterCstr, nodeId: cy.NodeId) !GenValue {
    _ = nodeId;
    const data = c.irGetExprData(idx, .typeSym);

    const inst = try c.rega.selectForDstInst(cstr, true);
    try c.buf.pushOp1(.sym, @intFromEnum(cy.heap.MetaTypeKind.object));
    try c.buf.pushOperandsRaw(std.mem.asBytes(&data.typeId));
    try c.buf.pushOperand(inst.dst);

    const value = genValue(c, inst.dst, true);
    return finishInst(c, value, inst.finalDst);
}

/// Reserve params and captured vars.
/// Function stack layout:
/// [startLocal/retLocal] [retInfo] [retAddress] [prevFramePtr] [callee] [params...] [var locals...] [temp locals...]
/// `callee` is reserved so that function values can call static functions with the same call convention.
/// For this reason, `callee` isn't freed in the function body and a separate release inst is required for lambda calls.
/// A closure can also occupy the callee and is used to do captured var lookup.
fn reserveFuncRegs(c: *Chunk, maxIrLocals: u8, numParamCopies: u8, params: []align(1) const ir.FuncParam) !void {
    const numParams = params.len;
    try c.genIrLocalMapStack.resize(c.alloc, c.curBlock.irLocalMapStart + maxIrLocals);

    const maxLocalRegs = 4 + 1 + numParams + numParamCopies + (maxIrLocals - numParams);
    log.tracev("reserveFuncRegs {} {}", .{c.curBlock.localStart, maxLocalRegs});
    try initBlockLocals(c, @intCast(maxLocalRegs));

    // First local is reserved for a single return value.
    // Second local is reserved for the return info.
    // Third local is reserved for the return address.
    // Fourth local is reserved for the previous frame pointer.
    var nextReg: u8 = 4;

    // An extra callee slot is reserved so that function values
    // can call static functions with the same call convention.
    // It's also used to store the closure object.
    c.curBlock.closureLocal = nextReg;
    nextReg += 1;

    // Reserve func params.
    var paramCopyIdx: u8 = 0;
    for (params, 0..) |param, i| {
        if (param.isCopy) {
            // Forward reserve the param copy.
            const reg: RegisterId = @intCast(4 + 1 + numParams + paramCopyIdx);
            c.genIrLocalMapStack.items[c.curBlock.irLocalMapStart + i] = reg;

            c.genLocalStack.items[c.curBlock.localStart + 4 + 1 + numParams + paramCopyIdx] = .{
                .some = .{
                    .owned = true,
                    .rcCandidate = c.sema.isRcCandidateType(param.declType),
                    .boxed = param.isBoxed,
                },
            };

            // Copy param to local.
            if (param.isBoxed) {
                try c.pushFailableDebugSym(c.curBlock.debugNodeId);
                // Retain param and box.
                try c.buf.pushOp1(.retain, nextReg);
                try c.buf.pushOp2(.box, nextReg, reg);
            } else {
                try c.buf.pushOp2(.copyRetainSrc, nextReg, reg);
            }

            paramCopyIdx += 1;
        } else {
            c.genIrLocalMapStack.items[c.curBlock.irLocalMapStart + i] = nextReg;
            c.genLocalStack.items[c.curBlock.localStart + 4 + 1 + i] = .{
                .some = .{
                    .owned = false,
                    .rcCandidate = c.sema.isRcCandidateType(param.declType),
                    .boxed = false,
                },
            };
        }
        log.tracev("reserve param: {}", .{i});
        nextReg += 1;
    }

    c.curBlock.startLocalReg = nextReg;
    nextReg += numParamCopies;
    c.curBlock.nextLocalReg = nextReg;

    // Reset temp register state.
    const tempRegStart = nextReg + (maxIrLocals - numParams);
    c.rega.resetState(@intCast(tempRegStart));
}

fn setFuncSym(c: *Chunk, idx: usize, nodeId: cy.NodeId) !void {
    const setData = c.irGetStmtData(idx, .setFuncSym).generic;
    const funcSymIdx = c.irAdvanceStmt(idx, .setFuncSym);
    const data = c.irGetExprData(funcSymIdx, .funcSym);

    const rightv = try genExpr(c, setData.right, RegisterCstr.tempMustRetain);

    const pc = c.buf.len();
    try c.pushFailableDebugSym(nodeId);
    try c.buf.pushOp3(.setStaticFunc, 0, 0, rightv.local);
    const rtId = c.compiler.genSymMap.get(data.func).?.funcSym.id;
    c.buf.setOpArgU16(pc + 1, @intCast(rtId));

    _ = unwindAndFreeTemp(c, rightv);
}

fn setVarSym(c: *Chunk, idx: usize, nodeId: cy.NodeId) !void {
    _ = nodeId;
    const data = c.irGetStmtData(idx, .setVarSym).generic;
    const varSymIdx = c.irAdvanceStmt(idx, .setVarSym);
    const varSym = c.irGetExprData(varSymIdx, .varSym);

    const id = c.compiler.genSymMap.get(varSym.sym).?.varSym.id;
    const rightv = try genExpr(c, data.right, RegisterCstr.toVarSym(id));
    if (rightv.isRetainedTemp()) _ = c.popRetainedTemp();
}

fn declareLocal(c: *Chunk, idx: u32, nodeId: cy.NodeId) !void {
    const data = c.irGetStmtData(idx, .declareLocal);
    if (data.assign) {
        // Don't advance nextLocalReg yet since the rhs hasn't generated so the
        // alive locals should not include this declaration.
        const reg = try reserveLocalReg(c, data.id, data.declType, data.isBoxed, nodeId, false);

        const exprIdx = c.irAdvanceStmt(idx, .declareLocal);
        const val = try genExpr(c, exprIdx, RegisterCstr.toLocal(reg, false));

        const local = getLocalInfoPtr(c, reg);

        if (local.some.boxed) {
            try c.pushOptionalDebugSym(nodeId);
            try c.buf.pushOp2(.box, reg, reg);
        }
        local.some.rcCandidate = val.retained;

        // rhs has generated, increase `nextLocalReg`.
        c.curBlock.nextLocalReg += 1;
        log.tracev("declare {}, rced: {} ", .{val.local, local.some.rcCandidate});
    } else {
        const reg = try reserveLocalReg(c, data.id, data.declType, data.isBoxed, nodeId, true);

        // Not yet initialized, so it does not have a refcount.
        getLocalInfoPtr(c, reg).some.rcCandidate = false;
    }
}

fn setCaptured(c: *Chunk, idx: usize, nodeId: cy.NodeId) !void {
    _ = nodeId;
    const data = c.irGetStmtData(idx, .setCaptured).generic;
    const capIdx = c.irAdvanceStmt(idx, .setCaptured);
    const capData = c.irGetExprData(capIdx, .captured);

    // RHS.
    const dstRetained = c.sema.isRcCandidateType(data.leftT.id);
    _ = try genExpr(c, data.right, RegisterCstr.toCaptured(capData.idx, dstRetained));
}

fn irSetLocal(c: *Chunk, idx: usize, nodeId: cy.NodeId) !void {
    const data = c.irGetStmtData(idx, .setLocal).generic;
    const localIdx = c.irAdvanceStmt(idx, .setLocal);
    const localData = c.irGetExprData(localIdx, .local);
    try setLocal(c, localData, data.right, nodeId, .{});
}

const SetLocalOptions = struct {
    rightv: ?GenValue = null,
    extraIdx: u32 = cy.NullId,
};

fn setLocal(c: *Chunk, data: ir.Local, rightIdx: u32, nodeId: cy.NodeId, opts: SetLocalOptions) !void {
    const reg = toLocalReg(c, data.id);
    const local = getLocalInfo(c, reg);

    var dst: RegisterCstr = undefined;
    if (local.some.boxed) {
        dst = RegisterCstr.toBoxedLocal(reg, local.some.rcCandidate);
    } else {
        dst = RegisterCstr.toLocal(reg, local.some.rcCandidate);
    }

    var rightv: GenValue = undefined;
    if (opts.rightv) |rightv_| {
        rightv = rightv_;
        if (rightv.local != reg) {
            // Move to local.
            const desc = c.descExtra(nodeId, opts.extraIdx);
            _ = try genToDst(c, rightv, dst, desc);
        }
    } else {
        rightv = try genExpr(c, rightIdx, dst);
    }

    // Update retained state.
    getLocalInfoPtr(c, reg).some.rcCandidate = rightv.retained;
}

fn setLocalType(c: *Chunk, idx: usize) !void {
    const data = c.irGetStmtData(idx, .setLocalType);
    const reg = toLocalReg(c, data.local);
    const local = getLocalInfoPtr(c, reg);
    local.some.rcCandidate = c.sema.isRcCandidateType(data.type.id);
}

fn opSet(c: *Chunk, idx: usize, nodeId: cy.NodeId) !void {
    _ = nodeId;
    // TODO: Perform optimizations depending on the next set* code.
    const setIdx = c.irAdvanceStmt(idx, .opSet);
    try genStmt(c, @intCast(setIdx));
}

// fn opSetField(c: *Chunk, data: ir.OpSet, nodeId: cy.NodeId) !void {
//     try pushIrData(c, .{ .opSet = data });
//     try pushBasic(c, nodeId);

//     // Recv
//     try pushCustom(c, .opSetFieldLeftEnd);
//     try pushCstr(c, Cstr.initBc(RegisterCstr.simple));
// }

// fn opSetFieldLeftEnd(c: *Chunk) !void {
//     const top = getTop(c);
//     const info = getLastTaskInfo(c).basic;
//     const data = getLastIrData(c).opSet;

//     const recv = c.genValueStack.getLast();
//     const dst = try c.rega.consumeNextTemp();

//     top.type = .opSetFieldRightEnd;
//     try pushCstr(c, Cstr.initBc(RegisterCstr.exact(dst)));
//     const funcSigId = try sema.ensureFuncSig(c.compiler, &.{ bt.Any, bt.Any }, bt.Any);
//     try preCallGenericBinOp(c, .{ .op = data.op, .funcSigId = funcSigId }, info.nodeId);
//     try fieldDynamic(c, .{ .name = data.set.field.name }, .{ .recv = recv }, info.nodeId);
// }

// fn opSetFieldRightEnd(c: *Chunk) !void {
//     const data = popIrData(c).opSet;
//     const info = getLastTaskInfo(c).basic;

//     getTop(c).type = .opSetFieldEnd;
//     const val = popValue(c);
//     const recv = popValue(c);
//     try setField(c, data.set.field, .{ .recv = recv, .right = val }, info.nodeId);
// }

// fn opSetFieldEnd(c: *Chunk) !void {
//     const info = popTaskInfo(c).basic;

//     const retainedTemps = c.popUnwindTempsFrom(info.retainedStart);
//     try pushReleases(c, retainedTemps, info.nodeId);

//     c.rega.setNextTemp(info.tempStart);
//     removeTop(c);
// }

// fn opSetObjectField(c: *Chunk, data: ir.OpSet, nodeId: cy.NodeId) !void {
//     const opStrat = getOpStrat(data.op, data.leftT) orelse {
//         return c.reportErrorAt("Unsupported op: {}", &.{v(data.op)}, nodeId);
//     };

//     try pushIrData(c, .{ .opSet = data });
//     try pushBasic(c, nodeId);
//     try pushDataU8(c, .{ .opStrat = opStrat });

//     // Recv
//     try pushCustom(c, .opSetObjectFieldLeftEnd);
//     try pushCstr(c, Cstr.initBc(RegisterCstr.simple));
// }

// fn opSetObjectFieldLeftEnd(c: *Chunk) !void {
//     const top = getTop(c);
//     const info = getLastTaskInfo(c).basic;
//     const data = getLastIrData(c).opSet;
//     const opStrat = popDataU8(c).opStrat;

//     const recv = c.genValueStack.getLast();
//     const dst = try c.rega.consumeNextTemp();

//     const fieldIdx = data.set.objectField.idx;
//     switch (opStrat) {
//         .inlineOp => {
//             top.type = .opSetObjectFieldRightEnd;
//             try pushCstr(c, Cstr.initBc(RegisterCstr.exact(dst)));
//             const binLeftCstr = Task.init(Cstr.initBc(RegisterCstr.exact(dst)));
//             try binOp(c, .{ .op = data.op, .leftT = data.leftT }, .{ .left = binLeftCstr }, info.nodeId);
//             try fieldStatic(c, .{ .typeId = data.leftT, .idx = fieldIdx }, .{ .recv = recv }, info.nodeId);
//         },
//         .callObjSym => {
//             top.type = .opSetObjectFieldRightEnd;
//             try pushCstr(c, Cstr.initBc(RegisterCstr.exact(dst)));
//             const funcSigId = try sema.ensureFuncSig(c.compiler, &.{ bt.Any, bt.Any }, bt.Any);
//             try preCallGenericBinOp(c, .{ .op = data.op, .funcSigId = funcSigId }, info.nodeId);
//             try fieldStatic(c, .{ .typeId = data.leftT, .idx = fieldIdx }, .{ .recv = recv }, info.nodeId);
//         },
//     }
// }

// fn opSetObjectFieldRightEnd(c: *Chunk) !void {
//     const data = popIrData(c).opSet;
//     const info = getLastTaskInfo(c).basic;

//     getTop(c).type = .opSetObjectFieldEnd;
//     const val = popValue(c);
//     const recv = popValue(c);
//     try setObjectField(c, data.set.objectField, .{ .recv = recv, .right = val }, info.nodeId);
// }

// fn opSetObjectFieldEnd(c: *Chunk) !void {
//     const info = popTaskInfo(c).basic;

//     const retainedTemps = c.popUnwindTempsFrom(info.retainedStart);
//     try pushReleases(c, retainedTemps, info.nodeId);

//     c.rega.setNextTemp(info.tempStart);
//     removeTop(c);
// }

// fn opSetLocal(c: *Chunk, data: ir.OpSet, nodeId: cy.NodeId) !void {
//     const opStrat = getOpStrat(data.op, data.leftT) orelse {
//         return c.reportErrorAt("Unsupported op: {}", &.{v(data.op)}, nodeId);
//     };

//     const dst = toLocalReg(c, data.set.local.id);
//     try pushCustom(c, .popValue);
//     try pushCstr(c, Cstr.initBc(RegisterCstr.exact(dst)));
//     switch (opStrat) {
//         .inlineOp => {
//             const local = Task.initRes(genValue(c, dst, false));
//             try genBinOp(c, .{ .op = data.op, .leftT = data.leftT }, .{ .left = local }, nodeId);
//         },
//         .callObjSym => {
//             const funcSigId = try sema.ensureFuncSig(c.compiler, &.{ bt.Any, bt.Any }, bt.Any);
//             try genCallObjSymBinOp(c, .{ .op = data.op, .funcSigId = funcSigId }, nodeId);
//             try genLocal(c, .{ .id = data.set.local.id }, nodeId);
//         },
//     }
// }

// pub const OpStrat = enum {
//     inlineOp, 
//     callObjSym,
// };

// fn getOpStrat(op: cy.BinaryExprOp, leftT: TypeId) ?OpStrat {
//     switch (op) {
//         .star,
//         .slash,
//         .percent,
//         .caret,
//         .plus,
//         .minus => {
//             if (leftT == bt.Float or leftT == bt.Integer) {
//                 return .inlineOp;
//             } else {
//                 return .callObjSym;
//             }
//         },
//         else => {
//             return null;
//         },
//     }
// }

fn genOr(c: *Chunk, idx: usize, data: ir.BinOp, cstr: RegisterCstr, nodeId: cy.NodeId) !GenValue {
    _ = nodeId;
    const useCondAsDst = cstr.type == .simple;

    const temp = try c.rega.consumeNextTemp();
    // Always retain to make sure both branches return the same GenValue.
    // Without this, the parent could produce a release inst for a branch that didn't retain.
    const condCstr = RegisterCstr.exactMustRetain(temp);

    const leftIdx = c.irAdvanceExpr(idx, .preBinOp);
    const leftv = try genExpr(c, leftIdx, condCstr);
    var resv = leftv;

    const leftFalseJump = try c.pushEmptyJumpNotCond(leftv.local);
    if (leftv.isRetainedTemp()) {
        _ = c.popRetainedTemp();
    }

    // Gen left to finalCstr.
    if (!useCondAsDst) {
        resv = try genToFinalDst(c, leftv, cstr);
    }
    const leftEndJump = try c.pushEmptyJump();

    // RHS, gen to final dst.
    c.patchJumpNotCondToCurPc(leftFalseJump);
    var finalCstr = cstr;
    if (useCondAsDst) {
        finalCstr = condCstr;
    }
    const rightv = try genExpr(c, data.right, finalCstr);
    if (rightv.isRetainedTemp()) {
        _ = c.popRetainedTemp();
    }
    c.patchJumpToCurPc(leftEndJump);

    if (rightv.retained) resv.retained = true;

    if (useCondAsDst or cstr.type == .exact) {
        try pushOptUnwindableTemp(c, resv);
    }
    return resv;
}

fn genAndOp(c: *Chunk, idx: usize, data: ir.BinOp, cstr: RegisterCstr, nodeId: cy.NodeId) !GenValue {
    _ = nodeId;
    const useCondAsDst = cstr.type == .simple;
    const temp = try c.rega.consumeNextTemp();
    const condCstr = RegisterCstr.exactMustRetain(temp);

    const leftIdx = c.irAdvanceExpr(idx, .preBinOp);
    const leftv = try genExpr(c, leftIdx, condCstr);
    const leftTrueJump = try c.pushEmptyJumpCond(leftv.local);
    if (leftv.isRetainedTemp()) {
        _ = c.popRetainedTemp();
    }

    // Gen left to finalCstr.
    if (!useCondAsDst) {
        _ = try genToFinalDst(c, leftv, cstr);
    }
    const leftEndJump = try c.pushEmptyJump();

    // RHS.
    c.patchJumpCondToCurPc(leftTrueJump);
    var finalCstr = cstr;
    if (useCondAsDst) {
        finalCstr = condCstr;
    }
    const rightv = try genExpr(c, data.right, finalCstr);
    if (rightv.isRetainedTemp()) {
        _ = c.popRetainedTemp();
    }
    
    c.patchJumpToCurPc(leftEndJump);

    if (useCondAsDst or cstr.type == .exact) {
        try pushOptUnwindableTemp(c, rightv);
    }
    return rightv;
}

fn retStmt(c: *Chunk) !void {
    if (c.curBlock.type == .main) {
        try genBlockReleaseLocals(c);
        try c.buf.pushOp1(.end, 255);
    } else {
        try genBlockReleaseLocals(c);
        try c.buf.pushOp(.ret0);
    }
}

// TODO: Make ret inst take reg to avoid extra copy inst.
fn retExprStmt(c: *Chunk, idx: usize, nodeId: cy.NodeId) !void {
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

    _ = unwindAndFreeTemp(c, childv);

    try genBlockReleaseLocals(c);
    if (c.curBlock.type == .main) {
        try c.buf.pushOp1(.end, @intCast(childv.local));
    } else {
        try c.buf.pushOp(.ret1);
    }
}

pub const Local = union {
    some: struct {
        /// If `boxed` is true, this refers to the child value.
        /// This is updated by assignments and explicit updateLocalType.
        rcCandidate: bool,

        /// Whether the local is owned by the block. eg. Read-only func params would not be owned.
        owned: bool,

        boxed: bool,
    },
    uninited: void,
};

fn getAliveLocals(c: *Chunk, startLocalReg: u8) []const Local {
    log.tracev("localStart {} {}", .{c.curBlock.localStart, c.curBlock.nextLocalReg});
    const start = c.curBlock.localStart + startLocalReg;
    const end = c.curBlock.localStart + c.curBlock.nextLocalReg;
    return c.genLocalStack.items[start..end];
}

fn genBlockReleaseLocals(c: *Chunk) !void {
    const block = c.curBlock;
    try genReleaseLocals(c, block.startLocalReg, block.debugNodeId);
}

/// Only the locals that are alive at this moment are considered for release.
fn genReleaseLocals(c: *Chunk, startLocalReg: u8, debugNodeId: cy.NodeId) !void {
    const start = c.operandStack.items.len;
    defer c.operandStack.items.len = start;

    const locals = getAliveLocals(c, startLocalReg);
    log.tracev("Generate release locals: start={}, count={}", .{startLocalReg, locals.len});
    for (locals, 0..) |local, i| {
        if (local.some.owned) {
            if (local.some.rcCandidate or local.some.boxed) {
                try c.operandStack.append(c.alloc, @intCast(startLocalReg + i));
            }
        }
    }
    
    const regs = c.operandStack.items[start..];
    if (regs.len > 0) {
        try pushReleases(c, regs, debugNodeId);
    }
}

fn genFuncEnd(c: *Chunk) !void {
    try genBlockReleaseLocals(c);
    if (c.curBlock.requiresEndingRet1) {
        try c.buf.pushOp(.ret1);
    } else {
        try c.buf.pushOp(.ret0);
    }
}

fn initBlockLocals(c: *Chunk, numLocalRegs: u8) !void {
    try c.genLocalStack.resize(c.alloc, c.curBlock.localStart + numLocalRegs);
    if (cy.Trace) {
        // Fill with uninited tag.
        @memset(c.genLocalStack.items[c.curBlock.localStart..c.curBlock.localStart + numLocalRegs], .{ .uninited = {}});
    }
}

fn reserveFiberCallRegs(c: *Chunk, numArgs: u8) !void {
    var nextReg: u8 = 0;

    if (numArgs > 0) {
        // Advance 1 + prelude + numArgs.
        nextReg += 1 + 5 + numArgs;
    }

    c.curBlock.startLocalReg = nextReg;
    c.curBlock.nextLocalReg = nextReg;

    // Reset temp register state.
    var tempRegStart = nextReg;

    // Temp start must be at least 1 since 0 is used to check for main stack.
    if (tempRegStart == 0) {
        tempRegStart = 1;
    }
    c.rega.resetState(tempRegStart);

    try c.genIrLocalMapStack.resize(c.alloc, c.curBlock.irLocalMapStart);
    try initBlockLocals(c, c.rega.nextTemp);
}

pub fn reserveMainRegs(c: *Chunk, maxLocals: u8) !void {
    var nextReg: u8 = 0;

    // Reserve the first slot for a JIT return addr.
    nextReg += 1;

    c.curBlock.startLocalReg = nextReg;
    c.curBlock.nextLocalReg = nextReg;

    // Reset temp register state.
    // Ensure tempStart is at least 1 so the runtime can easily check
    // if the framePtr is at main or a function. (since call rets are usually allocated on the temp stack)
    var tempRegStart = nextReg + maxLocals;
    if (tempRegStart == 0) {
        tempRegStart = 1;
    }
    c.rega.resetState(tempRegStart);

    try c.genIrLocalMapStack.resize(c.alloc, c.curBlock.irLocalMapStart + maxLocals);
    try initBlockLocals(c, c.rega.nextTemp);
}

pub const CallInst = struct {
    ret: RegisterId,
    numPreludeTemps: u8,
    finalDst: ?RegisterCstr,
};

/// Returns gen strategy and advances the temp local.
pub fn beginCall(c: *Chunk, cstr: RegisterCstr, hasCalleeValue: bool) !CallInst {
    var ret: RegisterId = undefined;
    var allocTempRet = true;

    // Optimization: Check to use dst cstr as ret.
    if (cstr.type == .exact) {
        if (cstr.data.exact + 1 == c.rega.nextTemp) {
            if (cstr.data.exact != 0) {
                ret = cstr.data.exact;
                allocTempRet = false;
            }
        }
    } else if (cstr.type == .local) {
        if (!cstr.data.local.retained and cstr.data.local.reg + 1 == c.rega.nextTemp) {
            if (cstr.data.local.reg != 0) {
                ret = cstr.data.local.reg;
                allocTempRet = false;
            }
        }
    }

    if (allocTempRet) {
        log.tracev("alloc ret temp", .{});
        ret = try c.rega.consumeNextTemp();
        // Assumes nextTemp is never 0.
        if (cy.Trace and ret == 0) return error.Unexpected;
    }
    const tempStart = c.rega.nextTemp;

    // Reserve registers for return value and return info.
    _ = try c.rega.consumeNextTemp();
    _ = try c.rega.consumeNextTemp();
    _ = try c.rega.consumeNextTemp();

    if (!hasCalleeValue) {
        // Reserve callee reg.
        _ = try c.rega.consumeNextTemp();
    }

    // Compute the number of preludes to be unwinded after the call inst.
    var numPreludeTemps = c.rega.nextTemp - tempStart;
    var finalDst: ?RegisterCstr = null;
    switch (cstr.type) {
        .local => {
            if (cstr.data.local.reg != ret) {
                finalDst = cstr;
            }
        },
        .varSym,
        .boxedLocal => {
            finalDst = cstr;
        },
        .exact => {
            if (cstr.data.exact != ret) {
                finalDst = cstr;
            }
        },
        else => {},
    }
    return .{
        .ret = ret,
        .finalDst = finalDst,
        .numPreludeTemps = numPreludeTemps,
    };
}

fn finishInst(c: *Chunk, val: GenValue, optDst: ?RegisterCstr) !GenValue {
    if (optDst) |dst| {
        const final = try genToFinalDst(c, val, dst);
        try pushOptUnwindableTemp(c, final);
        return final;
    } else {
        try pushOptUnwindableTemp(c, val);
        return val;
    }
}

fn genToDst(c: *Chunk, val: GenValue, dst: RegisterCstr, desc: cy.bytecode.InstDesc) !GenValue {
    switch (dst.type) {
        .local => {
            const local = dst.data.local;
            if (val.local == local.reg) return error.Unexpected;
            if (local.retained) {
                try c.buf.pushOp2Ext(.copyReleaseDst, val.local, local.reg, desc);
            } else {
                try c.buf.pushOp2Ext(.copy, val.local, local.reg, desc);
            }
            // Parent only cares about the retained property.
            return GenValue.initRetained(val.retained);
        },
        .boxedLocal => {
            const boxed = dst.data.boxedLocal;
            if (val.local == boxed.reg) return error.Unexpected;
            if (boxed.retained) {
                try c.buf.pushOp2Ext(.setBoxValueRelease, boxed.reg, val.local, desc);
            } else {
                try c.buf.pushOp2Ext(.setBoxValue, boxed.reg, val.local, desc);
            }
            return GenValue.initRetained(val.retained);
        },
        .varSym => {
            // Set var assumes retained src.
            const pc = c.buf.len();
            try c.buf.pushOp3(.setStaticVar, 0, 0, val.local);
            c.buf.setOpArgU16(pc + 1, @intCast(dst.data.varSym));
            return GenValue.initRetained(val.retained);
        },
        .captured => {
            const captured = dst.data.captured;
            try c.buf.pushOp3Ext(.setCaptured, c.curBlock.closureLocal, captured.idx, val.local, desc);
            return GenValue.initRetained(val.retained);
        },
        .exact => {
            if (val.local == dst.data.exact) return error.Unexpected;
            try c.buf.pushOp2(.copy, val.local, dst.data.exact);
            return genValue(c, dst.data.exact, val.retained);
        },
        else => {
            log.tracev("{}", .{dst.type});
            return error.TODO;
        },
    }
}

fn genToFinalDst(c: *Chunk, val: GenValue, dst: RegisterCstr) !GenValue {
    log.tracev("genToFinalDst src: {} dst: {s}", .{val.local, @tagName(dst.type)});

    const desc = cy.bytecode.InstDesc{};
    const res = try genToDst(c, val, dst, desc);

    // Check to remove the temp that is used to move to final dst.
    if (val.isTempLocal) c.rega.freeTemps(1);
    return res;
}

pub fn endCall(c: *Chunk, inst: CallInst, retained: bool) !GenValue {
    c.rega.freeTemps(inst.numPreludeTemps);
    const val = genValue(c, inst.ret, retained);
    return finishInst(c, val, inst.finalDst);
}

fn forIterStmt(c: *Chunk, idx: usize, nodeId: cy.NodeId) !void {
    const data = c.irGetStmtData(idx, .forIterStmt);

    if (data.countLocal != null) {
        // Counter increment. Always 1.
        _ = try c.rega.consumeNextTemp();
        // Counter.
        _ = try c.rega.consumeNextTemp();

        // Initialize count.
        try c.buf.pushOp2(.constI8, 1, c.rega.nextTemp - 2);
        try c.buf.pushOp2(.constI8, @bitCast(@as(i8, -1)), c.rega.nextTemp - 1);
    }

    // Reserve temp local for iterator.
    const iterTemp = try c.rega.consumeNextTemp();

    // Iterable.
    const iterIdx = c.irAdvanceStmt(idx, .forIterStmt);
    const iterv = try genExpr(c, iterIdx, RegisterCstr.exact(iterTemp + cy.vm.CallArgStart));

    const node = c.nodes[nodeId];
    const header = c.nodes[node.head.forIterStmt.header];
    const iterNodeId = header.head.forIterHeader.iterable;
    const eachNodeId = header.head.forIterHeader.eachClause;

    const funcSigId = try c.sema.ensureFuncSig(&.{ bt.Any }, bt.Any);
    var extraIdx = try c.fmtExtraDesc("iterator()", .{});
    try pushCallObjSymExt(c, iterTemp, 1,
        @intCast(c.compiler.iteratorMGID), @intCast(funcSigId),
        iterNodeId, extraIdx);
    _ = try c.pushRetainedTemp(iterTemp);

    if (iterv.isRetainedTemp()) {
        _ = c.popRetainedTemp();
        try pushRelease(c, iterv.local, iterNodeId);
    }

    try pushSubBlock(c, true, nodeId);
    try genStmts(c, data.declHead);

    const bodyPc = c.buf.ops.items.len;

    // next()
    try genIterNext(c, iterTemp, data.countLocal != null, iterNodeId);
    if (data.eachLocal) |eachLocal| {
        extraIdx = try c.fmtExtraDesc("copy next() to local", .{});
        const resTemp = genValue(c, iterTemp + 1, true);
        try setLocal(c, .{ .id = eachLocal }, undefined, eachNodeId, .{ .rightv = resTemp, .extraIdx = extraIdx });
    }
    if (data.countLocal) |countLocal| {
        extraIdx = try c.fmtExtraDesc("copy count to local", .{});
        const countTemp = genValue(c, iterTemp - 1, false);
        try setLocal(c, .{ .id = countLocal }, undefined, eachNodeId, .{ .rightv = countTemp, .extraIdx = extraIdx });
    }
    const hasCounter = data.countLocal != null;

    const resNoneJump = try c.pushEmptyJumpNone(iterTemp + 1);

    const jumpStackSave: u32 = @intCast(c.subBlockJumpStack.items.len);

    try genStmts(c, data.bodyHead);

    const sblock = c.subBlocks.getLast();
    try genReleaseLocals(c, sblock.nextLocalReg, sblock.nodeId);
    // Pop sub block.
    try popLoopSubBlock(c);

    const contPc = c.buf.ops.items.len;
    try c.pushJumpBackTo(bodyPc);
    c.patchJumpNoneToCurPc(resNoneJump);

    c.patchForBlockJumps(jumpStackSave, c.buf.ops.items.len, contPc);
    c.subBlockJumpStack.items.len = jumpStackSave;

    // TODO: Iter local should be a reserved hidden local (instead of temp) so it can be cleaned up by endLocals when aborting the current fiber.
    try c.pushOptionalDebugSym(nodeId);
    try pushRelease(c, iterTemp, iterNodeId);

    if (hasCounter) {
        c.rega.nextTemp -= 2;
    }
    c.rega.nextTemp -= 1;
    _ = c.popRetainedTemp();
}

fn genIterNext(c: *Chunk, iterTemp: u8, hasCounter: bool, iterNodeId: cy.NodeId) !void {
    var extraIdx = try c.fmtExtraDesc("push iterator arg", .{});
    try c.buf.pushOp2Ext(.copy, iterTemp, iterTemp + cy.vm.CallArgStart + 1, c.descExtra(iterNodeId, extraIdx));

    const funcSigId = try c.sema.ensureFuncSig(&.{ bt.Any }, bt.Any);
    extraIdx = try c.fmtExtraDesc("next()", .{});
    try pushCallObjSymExt(c, iterTemp + 1, 1,
        @intCast(c.compiler.nextMGID), @intCast(funcSigId),
        iterNodeId, extraIdx);

    if (hasCounter) {
        try pushInlineBinExpr(c, .addInt, iterTemp-1, iterTemp-2, iterTemp-1, iterNodeId);
    }
}

fn whileOptStmt(c: *cy.Chunk, idx: usize, nodeId: cy.NodeId) !void {
    const data = c.irGetStmtData(idx, .whileOptStmt);
    const topPc = c.buf.ops.items.len;

    const sblockJumpStart: u32 = @intCast(c.subBlockJumpStack.items.len);

    try pushSubBlock(c, true, nodeId);
    try genStmt(c, data.capIdx);

    // Optional.
    const optIdx = c.irAdvanceStmt(idx, .whileOptStmt);
    const optv = try genExpr(c, optIdx, RegisterCstr.simple);

    const optNoneJump = try c.pushEmptyJumpNone(optv.local);

    // Copy to captured var.
    try setLocal(c, .{ .id = data.someLocal }, undefined, nodeId, .{ .rightv = optv });

    _ = unwindAndFreeTemp(c, optv);
    // No release, captured var consumes it.

    try genStmts(c, data.bodyHead);

    const sblock = c.subBlocks.getLast();
    try genReleaseLocals(c, sblock.nextLocalReg, sblock.nodeId);
    try popLoopSubBlock(c);

    try c.pushJumpBackTo(topPc);
    c.patchJumpNoneToCurPc(optNoneJump);

    // No need to free optv if it is none.

    c.patchForBlockJumps(sblockJumpStart, c.buf.ops.items.len, topPc);
    c.subBlockJumpStack.items.len = sblockJumpStart;
}

fn whileInfStmt(c: *Chunk, idx: usize, nodeId: cy.NodeId) !void {
    const data = c.irGetStmtData(idx, .whileInfStmt);

    // Loop top.
    const topPc = c.buf.ops.items.len;

    const sblockJumpStart: u32 = @intCast(c.subBlockJumpStack.items.len);

    try pushSubBlock(c, true, nodeId);

    try genStmts(c, data.bodyHead);

    const sblock = c.subBlocks.getLast();
    try genReleaseLocals(c, sblock.nextLocalReg, sblock.nodeId);
    try popLoopSubBlock(c);

    try c.pushJumpBackTo(topPc);

    c.patchForBlockJumps(sblockJumpStart, c.buf.ops.items.len, topPc);
    c.subBlockJumpStack.items.len = sblockJumpStart;
}

fn whileCondStmt(c: *Chunk, idx: usize, nodeId: cy.NodeId) !void {
    const data = c.irGetStmtData(idx, .whileCondStmt);

    // Loop top.
    const topPc = c.buf.ops.items.len;

    const sblockJumpStart: u32 = @intCast(c.subBlockJumpStack.items.len);

    const condIdx = c.irAdvanceStmt(idx, .whileCondStmt);
    const condNodeId = c.irGetNode(condIdx);
    const condv = try genExpr(c, condIdx, RegisterCstr.simple);

    const condMissJump = try c.pushEmptyJumpNotCond(condv.local);

    if (unwindAndFreeTemp(c, condv)) {
        // ARC cleanup.
        try pushRelease(c, condv.local, condNodeId);
    }

    try pushSubBlock(c, true, nodeId);

    // Enter while body.
    try genStmts(c, data.bodyHead);

    const sblock = c.subBlocks.getLast();
    try genReleaseLocals(c, sblock.nextLocalReg, sblock.nodeId);
    try popLoopSubBlock(c);

    try c.pushJumpBackTo(topPc);
    c.patchJumpNotCondToCurPc(condMissJump);

    // No need to free cond if false.

    c.patchForBlockJumps(sblockJumpStart, c.buf.ops.items.len, topPc);
    c.subBlockJumpStack.items.len = sblockJumpStart;
}

fn destrElemsStmt(c: *Chunk, idx: usize, nodeId: cy.NodeId) !void {
    const data = c.irGetStmtData(idx, .destrElemsStmt);
    const localsIdx = c.irAdvanceStmt(idx, .destrElemsStmt);
    const locals = c.irGetArray(localsIdx, u8, data.numLocals);

    const rightv = try genExpr(c, data.right, RegisterCstr.simple);

    try c.pushFailableDebugSym(nodeId);
    try c.buf.pushOp2(.seqDestructure, rightv.local, @intCast(locals.len));
    const start = c.buf.ops.items.len;
    try c.buf.ops.resize(c.alloc, c.buf.ops.items.len + locals.len);
    for (locals, 0..) |local, i| {
        const reg = toLocalReg(c, local);
        const regInfo = getLocalInfoPtr(c, reg);
        regInfo.some.rcCandidate = c.sema.isRcCandidateType(bt.Any);
        c.buf.ops.items[start+i] = .{ .val = reg };
    }

    if (rightv.isRetainedTemp()) _ = c.popRetainedTemp();
}

fn forRangeStmt(c: *Chunk, idx: usize, nodeId: cy.NodeId) !void {
    const data = c.irGetStmtData(idx, .forRangeStmt);

    // Reserve vars until end of block, hidden from user.
    const counter = try c.rega.consumeNextTemp();
    const rangeEnd = try c.rega.consumeNextTemp();

    // Range start.
    const startIdx = c.irAdvanceStmt(idx, .forRangeStmt);
    const startv = try genExpr(c, startIdx, RegisterCstr.simple);

    // Range end.
    const endv = try genExpr(c, data.rangeEnd, RegisterCstr.exact(rangeEnd));

    // Begin sub-block.
    try pushSubBlock(c, true, nodeId);

    // Copy counter to itself if no each clause
    var eachLocal: RegisterId = counter;
    if (data.eachLocal) |irVar| {
        try genStmt(c, data.declHead);
        eachLocal = toLocalReg(c, irVar);
    }

    const initPc = c.buf.ops.items.len;
    try c.pushFailableDebugSym(nodeId);
    try c.buf.pushOpSlice(.forRangeInit, &.{ startv.local, rangeEnd, @intFromBool(data.increment),
        counter, eachLocal, 0, 0 });

    if (unwindAndFreeTemp(c, startv)) {
        decTraceRetainedTempStart(c);
    }
    if (startv.isTempLocal) {
        decTraceTempStart(c);
    }
    if (unwindTemp(c, endv)) {
        decTraceRetainedTempStart(c);
    }

    const bodyPc = c.buf.ops.items.len;
    const jumpStackSave = c.subBlockJumpStack.items.len;
    try genStmts(c, data.bodyHead);

    const sblock = c.subBlocks.getLast();
    try genReleaseLocals(c, sblock.nextLocalReg, sblock.nodeId);

    // End sub-block.
    try popLoopSubBlock(c);

    // Perform counter update and perform check against end range.
    const jumpBackOffset: u16 = @intCast(c.buf.ops.items.len - bodyPc);
    const forRangeOp = c.buf.ops.items.len;
    // The forRange op is patched by forRangeInit at runtime.
    c.buf.setOpArgU16(initPc + 6, @intCast(c.buf.ops.items.len - initPc));
    try c.buf.pushOpSlice(.forRange, &.{ counter, rangeEnd, eachLocal, 0, 0 });
    c.buf.setOpArgU16(forRangeOp + 4, jumpBackOffset);

    c.patchForBlockJumps(jumpStackSave, c.buf.ops.items.len, forRangeOp);
    c.subBlockJumpStack.items.len = jumpStackSave;

    c.rega.freeTemps(2);
}

fn tryStmt(c: *cy.Chunk, idx: usize, nodeId: cy.NodeId) !void {
    const data = c.irGetStmtData(idx, .tryStmt);
    const pushTryPc = c.buf.ops.items.len;
    try c.buf.pushOpSlice(.pushTry, &.{0, 0, 0, 0});

    try pushSubBlock(c, false, nodeId);
    try genStmts(c, data.bodyHead);
    try popSubBlock(c);

    const popTryPc = c.buf.ops.items.len;
    try c.buf.pushOp2(.popTry, 0, 0);
    c.buf.setOpArgU16(pushTryPc + 3, @intCast(c.buf.ops.items.len - pushTryPc));

    try pushSubBlock(c, false, nodeId);
    try genStmts(c, data.catchBodyHead);
    var errReg: RegisterId = cy.NullU8;
    if (data.hasErrLocal) {
        errReg = toLocalReg(c, data.errLocal);
    }
    try popSubBlock(c);

    // Patch pushTry with errReg.
    c.buf.setOpArgs1(pushTryPc + 1, errReg);

    c.buf.setOpArgU16(popTryPc + 1, @intCast(c.buf.ops.items.len - popTryPc));
}

fn genTryExpr(c: *Chunk, idx: usize, cstr: RegisterCstr, nodeId: cy.NodeId) !GenValue {
    _ = nodeId;
    const data = c.irGetExprData(idx, .tryExpr);
    const pushTryPc = c.buf.ops.items.len;
    try c.buf.pushOpSlice(.pushTry, &.{ 0, 0, 0, 0 });

    var finalCstr = cstr;
    if (finalCstr.type == .simple) {
        const temp = try c.rega.consumeNextTemp();
        finalCstr = RegisterCstr.initExact(temp, cstr.mustRetain);
    }

    // Body expr.
    const childIdx = c.irAdvanceExpr(idx, .tryExpr);
    const childv = try genExpr(c, childIdx, finalCstr);

    const popTryPc = c.buf.ops.items.len;
    try c.buf.pushOp2(.popTry, 0, 0);
    c.buf.setOpArgU16(pushTryPc + 3, @intCast(c.buf.ops.items.len - pushTryPc));

    var val = childv;
    if (data.catchBody != cy.NullId) {
        // Error is not copied anywhere.
        c.buf.setOpArgs1(pushTryPc + 1, cy.NullU8);

        // Catch expr.
        const catchv = try genExpr(c, data.catchBody, finalCstr);
        if (catchv.retained) {
            val.retained = true;
        }
    } else {
        const inst = try c.rega.selectForDstInst(finalCstr, false);
        c.buf.setOpArgs1(pushTryPc + 1, inst.dst);
        c.buf.setOpArgs1(pushTryPc + 2, @intFromBool(false));

        const catchv = genValue(c, inst.dst, false);
        _ = try finishInst(c, catchv, inst.finalDst);
    }
    c.buf.setOpArgU16(popTryPc + 1, @intCast(c.buf.ops.items.len - popTryPc));
    return val;
}

fn genCondExpr(c: *Chunk, idx: usize, cstr: RegisterCstr, nodeId: cy.NodeId) !GenValue {
    _ = nodeId;
    const data = c.irGetExprData(idx, .condExpr);
    const condIdx = c.irAdvanceExpr(idx, .condExpr);
    const condNodeId = c.irGetNode(condIdx);

    var finalCstr = cstr;
    if (finalCstr.type == .simple) {
        const temp = try c.rega.consumeNextTemp();
        finalCstr = RegisterCstr.initExact(temp, cstr.mustRetain);
    }

    // Cond.
    const condv = try genExpr(c, condIdx, RegisterCstr.simple);

    const condFalseJump = try c.pushEmptyJumpNotCond(condv.local);
    if (unwindAndFreeTemp(c, condv)) {
        // ARC cleanup.
        try pushRelease(c, condv.local, condNodeId);
    }

    // If body.
    const bodyv = try genExpr(c, data.body, finalCstr);
    const bodyEndJump = try c.pushEmptyJump();

    // Else body.
    // Don't need to free cond since it evaluated to false.
    c.patchJumpNotCondToCurPc(condFalseJump);
    const elsev = try genExpr(c, data.elseBody, finalCstr);

    // End.
    c.patchJumpToCurPc(bodyEndJump);

    var val = bodyv;
    if (elsev.retained) val.retained = true;
    return val;
}

fn ifStmt(c: *cy.Chunk, idx: usize, nodeId: cy.NodeId) !void {
    const data = c.irGetStmtData(idx, .ifStmt);
    const bodyEndJumpsStart = c.listDataStack.items.len;

    var condIdx = c.irAdvanceStmt(idx, .ifStmt);
    var condNodeId = c.irGetNode(condIdx);
    var condv = try genExpr(c, condIdx, RegisterCstr.simple);

    var prevCaseMissJump = try c.pushEmptyJumpNotCond(condv.local);

    // ARC cleanup for true case.
    if (unwindAndFreeTemp(c, condv)) {
        try pushRelease(c, condv.local, condNodeId);
    }

    try pushSubBlock(c, false, nodeId);
    try genStmts(c, data.bodyHead);
    try popSubBlock(c);

    var hasElse = false;

    if (data.numElseBlocks > 0) {
        const elseBlocks = c.irGetArray(data.elseBlocks, u32, data.numElseBlocks);

        for (elseBlocks) |elseIdx| {
            const elseBlockNodeId = c.irGetNode(elseIdx);
            const elseBlock = c.irGetExprData(elseIdx, .elseBlock);

            const bodyEndJump = try c.pushEmptyJump();
            try c.listDataStack.append(c.alloc, .{ .pc = bodyEndJump });

            // Jump here from prev case miss.
            c.patchJumpNotCondToCurPc(prevCaseMissJump);

            if (!elseBlock.isElse) {
                condIdx = c.irAdvanceExpr(elseIdx, .elseBlock);
                condNodeId = c.irGetNode(condIdx);
                condv = try genExpr(c, condIdx, RegisterCstr.simple);
                prevCaseMissJump = try c.pushEmptyJumpNotCond(condv.local);

                // ARC cleanup for true case.
                if (unwindAndFreeTemp(c, condv)) {
                    try pushRelease(c, condv.local, condNodeId);
                }
            } else {
                hasElse = true;
            }

            try pushSubBlock(c, false, elseBlockNodeId);
            try genStmts(c, elseBlock.bodyHead);
            try popSubBlock(c);
        }
    }

    // Jump here from all body ends.
    const bodyEndJumps = c.listDataStack.items[bodyEndJumpsStart..];
    for (bodyEndJumps) |jump| {
        c.patchJumpToCurPc(jump.pc);
    }
    c.listDataStack.items.len = bodyEndJumpsStart;

    if (!hasElse) {
        // Jump here from prev case miss.
        c.patchJumpNotCondToCurPc(prevCaseMissJump);
    }
}

fn switchStmt(c: *Chunk, idx: usize, nodeId: cy.NodeId) !void {
    const blockIdx = c.irAdvanceStmt(idx, .switchStmt);
    _ = try genSwitchBlock(c, blockIdx, null, nodeId);
}

fn genSwitchBlock(c: *Chunk, idx: usize, cstr: ?RegisterCstr, nodeId: cy.NodeId) !GenValue {
    const data = c.irGetExprData(idx, .switchBlock);

    var childBreakJumpsStart: u32 = undefined;
    if (!data.leftAssign) {
        childBreakJumpsStart = @intCast(c.subBlockJumpStack.items.len);
    }

    const casesIdx = c.irAdvanceExpr(idx, .switchBlock);
    const cases = c.irGetArray(casesIdx, u32, data.numCases);

    // Expr.
    const exprIdx = c.irAdvanceArray(casesIdx, u32, cases);
    const exprv = try genExpr(c, exprIdx, RegisterCstr.simple);

    const caseBodyEndJumpsStart = c.listDataStack.items.len;

    var prevCaseMissJump: u32 = cy.NullId;
    var hasElse = false;
    for (cases) |caseIdx| {
        const caseNodeId = c.irGetNode(caseIdx);
        const case = c.irGetExprData(caseIdx, .switchCase);
        const isElse = case.numConds == 0;

        // Jump here from prev case miss.
        if (prevCaseMissJump != cy.NullId) {
            c.patchJumpToCurPc(prevCaseMissJump);
        }

        if (!isElse) {
            const condMatchJumpsStart = c.listDataStack.items.len;

            const condsIdx = c.irAdvanceExpr(caseIdx, .switchCase);

            const conds = c.irGetArray(condsIdx, u32, case.numConds);
            for (conds) |condIdx| {
                const condNodeId = c.irGetNode(condIdx);
                const condv = try genExpr(c, condIdx, RegisterCstr.simple);
                const temp = try c.rega.consumeNextTemp();

                try c.pushOptionalDebugSym(condNodeId);
                try c.buf.pushOp3Ext(.compare, exprv.local, condv.local, temp, c.desc(condNodeId));

                const condMissJump = try c.pushEmptyJumpNotCond(temp);
                const retainedTemp = unwindAndFreeTemp(c, condv);

                if (retainedTemp) {
                    try pushRelease(c, condv.local, condNodeId);
                }

                const condMatchJump = try c.pushEmptyJump();
                try c.listDataStack.append(c.alloc, .{ .pc = condMatchJump });
                c.patchJumpNotCondToCurPc(condMissJump);
                // Miss continues to next cond.

                if (retainedTemp) {
                    try pushRelease(c, condv.local, condNodeId);
                }

                // Unwind `temp`.
                c.rega.freeTemps(1);
            }

            // No cond matches. Jump to next case.
            prevCaseMissJump = try c.pushEmptyJump();

            // Jump here from all matching conds.
            for (c.listDataStack.items[condMatchJumpsStart..]) |pc| {
                c.patchJumpToCurPc(pc.pc);
            }
            c.listDataStack.items.len = condMatchJumpsStart;
        } else {
            hasElse = true;
            prevCaseMissJump = cy.NullId;
        }

        if (case.bodyIsExpr) {
            _ = try genExpr(c, case.bodyHead, cstr.?);
        } else {
            try pushSubBlock(c, false, caseNodeId);
            try genStmts(c, case.bodyHead);
            try popSubBlock(c);
        }

        const caseBodyEndJump = try c.pushEmptyJump();
        try c.listDataStack.append(c.alloc, .{ .jumpToEndPc = caseBodyEndJump });
    }

    // Jump here from prev case miss.
    if (prevCaseMissJump != cy.NullId) {
        c.patchJumpToCurPc(prevCaseMissJump);
    }

    if (data.leftAssign and !hasElse) {
        // Gen none return for missing else.
        _ = try genNone(c, cstr.?, nodeId);
    }

    // Jump here from case body ends.
    for (c.listDataStack.items[caseBodyEndJumpsStart..]) |pc| {
        c.patchJumpToCurPc(pc.jumpToEndPc);
    }
    c.listDataStack.items.len = caseBodyEndJumpsStart;

    // Jump here from nested child breaks.
    if (!data.leftAssign) {
        const newLen = c.patchBreaks(childBreakJumpsStart, c.buf.ops.items.len);
        c.subBlockJumpStack.items.len = newLen;
    }

    // Unwind switch expr.
    if (unwindAndFreeTemp(c, exprv)) {
        try pushRelease(c, exprv.local, nodeId);
    }

    // Complete with no value since assign statement doesn't do anything with it.
    return GenValue.initNoValue();
}

fn genMap(c: *Chunk, idx: usize, cstr: RegisterCstr, nodeId: cy.NodeId) !GenValue {
    _ = nodeId;
    const data = c.irGetExprData(idx, .map);
    const keysIdx = c.irAdvanceExpr(idx, .map);
    const keys = c.irGetArray(keysIdx, []const u8, data.numArgs);
    const args = c.irGetArray(data.args, u32, data.numArgs);

    const inst = try c.rega.selectForDstInst(cstr, true);

    const constIdxStart = c.listDataStack.items.len;
    for (keys) |key| {
        const constIdx = try c.buf.getOrPushStaticStringConst(key);
        try c.listDataStack.append(c.alloc, .{ .constIdx = @intCast(constIdx) });
    }

    const argStart = c.rega.getNextTemp();
    for (args) |argIdx| {
        const temp = try c.rega.consumeNextTemp();
        try genAndPushExpr(c, argIdx, RegisterCstr.exactMustRetain(temp));
    }

    if (data.numArgs == 0) {
        try c.buf.pushOp1(.mapEmpty, inst.dst);
    } else {
        const constIdxes = c.listDataStack.items[constIdxStart..];
        c.listDataStack.items.len = constIdxStart;

        try c.buf.pushOp3(.map, argStart, data.numArgs, inst.dst);
        const start = try c.buf.reserveData(data.numArgs * 2);
        for (constIdxes, 0..) |item, i| {
            c.buf.setOpArgU16(start + i*2, @intCast(item.constIdx));
        }

        const argvs = popValues(c, data.numArgs);
        try checkArgs(argStart, argvs);
        _ = unwindTemps(c, argvs);
    }

    const val = genValue(c, inst.dst, true);
    return finishInst(c, val, inst.finalDst);
}

fn genList(c: *Chunk, idx: usize, cstr: RegisterCstr, nodeId: cy.NodeId) !GenValue {
    const data = c.irGetExprData(idx, .list);
    const argsIdx = c.irAdvanceExpr(idx, .list);
    const args = c.irGetArray(argsIdx, u32, data.numArgs);

    const inst = try c.rega.selectForDstInst(cstr, true);
    const argStart = c.rega.getNextTemp();

    for (args) |argIdx| {
        const temp = try c.rega.consumeNextTemp();
        try genAndPushExpr(c, argIdx, RegisterCstr.exactMustRetain(temp));
    }

    try c.pushFailableDebugSym(nodeId);
    try c.buf.pushOp3Ext(.list, argStart, data.numArgs, inst.dst, c.desc(nodeId));

    const argvs = popValues(c, data.numArgs);
    try checkArgs(argStart, argvs);
    _ = unwindTemps(c, argvs);

    const val = genValue(c, inst.dst, true);
    return finishInst(c, val, inst.finalDst);
}

const BlockType = enum(u8) {
    main,
    fiber,
    func,
};

fn pushFiberBlock(c: *Chunk, numArgs: u8, nodeId: cy.NodeId) !void {
    log.tracev("push fiber block: {}", .{numArgs});

    try pushBlock(c, .fiber, nodeId);
    c.curBlock.frameLoc = nodeId;

    try reserveFiberCallRegs(c, numArgs);
}

fn popFiberBlock(c: *Chunk) !void {
    try genBlockReleaseLocals(c);
    try popBlock(c);

    // Pop boundary index.
    _ = c.popRetainedTemp();
}

fn pushFuncBlockCommon(c: *Chunk, maxIrLocals: u8, numParamCopies: u8, params: []align(1) const ir.FuncParam, func: *cy.Func, nodeId: cy.NodeId) !void {
    try pushBlock(c, .func, nodeId);

    c.curBlock.frameLoc = nodeId;

    if (c.compiler.config.genDebugFuncMarkers) {
        try c.compiler.buf.pushDebugFuncStart(func, c.id);
    }

    // `reserveFuncRegs` may emit copy and box insts.
    try reserveFuncRegs(c, maxIrLocals, numParamCopies, params);
}

pub const Sym = union {
    varSym: struct {
        id: u32,
    },
    funcSym: struct {
        id: u32,

        // Used by jit.
        pc: u32,
    },
    hostFuncSym: struct {
        id: u32,
        ptr: vmc.HostFuncFn,
    },
};

pub fn pushFuncBlock(c: *Chunk, data: ir.FuncDecl, params: []align(1) const ir.FuncParam, nodeId: cy.NodeId) !void {
    log.tracev("push func block: {}, {}, {}, {}", .{data.func.numParams, data.maxLocals, data.func.isMethod, nodeId});
    try pushFuncBlockCommon(c, data.maxLocals, data.numParamCopies, params, data.func, nodeId);
}

pub fn popFuncBlockCommon(c: *Chunk, func: *cy.Func) !void {
    // TODO: Check last statement to skip adding ret.
    try genFuncEnd(c);

    // Pop the null boundary index.
    _ = c.popRetainedTemp();

    // Check that next temp is at the start which indicates it has been reset after statements.
    if (cy.Trace) {
        if (c.rega.nextTemp > c.rega.tempStart) {
            return c.reportErrorAt("Temp registers were not reset. {} > {}", &.{v(c.rega.nextTemp), v(c.rega.tempStart)}, c.curBlock.debugNodeId);
        }
        if (c.curBlock.resetVerboseOnBlockEnd) {
            cy.verbose = false;
        }
    }

    if (c.compiler.config.genDebugFuncMarkers) {
        try c.compiler.buf.pushDebugFuncEnd(func, c.id);
    }

    c.blocks.items.len -= 1;
    if (c.blocks.items.len > 0) {
        c.curBlock = &c.blocks.items[c.blocks.items.len-1];

        // Restore register allocator state.
        c.rega.restoreState(c.curBlock.regaTempStart, c.curBlock.regaNextTemp, c.curBlock.regaMaxTemp);
    }
}

fn genLambda(c: *Chunk, idx: usize, cstr: RegisterCstr, nodeId: cy.NodeId) !GenValue {
    const data = c.irGetExprData(idx, .lambda);
    const func = data.func;
    const paramsIdx = c.irAdvanceExpr(idx, .lambda);
    const params = c.irGetArray(paramsIdx, ir.FuncParam, func.numParams);

    const inst = try c.rega.selectForDstInst(cstr, true);

    // Prepare jump to skip the body.
    const skipJump = try c.pushEmptyJump();

    log.tracev("push lambda block: {}, {}", .{func.numParams, data.maxLocals});
    const funcPc = c.buf.ops.items.len;
    try pushFuncBlockCommon(c, data.maxLocals, data.numParamCopies, params, func, nodeId);

    try genStmts(c, data.bodyHead);

    const stackSize = c.getMaxUsedRegisters();
    try popFuncBlockCommon(c, func);
    c.patchJumpToCurPc(skipJump);
    const offset: u16 = @intCast(c.buf.ops.items.len - funcPc);

    if (data.numCaptures == 0) {
        const start = c.buf.ops.items.len;
        try c.pushOptionalDebugSym(func.declId);
        try c.buf.pushOpSliceExt(.lambda, &.{ 0, 0, func.numParams, stackSize, 0, 0, inst.dst }, c.desc(nodeId));
        c.buf.setOpArgU16(start + 1, offset);
        c.buf.setOpArgU16(start + 5, @intCast(func.funcSigId));
    } else {
        const captures = c.irGetArray(data.captures, u8, data.numCaptures);
        const start = c.buf.ops.items.len;
        try c.pushOptionalDebugSym(func.declId);
        try c.buf.pushOpSlice(.closure, &.{
            0, 0, func.numParams, @as(u8, @intCast(captures.len)), stackSize, 
            0, 0, cy.vm.CalleeStart, inst.dst
        });
        c.buf.setOpArgU16(start + 1, offset);
        c.buf.setOpArgU16(start + 6, @intCast(func.funcSigId));

        const operandStart = try c.buf.reserveData(captures.len);
        for (captures, 0..) |irVar, i| {
            const reg = toLocalReg(c, irVar);
            c.buf.ops.items[operandStart + i].val = reg;
        }
    }

    const val = genValue(c, inst.dst, true);
    return finishInst(c, val, inst.finalDst);
}

pub fn shouldGenMainScopeReleaseOps(c: *cy.VMcompiler) bool {
    return !c.vm.config.singleRun;
}

pub fn pushBlock(c: *Chunk, btype: BlockType, debugNodeId: cy.NodeId) !void {
    // Save register allocator state.
    if (c.blocks.items.len > 0) {
        c.curBlock.regaTempStart = c.rega.tempStart;
        c.curBlock.regaNextTemp = c.rega.nextTemp;
        c.curBlock.regaMaxTemp = c.rega.maxTemp;
    }

    try c.blocks.append(c.alloc, GenBlock.init(btype));
    c.curBlock = &c.blocks.items[c.blocks.items.len-1];
    c.curBlock.irLocalMapStart = @intCast(c.genIrLocalMapStack.items.len);
    c.curBlock.localStart = @intCast(c.genLocalStack.items.len);
    c.curBlock.debugNodeId = debugNodeId;

    try c.pushUnwindTempBoundary();
    if (cy.Trace) {
        c.curBlock.retainedTempStart = @intCast(c.getUnwindTempsLen());
    }
}

pub fn popBlock(c: *Chunk) !void {
    log.tracev("pop gen block", .{});
    c.genIrLocalMapStack.items.len = c.curBlock.irLocalMapStart;
    c.genLocalStack.items.len = c.curBlock.localStart;
    var last = c.blocks.pop();

    // Check that next temp is at the start which indicates it has been reset after statements.
    if (cy.Trace) {
        if (c.rega.nextTemp > c.rega.tempStart) {
            return c.reportErrorAt("Temp registers were not reset. {} > {}", &.{v(c.rega.nextTemp), v(c.rega.tempStart)}, last.debugNodeId);
        }
        if (c.curBlock.resetVerboseOnBlockEnd) {
            cy.verbose = false;
        }
    }

    last.deinit(c.alloc);
    if (c.blocks.items.len > 0) {
        c.curBlock = &c.blocks.items[c.blocks.items.len-1];

        // Restore register allocator state.
        c.rega.restoreState(c.curBlock.regaTempStart, c.curBlock.regaNextTemp, c.curBlock.regaMaxTemp);
    }
}

pub fn pushSubBlock(c: *Chunk, isLoop: bool, nodeId: cy.NodeId) !void {
    log.tracev("push sblock {} nextTemp: {}", .{c.curBlock.sBlockDepth, c.rega.nextTemp});
    c.curBlock.sBlockDepth += 1;

    const idx = c.subBlocks.items.len;
    try c.subBlocks.append(c.alloc, .{
        .nodeId = nodeId,
        .isLoopBlock = isLoop,
        .nextLocalReg = c.curBlock.nextLocalReg,
    });

    if (cy.Trace) {
        c.subBlocks.items[idx].retainedTempStart = @intCast(c.getUnwindTempsLen());
        c.subBlocks.items[idx].tempStart = @intCast(c.rega.nextTemp);
    }
}

pub fn popSubBlock(c: *Chunk) !void {
    log.tracev("pop sblock {}", .{c.curBlock.sBlockDepth});

    const sblock = c.subBlocks.pop();

    c.curBlock.sBlockDepth -= 1;

    try genReleaseLocals(c, sblock.nextLocalReg, sblock.nodeId);

    // Restore nextLocalReg.
    c.curBlock.nextLocalReg = sblock.nextLocalReg;
}

pub fn popLoopSubBlock(c: *Chunk) !void {
    const sblock = c.subBlocks.pop();
    c.curBlock.sBlockDepth -= 1;

    // Restore nextLocalReg.
    c.curBlock.nextLocalReg = sblock.nextLocalReg;
}

pub const GenSubBlock = struct {
    nodeId: cy.NodeId,
    nextLocalReg: u8,
    isLoopBlock: bool,

    // Used to check the stack state after each stmt.
    retainedTempStart: if (cy.Trace) u32 else void = undefined,
    tempStart: if (cy.Trace) u32 else void = undefined,
};

pub const GenBlock = struct {
    type: BlockType,
    frameLoc: cy.NodeId = cy.NullId,

    /// Whether codegen should create an ending that returns 1 arg.
    /// Otherwise `ret0` is generated.
    requiresEndingRet1: bool,

    /// If the function body belongs to a closure, this local
    /// contains the closure's value which is then used to perform captured var lookup.
    closureLocal: u8,

    /// Register allocator state.
    regaTempStart: u8,
    regaNextTemp: u8,
    regaMaxTemp: u8,

    /// Starts after the prelude registers.
    startLocalReg: u8,

    /// Increased as locals are declared.
    /// Entering a sub block saves it to be restored on sub block exit.
    /// This and `startLocalReg` provide the range of currently alive vars
    /// which is recorded in the error rewind table.
    nextLocalReg: u8,

    irLocalMapStart: u32,
    localStart: u32,

    debugNodeId: cy.NodeId,

    sBlockDepth: u32,

    /// If the last stmt is an expr stmt, return the local instead of releasing it. (Only for main block.)
    endLocal: u8 = cy.NullU8,

    /// LLVM
    // funcRef: if (cy.hasJIT) llvm.ValueRef else void = undefined,

    retainedTempStart: if (cy.Trace) u32 else void = undefined,

    resetVerboseOnBlockEnd: bool,

    fn init(btype: BlockType) GenBlock {
        return .{
            .type = btype,
            .requiresEndingRet1 = false,
            .closureLocal = cy.NullU8,
            .regaTempStart = undefined,
            .regaNextTemp = undefined,
            .regaMaxTemp = undefined,
            .irLocalMapStart = 0,
            .localStart = 0,
            .startLocalReg = 0,
            .nextLocalReg = 0,
            .debugNodeId = cy.NullId,
            .sBlockDepth = 0,
            .resetVerboseOnBlockEnd = false,
        };
    }

    fn deinit(self: *GenBlock, alloc: std.mem.Allocator) void {
        _ = self;
        _ = alloc;
    }
};

fn genConstFloat(c: *Chunk, val: f64, dst: LocalId, nodeId: cy.NodeId) !GenValue {
    const idx = try c.buf.getOrPushConst(cy.Value.initF64(val));
    try genConst(c, idx, dst, false, nodeId);
    return genValue(c, dst, false);
}

fn genConst(c: *cy.Chunk, idx: usize, dst: u8, retain: bool, nodeId: cy.NodeId) !void {
    const pc = c.buf.len();
    if (retain) {
        try c.buf.pushOp3Ext(.constRetain, 0, 0, dst, c.desc(nodeId));
    } else {
        try c.buf.pushOp3Ext(.constOp, 0, 0, dst, c.desc(nodeId));
    }
    c.buf.setOpArgU16(pc + 1, @intCast(idx));
}

pub fn checkArgs(start: RegisterId, argvs: []const GenValue) !void {
    if (cy.Trace) {
        for (argvs, 0..) |argv, i| {
            if (argv.local != start + i) {
                log.tracev("Expected argv at {}, got {}.", .{start+i, argv.local});
                return error.Unexpected;
            }
        }
    }
}

fn decTraceRetainedTempStart(c: *Chunk) void {
    if (cy.Trace) {
        if (c.curBlock.sBlockDepth > 0) {
            c.subBlocks.items[c.subBlocks.items.len-1].retainedTempStart -= 1;
        } else {
            c.curBlock.retainedTempStart -= 1;
        }
    }
}

fn decTraceTempStart(c: *Chunk) void {
    if (cy.Trace) {
        if (c.curBlock.sBlockDepth > 0) {
            c.subBlocks.items[c.subBlocks.items.len-1].tempStart -= 1;
        } else {
            // Nop.
        }
    }
}

/// Check stack after statement.
pub fn checkStack(c: *Chunk, nodeId: cy.NodeId) !void {
    if (cy.Trace) {
        var retainedTempStart: usize = undefined;
        var tempStart: usize = undefined;
        if (c.curBlock.sBlockDepth > 0) {
            retainedTempStart = c.subBlocks.getLast().retainedTempStart;
            tempStart = c.subBlocks.getLast().tempStart;
        } else {
            retainedTempStart = c.curBlock.retainedTempStart;
            tempStart = c.rega.tempStart;
        }
        if (c.unwindTempIndexStack.items.len != retainedTempStart) {
            return c.reportErrorAt("Expected {} unwindable retained temps, got {}",
                &.{v(retainedTempStart), v(c.unwindTempIndexStack.items.len)}, nodeId);
        }

        if (c.rega.nextTemp != tempStart) {
            return c.reportErrorAt("Expected {} temp start, got {}",
                &.{v(tempStart), v(c.rega.nextTemp)}, nodeId);
        }
    }
}

fn exprStmt(c: *Chunk, idx: usize, nodeId: cy.NodeId) !void {
    const data = c.irGetStmtData(idx, .exprStmt);

    const cstr = RegisterCstr.initSimple(data.returnMain);

    const expr = c.irAdvanceStmt(idx, .exprStmt);
    const exprv = try genExpr(c, expr, cstr);
    if (unwindAndFreeTemp(c, exprv)) {
        // ARC cleanup.
        if (!data.returnMain) {
            // TODO: Merge with previous release inst.
            try pushRelease(c, exprv.local, nodeId);
        }
    }

    if (data.returnMain) {
        c.curBlock.endLocal = exprv.local;
    }
}

pub fn releaseIfRetainedTempAt(c: *cy.Chunk, val: GenValue, nodeId: cy.NodeId) !void {
    if (val.retained and val.isTempLocal) {
        try pushRelease(c, val.local, nodeId);
    }
}

pub fn releaseIfRetainedTemp(c: *cy.Chunk, val: GenValue) !void {
    return releaseIfRetainedTempAt(c, val, cy.NullId);
}

const LocalId = u8;

pub fn pushOptUnwindableTemp(c: *Chunk, val: GenValue) !void {
    if (val.isRetainedTemp()) {
        try c.pushRetainedTemp(val.local);
    }
}

fn pushValue(c: *Chunk, reg: RegisterId, retained: bool) !GenValue {
    if (c.isTempLocal(reg)) {
        log.tracev("temp value at: {}, retained: {}", .{reg, retained});
        // If this value is a retained temp, push for unwinding.
        if (retained) {
            try c.pushRetainedTemp(reg);
        }
        return GenValue.initTempValue(reg, retained);
    } else {
        log.tracev("local value at: {}, retained: {}", .{reg, retained});
        return GenValue.initLocalValue(reg, retained);
    }
}

pub fn genValue(c: *const Chunk, local: LocalId, retained: bool) GenValue {
    if (c.isTempLocal(local)) {
        return GenValue.initTempValue(local, retained);
    } else {
        return GenValue.initLocalValue(local, retained);
    }
}

const GenValueType = enum {
    generic,
    jitCondFlag,
};

pub const GenValue = struct {
    type: GenValueType,
    /// TODO: Rename to reg.
    local: LocalId,
    isTempLocal: bool,

    /// Whether this value was retained by 1 refcount.
    retained: bool,

    data: union {
        jitCondFlag: struct {
            type: jitgen.JitCondFlagType,
        },
    } = undefined,

    fn initJitCondFlag(condt: jitgen.JitCondFlagType) GenValue {
        return .{ .type = .jitCondFlag,
            .local = undefined, .isTempLocal = false, .retained = false,
            .data = .{ .jitCondFlag = .{
                .type = condt,
            }},
        };
    }

    fn initRetained(retained: bool) GenValue {
        return .{
            .type = .generic,
            .local = 255,
            .isTempLocal = false,
            .retained = retained,
        };
    }

    fn initNoValue() GenValue {
        return .{
            .type = .generic,
            .local = 255,
            .isTempLocal = false,
            .retained = false,
        };
    }

    pub fn initLocalValue(local: LocalId, retained: bool) GenValue {
        return .{
            .type = .generic,
            .local = local,
            .isTempLocal = false,
            .retained = retained,
        };
    }

    pub fn initTempValue(local: LocalId, retained: bool) GenValue {
        return .{
            .type = .generic,
            .local = local,
            .isTempLocal = true,
            .retained = retained,
        };
    }

    pub fn isRetainedTemp(self: GenValue) bool {
        return self.isTempLocal and self.retained;
    }
};

fn pushReleaseOpt2(self: *Chunk, pushRegA: bool, rega: u8, pushRegB: bool, regb: u8, debugNodeId: cy.NodeId) !void {
    if (pushRegA and pushRegB) {
        try pushReleases(self, &.{ rega, regb }, debugNodeId);
    } else {
        if (pushRegA) {
            try pushRelease(self, rega, debugNodeId);
        }
        if (pushRegB) {
            try pushRelease(self, regb, debugNodeId);
        }
    }
}

fn pushReleaseVals(self: *Chunk, vals: []const GenValue, debugNodeId: cy.NodeId) !void {
    if (vals.len > 1) {
        try self.pushOptionalDebugSym(debugNodeId);
        try self.buf.pushOp1(.releaseN, @intCast(vals.len));

        const start = self.buf.ops.items.len;
        try self.buf.ops.resize(self.alloc, self.buf.ops.items.len + vals.len);
        for (vals, 0..) |val, i| {
            self.buf.ops.items[start+i] = .{ .val = val.local };
        }
    } else if (vals.len == 1) {
        try pushRelease(self, vals[0].local, debugNodeId);
    }
}

fn pushReleases(self: *Chunk, regs: []const u8, debugNodeId: cy.NodeId) !void {
    if (regs.len > 1) {
        try self.pushOptionalDebugSym(debugNodeId);
        try self.buf.pushOp1(.releaseN, @intCast(regs.len));
        try self.buf.pushOperands(regs);
    } else if (regs.len == 1) {
        try pushRelease(self, regs[0], debugNodeId);
    }
}

fn pushRelease(c: *Chunk, local: u8, nodeId: cy.NodeId) !void {
    try c.pushOptionalDebugSym(nodeId);
    try c.buf.pushOp1Ext(.release, local, c.desc(nodeId));
}

fn pushReleaseExt(c: *Chunk, local: u8, nodeId: cy.NodeId, extraIdx: u32) !void {
    try c.pushOptionalDebugSym(nodeId);
    try c.buf.pushOp1Ext(.release, local, c.descExtra(nodeId, extraIdx));
}

fn genConstIntExt(c: *Chunk, val: u48, dst: LocalId, desc: cy.bytecode.InstDesc) !GenValue {
    // TODO: Can be constU8.
    if (val <= std.math.maxInt(i8)) {
        try c.buf.pushOp2Ext(.constI8, @bitCast(@as(i8, @intCast(val))), dst, desc);
        return genValue(c, dst, false);
    }
    const idx = try c.buf.getOrPushConst(cy.Value.initInt(@intCast(val)));
    try genConst(c, idx, dst, false, desc.nodeId);
    return genValue(c, dst, false);
}

fn genCallTypeCheck(c: *cy.Chunk, startLocal: u8, numArgs: u32, funcSigId: sema.FuncSigId, nodeId: cy.NodeId) !void {
    try c.pushFailableDebugSym(nodeId);
    const start = c.buf.ops.items.len;
    try c.buf.pushOpSlice(.callTypeCheck, &[_]u8{ startLocal, @intCast(numArgs), 0, 0, });
    c.buf.setOpArgU16(start + 3, @intCast(funcSigId));
}

fn pushCallSym(c: *cy.Chunk, startLocal: u8, numArgs: u32, numRet: u8, symId: u32, nodeId: cy.NodeId) !void {
    try c.pushFailableDebugSym(nodeId);
    const start = c.buf.ops.items.len;
    try c.buf.pushOpSliceExt(.callSym, &.{ startLocal, @as(u8, @intCast(numArgs)), numRet, 0, 0, 0, 0, 0, 0, 0, 0 }, c.desc(nodeId));
    c.buf.setOpArgU16(start + 4, @intCast(symId));
}

fn reserveLocalRegAt(c: *Chunk, irLocalId: u8, declType: types.TypeId, boxed: bool, reg: u8, nodeId: cy.NodeId) !void {
    // Stacks are always big enough because of pushBlock.
    log.tracev("reserve {} {} {*} {} {}", .{ irLocalId, reg, c.curBlock, c.curBlock.irLocalMapStart, c.curBlock.localStart });
    log.tracev("local stacks {} {}", .{ c.genIrLocalMapStack.items.len, c.genLocalStack.items.len });
    c.genIrLocalMapStack.items[c.curBlock.irLocalMapStart + irLocalId] = reg;
    c.genLocalStack.items[c.curBlock.localStart + reg] = .{
        .some = .{
            .owned = true,
            .rcCandidate = c.sema.isRcCandidateType(declType),
            .boxed = boxed,
        },
    };
    if (cy.Trace) {
        const nodeStr = try c.encoder.formatNode(nodeId, &cy.tempBuf);
        log.tracev("reserve {}: {s}", .{reg, nodeStr});
    }
}

pub fn reserveLocalReg(c: *Chunk, irVarId: u8, declType: types.TypeId, boxed: bool, nodeId: cy.NodeId, advanceNext: bool) !RegisterId {
    try reserveLocalRegAt(c, irVarId, declType, boxed, c.curBlock.nextLocalReg, nodeId);
    defer {
        if (advanceNext) {
            c.curBlock.nextLocalReg += 1;
        }
    }
    return c.curBlock.nextLocalReg;
}

fn pushCallObjSym(chunk: *cy.Chunk, ret: u8, numArgs: u8, symId: u8, callSigId: u16, nodeId: cy.NodeId) !void {
    try pushCallObjSymExt(chunk, ret, numArgs, symId, callSigId, nodeId, cy.NullId);
}

fn pushCallObjSymExt(chunk: *cy.Chunk, ret: u8, numArgs: u8, symId: u8, callSigId: u16, nodeId: cy.NodeId, extraIdx: u32) !void {
    try chunk.pushFailableDebugSym(nodeId);
    const start = chunk.buf.ops.items.len;
    try chunk.buf.pushOpSliceExt(.callObjSym, &.{
        ret, numArgs, 0, symId, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    }, chunk.descExtra(nodeId, extraIdx));
    chunk.buf.setOpArgU16(start + 5, callSigId);
}

fn pushInlineUnExpr(c: *cy.Chunk, code: cy.OpCode, child: u8, dst: u8, nodeId: cy.NodeId) !void {
    try c.pushFailableDebugSym(nodeId);
    try c.buf.pushOpSlice(code, &.{ child, dst, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 });
}

fn pushInlineBinExpr(c: *cy.Chunk, code: cy.OpCode, left: u8, right: u8, dst: u8, nodeId: cy.NodeId) !void {
    try c.pushFailableDebugSym(nodeId);
    try c.buf.pushOpSliceExt(code, &.{ left, right, dst, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }, c.desc(nodeId));
}

fn pushInlineTernExpr(c: *cy.Chunk, code: cy.OpCode, a: u8, b: u8, c_: u8, dst: u8, nodeId: cy.NodeId) !void {
    try c.pushFailableDebugSym(nodeId);
    try c.buf.pushOpSlice(code, &.{ a, b, c_, dst, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 });
}

fn mainEnd(c: *cy.Chunk, reg: ?u8) !void {
    try c.buf.pushOp1(.end, reg orelse cy.NullU8);
}

fn getIntUnaryOpCode(op: cy.UnaryOp) cy.OpCode {
    return switch (op) {
        .bitwiseNot => .bitwiseNot,
        .minus => .negInt,
        else => cy.fatal(),
    };
}

fn getFloatUnaryOpCode(op: cy.UnaryOp) cy.OpCode {
    return switch (op) {
        .minus => .negFloat,
        else => cy.fatal(),
    };
}

fn getFloatOpCode(op: cy.BinaryExprOp) cy.OpCode {
    return switch (op) {
        .less => .lessFloat,
        .greater => .greaterFloat,
        .less_equal => .lessEqualFloat,
        .greater_equal => .greaterEqualFloat,
        .minus => .subFloat,
        .plus => .addFloat,
        .slash => .divFloat,
        .percent => .modFloat,
        .star => .mulFloat,
        .caret => .powFloat,
        else => cy.fatal(),
    };
}

fn getIntOpCode(op: cy.BinaryExprOp) cy.OpCode {
    return switch (op) {
        .less => .lessInt,
        .greater => .greaterInt,
        .less_equal => .lessEqualInt,
        .greater_equal => .greaterEqualInt,
        .minus => .subInt,
        .plus => .addInt,
        .slash => .divInt,
        .percent => .modInt,
        .star => .mulInt,
        .caret => .powInt,
        .bitwiseAnd => .bitwiseAnd,
        .bitwiseOr => .bitwiseOr,
        .bitwiseXor => .bitwiseXor,
        .bitwiseLeftShift => .bitwiseLeftShift,
        .bitwiseRightShift => .bitwiseRightShift,
        else => cy.fatal(),
    };
}

fn pushObjectInit(c: *cy.Chunk, typeId: cy.TypeId, startLocal: u8, numFields: u8, dst: RegisterId, debugNodeId: cy.NodeId) !void {
    if (numFields <= 4) {
        try c.pushOptionalDebugSym(debugNodeId);
        const start = c.buf.ops.items.len;
        try c.buf.pushOpSlice(.objectSmall, &[_]u8{ 0, 0, startLocal, numFields, dst });
        c.buf.setOpArgU16(start + 1, @intCast(typeId)); 
    } else {
        try c.pushFailableDebugSym(debugNodeId);
        const start = c.buf.ops.items.len;
        try c.buf.pushOpSlice(.object, &[_]u8{ 0, 0, startLocal, numFields, dst });
        c.buf.setOpArgU16(start + 1, @intCast(typeId)); 
    }
}

fn pushField(c: *cy.Chunk, recv: u8, dst: u8, fieldId: u16, debugNodeId: cy.NodeId) !void {
    try c.pushFailableDebugSym(debugNodeId);
    const start = c.buf.ops.items.len;
    try c.buf.pushOpSliceExt(.field, &.{ recv, dst, 0, 0, 0, 0, 0 }, c.desc(debugNodeId));
    c.buf.setOpArgU16(start + 3, fieldId);
}

fn pushObjectField(c: *cy.Chunk, recv: u8, fieldIdx: u8, dst: u8, debugNodeId: cy.NodeId) !void {
    try c.pushOptionalDebugSym(debugNodeId);
    try c.buf.pushOpSliceExt(.objectField, &.{ recv, fieldIdx, dst }, c.desc(debugNodeId));
}

pub const PreferDst = struct {
    dst: RegisterId,
    canUseDst: bool,

    pub fn nextCstr(self: *PreferDst, val: GenValue) RegisterCstr {
        if (val.local == self.dst) {
            self.canUseDst = false;
        }
        return RegisterCstr.preferIf(self.dst, self.canUseDst);
    }
};