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

const Cstr = cy.register.Cstr;
const RegisterId = cy.register.RegisterId;
const TypeId = types.TypeId;

const Chunk = cy.chunk.Chunk;

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
        stmt = c.ir.getStmtNext(stmt);
    }
}

fn genStmt(c: *Chunk, idx: u32) anyerror!void {
    const code = c.ir.getStmtCode(idx);
    const nodeId = c.ir.getNode(idx);
    c.curNodeId = nodeId;
    if (cy.Trace) {
        const contextStr = try c.encoder.formatNode(nodeId, &cy.tempBuf);
        if (cy.verbose) {
            rt.logFmt("{}| {}: `{}` unw={} ntmp={}", &.{
                cy.fmt.repeat(' ', c.indent * 4), v(@tagName(code)), v(contextStr),
                v(c.unwindTempIndexStack.items.len), v(c.rega.nextTemp),
            });
        }
    }

    var tempRetainedStart: usize = undefined;
    var tempStart: usize = undefined;
    if (c.procs.items.len > 0) {
        tempRetainedStart = @intCast(c.unwindTempIndexStack.items.len);
        tempStart = c.rega.nextTemp;
    }

    switch (code) {
        .breakStmt          => try breakStmt(c, nodeId),
        .contStmt           => try contStmt(c, nodeId),
        .declareLocal       => try declareLocal(c, idx, nodeId),
        .declareLocalInit   => try declareLocalInit(c, idx, nodeId),
        .destrElemsStmt     => try destrElemsStmt(c, idx, nodeId),
        .exprStmt           => try exprStmt(c, idx, nodeId),
        .forIterStmt        => try forIterStmt(c, idx, nodeId),
        .forRangeStmt       => try forRangeStmt(c, idx, nodeId),
        .funcBlock          => try funcBlock(c, idx, nodeId),
        .ifStmt             => try genIfStmt(c, idx, nodeId),
        .mainBlock          => try mainBlock(c, idx, nodeId),
        .block              => try genBlock(c, idx, nodeId),
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
        .verbose            => try verbose(c, idx, nodeId),
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

    if (cy.Trace) {
        if (cy.verbose) {
            rt.logFmt("{}| end {} unw={} ntmp={}", &.{
                cy.fmt.repeat(' ', c.indent * 4), v(@tagName(code)),
                v(c.unwindTempIndexStack.items.len), v(c.rega.nextTemp),
            });
        }
    }

    // Check stack after statement.
    if (c.procs.items.len > 0) {
        if (c.unwindTempIndexStack.items.len != tempRetainedStart) {
            return c.reportErrorAt("Expected {} unwindable retained temps, got {}",
                &.{v(tempRetainedStart), v(c.unwindTempIndexStack.items.len)}, nodeId);
        }

        if (c.rega.nextTemp != tempStart) {
            return c.reportErrorAt("Expected {} temp start, got {}",
                &.{v(tempStart), v(c.rega.nextTemp)}, nodeId);
        }
    }
}

fn genChunkInner(c: *Chunk) !void {
    c.dataStack.clearRetainingCapacity();
    c.dataU8Stack.clearRetainingCapacity();
    c.listDataStack.clearRetainingCapacity();

    c.genValueStack.clearRetainingCapacity();
    c.indent = 0;

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
}

fn genAndPushExpr(c: *Chunk, idx: usize, cstr: Cstr) !GenValue {
    const val = try genExpr(c, idx, cstr);
    try c.genValueStack.append(c.alloc, val);
    return val;
}

fn genExpr(c: *Chunk, idx: usize, cstr: Cstr) anyerror!GenValue {
    const code = c.ir.getExprCode(idx);
    const nodeId = c.ir.getNode(idx);
    if (cy.Trace) {
        c.indent += 1;
        const contextStr = try c.encoder.formatNode(nodeId, &cy.tempBuf);
        if (cy.verbose) {
            rt.logFmt("{}( {}: `{}` {} unw={} ntmp={}", &.{
                cy.fmt.repeat(' ', c.indent * 4), v(@tagName(code)), v(contextStr), v(@tagName(cstr.type)),
                v(c.unwindTempIndexStack.items.len), v(c.rega.nextTemp),
            });
        }
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
        .switchExpr         => genSwitch(c, idx, cstr, nodeId),
        .symbol             => genSymbol(c, idx, cstr, nodeId),
        .throw              => genThrow(c, idx, nodeId),
        .truev              => genTrue(c, cstr, nodeId),
        .tryExpr            => genTryExpr(c, idx, cstr, nodeId),
        .typeSym            => genTypeSym(c, idx, cstr, nodeId),
        .varSym             => genVarSym(c, idx, cstr, nodeId),
        .blockExpr          => genBlockExpr(c, idx, cstr, nodeId),
        else => {
            rt.errZFmt(c.vm, "{}", .{code});
            return error.TODO;
        }
    };
    if (cy.Trace) {
        if (cy.verbose) {
            rt.logFmt("{}) end {} unw={} ntmp={}", &.{
                cy.fmt.repeat(' ', c.indent * 4), v(@tagName(code)),
                v(c.unwindTempIndexStack.items.len), v(c.rega.nextTemp),
            });
        }
        c.indent -= 1;
    }
    return res;
}

fn pushDebugLabel(c: *Chunk, idx: usize) !void {
    const data = c.ir.getStmtData(idx, .pushDebugLabel);
    try c.buf.pushDebugLabel(data.name);
}

fn mainBlock(c: *Chunk, idx: usize, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(idx, .mainBlock);
    log.tracev("main block: {}", .{data.maxLocals});

    try pushProc(c, .main, nodeId);
    c.curBlock.frameLoc = 0;

    try reserveMainRegs(c, data.maxLocals);

    var child = data.bodyHead;
    while (child != cy.NullId) {
        try genStmt(c, child);
        child = c.ir.getStmtNext(child);
    }

    if (shouldGenMainScopeReleaseOps(c.compiler)) {
        try genBlockReleaseLocals(c);
    }
    if (c.curBlock.endLocal != cy.NullU8) {
        try mainEnd(c, c.curBlock.endLocal);
    } else {
        try mainEnd(c, null);
    }
    try popProc(c);

    c.buf.mainStackSize = c.getMaxUsedRegisters();

    // Pop boundary index.
    try popUnwind(c, cy.NullU8);
}

fn funcBlock(c: *Chunk, idx: usize, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(idx, .funcBlock);
    const func = data.func;
    const paramsIdx = c.ir.advanceStmt(idx, .funcBlock);
    const params = c.ir.getArray(paramsIdx, ir.FuncParam, func.numParams);

    // Reserve jump to skip the body.
    const skipJump = try c.pushEmptyJump();

    const funcPc = c.buf.ops.items.len;

    try pushFuncBlock(c, data, params, nodeId);

    try genStmts(c, data.bodyHead);

    // Get stack size.
    const stackSize = c.getMaxUsedRegisters();

    // Patch empty func sym slot.
    const rtId = c.compiler.genSymMap.get(func).?.funcSym.id;
    const rtFunc = rt.FuncSymbol.initFunc(funcPc, stackSize, func.numParams, func.funcSigId, func.reqCallTypeCheck);
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

fn genCoresume(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const inst = try c.rega.selectForDstInst(cstr, true, nodeId);

    const childIdx = c.ir.advanceExpr(idx, .coresume);
    const childv = try genExpr(c, childIdx, Cstr.simpleRetain);

    try c.buf.pushOp2(.coresume, childv.local, inst.dst);

    try popTempAndUnwind(c, childv);

    return finishDstInst(c, inst, true);
}

fn genCoyield(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    _ = idx;
    try c.buf.pushOp2(.coyield, c.curBlock.startLocalReg, c.curBlock.nextLocalReg);
    // TODO: return coyield expression.
    return genNone(c, cstr, nodeId);
}

fn genCoinitCall(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const callIdx = c.ir.advanceExpr(idx, .coinitCall);
    const callCode: ir.ExprCode = @enumFromInt(c.ir.buf.items[callIdx]);
    const data = c.ir.getExprData(callIdx, .pre);

    const inst = try c.rega.selectForDstInst(cstr, true, nodeId);

    const tempStart = c.rega.nextTemp;
    var numArgs: u32 = 0;
    var args: []align(1) const u32 = undefined;
    if (callCode == .preCallFuncSym) {
        numArgs = data.callFuncSym.numArgs;

        const argsIdx = c.ir.advanceExpr(callIdx, .preCallFuncSym);
        args = c.ir.getArray(argsIdx, u32, numArgs);
    } else if (callCode == .preCall) {
        numArgs = data.call.numArgs;
        args = c.ir.getArray(data.call.args, u32, numArgs);

        const calleeIdx = c.ir.advanceExpr(callIdx, .preCall);
        const temp = try c.rega.consumeNextTemp();
        const val = try genAndPushExpr(c, calleeIdx, Cstr.toTempRetain(temp));
        try pushUnwindValue(c, val);
    } else return error.Unexpected;

    for (args) |argIdx| {
        const temp = try c.rega.consumeNextTemp();
        const val = try genAndPushExpr(c, argIdx, Cstr.toTempRetain(temp));
        try pushUnwindValue(c, val);
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
        try pushCall(c, callRet, numArgs, 1, callExprId);
    } else return error.Unexpected;

    try c.buf.pushOp(.coreturn);
    c.buf.setOpArgs1(coinitPc + 4, @intCast(c.buf.ops.items.len - coinitPc));

    try popFiberBlock(c);

    const argvs = popValues(c, numTotalArgs);
    try popTempAndUnwinds(c, argvs);

    return finishDstInst(c, inst, true);
}

fn genCast(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .cast);
    const childIdx = c.ir.advanceExpr(idx, .cast);

    if (!data.isRtCast) {
        return genExpr(c, childIdx, cstr);
    }

    const inst = try c.rega.selectForDstInst(cstr, false, nodeId);

    // TODO: If inst.dst is a temp, this should have a cstr of localOrExact.
    const childv = try genExpr(c, childIdx, Cstr.simple);
    try pushUnwindValue(c, childv);

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

    try popTempAndUnwind(c, childv);

    return finishDstInst(c, inst, childv.retained);
} 

const FieldOptions = struct {
    recv: ?GenValue = null,
};

fn genFieldDynamic(c: *Chunk, idx: usize, cstr: Cstr, opts: FieldOptions, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .fieldDynamic).dynamic;
    const recIdx = c.ir.advanceExpr(idx, .fieldDynamic);

    const inst = try c.rega.selectForDstInst(cstr, true, nodeId);
    const ownRecv = opts.recv == null;

    var recv: GenValue = undefined;
    if (opts.recv) |recv_| {
        recv = recv_;
    } else {
        recv = try genExpr(c, recIdx, Cstr.simple);
        try pushUnwindValue(c, recv);
    }

    const fieldId = try c.compiler.vm.ensureFieldSym(data.name);
    try pushField(c, recv.local, inst.dst, @intCast(fieldId), nodeId);

    if (ownRecv) {
        try popTempAndUnwind(c, recv);
        try releaseTempValue(c, recv, nodeId);
    }

    return finishDstInst(c, inst, true);
}

fn genFieldStatic(c: *Chunk, idx: usize, cstr: Cstr, opts: FieldOptions, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .fieldStatic).static;
    const recIdx = c.ir.advanceExpr(idx, .fieldStatic);

    const inst = try c.rega.selectForNoErrInst(cstr, true, nodeId);
    const ownRecv = opts.recv == null;

    var recv: GenValue = undefined;
    if (opts.recv) |recv_| {
        recv = recv_;
    } else {
        recv = try genExpr(c, recIdx, Cstr.simple);
    }

    // Prereleasing seems it could be problematic for code like:
    // var node = [Node ...]
    // node = node.next
    // But you can't initialize a Node if the type of next is also Node.
    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, nodeId);
    }
    try pushObjectField(c, recv.local, data.idx, inst.dst, nodeId);

    if (ownRecv) {
        try popTempValue(c, recv);
        // ARC cleanup.
        try releaseTempValue(c, recv, nodeId);
    }

    const willRetain = c.sema.isRcCandidateType(data.typeId);
    return finishNoErrInst(c, inst, willRetain);
}

fn genObjectInit(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .objectInit);

    const inst = try c.rega.selectForDstInst(cstr, true, nodeId);

    // TODO: Would it be faster/efficient to copy the fields into contiguous registers
    //       and copy all at once to heap or pass locals into the operands and iterate each and copy to heap?
    //       The current implementation is the former.
    const argsIdx = c.ir.advanceExpr(idx, .objectInit);
    const args = c.ir.getArray(argsIdx, u32, data.numArgs);
    const argStart = c.rega.getNextTemp();
    for (args) |argIdx| {
        const temp = try c.rega.consumeNextTemp();
        const val = try genAndPushExpr(c, argIdx, Cstr.toTempRetain(temp));
        try pushUnwindValue(c, val);
    }

    const sym = c.sema.getTypeSym(data.typeId);
    switch (sym.type) {
        .object => {
            const obj = sym.cast(.object);
            if (data.numFieldsToCheck > 0) {
                try c.pushFailableDebugSym(nodeId);
                try c.buf.pushOp2(.objectTypeCheck, argStart, @intCast(data.numFieldsToCheck));

                const checkFields = c.ir.getArray(data.fieldsToCheck, u8, data.numFieldsToCheck);

                for (checkFields) |fidx| {
                    const start = c.buf.ops.items.len;
                    try c.buf.pushOperands(&.{ @as(u8, @intCast(fidx)), 0, 0, 0, 0 });
                    c.buf.setOpArgU32(start + 1, obj.fields[fidx].type);
                }
            }
        },
        .enumType => {
        },
        else => return error.Unexpected,
    }

    try pushObjectInit(c, data.typeId, argStart, @intCast(data.numArgs), inst.dst, nodeId);

    const argvs = popValues(c, data.numArgs);
    try popTempAndUnwinds(c, argvs);

    return finishDstInst(c, inst, true);
}

fn breakStmt(c: *Chunk, nodeId: cy.NodeId) !void {
    // Release from startLocal of the first parent loop block.
    var idx = c.blocks.items.len-1;
    while (true) {
        const b = c.blocks.items[idx];
        if (b.isLoopBlock) {
            try genReleaseLocals(c, b.nextLocalReg, nodeId);
            break;
        }
        idx -= 1;
    }

    const pc = try c.pushEmptyJumpExt(c.desc(nodeId));
    try c.blockJumpStack.append(c.alloc, .{ .jumpT = .brk, .pc = pc });
}

fn contStmt(c: *Chunk, nodeId: cy.NodeId) !void {
    // Release from startLocal of the first parent loop block.
    var idx = c.blocks.items.len-1;
    while (true) {
        const b = c.blocks.items[idx];
        if (b.isLoopBlock) {
            try genReleaseLocals(c, b.nextLocalReg, nodeId);
            break;
        }
        idx -= 1;
    }

    const pc = try c.pushEmptyJumpExt(c.desc(nodeId));
    try c.blockJumpStack.append(c.alloc, .{ .jumpT = .cont, .pc = pc });
}

fn genThrow(c: *Chunk, idx: usize, nodeId: cy.NodeId) !GenValue {
    const childIdx = c.ir.advanceExpr(idx, .throw);
    const childv = try genExpr(c, childIdx, Cstr.simple);

    try c.pushFailableDebugSym(nodeId);
    try c.buf.pushOp1(.throw, childv.local);

    try popTempAndUnwind(c, childv);
    return GenValue.initNoValue();
}

fn genNone(c: *Chunk, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const inst = try c.rega.selectForNoErrInst(cstr, false, nodeId);
    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, nodeId);
    }
    try pushNone(c, inst.dst);
    return finishNoErrInst(c, inst, false);
}

fn pushNone(c: *Chunk, dst: LocalId) !void {
    try c.buf.pushOp1(.none, dst);
}

fn setCallObjSymTern(c: *Chunk, idx: usize, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(idx, .setCallObjSymTern).callObjSymTern;

    const inst = try beginCall(c, Cstr.none, false, nodeId);

    var args: [4]GenValue = undefined;
    const recIdx = c.ir.advanceStmt(idx, .setCallObjSymTern);
    var temp = try c.rega.consumeNextTemp();
    args[0] = try genExpr(c, recIdx, Cstr.toTemp(temp));
    try pushUnwindValue(c, args[0]);
    temp = try c.rega.consumeNextTemp();
    args[1] = try genExpr(c, data.index, Cstr.toTemp(temp));
    try pushUnwindValue(c, args[1]);
    temp = try c.rega.consumeNextTemp();
    args[2] = try genExpr(c, data.right, Cstr.toTemp(temp));
    try pushUnwindValue(c, args[2]);

    const mgId = try c.compiler.vm.ensureMethodGroup(data.name);
    try pushCallObjSym(c, inst.ret, 3,
        @intCast(mgId), @intCast(data.funcSigId), nodeId);

    var retained = try popTempAndUnwinds2(c, args[0..3]);

    // Include ret value for release.
    retained.len += 1;
    retained[retained.len-1] = GenValue.initTempValue(inst.ret, true);

    try pushReleaseVals(c, retained, nodeId);

    // Unwind return temp as well.
    c.rega.freeTemps(inst.numPreludeTemps + 1);
}

fn setIndex(c: *Chunk, idx: usize, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(idx, .setIndex).index;
    if (data.recvT != bt.List and data.recvT != bt.Map) {
        return error.Unexpected;
    }

    // None of the operands force a retain since it must mimic the generic $setIndex.
    const valsStart = c.genValueStack.items.len;

    const noneRet = try c.rega.consumeNextTemp();

    // Recv.
    const recIdx = c.ir.advanceStmt(idx, .setIndex);
    const recv = try genAndPushExpr(c, recIdx, Cstr.simple);
    try pushUnwindValue(c, recv);

    // Index
    const indexv = try genAndPushExpr(c, data.index, Cstr.simple);
    try pushUnwindValue(c, indexv);

    // RHS
    const rightv = try genAndPushExpr(c, data.right, Cstr.simple);
    try pushUnwindValue(c, rightv);

    const argvs = c.genValueStack.items[valsStart..];
    c.genValueStack.items.len = valsStart;

    if (data.recvT == bt.List) {
        try pushInlineTernExpr(c, .setIndexList, recv.local, indexv.local, rightv.local, noneRet, nodeId);
    } else if (data.recvT == bt.Map) {
        try pushInlineTernExpr(c, .setIndexMap, recv.local, indexv.local, rightv.local, noneRet, nodeId);
    } else return error.Unexpected;

    const retained = try popTempAndUnwinds2(c, argvs);
    c.rega.freeTemps(1);

    // ARC cleanup.
    try pushReleaseVals(c, retained, nodeId);
}

const SetFieldOptions = struct {
    recv: ?GenValue = null,
    right: ?GenValue = null,
};

fn setField(c: *Chunk, idx: usize, opts: SetFieldOptions, nodeId: cy.NodeId) !void {
    const setData = c.ir.getStmtData(idx, .setField).generic;
    const fieldIdx = c.ir.advanceStmt(idx, .setField);
    const data = c.ir.getExprData(fieldIdx, .fieldDynamic).dynamic;

    const fieldId = try c.compiler.vm.ensureFieldSym(data.name);
    const ownRecv = opts.recv == null;

    // LHS
    var recv: GenValue = undefined;
    if (opts.recv) |recv_| {
        recv = recv_;
    } else {
        const recIdx = c.ir.advanceExpr(fieldIdx, .fieldDynamic);
        recv = try genExpr(c, recIdx, Cstr.simple);
        try pushUnwindValue(c, recv);
    }

    // RHS
    var rightv: GenValue = undefined;
    if (opts.right) |right| {
        rightv = right;
    } else {
        rightv = try genExpr(c, setData.right, Cstr.simpleRetain);
        try pushUnwindValue(c, rightv);
    }

    // Performs runtime type check.
    const pc = c.buf.ops.items.len;
    try c.pushFailableDebugSym(nodeId);
    try c.buf.pushOpSlice(.setField, &.{ recv.local, 0, 0, rightv.local, 0, 0, 0, 0, 0 });
    c.buf.setOpArgU16(pc + 2, @intCast(fieldId));

    try popTempAndUnwind(c, rightv);
    if (ownRecv) {
        try popTempAndUnwind(c, recv);
        // ARC cleanup. Right is not released since it's being assigned to the field.
        try releaseTempValue(c, recv, nodeId);
    }
}

const SetObjectFieldOptions = struct {
    recv: ?GenValue = null,
    right: ?GenValue = null,
};

fn setObjectField(c: *Chunk, idx: usize, opts: SetObjectFieldOptions, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(idx, .setObjectField).generic;
    const requireTypeCheck = data.leftT.id != bt.Any and data.rightT.dynamic;
    const fieldIdx = c.ir.advanceStmt(idx, .setObjectField);
    const fieldData = c.ir.getExprData(fieldIdx, .fieldStatic).static;

    // Receiver.
    var recv: GenValue = undefined;
    if (opts.recv) |recv_| {
        recv = recv_;
    } else {
        const recIdx = c.ir.advanceExpr(fieldIdx, .fieldStatic);
        recv = try genExpr(c, recIdx, Cstr.simple);
        try pushUnwindValue(c, recv);
    }

    // Right.
    var rightv: GenValue = undefined;
    if (opts.right) |right| {
        rightv = right;
    } else {
        rightv = try genExpr(c, data.right, Cstr.simpleRetain);
        try pushUnwindValue(c, rightv);
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

    try popTempAndUnwind(c, rightv);
    if (ownRecv) {
        try popTempAndUnwind(c, recv);

        // ARC cleanup. Right is not released since it's being assigned to the field.
        try releaseTempValue(c, recv, nodeId);
    }
}

fn genError(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .errorv);

    const symId = try c.compiler.vm.ensureSymbol(data.name);
    const errval = cy.Value.initErrorSymbol(@intCast(symId));
    const constIdx = try c.buf.getOrPushConst(errval);

    const inst = try c.rega.selectForNoErrInst(cstr, false, nodeId);
    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, nodeId);
    }
    try genConst(c, constIdx, inst.dst, false, nodeId);
    return finishNoErrInst(c, inst, false);
}

fn genSymbol(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .symbol);

    const symId = try c.compiler.vm.ensureSymbol(data.name);
    const inst = try c.rega.selectForNoErrInst(cstr, false, nodeId);
    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, nodeId);
    }
    try c.buf.pushOp2(.tagLiteral, @intCast(symId), inst.dst);
    return finishNoErrInst(c, inst, false);
}

fn genTrue(c: *Chunk, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const inst = try c.rega.selectForNoErrInst(cstr, false, nodeId);
    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, nodeId);
    }
    try c.buf.pushOp1(.true, inst.dst);
    return finishNoErrInst(c, inst, false);
}

fn genFalse(c: *Chunk, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const inst = try c.rega.selectForNoErrInst(cstr, false, nodeId);
    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, nodeId);
    }
    try c.buf.pushOp1(.false, inst.dst);
    return finishNoErrInst(c, inst, false);
}

fn genInt(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .int);

    const inst = try c.rega.selectForNoErrInst(cstr, false, nodeId);
    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, nodeId);
    }
    _ = try genConstIntExt(c, data.val, inst.dst, c.desc(nodeId));
    return finishNoErrInst(c, inst, false);
}

fn genFloat(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .float);
    const inst = try c.rega.selectForNoErrInst(cstr, false, nodeId);
    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, nodeId);
    }
    _ = try genConstFloat(c, data.val, inst.dst, nodeId);
    return finishNoErrInst(c, inst, false);
}

fn genStringTemplate(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .stringTemplate);
    const strsIdx = c.ir.advanceExpr(idx, .stringTemplate);
    const strs = c.ir.getArray(strsIdx, []const u8, data.numExprs+1);
    const args = c.ir.getArray(data.args, u32, data.numExprs);

    const inst = try c.rega.selectForDstInst(cstr, true, nodeId); 
    const argStart = c.rega.getNextTemp();

    for (args, 0..) |argIdx, i| {
        const temp = try c.rega.consumeNextTemp();
        if (cy.Trace and temp != argStart + i) return error.Unexpected;
        const val = try genAndPushExpr(c, argIdx, Cstr.toTemp(temp));
        try pushUnwindValue(c, val);
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
    const retained = try popTempAndUnwinds2(c, argvs);
    try pushReleaseVals(c, retained, nodeId);

    return finishDstInst(c, inst, true);
}

fn genString(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .string);
    const str = try c.unescapeString(data.literal);
    const inst = try c.rega.selectForNoErrInst(cstr, true, nodeId);
    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, nodeId);
    }
    try pushStringConst(c, str, inst.dst, nodeId);
    return finishNoErrInst(c, inst, true);
}

fn pushStringConst(c: *Chunk, str: []const u8, dst: RegisterId, nodeId: cy.NodeId) !void {
    const idx = try c.buf.getOrPushStaticStringConst(str);
    try genConst(c, idx, dst, true, nodeId);
}

fn genSlice(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .preSlice).slice;
    const inst = try c.rega.selectForDstInst(cstr, true, nodeId);

    if (data.recvT != bt.List) {
        return error.Unexpected;
    }

    var args: [3]GenValue = undefined;
    const recIdx = c.ir.advanceExpr(idx, .preSlice);
    args[0] = try genExpr(c, recIdx, Cstr.simple);
    try pushUnwindValue(c, args[0]);
    args[1] = try genExpr(c, data.left, Cstr.simple);
    try pushUnwindValue(c, args[1]);
    args[2] = try genExpr(c, data.right, Cstr.simple);
    try pushUnwindValue(c, args[2]);

    try pushInlineTernExpr(c, .sliceList, args[0].local, args[1].local, args[2].local, inst.dst, nodeId);

    const retained = try popTempAndUnwinds2(c, &args);

    // ARC cleanup.
    try pushReleaseVals(c, retained, nodeId);

    return finishDstInst(c, inst, true);
}

fn genUnOp(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .preUnOp).unOp;
    const childIdx = c.ir.advanceExpr(idx, .preUnOp);
    const inst = try c.rega.selectForDstInst(cstr, false, nodeId);

    const canUseDst = !c.isParamOrLocalVar(inst.dst);
    const childv = try genExpr(c, childIdx, Cstr.preferVolatileIf(canUseDst, inst.dst));
    try pushUnwindValue(c, childv);

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

    if (childv.isTemp()) {
        if (childv.data.temp != inst.dst) {
            try popTemp(c, childv.data.temp);
        }
        if (try popUnwindValue(c, childv)) {
            try pushRelease(c, childv.local, nodeId);
        }
    }

    return finishDstInst(c, inst, false);
}

fn genCallObjSym(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .preCallObjSym).callObjSym;

    const inst = try beginCall(c, cstr, false, nodeId);

    // Receiver.
    const recIdx = c.ir.advanceExpr(idx, .preCallObjSym);
    const argStart = c.rega.nextTemp;
    var temp = try c.rega.consumeNextTemp();
    const recv = try genExpr(c, recIdx, Cstr.toTemp(temp));
    try pushUnwindValue(c, recv);
    try c.genValueStack.append(c.alloc, recv);

    const args = c.ir.getArray(data.args, u32, data.numArgs);
    for (args, 0..) |argIdx, i| {
        temp = try c.rega.consumeNextTemp();
        if (cy.Trace and temp != argStart + 1 + i) return error.Unexpected;
        const val = try genAndPushExpr(c, argIdx, Cstr.toTemp(temp));
        try pushUnwindValue(c, val);
    }

    const mgId = try c.compiler.vm.ensureMethodGroup(data.name);
    try pushCallObjSym(c, inst.ret, data.numArgs + 1,
        @intCast(mgId), @intCast(data.funcSigId), nodeId);

    const argvs = popValues(c, data.numArgs+1);
    try checkArgs(argStart, argvs);

    const retained = try popTempAndUnwinds2(c, argvs);
    try pushReleaseVals(c, retained, nodeId);

    return endCall(c, inst, true);
}

fn genCallFuncSym(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .preCallFuncSym).callFuncSym;

    if (data.func.type == .hostInlineFunc) {
        // TODO: Make this handle all host inline funcs.
        if (@intFromPtr(data.func.data.hostInlineFunc.ptr) == @intFromPtr(cy.builtins.appendList)) {
            const inst = try c.rega.selectForDstInst(cstr, false, nodeId);
            const args = c.ir.getArray(data.args, u32, data.numArgs);
            const recv = try genExpr(c, args[0], Cstr.simple);
            try pushUnwindValue(c, recv);
            const itemv = try genExpr(c, args[1], Cstr.simple);
            try pushUnwindValue(c, itemv);

            if (data.hasDynamicArg) {
                try pushTypeCheck(c, recv.local, bt.List, nodeId);
            }

            try pushInlineBinExpr(c, .appendList, recv.local, itemv.local, inst.dst, nodeId);

            try popTempAndUnwind(c, itemv);
            try popTempAndUnwind(c, recv);

            // ARC cleanup.
            try releaseTempValue2(c, recv, itemv, nodeId);

            return finishDstInst(c, inst, false);
        } else {
            return error.UnsupportedInline;
        }
    } else {
        const inst = try beginCall(c, cstr, false, nodeId);

        const args = c.ir.getArray(data.args, u32, data.numArgs);

        const argStart = c.rega.nextTemp;
        for (args, 0..) |argIdx, i| {
            const temp = try c.rega.consumeNextTemp();
            if (cy.Trace and temp != argStart + i) return error.Unexpected;
            const val = try genAndPushExpr(c, argIdx, Cstr.toTemp(temp));
            try pushUnwindValue(c, val);
        }

        if (data.hasDynamicArg) {
            try genCallTypeCheck(c, inst.ret + cy.vm.CallArgStart, data.numArgs, data.func.funcSigId, nodeId);
        }

        const rtId = c.compiler.genSymMap.get(data.func).?.funcSym.id;
        try pushCallSym(c, inst.ret, data.numArgs, 1, rtId, nodeId);

        const argvs = popValues(c, data.numArgs);
        try checkArgs(argStart, argvs);

        const retained = try popTempAndUnwinds2(c, argvs);
        try pushReleaseVals(c, retained, nodeId);

        const retRetained = c.sema.isRcCandidateType(data.func.retType);
        return endCall(c, inst, retRetained);
    }
}

fn genCall(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .preCall).call;
    const inst = try beginCall(c, cstr, true, nodeId);

    // Callee.
    const calleeIdx = c.ir.advanceExpr(idx, .preCall);
    const argStart = c.rega.nextTemp;
    var temp = try c.rega.consumeNextTemp();
    const calleev = try genExpr(c, calleeIdx, Cstr.toTemp(temp));
    try pushUnwindValue(c, calleev);
    try c.genValueStack.append(c.alloc, calleev);

    // Args.
    const args = c.ir.getArray(data.args, u32, data.numArgs);
    for (args, 0..) |argIdx, i| {
        temp = try c.rega.consumeNextTemp();
        if (cy.Trace and temp != argStart + 1 + i) return error.Unexpected;
        const val = try genAndPushExpr(c, argIdx, Cstr.toTemp(temp));
        try pushUnwindValue(c, val);
    }

    const calleeAndArgvs = popValues(c, data.numArgs+1);
    try checkArgs(argStart, calleeAndArgvs);

    try c.pushFailableDebugSym(nodeId);
    try c.buf.pushOp3Ext(.call, inst.ret, data.numArgs, 1, c.desc(nodeId));

    const retained = try popTempAndUnwinds2(c, calleeAndArgvs);
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

fn genCallObjSymUnOp(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .preCallObjSymUnOp).callObjSymUnOp;
    const inst = try beginCall(c, cstr, false, nodeId);

    const childIdx = c.ir.advanceExpr(idx, .preCallObjSymUnOp);

    var temp = try c.rega.consumeNextTemp();
    const childv = try genExpr(c, childIdx, Cstr.toTemp(temp));
    try pushUnwindValue(c, childv);

    const mgId = try getUnMGID(c, data.op);

    try pushCallObjSym(c, inst.ret, 1, @intCast(mgId), @intCast(data.funcSigId), nodeId);

    try popTempAndUnwind(c, childv);
    try releaseTempValue(c, childv, nodeId);

    return endCall(c, inst, true);
}

fn genCallObjSymBinOp(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .preCallObjSymBinOp).callObjSymBinOp;
    const leftIdx = c.ir.advanceExpr(idx, .preCallObjSymBinOp);

    const inst = try beginCall(c, cstr, false, nodeId);

    const leftPc = c.buf.ops.items.len;
    // copy inst doesn't create a debug symbol, otherwise that needs to be moved as well.

    var temp = try c.rega.consumeNextTemp();
    const leftv = try genExpr(c, leftIdx, Cstr.toTemp(temp));
    try pushUnwindValue(c, leftv);

    const leftCopySave = extractIfCopyInst(c, leftPc);
    const hasLeftCopy = leftCopySave != null;

    const rightPc = c.buf.ops.items.len;
    temp = try c.rega.consumeNextTemp();
    const rightv = try genExpr(c, data.right, Cstr.toTemp(temp));
    try pushUnwindValue(c, rightv);

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

    try popTempAndUnwind(c, rightv);
    try popTempAndUnwind(c, leftv);
    try releaseTempValue2(c, leftv, rightv, nodeId);

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

fn genBinOp(c: *Chunk, idx: usize, cstr: Cstr, opts: BinOpOptions, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .preBinOp).binOp;
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
    const inst = try c.rega.selectForDstInst(cstr, willRetain, nodeId);

    var prefer = PreferDst{
        .dst = inst.dst,
        .canUseDst = !c.isParamOrLocalVar(inst.dst),
    };

    // Lhs.
    var leftv: GenValue = undefined;
    if (opts.left) |left| {
        leftv = left;
    } else {
        const leftIdx = c.ir.advanceExpr(idx, .preBinOp);
        leftv = try genExpr(c, leftIdx, Cstr.preferVolatileIf(prefer.canUseDst, prefer.dst));
        try pushUnwindValue(c, leftv);
    }

    // Rhs.
    const rightv = try genExpr(c, data.right, prefer.nextCstr(leftv));
    try pushUnwindValue(c, rightv);

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

    const tempRight = rightv.isTemp();
    if (tempRight and rightv.data.temp != inst.dst) {
        try popTemp(c, rightv.data.temp);
    }
    const retainedRight = try popUnwindValue(c, rightv);

    var retainedLeft = false;
    if (opts.left == null) {
        const tempLeft = leftv.isTemp();
        if (tempLeft and leftv.data.temp != inst.dst) {
            try popTemp(c, leftv.data.temp);
        }
        retainedLeft = try popUnwindValue(c, leftv);
    }

    // ARC cleanup.
    try releaseCond2(c, retainedLeft, leftv.local, retainedRight, rightv.local, nodeId);

    return finishDstInst(c, inst, retained);
}

fn genCaptured(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .captured);

    const inst = try c.rega.selectForNoErrInst(cstr, true, nodeId);
    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, nodeId);
    }
    try c.buf.pushOp3(.captured, c.curBlock.closureLocal, data.idx, inst.dst);

    return finishNoErrInst(c, inst, true);
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

fn genLocalReg(c: *Chunk, reg: RegisterId, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const local = getLocalInfo(c, reg);

    if (!local.some.lifted) {
        const inst = try c.rega.selectForLocalInst(cstr, reg, local.some.rcCandidate, nodeId);
        if (inst.dst != reg) {
            if (inst.retainSrc) {
                if (inst.releaseDst) {
                    try c.pushOp(.copyRetainRelease, &.{ reg, inst.dst }, nodeId);
                } else {
                    try c.pushOp(.copyRetainSrc, &.{ reg, inst.dst }, nodeId);
                }
            } else {
                if (inst.releaseDst) {
                    try c.pushOp(.copyReleaseDst, &.{ reg, inst.dst }, nodeId);
                } else {
                    try c.pushOp(.copy, &.{ reg, inst.dst }, nodeId);
                }
            }
        } else {
            // Nop. When the cstr allows returning the local itself.
            if (inst.retainSrc) {
                try c.pushOp(.retain, &.{ reg }, nodeId);
            } else {
                // Nop.
            }
        }
        return finishCopyInst(c, inst, inst.retainSrc);
    } else {
        // Special case when src local is lifted.
        var retainSrc = false;
        if (local.some.rcCandidate) {
            switch (cstr.type) {
                .liftedLocal,
                .localReg => {
                    retainSrc = true;
                },
                .simple => {
                    if (cstr.data.simple.retain) {
                        retainSrc = true;
                    }
                },
                .tempReg => {
                    if (cstr.data.reg.retain) {
                        retainSrc = true;
                    }
                },
                else => {},
            }
        }
        const inst = try c.rega.selectForDstInst(cstr, retainSrc, nodeId);

        if (retainSrc) {
            try c.pushOp(.boxValueRetain, &.{ reg, inst.dst }, nodeId);
        } else {
            try c.pushOp(.boxValue, &.{ reg, inst.dst }, nodeId);
        }

        return finishDstInst(c, inst, retainSrc);
    }
}

fn genLocal(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .local);
    const reg = toLocalReg(c, data.id);
    return genLocalReg(c, reg, cstr, nodeId);
}

fn genEnumMemberSym(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .enumMemberSym);

    const inst = try c.rega.selectForNoErrInst(cstr, false, nodeId);
    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, nodeId);
    }

    const val = cy.Value.initEnum(@intCast(data.type), @intCast(data.val));
    const constIdx = try c.buf.getOrPushConst(val);
    try genConst(c, constIdx, inst.dst, false, nodeId);

    return finishNoErrInst(c, inst, false);
}

fn genVarSym(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .varSym);

    const varId = c.compiler.genSymMap.get(data.sym).?.varSym.id;

    const inst = try c.rega.selectForNoErrInst(cstr, true, nodeId);
    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, nodeId);
    }

    try c.pushOptionalDebugSym(nodeId);       
    const pc = c.buf.len();
    try c.buf.pushOp3(.staticVar, 0, 0, inst.dst);
    c.buf.setOpArgU16(pc + 1, @intCast(varId));

    return finishNoErrInst(c, inst, true);
}

fn genFuncSym(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .funcSym);

    const inst = try c.rega.selectForDstInst(cstr, true, nodeId);

    const pc = c.buf.len();
    try c.pushOptionalDebugSym(nodeId);
    try c.buf.pushOp3(.staticFunc, 0, 0, inst.dst);
    const rtId = c.compiler.genSymMap.get(data.func).?.funcSym.id;
    c.buf.setOpArgU16(pc + 1, @intCast(rtId));

    return finishDstInst(c, inst, true);
}

fn genTypeSym(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .typeSym);

    const inst = try c.rega.selectForDstInst(cstr, true, nodeId);
    try c.buf.pushOp1(.sym, @intFromEnum(cy.heap.MetaTypeKind.object));
    try c.buf.pushOperandsRaw(std.mem.asBytes(&data.typeId));
    try c.buf.pushOperand(inst.dst);

    return finishDstInst(c, inst, true);
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
                    .lifted = param.lifted,
                },
            };

            // Copy param to local.
            if (param.lifted) {
                try c.pushFailableDebugSym(c.curBlock.debugNodeId);
                // Retain param and box.
                try c.buf.pushOp1(.retain, nextReg);
                try c.buf.pushOp2(.box, nextReg, reg);
            } else {
                try c.pushOp(.copyRetainSrc, &.{ nextReg, reg }, c.curBlock.debugNodeId);
            }

            paramCopyIdx += 1;
        } else {
            c.genIrLocalMapStack.items[c.curBlock.irLocalMapStart + i] = nextReg;
            c.genLocalStack.items[c.curBlock.localStart + 4 + 1 + i] = .{
                .some = .{
                    .owned = false,
                    .rcCandidate = c.sema.isRcCandidateType(param.declType),
                    .lifted = false,
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
    const setData = c.ir.getStmtData(idx, .setFuncSym).generic;
    const funcSymIdx = c.ir.advanceStmt(idx, .setFuncSym);
    const data = c.ir.getExprData(funcSymIdx, .funcSym);

    const rightv = try genExpr(c, setData.right, Cstr.simpleRetain);
    try pushUnwindValue(c, rightv);

    const pc = c.buf.len();
    try c.pushFailableDebugSym(nodeId);
    try c.buf.pushOp3(.setStaticFunc, 0, 0, rightv.local);
    const rtId = c.compiler.genSymMap.get(data.func).?.funcSym.id;
    c.buf.setOpArgU16(pc + 1, @intCast(rtId));

    try popTempAndUnwind(c, rightv);
}

fn setVarSym(c: *Chunk, idx: usize, nodeId: cy.NodeId) !void {
    _ = nodeId;
    const data = c.ir.getStmtData(idx, .setVarSym).generic;
    const varSymIdx = c.ir.advanceStmt(idx, .setVarSym);
    const varSym = c.ir.getExprData(varSymIdx, .varSym);

    const id = c.compiler.genSymMap.get(varSym.sym).?.varSym.id;
    const rightv = try genExpr(c, data.right, Cstr.toVarSym(id));
    _ = try popUnwindValue(c, rightv);
}

fn declareLocalInit(c: *Chunk, idx: u32, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(idx, .declareLocalInit);

    var reg: u8 = undefined;
    if (!data.zeroMem) {
        // Don't advance nextLocalReg yet since the rhs hasn't generated so the
        // alive locals should not include this declaration.
        reg = try reserveLocalReg(c, data.id, data.declType, data.lifted, nodeId, false);
    } else {
        reg = try reserveLocalReg(c, data.id, data.declType, data.lifted, nodeId, true);
        try c.buf.pushOp2Ext(.constI8, 0, reg, c.desc(nodeId));
    }

    const val = try genExpr(c, data.init, Cstr.toLocal(reg, false));

    const local = getLocalInfoPtr(c, reg);

    if (local.some.lifted) {
        try c.pushOptionalDebugSym(nodeId);
        try c.buf.pushOp2(.box, reg, reg);
    }
    local.some.rcCandidate = val.retained;

    if (!data.zeroMem) {
        // rhs has generated, increase `nextLocalReg`.
        c.curBlock.nextLocalReg += 1;
    }
    log.tracev("declare {}, rced: {} ", .{val.local, local.some.rcCandidate});
}

fn declareLocal(c: *Chunk, idx: u32, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(idx, .declareLocal);

    const reg = try reserveLocalReg(c, data.id, data.declType, data.lifted, nodeId, true);

    // Not yet initialized, so it does not have a refcount.
    getLocalInfoPtr(c, reg).some.rcCandidate = false;
}

fn setCaptured(c: *Chunk, idx: usize, nodeId: cy.NodeId) !void {
    _ = nodeId;
    const data = c.ir.getStmtData(idx, .setCaptured).generic;
    const capIdx = c.ir.advanceStmt(idx, .setCaptured);
    const capData = c.ir.getExprData(capIdx, .captured);

    // RHS.
    // const dstRetained = c.sema.isRcCandidateType(data.leftT.id);
    _ = try genExpr(c, data.right, Cstr.toCaptured(capData.idx));
}

fn irSetLocal(c: *Chunk, idx: usize, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(idx, .setLocal).generic;
    const localIdx = c.ir.advanceStmt(idx, .setLocal);
    const localData = c.ir.getExprData(localIdx, .local);
    try setLocal(c, localData, data.right, nodeId, .{});
}

const SetLocalOptions = struct {
    rightv: ?GenValue = null,
    extraIdx: ?u32 = null,
};

fn setLocal(c: *Chunk, data: ir.Local, rightIdx: u32, nodeId: cy.NodeId, opts: SetLocalOptions) !void {
    const reg = toLocalReg(c, data.id);
    const local = getLocalInfo(c, reg);

    var dst: Cstr = undefined;
    if (local.some.lifted) {
        dst = Cstr.toLiftedLocal(reg, local.some.rcCandidate);
    } else {
        dst = Cstr.toLocal(reg, local.some.rcCandidate);
    }

    var rightv: GenValue = undefined;
    if (opts.rightv) |rightv_| {
        rightv = rightv_;
        if (rightv.local != reg) {
            // Move to local.
            _ = try genToExactDesc(c, rightv, dst, nodeId, opts.extraIdx);
        }
    } else {
        rightv = try genExpr(c, rightIdx, dst);
    }

    // Update retained state.
    getLocalInfoPtr(c, reg).some.rcCandidate = rightv.retained;
}

fn setLocalType(c: *Chunk, idx: usize) !void {
    const data = c.ir.getStmtData(idx, .setLocalType);
    const reg = toLocalReg(c, data.local);
    const local = getLocalInfoPtr(c, reg);
    local.some.rcCandidate = c.sema.isRcCandidateType(data.type.id);
}

fn opSet(c: *Chunk, idx: usize, nodeId: cy.NodeId) !void {
    _ = nodeId;
    // TODO: Perform optimizations depending on the next set* code.
    const setIdx = c.ir.advanceStmt(idx, .opSet);
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
//             const local = Task.initRes(regValue(c, dst, false));
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

fn genOr(c: *Chunk, idx: usize, data: ir.BinOp, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const leftIdx = c.ir.advanceExpr(idx, .preBinOp);

    if (cstr.isExact()) {
        const leftv = try genExpr(c, leftIdx, Cstr.simple);
        const jumpFalse = try c.pushEmptyJumpNotCond(leftv.local);

        // Gen left to finalCstr. Require retain to merge with right.
        const finalLeftv = try genToExact(c, leftv, cstr.toRetained(), nodeId);
        const jumpEnd = try c.pushEmptyJump();

        // RHS, gen to final dst.
        c.patchJumpNotCondToCurPc(jumpFalse);
        try releaseTempValue(c, leftv, nodeId);
        const rightv = try genExpr(c, data.right, cstr);

        c.patchJumpToCurPc(jumpEnd);
        try popTempValue(c, leftv);

        return regValue(c, rightv.local, finalLeftv.retained or rightv.retained);
    } else {
        // Can use cond as merged result. Require retain to merge with right.
        const cond = try c.rega.consumeNextTemp();
        const condCstr = Cstr.toTempRetain(cond);
        const leftv = try genExpr(c, leftIdx, condCstr);
        const jumpTrue = try c.pushEmptyJumpCond(cond);

        const rightv = try genExpr(c, data.right, condCstr);
        c.patchJumpCondToCurPc(jumpTrue);

        return regValue(c, cond, leftv.retained or rightv.retained);
    }
}

fn genAndOp(c: *Chunk, idx: usize, data: ir.BinOp, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const leftIdx = c.ir.advanceExpr(idx, .preBinOp);

    if (cstr.isExact()) {
        const leftv = try genExpr(c, leftIdx, Cstr.simple);
        const jumpFalse = try c.pushEmptyJumpNotCond(leftv.local);

        // RHS. Goes to final dst whether true or false.
        const rightv = try genExpr(c, data.right, cstr);
        try releaseTempValue(c, leftv, nodeId);
        const jumpEnd = try c.pushEmptyJump();

        // Copy left to dst. Can assume leftv is a non-rc value.
        c.patchJumpNotCondToCurPc(jumpFalse);
        _ = try genToExact(c, leftv, cstr, nodeId);

        c.patchJumpToCurPc(jumpEnd);

        try popTempAndUnwind(c, leftv);
        return regValue(c, rightv.local, rightv.retained);
    } else {
        // Merged branch result.
        const res = try c.rega.consumeNextTemp();
        const leftv = try genExpr(c, leftIdx, Cstr.simple);
        const jumpFalse = try c.pushEmptyJumpNotCond(leftv.local);
        try popTempValue(c, leftv);

        // RHS. Goes to res whether true or false.
        try releaseTempValue(c, leftv, nodeId);
        const rightv = try genExpr(c, data.right, Cstr.toTempRetain(res));
        const jumpEnd = try c.pushEmptyJump();

        c.patchJumpNotCondToCurPc(jumpFalse);
        // Require retain to merge with right.
        _ = try genToExact(c, leftv, Cstr.toTempRetain(res), nodeId);

        c.patchJumpToCurPc(jumpEnd);

        return regValue(c, res, leftv.retained or rightv.retained);
    }
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
    const childIdx = c.ir.advanceStmt(idx, .retExprStmt);

    // TODO: If the returned expr is a local, consume the local after copying to reg 0.
    var childv: GenValue = undefined;
    if (c.curBlock.type == .main) {
        // Main block.
        childv = try genExpr(c, childIdx, Cstr.simpleRetain);
    } else {
        childv = try genExpr(c, childIdx, Cstr.ret);
    }

    try popTempAndUnwind(c, childv);

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

        lifted: bool,
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
            if (local.some.rcCandidate or local.some.lifted) {
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
    log.tracev("reserveMainRegs maxLocals={}\n", .{maxLocals});
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
    finalDst: ?Cstr,
    nodeId: cy.NodeId,
};

/// Returns gen strategy and advances the temp local.
pub fn beginCall(c: *Chunk, cstr: Cstr, hasCalleeValue: bool, nodeId: cy.NodeId) !CallInst {
    var ret: RegisterId = undefined;
    var allocTempRet = true;

    // Optimization: Check to use dst cstr as ret.
    if (cstr.type == .tempReg or cstr.type == .localReg) {
        if (!cstr.data.reg.releaseDst and cstr.data.reg.dst + 1 == c.rega.nextTemp) {
            if (cstr.data.reg.dst != 0) {
                ret = cstr.data.reg.dst;
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
    var finalDst: ?Cstr = null;
    switch (cstr.type) {
        .tempReg,
        .localReg => {
            if (cstr.data.reg.dst != ret) {
                finalDst = cstr;
            }
        },
        .varSym,
        .liftedLocal => {
            finalDst = cstr;
        },
        else => {},
    }
    return .{
        .ret = ret,
        .finalDst = finalDst,
        .numPreludeTemps = numPreludeTemps,
        .nodeId = nodeId,
    };
}

fn finishInst(c: *Chunk, dst: RegisterId, finalDstOpt: ?Cstr, retainedToDst: bool, nodeId: cy.NodeId) !GenValue {
    if (finalDstOpt) |finalDst| {
        const val = regValue(c, dst, retainedToDst);
        return try genToFinalDst(c, val, finalDst, nodeId);
    } else {
        return regValue(c, dst, retainedToDst);
    }
}

fn finishCallInst(c: *Chunk, inst: CallInst, retainedToDst: bool) !GenValue {
    return finishInst(c, inst.ret, inst.finalDst, retainedToDst, inst.nodeId);
}

fn finishDstInst(c: *Chunk, inst: cy.register.DstInst, retainedToDst: bool) !GenValue {
    return finishInst(c, inst.dst, inst.finalDst, retainedToDst, inst.nodeId);
}

fn finishNoErrInst(c: *Chunk, inst: cy.register.NoErrInst, retainedToDst: bool) !GenValue {
    return finishInst(c, inst.dst, inst.finalDst, retainedToDst, inst.nodeId);
}

fn finishCopyInst(c: *Chunk, inst: cy.register.CopyInst, retainedToDst: bool) !GenValue {
    return finishInst(c, inst.dst, inst.finalDst, retainedToDst, inst.nodeId);
}

fn shouldRetain(c: *Chunk, retainCstr: bool, src: GenValue) bool {
    if (!retainCstr) return false;

    if (!src.isTemp()) {
        const local = getLocalInfo(c, src.local);
        if (local.some.rcCandidate) {
            return true;
        }
        return false;
    } else {
        if (src.retained) {
            // Moving from temp to another location doesn't require a retain.
            return false;
        } else {
            return true;
        }
    }
}

fn genToExact(c: *Chunk, src: GenValue, dst: Cstr, nodeId: cy.NodeId) !GenValue {
    return genToExactDesc(c, src, dst, nodeId, null);
}

/// Assumes `src` is a register and `dst` is an exact constraint.
fn genToExactDesc(c: *Chunk, src: GenValue, dst: Cstr, nodeId: cy.NodeId, extraIdx: ?u32) !GenValue {
    switch (dst.type) {
        .localReg,
        .tempReg => {
            const reg = dst.data.reg;
            if (src.local == reg.dst) return error.Unexpected;

            const retain = shouldRetain(c, reg.retain, src);
            if (reg.releaseDst) {
                if (retain) {
                    try c.pushOpDesc(.copyRetainRelease, &.{ src.local, reg.dst }, nodeId, extraIdx);
                } else {
                    try c.pushOpDesc(.copyReleaseDst, &.{ src.local, reg.dst }, nodeId, extraIdx);
                }
            } else {
                if (retain) {
                    try c.pushOpDesc(.copyRetainSrc, &.{ src.local, reg.dst }, nodeId, extraIdx);
                } else {
                    try c.pushOpDesc(.copy, &.{ src.local, reg.dst }, nodeId, extraIdx);
                }
            }
            if (retain) {
                return regValue(c, reg.dst, true);
            } else {
                return regValue(c, reg.dst, src.retained);
            }
        },
        .liftedLocal => {
            const lifted = dst.data.liftedLocal;
            if (src.local == lifted.reg) return error.Unexpected;

            if (lifted.rcCandidate) {
                try c.pushOpDesc(.setBoxValueRelease, &.{ lifted.reg, src.local }, nodeId, extraIdx);
            } else {
                try c.pushOpDesc(.setBoxValue, &.{ lifted.reg, src.local }, nodeId, extraIdx);
            }
            return GenValue.initRetained(src.retained);
        },
        .varSym => {
            // Set var assumes retained src.
            const pc = c.buf.len();
            try c.pushOpDesc(.setStaticVar, &.{ 0, 0, src.local }, nodeId, extraIdx);
            c.buf.setOpArgU16(pc + 1, @intCast(dst.data.varSym));
            return GenValue.initRetained(src.retained);
        },
        .captured => {
            const captured = dst.data.captured;
            try c.pushOpDesc(.setCaptured, &.{ c.curBlock.closureLocal, captured.idx, src.local }, nodeId, extraIdx);
            return GenValue.initRetained(src.retained);
        },
        else => {
            log.tracev("{}", .{dst.type});
            return error.NotExactCstr;
        },
    }
}

fn genToFinalDst(c: *Chunk, val: GenValue, dst: Cstr, nodeId: cy.NodeId) !GenValue {
    log.tracev("genToFinalDst src: {} dst: {s}", .{val.local, @tagName(dst.type)});

    const res = try genToExact(c, val, dst, nodeId);

    // Check to remove the temp that is used to move to final dst.
    if (val.isTemp()) try popTemp(c, val.data.temp);
    return res;
}

pub fn endCall(c: *Chunk, inst: CallInst, retained: bool) !GenValue {
    c.rega.freeTemps(inst.numPreludeTemps);
    return finishCallInst(c, inst, retained);
}

fn forIterStmt(c: *Chunk, idx: usize, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(idx, .forIterStmt);

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
    const iterIdx = c.ir.advanceStmt(idx, .forIterStmt);
    const iterv = try genExpr(c, iterIdx, Cstr.toTemp(iterTemp + cy.vm.CallArgStart));

    const node = c.nodes[nodeId];
    const header = c.nodes[node.head.forIterStmt.header];
    const iterNodeId = header.head.forIterHeader.iterable;
    const eachNodeId = header.head.forIterHeader.eachClause;

    const funcSigId = try c.sema.ensureFuncSig(&.{ bt.Any }, bt.Any);
    var extraIdx = try c.fmtExtraDesc("iterator()", .{});
    try pushCallObjSymExt(c, iterTemp, 1,
        @intCast(c.compiler.iteratorMGID), @intCast(funcSigId),
        iterNodeId, extraIdx);

    try releaseIf(c, iterv.retained, iterv.local, iterNodeId);
    _ = try pushUnwind(c, iterTemp);

    try pushBlock(c, true, nodeId);
    try genStmts(c, data.declHead);

    const bodyPc = c.buf.ops.items.len;

    // next()
    try genIterNext(c, iterTemp, data.countLocal != null, iterNodeId);
    if (data.eachLocal) |eachLocal| {
        extraIdx = try c.fmtExtraDesc("copy next() to local", .{});
        const resTemp = regValue(c, iterTemp + 1, true);
        try setLocal(c, .{ .id = eachLocal }, undefined, eachNodeId, .{ .rightv = resTemp, .extraIdx = extraIdx });
    }
    if (data.countLocal) |countLocal| {
        extraIdx = try c.fmtExtraDesc("copy count to local", .{});
        const countTemp = regValue(c, iterTemp - 1, false);
        try setLocal(c, .{ .id = countLocal }, undefined, eachNodeId, .{ .rightv = countTemp, .extraIdx = extraIdx });
    }
    const hasCounter = data.countLocal != null;

    const resNoneJump = try c.pushEmptyJumpNone(iterTemp + 1);

    const jumpStackSave: u32 = @intCast(c.blockJumpStack.items.len);

    try genStmts(c, data.bodyHead);

    const b = c.blocks.getLast();
    try genReleaseLocals(c, b.nextLocalReg, b.nodeId);
    // Pop sub block.
    try popLoopBlock(c);

    const contPc = c.buf.ops.items.len;
    try c.pushJumpBackTo(bodyPc);
    c.patchJumpNoneToCurPc(resNoneJump);

    c.patchForBlockJumps(jumpStackSave, c.buf.ops.items.len, contPc);
    c.blockJumpStack.items.len = jumpStackSave;

    // TODO: Iter local should be a reserved hidden local (instead of temp) so it can be cleaned up by endLocals when aborting the current fiber.
    try c.pushOptionalDebugSym(nodeId);
    try pushRelease(c, iterTemp, iterNodeId);

    try popUnwind(c, iterTemp);

    if (hasCounter) {
        c.rega.nextTemp -= 2;
    }
    c.rega.nextTemp -= 1;
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
    const data = c.ir.getStmtData(idx, .whileOptStmt);
    const topPc = c.buf.ops.items.len;

    const blockJumpStart: u32 = @intCast(c.blockJumpStack.items.len);

    try pushBlock(c, true, nodeId);
    try genStmt(c, data.capIdx);

    // Optional.
    const optIdx = c.ir.advanceStmt(idx, .whileOptStmt);
    const optv = try genExpr(c, optIdx, Cstr.simple);

    const optNoneJump = try c.pushEmptyJumpNone(optv.local);

    // Copy to captured var.
    try setLocal(c, .{ .id = data.someLocal }, undefined, nodeId, .{ .rightv = optv });

    try popTempValue(c, optv);
    // No release, captured var consumes it.

    try genStmts(c, data.bodyHead);

    const b = c.blocks.getLast();
    try genReleaseLocals(c, b.nextLocalReg, b.nodeId);
    try popLoopBlock(c);

    try c.pushJumpBackTo(topPc);
    c.patchJumpNoneToCurPc(optNoneJump);

    // No need to free optv if it is none.

    c.patchForBlockJumps(blockJumpStart, c.buf.ops.items.len, topPc);
    c.blockJumpStack.items.len = blockJumpStart;
}

fn whileInfStmt(c: *Chunk, idx: usize, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(idx, .whileInfStmt);

    // Loop top.
    const topPc = c.buf.ops.items.len;

    const blockJumpStart: u32 = @intCast(c.blockJumpStack.items.len);

    try pushBlock(c, true, nodeId);

    try genStmts(c, data.bodyHead);

    const b = c.blocks.getLast();
    try genReleaseLocals(c, b.nextLocalReg, b.nodeId);
    try popLoopBlock(c);

    try c.pushJumpBackTo(topPc);

    c.patchForBlockJumps(blockJumpStart, c.buf.ops.items.len, topPc);
    c.blockJumpStack.items.len = blockJumpStart;
}

fn whileCondStmt(c: *Chunk, idx: usize, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(idx, .whileCondStmt);

    // Loop top.
    const topPc = c.buf.ops.items.len;

    const blockJumpStart: u32 = @intCast(c.blockJumpStack.items.len);

    const condIdx = c.ir.advanceStmt(idx, .whileCondStmt);
    const condNodeId = c.ir.getNode(condIdx);
    const condv = try genExpr(c, condIdx, Cstr.simple);

    const condMissJump = try c.pushEmptyJumpNotCond(condv.local);

    try popTempValue(c, condv);
    try releaseTempValue(c, condv, condNodeId);

    try pushBlock(c, true, nodeId);

    // Enter while body.
    try genStmts(c, data.bodyHead);

    const b = c.blocks.getLast();
    try genReleaseLocals(c, b.nextLocalReg, b.nodeId);
    try popLoopBlock(c);

    try c.pushJumpBackTo(topPc);
    c.patchJumpNotCondToCurPc(condMissJump);

    // No need to free cond if false.

    c.patchForBlockJumps(blockJumpStart, c.buf.ops.items.len, topPc);
    c.blockJumpStack.items.len = blockJumpStart;
}

fn destrElemsStmt(c: *Chunk, idx: usize, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(idx, .destrElemsStmt);
    const localsIdx = c.ir.advanceStmt(idx, .destrElemsStmt);
    const locals = c.ir.getArray(localsIdx, u8, data.numLocals);

    const rightv = try genExpr(c, data.right, Cstr.simple);

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

    _ = try popUnwindValue(c, rightv);
}

fn forRangeStmt(c: *Chunk, idx: usize, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(idx, .forRangeStmt);

    // Reserve vars until end of block, hidden from user.
    const counter = try c.rega.consumeNextTemp();
    const rangeEnd = try c.rega.consumeNextTemp();

    // Range start.
    const startIdx = c.ir.advanceStmt(idx, .forRangeStmt);
    const startv = try genExpr(c, startIdx, Cstr.simple);

    // Range end.
    const endv = try genExpr(c, data.rangeEnd, Cstr.toTemp(rangeEnd));
    try pushUnwindValue(c, endv);

    // Begin sub-block.
    try pushBlock(c, true, nodeId);

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

    try popTempAndUnwind(c, startv);

    const bodyPc = c.buf.ops.items.len;
    const jumpStackSave = c.blockJumpStack.items.len;
    try genStmts(c, data.bodyHead);

    const b = c.blocks.getLast();
    try genReleaseLocals(c, b.nextLocalReg, b.nodeId);

    // End sub-block.
    try popLoopBlock(c);

    // Perform counter update and perform check against end range.
    const jumpBackOffset: u16 = @intCast(c.buf.ops.items.len - bodyPc);
    const forRangeOp = c.buf.ops.items.len;
    // The forRange op is patched by forRangeInit at runtime.
    c.buf.setOpArgU16(initPc + 6, @intCast(c.buf.ops.items.len - initPc));
    try c.buf.pushOpSlice(.forRange, &.{ counter, rangeEnd, eachLocal, 0, 0 });
    c.buf.setOpArgU16(forRangeOp + 4, jumpBackOffset);

    c.patchForBlockJumps(jumpStackSave, c.buf.ops.items.len, forRangeOp);
    c.blockJumpStack.items.len = jumpStackSave;

    try popTempAndUnwind(c, endv);
    try popTemp(c, counter);
}

fn verbose(c: *cy.Chunk, idx: usize, nodeId: cy.NodeId) !void {
    _ = nodeId;
    const data = c.ir.getStmtData(idx, .verbose);
    cy.verbose = data.verbose;
}

fn tryStmt(c: *cy.Chunk, idx: usize, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(idx, .tryStmt);
    const pushTryPc = c.buf.ops.items.len;
    try c.buf.pushOpSlice(.pushTry, &.{0, 0, 0, 0});

    try pushBlock(c, false, nodeId);
    try genStmts(c, data.bodyHead);
    try popBlock(c);

    const popTryPc = c.buf.ops.items.len;
    try c.buf.pushOp2(.popTry, 0, 0);
    c.buf.setOpArgU16(pushTryPc + 3, @intCast(c.buf.ops.items.len - pushTryPc));

    try pushBlock(c, false, nodeId);
    try genStmts(c, data.catchBodyHead);
    var errReg: RegisterId = cy.NullU8;
    if (data.hasErrLocal) {
        errReg = toLocalReg(c, data.errLocal);
    }
    try popBlock(c);

    // Patch pushTry with errReg.
    c.buf.setOpArgs1(pushTryPc + 1, errReg);

    c.buf.setOpArgU16(popTryPc + 1, @intCast(c.buf.ops.items.len - popTryPc));
}

const MergeCstr = struct {
    cstr: Cstr,
    dstIsOwned: bool,

    fn init(c: *Chunk, cstr: Cstr) !MergeCstr {
        if (cstr.isExact()) {
            return .{
                .cstr = cstr,
                .dstIsOwned = false,
            };
        } else {
            const temp = try c.rega.consumeNextTemp();
            // Require retain so paths can be merged.
            return .{
                .cstr = Cstr.toTempRetain(temp),
                .dstIsOwned = true,
            };
        }
    }

    fn pushUnwindValue(self: *const MergeCstr, c: *Chunk, val: GenValue) !void {
        if (self.dstIsOwned) {
            try Root.pushUnwindValue(c, val);
        }
    }
};

const Root = @This();

fn genTryExpr(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .tryExpr);
    const pushTryPc = c.buf.ops.items.len;
    try c.buf.pushOpSlice(.pushTry, &.{ 0, 0, 0, 0 });

    const mcstr = try MergeCstr.init(c, cstr);

    // Body expr.
    const childIdx = c.ir.advanceExpr(idx, .tryExpr);
    const childv = try genExpr(c, childIdx, mcstr.cstr);

    const popTryPc = c.buf.ops.items.len;
    try c.buf.pushOp2(.popTry, 0, 0);
    c.buf.setOpArgU16(pushTryPc + 3, @intCast(c.buf.ops.items.len - pushTryPc));

    var val = childv;
    if (data.catchBody != cy.NullId) {
        // Error is not copied anywhere.
        c.buf.setOpArgs1(pushTryPc + 1, cy.NullU8);

        // Catch expr.
        const catchv = try genExpr(c, data.catchBody, mcstr.cstr);
        if (catchv.retained) {
            val.retained = true;
        }
    } else {
        const inst = try c.rega.selectForDstInst(mcstr.cstr, false, nodeId);
        c.buf.setOpArgs1(pushTryPc + 1, inst.dst);
        c.buf.setOpArgs1(pushTryPc + 2, @intFromBool(false));

        _ = try finishDstInst(c, inst, false);
    }
    c.buf.setOpArgU16(popTryPc + 1, @intCast(c.buf.ops.items.len - popTryPc));
    return val;
}

fn genCondExpr(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    _ = nodeId;
    const data = c.ir.getExprData(idx, .condExpr);
    const condIdx = c.ir.advanceExpr(idx, .condExpr);
    const condNodeId = c.ir.getNode(condIdx);

    var finalCstr = cstr;
    if (!finalCstr.isExact()) {
        const temp = try c.rega.consumeNextTemp();
        finalCstr = Cstr.toTemp(temp);
    }

    // Cond.
    const condv = try genExpr(c, condIdx, Cstr.simple);

    const condFalseJump = try c.pushEmptyJumpNotCond(condv.local);

    try releaseTempValue(c, condv, condNodeId);
    try popTempValue(c, condv);

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

fn genIfStmt(c: *cy.Chunk, idx: usize, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(idx, .ifStmt);
    const bodyEndJumpsStart = c.listDataStack.items.len;

    var condIdx = c.ir.advanceStmt(idx, .ifStmt);
    var condNodeId = c.ir.getNode(condIdx);
    var condv = try genExpr(c, condIdx, Cstr.simple);
    try pushUnwindValue(c, condv);

    var prevCaseMissJump = try c.pushEmptyJumpNotCond(condv.local);

    // ARC cleanup for true case.
    try popTempAndUnwind(c, condv);
    try releaseTempValue(c, condv, condNodeId);

    try pushBlock(c, false, nodeId);
    try genStmts(c, data.bodyHead);
    try popBlock(c);

    var hasElse = false;

    if (data.numElseBlocks > 0) {
        const elseBlocks = c.ir.getArray(data.elseBlocks, u32, data.numElseBlocks);

        for (elseBlocks) |elseIdx| {
            const elseBlockNodeId = c.ir.getNode(elseIdx);
            const elseBlock = c.ir.getExprData(elseIdx, .elseBlock);

            const bodyEndJump = try c.pushEmptyJump();
            try c.listDataStack.append(c.alloc, .{ .pc = bodyEndJump });

            // Jump here from prev case miss.
            c.patchJumpNotCondToCurPc(prevCaseMissJump);

            if (!elseBlock.isElse) {
                condIdx = c.ir.advanceExpr(elseIdx, .elseBlock);
                condNodeId = c.ir.getNode(condIdx);
                condv = try genExpr(c, condIdx, Cstr.simple);
                try pushUnwindValue(c, condv);
                prevCaseMissJump = try c.pushEmptyJumpNotCond(condv.local);

                // ARC cleanup for true case.
                try popTempAndUnwind(c, condv);
                try releaseTempValue(c, condv, condNodeId);
            } else {
                hasElse = true;
            }

            try pushBlock(c, false, elseBlockNodeId);
            try genStmts(c, elseBlock.bodyHead);
            try popBlock(c);
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

fn genBlockExpr(c: *Chunk, loc: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(loc, .blockExpr);

    // Select merged dst.
    const inst = try c.rega.selectForNoErrInst(cstr, true, nodeId);
    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, nodeId);
    }

    try pushBlock(c, false, nodeId);
    const b = c.genBlock();
    b.blockExprCstr = Cstr.toTempRetain(inst.dst);

    try genStmts(c, data.bodyHead);

    try popBlock(c);

    return finishNoErrInst(c, inst, true);
}

fn switchStmt(c: *Chunk, idx: usize, nodeId: cy.NodeId) !void {
    const switchLoc = c.ir.advanceStmt(idx, .switchStmt);
    _ = try genSwitch(c, switchLoc, null, nodeId);
}

fn genSwitch(c: *Chunk, idx: usize, cstr: ?Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .switchExpr);
    const isStmt = cstr == null;

    var childBreakJumpsStart: u32 = undefined;
    if (isStmt) {
        childBreakJumpsStart = @intCast(c.blockJumpStack.items.len);
    }

    const casesIdx = c.ir.advanceExpr(idx, .switchExpr);
    const cases = c.ir.getArray(casesIdx, u32, data.numCases);

    // Expr.
    const exprv = try genExpr(c, data.expr, Cstr.simple);
    try pushUnwindValue(c, exprv);

    const caseBodyEndJumpsStart = c.listDataStack.items.len;

    var prevCaseMissJump: u32 = cy.NullId;
    var hasElse = false;
    for (cases) |caseIdx| {
        const caseNodeId = c.ir.getNode(caseIdx);
        const case = c.ir.getExprData(caseIdx, .switchCase);
        const isElse = case.numConds == 0;

        // Jump here from prev case miss.
        if (prevCaseMissJump != cy.NullId) {
            c.patchJumpToCurPc(prevCaseMissJump);
        }

        if (!isElse) {
            const condMatchJumpsStart = c.listDataStack.items.len;

            const condsIdx = c.ir.advanceExpr(caseIdx, .switchCase);

            const conds = c.ir.getArray(condsIdx, u32, case.numConds);
            for (conds) |condIdx| {
                const condNodeId = c.ir.getNode(condIdx);
                const temp = try c.rega.consumeNextTemp();
                const condv = try genExpr(c, condIdx, Cstr.simple);

                try c.pushOptionalDebugSym(condNodeId);
                try c.buf.pushOp3Ext(.compare, exprv.local, condv.local, temp, c.desc(condNodeId));
                try popTempValue(c, condv);

                const condMissJump = try c.pushEmptyJumpNotCond(temp);

                try releaseTempValue(c, condv, condNodeId);

                const condMatchJump = try c.pushEmptyJump();
                try c.listDataStack.append(c.alloc, .{ .pc = condMatchJump });
                c.patchJumpNotCondToCurPc(condMissJump);
                // Miss continues to next cond.

                try releaseTempValue(c, condv, condNodeId);

                try popTemp(c, temp);
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
            try pushBlock(c, false, caseNodeId);
            try genStmts(c, case.bodyHead);
            try popBlock(c);
        }

        const caseBodyEndJump = try c.pushEmptyJump();
        try c.listDataStack.append(c.alloc, .{ .jumpToEndPc = caseBodyEndJump });
    }

    // Jump here from prev case miss.
    if (prevCaseMissJump != cy.NullId) {
        c.patchJumpToCurPc(prevCaseMissJump);
    }

    if (!isStmt and !hasElse) {
        // Gen none return for missing else.
        _ = try genNone(c, cstr.?, nodeId);
    }

    // Jump here from case body ends.
    for (c.listDataStack.items[caseBodyEndJumpsStart..]) |pc| {
        c.patchJumpToCurPc(pc.jumpToEndPc);
    }
    c.listDataStack.items.len = caseBodyEndJumpsStart;

    // Jump here from nested child breaks.
    if (isStmt) {
        const newLen = c.patchBreaks(childBreakJumpsStart, c.buf.ops.items.len);
        c.blockJumpStack.items.len = newLen;
    }

    // Unwind switch expr.
    try popTempAndUnwind(c, exprv);
    try releaseTempValue(c, exprv, nodeId);

    // Complete with no value since assign statement doesn't do anything with it.
    return GenValue.initNoValue();
}

fn genMap(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .map);
    const keysIdx = c.ir.advanceExpr(idx, .map);
    const keys = c.ir.getArray(keysIdx, []const u8, data.numArgs);
    const args = c.ir.getArray(data.args, u32, data.numArgs);

    const inst = try c.rega.selectForDstInst(cstr, true, nodeId);

    const constIdxStart = c.listDataStack.items.len;
    for (keys) |key| {
        const constIdx = try c.buf.getOrPushStaticStringConst(key);
        try c.listDataStack.append(c.alloc, .{ .constIdx = @intCast(constIdx) });
    }

    const argStart = c.rega.getNextTemp();
    for (args) |argIdx| {
        const temp = try c.rega.consumeNextTemp();
        const val = try genAndPushExpr(c, argIdx, Cstr.toTempRetain(temp));
        try pushUnwindValue(c, val);
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
        try popTempAndUnwinds(c, argvs);
    }

    return finishDstInst(c, inst, true);
}

fn genList(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .list);
    const argsIdx = c.ir.advanceExpr(idx, .list);
    const args = c.ir.getArray(argsIdx, u32, data.numArgs);

    const inst = try c.rega.selectForDstInst(cstr, true, nodeId);
    const argStart = c.rega.getNextTemp();

    for (args) |argIdx| {
        const temp = try c.rega.consumeNextTemp();
        const val = try genAndPushExpr(c, argIdx, Cstr.toTempRetain(temp));
        try pushUnwindValue(c, val);
    }

    try c.pushFailableDebugSym(nodeId);
    try c.buf.pushOp3Ext(.list, argStart, data.numArgs, inst.dst, c.desc(nodeId));

    const argvs = popValues(c, data.numArgs);
    try checkArgs(argStart, argvs);
    try popTempAndUnwinds(c, argvs);

    return finishDstInst(c, inst, true);
}

const ProcType = enum(u8) {
    main,
    fiber,
    func,
};

fn pushFiberBlock(c: *Chunk, numArgs: u8, nodeId: cy.NodeId) !void {
    log.tracev("push fiber block: {}", .{numArgs});

    try pushProc(c, .fiber, nodeId);
    c.curBlock.frameLoc = nodeId;

    try reserveFiberCallRegs(c, numArgs);
}

fn popFiberBlock(c: *Chunk) !void {
    try genBlockReleaseLocals(c);
    try popProc(c);

    // Pop boundary index.
    try popUnwind(c, cy.NullU8);
}

fn pushFuncBlockCommon(c: *Chunk, maxIrLocals: u8, numParamCopies: u8, params: []align(1) const ir.FuncParam, func: *cy.Func, nodeId: cy.NodeId) !void {
    try pushProc(c, .func, nodeId);

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

pub fn pushFuncBlock(c: *Chunk, data: ir.FuncBlock, params: []align(1) const ir.FuncParam, nodeId: cy.NodeId) !void {
    log.tracev("push func block: {}, {}, {}, {}", .{data.func.numParams, data.maxLocals, data.func.isMethod, nodeId});
    try pushFuncBlockCommon(c, data.maxLocals, data.numParamCopies, params, data.func, nodeId);
}

pub fn popFuncBlockCommon(c: *Chunk, func: *cy.Func) !void {
    // TODO: Check last statement to skip adding ret.
    try genFuncEnd(c);

    // Pop the null boundary index.
    try popUnwind(c, cy.NullU8);

    if (c.compiler.config.genDebugFuncMarkers) {
        try c.compiler.buf.pushDebugFuncEnd(func, c.id);
    }

    try popProc(c);
}

fn genLambda(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .lambda);
    const func = data.func;
    const paramsIdx = c.ir.advanceExpr(idx, .lambda);
    const params = c.ir.getArray(paramsIdx, ir.FuncParam, func.numParams);

    const inst = try c.rega.selectForDstInst(cstr, true, nodeId);

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
        try c.buf.pushOpSliceExt(.lambda, &.{
            0, 0, func.numParams, stackSize, @intFromBool(func.reqCallTypeCheck), 0, 0, inst.dst }, c.desc(nodeId));
        c.buf.setOpArgU16(start + 1, offset);
        c.buf.setOpArgU16(start + 6, @intCast(func.funcSigId));
    } else {
        const captures = c.ir.getArray(data.captures, u8, data.numCaptures);
        const start = c.buf.ops.items.len;
        try c.pushOptionalDebugSym(func.declId);
        try c.buf.pushOpSlice(.closure, &.{
            0, 0, func.numParams, @as(u8, @intCast(captures.len)), stackSize, 
            0, 0, cy.vm.CalleeStart, @intFromBool(func.reqCallTypeCheck), inst.dst
        });
        c.buf.setOpArgU16(start + 1, offset);
        c.buf.setOpArgU16(start + 6, @intCast(func.funcSigId));

        const operandStart = try c.buf.reserveData(captures.len);
        for (captures, 0..) |irVar, i| {
            const reg = toLocalReg(c, irVar);
            c.buf.ops.items[operandStart + i].val = reg;
        }
    }

    return finishDstInst(c, inst, true);
}

pub fn shouldGenMainScopeReleaseOps(c: *cy.VMcompiler) bool {
    return !c.vm.config.singleRun;
}

fn genBlock(c: *Chunk, idx: usize, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(idx, .block);

    try pushBlock(c, false, nodeId);
    try genStmts(c, data.bodyHead);
    try popBlock(c);
}

pub fn pushProc(c: *Chunk, btype: ProcType, debugNodeId: cy.NodeId) !void {
    // Save register allocator state.
    if (c.procs.items.len > 0) {
        c.curBlock.regaTempStart = c.rega.tempStart;
        c.curBlock.regaNextTemp = c.rega.nextTemp;
        c.curBlock.regaMaxTemp = c.rega.maxTemp;
    }

    try c.procs.append(c.alloc, Proc.init(btype));
    c.curBlock = &c.procs.items[c.procs.items.len-1];
    c.curBlock.irLocalMapStart = @intCast(c.genIrLocalMapStack.items.len);
    c.curBlock.localStart = @intCast(c.genLocalStack.items.len);
    c.curBlock.debugNodeId = debugNodeId;

    try c.pushUnwindTempBoundary();
    if (cy.Trace) {
        c.curBlock.retainedTempStart = @intCast(c.getUnwindTempsLen());
        c.indent += 1;
    }
}

pub fn popProc(c: *Chunk) !void {
    log.tracev("pop gen block", .{});
    c.genIrLocalMapStack.items.len = c.curBlock.irLocalMapStart;
    c.genLocalStack.items.len = c.curBlock.localStart;
    var last = c.procs.pop();

    // Check that next temp is at the start which indicates it has been reset after statements.
    if (cy.Trace) {
        if (c.rega.nextTemp > c.rega.tempStart) {
            return c.reportErrorAt("Temp registers were not reset. {} > {}", &.{v(c.rega.nextTemp), v(c.rega.tempStart)}, last.debugNodeId);
        }
        c.indent -= 1;
    }

    last.deinit(c.alloc);
    if (c.procs.items.len > 0) {
        c.curBlock = &c.procs.items[c.procs.items.len-1];

        // Restore register allocator state.
        c.rega.restoreState(c.curBlock.regaTempStart, c.curBlock.regaNextTemp, c.curBlock.regaMaxTemp);
    }
}

pub fn pushBlock(c: *Chunk, isLoop: bool, nodeId: cy.NodeId) !void {
    log.tracev("push block {} nextTemp: {}", .{c.curBlock.sBlockDepth, c.rega.nextTemp});
    c.curBlock.sBlockDepth += 1;

    const idx = c.blocks.items.len;
    try c.blocks.append(c.alloc, .{
        .nodeId = nodeId,
        .isLoopBlock = isLoop,
        .nextLocalReg = c.curBlock.nextLocalReg,
        .blockExprCstr = undefined,
    });

    if (cy.Trace) {
        c.blocks.items[idx].retainedTempStart = @intCast(c.getUnwindTempsLen());
        c.blocks.items[idx].tempStart = @intCast(c.rega.nextTemp);
        c.indent += 1;
    }
}

pub fn popBlock(c: *Chunk) !void {
    log.tracev("pop block {}", .{c.curBlock.sBlockDepth});

    const b = c.blocks.pop();

    c.curBlock.sBlockDepth -= 1;

    try genReleaseLocals(c, b.nextLocalReg, b.nodeId);

    // Restore nextLocalReg.
    c.curBlock.nextLocalReg = b.nextLocalReg;

    if (cy.Trace) {
        c.indent -= 1;
    }
}

pub fn popLoopBlock(c: *Chunk) !void {
    const b = c.blocks.pop();
    c.curBlock.sBlockDepth -= 1;

    // Restore nextLocalReg.
    c.curBlock.nextLocalReg = b.nextLocalReg;

    if (cy.Trace) {
        c.indent -= 1;
    }
}

pub const Block = struct {
    nodeId: cy.NodeId,
    nextLocalReg: u8,
    isLoopBlock: bool,

    /// Dst for block expression.
    blockExprCstr: Cstr,

    // Used to check the stack state after each stmt.
    retainedTempStart: if (cy.Trace) u32 else void = undefined,
    tempStart: if (cy.Trace) u32 else void = undefined,
};

pub const Proc = struct {
    type: ProcType,
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

    fn init(btype: ProcType) Proc {
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
        };
    }

    fn deinit(self: *Proc, alloc: std.mem.Allocator) void {
        _ = self;
        _ = alloc;
    }
};

fn genConstFloat(c: *Chunk, val: f64, dst: LocalId, nodeId: cy.NodeId) !GenValue {
    const idx = try c.buf.getOrPushConst(cy.Value.initF64(val));
    try genConst(c, idx, dst, false, nodeId);
    return regValue(c, dst, false);
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
                log.tracev("Expected arg[{}] at {}, got {}.", .{i, start+i, argv.local});
                return error.Unexpected;
            }
        }
    }
}

fn exprStmt(c: *Chunk, idx: usize, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(idx, .exprStmt);
    const expr = c.ir.advanceStmt(idx, .exprStmt);

    if (data.isBlockResult) {
        const inMain = c.curBlock.sBlockDepth == 0;
        if (inMain) {
            const exprv = try genExpr(c, expr, Cstr.simpleRetain);
            c.curBlock.endLocal = exprv.local;
            try popTempValue(c, exprv);
        } else {
            // Return from block expression.
            const b = c.blocks.getLast();
            _ = try genExpr(c, expr, b.blockExprCstr);
        }
    } else {
        const exprv = try genExpr(c, expr, Cstr.simple);

        // TODO: Merge with previous release inst.
        try releaseTempValue(c, exprv, nodeId);

        try popTempValue(c, exprv);
    }
}

const LocalId = u8;

pub fn popTemp(c: *Chunk, reg: RegisterId) !void {
    if (reg + 1 != c.rega.nextTemp) {
        rt.logZFmt("Pop temp at {}, but `nextTemp` is at {}.", .{reg, c.rega.nextTemp});
        return error.BcGen;
    }
    c.rega.freeTemps(1);
}

pub fn popTempValue(c: *Chunk, val: GenValue) !void {
    if (val.type == .temp) {
        try popTemp(c, val.local);
    }
}

pub fn popTempAndUnwind(c: *Chunk, val: GenValue) !void {
    try popTempValue(c, val);
    _ = try popUnwindValue(c, val);
}

pub fn popTempAndUnwinds(c: *cy.Chunk, vals: []GenValue) !void {
    var i = vals.len;
    while (i > 0) {
        i -= 1;
        try popTempAndUnwind(c, vals[i]);
    }
}

/// Returns modified slice of retained temps.
pub fn popTempAndUnwinds2(c: *cy.Chunk, vals: []GenValue) ![]GenValue {
    var nextRetained: usize = vals.len;
    var i = vals.len;
    while (i > 0) {
        i -= 1;
        const val = vals[i];
        try popTempAndUnwind(c, val);
        if (val.isRetainedTemp()) {
            nextRetained -= 1;
            vals[nextRetained] = val;
        }
    }
    return vals[nextRetained..];
}

pub fn pushUnwind(c: *cy.Chunk, reg: RegisterId) !void {
    try c.pushUnwindTemp(reg);
}

pub fn pushUnwindValue(c: *cy.Chunk, val: GenValue) !void {
    if (val.isRetainedTemp()) {
        try c.pushUnwindTemp(val.local);
    }
}

pub fn popUnwind(c: *cy.Chunk, reg: RegisterId) !void {
    const pop = c.popUnwindTemp();
    if (pop != reg) {
        rt.logZFmt("Pop retained temp at {}, got {}.", .{reg, pop});
        return error.BcGen;
    }
}

fn popUnwindValue(c: *cy.Chunk, val: GenValue) !bool {
    if (val.isRetainedTemp()) {
        try popUnwind(c, val.data.temp);
        return true;
    }
    return false;
}

pub fn popValues(c: *Chunk, n: u32) []GenValue {
    const vals = c.genValueStack.items[c.genValueStack.items.len-n..];
    c.genValueStack.items.len = c.genValueStack.items.len-n;
    return vals;
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

pub fn regValue(c: *const Chunk, local: LocalId, retained: bool) GenValue {
    if (c.isTempLocal(local)) {
        return GenValue.initTempValue(local, retained);
    } else {
        return GenValue.initLocalValue(local, retained);
    }
}

const GenValueType = enum {
    generic,
    jitCondFlag,
    constant,
    temp,
};

pub const GenValue = struct {
    type: GenValueType,
    /// TODO: Rename to reg.
    local: LocalId,

    /// Whether this value was retained by 1 refcount.
    retained: bool,

    data: union {
        jitCondFlag: struct {
            type: jitgen.JitCondFlagType,
        },
        constant: struct {
            val: cy.Value,
        },
        temp: u8,
    } = undefined,

    pub fn initConstant(val: cy.Value) GenValue {
        return .{ .type = .constant, 
            .local = undefined, .retained = false,
            .data = .{ .constant = .{
                .val = val,
            }
        }};
    }

    fn initJitCondFlag(condt: jitgen.JitCondFlagType) GenValue {
        return .{ .type = .jitCondFlag,
            .local = undefined, .retained = false,
            .data = .{ .jitCondFlag = .{
                .type = condt,
            }},
        };
    }

    pub fn initRetained(retained: bool) GenValue {
        return .{
            .type = .generic,
            .local = 255,
            .retained = retained,
        };
    }

    fn initNoValue() GenValue {
        return .{
            .type = .generic,
            .local = 255,
            .retained = false,
        };
    }

    pub fn initLocalValue(local: LocalId, retained: bool) GenValue {
        return .{
            .type = .generic,
            .local = local,
            .retained = retained,
        };
    }

    pub fn initTempValue(local: LocalId, retained: bool) GenValue {
        return .{
            .type = .temp,
            .local = local,
            .retained = retained,
            .data = .{ .temp = local },
        };
    }

    pub fn isTemp(self: GenValue) bool {
        return self.type == .temp;
    }

    pub fn isRetainedTemp(self: GenValue) bool {
        return self.type == .temp and self.retained;
    }
};

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

fn releaseCond2(c: *Chunk, releaseA: bool, a: RegisterId, releaseB: bool, b: RegisterId, debugId: cy.NodeId) !void {
    if (releaseA and releaseB) {
        try pushReleases(c, &.{ a, b }, debugId);
    } else {
        if (releaseA) {
            try pushRelease(c, a, debugId);
        }
        if (releaseB) {
            try pushRelease(c, b, debugId);
        }
    }
}

fn releaseTempValue2(c: *Chunk, a: GenValue, b: GenValue, debugId: cy.NodeId) !void {
    const releaseA = a.isRetainedTemp();
    const releaseB = b.isRetainedTemp();
    if (releaseA and releaseB) {
        try pushReleases(c, &.{ a.data.temp, b.data.temp }, debugId);
    } else {
        if (releaseA) {
            try pushRelease(c, a.data.temp, debugId);
        }
        if (releaseB) {
            try pushRelease(c, b.data.temp, debugId);
        }
    }
}

fn releaseIf(c: *Chunk, cond: bool, reg: RegisterId, nodeId: cy.NodeId) !void {
    if (cond) {
        try pushRelease(c, reg, nodeId);
    }
}

fn releaseTempValue(c: *Chunk, val: GenValue, nodeId: cy.NodeId) !void {
    if (val.isRetainedTemp()) {
        try pushRelease(c, val.data.temp, nodeId);
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
        return regValue(c, dst, false);
    }
    const idx = try c.buf.getOrPushConst(cy.Value.initInt(@intCast(val)));
    try genConst(c, idx, dst, false, desc.nodeId);
    return regValue(c, dst, false);
}

fn pushTypeCheck(c: *cy.Chunk, local: RegisterId, typeId: cy.TypeId, nodeId: cy.NodeId) !void {
    try c.pushFailableDebugSym(nodeId);
    const start = c.buf.ops.items.len;
    try c.buf.pushOpSlice(.typeCheck, &[_]u8{ local, 0, 0, });
    c.buf.setOpArgU16(start + 2, @intCast(typeId));
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

fn pushCall(c: *cy.Chunk, ret: u8, numArgs: u32, numRet: u8, nodeId: cy.NodeId) !void {
    try c.pushFailableDebugSym(nodeId);
    try c.buf.pushOp3Ext(.call, ret, @intCast(numArgs), numRet, c.desc(nodeId));
}

fn reserveLocalRegAt(c: *Chunk, irLocalId: u8, declType: types.TypeId, lifted: bool, reg: u8, nodeId: cy.NodeId) !void {
    // Stacks are always big enough because of pushProc.
    log.tracev("reserve irId={} reg={} {*} {} {}", .{ irLocalId, reg, c.curBlock, c.curBlock.irLocalMapStart, c.curBlock.localStart });
    log.tracev("local stacks {} {}", .{ c.genIrLocalMapStack.items.len, c.genLocalStack.items.len });
    c.genIrLocalMapStack.items[c.curBlock.irLocalMapStart + irLocalId] = reg;
    c.genLocalStack.items[c.curBlock.localStart + reg] = .{
        .some = .{
            .owned = true,
            .rcCandidate = c.sema.isRcCandidateType(declType),
            .lifted = lifted,
        },
    };
    if (cy.Trace) {
        const nodeStr = try c.encoder.formatNode(nodeId, &cy.tempBuf);
        log.tracev("reserve {}: {s}", .{reg, nodeStr});
    }
}

pub fn reserveLocalReg(c: *Chunk, irVarId: u8, declType: types.TypeId, lifted: bool, nodeId: cy.NodeId, advanceNext: bool) !RegisterId {
    try reserveLocalRegAt(c, irVarId, declType, lifted, c.curBlock.nextLocalReg, nodeId);
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

    pub fn nextCstr(self: *PreferDst, val: GenValue) Cstr {
        switch (val.type) {
            .generic => {
                if (val.local == self.dst) {
                    self.canUseDst = false;
                }
            },
            .temp => {
                if (val.data.temp == self.dst) {
                    self.canUseDst = false;
                }
            },
            else => {},
        }
        return Cstr.preferVolatileIf(self.canUseDst, self.dst);
    }
};