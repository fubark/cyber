const std = @import("std");
const cy = @import("cyber.zig");
const jitgen = @import("jit/gen.zig");
const log = cy.log.scoped(.bc_gen);
const ir = cy.ir;
const rt = cy.rt;
const sema = cy.sema;
const cc = @import("capi.zig");
const types = cy.types;
const bt = types.BuiltinTypes;
const v = cy.fmt.v;
const vmc = cy.vmc;

const Cstr = cy.register.Cstr;
const RegisterId = cy.register.RegisterId;
const TypeId = types.TypeId;

const Chunk = cy.chunk.Chunk;

const gen = @This();

pub fn genAll(c: *cy.Compiler) !void {
    // Constants.
    c.vm.emptyString = try c.buf.getOrPushStaticAstring("");
    c.vm.emptyArray = try cy.heap.allocArray(c.vm, "");
    try c.vm.staticObjects.append(c.alloc, c.vm.emptyArray.asHeapObject());

    // Prepare types.
    for (c.sema.types.items, 0..) |stype, typeId| {
        if (stype.kind == .null) {
            // Skip placeholders.
            continue;
        }
        log.tracev("bc prepare type: {s}", .{stype.sym.name()});
        const sym = stype.sym;

        switch (sym.type) {
            .object_t => {
                const obj = sym.cast(.object_t);
                for (obj.fields[0..obj.numFields], 0..) |field, i| {
                    const fieldSymId = try c.vm.ensureFieldSym(field.sym.name());
                    try c.vm.addFieldSym(@intCast(typeId), fieldSymId, @intCast(i), field.type);
                }
            },
            .struct_t => {
                const obj = sym.cast(.struct_t);
                for (obj.fields[0..obj.numFields], 0..) |field, i| {
                    const fieldSymId = try c.vm.ensureFieldSym(field.sym.name());
                    try c.vm.addFieldSym(@intCast(typeId), fieldSymId, @intCast(i), field.type);
                }
            },
            .int_t,
            .float_t,
            .bool_t,
            .custom_object_t,
            .enum_t => {},
            else => {
                log.tracev("{}", .{sym.type});
                return error.Unsupported;
            },
        }
    }

    for (c.chunks.items) |chunk| {
        log.tracev("prep chunk", .{});
        for (chunk.syms.items) |sym| {
            try prepareSym(c, sym);
        }
        for (chunk.funcs.items) |func| {
            try prepareFunc(c, func);
        }
    }

    // After rt funcs are set up, prepare the overloaded func table.
    // Methods entries are also registered at this point
    // since they depend on overloaded func entries.
    for (c.chunks.items) |chunk| {
        for (chunk.syms.items) |sym| {
            if (sym.type == .func) {
                const func_sym = sym.cast(.func);
                if (func_sym.numFuncs > 1) {
                    // Add overload entries.
                    const rt_id = c.vm.overloaded_funcs.len;
                    try c.vm.overloaded_funcs.append(c.alloc, func_sym.numFuncs);
                    var cur: ?*cy.Func = func_sym.first;
                    var has_method = false;
                    while (cur != null) {
                        if (cur.?.isMethod) {
                            has_method = true;
                        }
                        if (c.genSymMap.get(@ptrCast(cur))) |gen_sym| {
                            try c.vm.overloaded_funcs.append(c.alloc, gen_sym.func.id);
                        } else {
                            return error.MissingFuncSym;
                        }
                        cur = cur.?.next;
                    }
                    try c.genSymMap.putNoClobber(c.alloc, sym, .{ .funcs = .{ .id = @intCast(rt_id) }});
                    if (has_method) {
                        const func = func_sym.first;
                        const parentT = func.sym.?.head.parent.?.getStaticType().?;
                        const name = func.name();
                        const method = try c.vm.ensureMethod(name);
                        try c.vm.addMethod(parentT, method, @intCast(rt_id), true);
                    }
                } else {
                    const func = func_sym.first;
                    if (func.isMethod) {
                        const parentT = func.sym.?.head.parent.?.getStaticType().?;
                        const name = func.name();
                        const method = try c.vm.ensureMethod(name);
                        const func_id = c.genSymMap.get(func).?.func.id;
                        try c.vm.addMethod(parentT, method, func_id, false);
                    }
                }
            }
        }
    }

    // Bind the rest that aren't in sema.
    try @call(.never_inline, cy.bindings.bindCore, .{c.vm});

    for (c.chunks.items) |chunk| {
        log.tracev("Perform codegen for chunk{}: {s}", .{chunk.id, chunk.srcUri});
        chunk.buf = &c.buf;
        try genChunk(chunk);
        log.tracev("Done. performChunkCodegen {s}", .{chunk.srcUri});
    }

    // Merge inst and const buffers.
    var reqLen = c.buf.ops.items.len + c.buf.consts.items.len * @sizeOf(cy.Value) + @alignOf(cy.Value) - 1;
    if (c.buf.ops.capacity < reqLen) {
        try c.buf.ops.ensureTotalCapacityPrecise(c.alloc, reqLen);
    }
    const constAddr = std.mem.alignForward(usize, @intFromPtr(c.buf.ops.items.ptr) + c.buf.ops.items.len, @alignOf(cy.Value));
    const constDst = @as([*]cy.Value, @ptrFromInt(constAddr))[0..c.buf.consts.items.len];
    const constSrc = try c.buf.consts.toOwnedSlice(c.alloc);
    std.mem.copy(cy.Value, constDst, constSrc);
    c.alloc.free(constSrc);
    c.buf.mconsts = constDst;

    // Final op address is known. Patch pc offsets.
    // for (c.vm.funcSyms.items()) |*sym| {
    //     if (sym.entryT == .func) {
    //         sym.inner.func.pc = .{ .ptr = c.buf.ops.items.ptr + sym.inner.func.pc.offset};
    //     }
    // }
    // for (c.vm.methodGroups.items()) |*sym| {
    //     if (sym.mapT == .one) {
    //         if (sym.inner.one.sym.entryT == .func) {
    //             sym.inner.one.sym.inner.func.pc = .{ .ptr = c.buf.ops.items.ptr + sym.inner.one.sym.inner.func.pc.offset };
    //         }
    //     } else if (sym.mapT == .many) {
    //         if (sym.inner.many.mruSym.entryT == .func) {
    //             sym.inner.many.mruSym.inner.func.pc = .{ .ptr = c.buf.ops.items.ptr + sym.inner.many.mruSym.inner.func.pc.offset };
    //         }
    //     }
    // }
    // var iter = c.vm.methodTable.iterator();
    // while (iter.next()) |entry| {
    //     const sym = entry.value_ptr;
    //     if (sym.entryT == .func) {
    //         sym.inner.func.pc = .{ .ptr = c.buf.ops.items.ptr + sym.inner.func.pc.offset };
    //     }
    // }
}

fn prepareSym(c: *cy.Compiler, sym: *cy.Sym) !void {
    switch (sym.type) {
        .hostVar => {
            const id = c.vm.varSyms.len;
            const rtVar = rt.VarSym.init(sym.cast(.hostVar).val);
            cy.arc.retain(c.vm, rtVar.value);
            try c.vm.varSyms.append(c.alloc, rtVar);
            try c.vm.varSymExtras.append(c.alloc, sym);
            try c.genSymMap.putNoClobber(c.alloc, sym, .{ .varSym = .{ .id = @intCast(id) }});
        },
        .userVar => {
            const id = c.vm.varSyms.len;
            const rtVar = rt.VarSym.init(cy.Value.initInt(0));
            try c.vm.varSyms.append(c.alloc, rtVar);
            try c.vm.varSymExtras.append(c.alloc, sym);
            try c.genSymMap.putNoClobber(c.alloc, sym, .{ .varSym = .{ .id = @intCast(id) }});
        },
        .typeTemplate,
        .custom_object_t,
        .bool_t,
        .int_t,
        .float_t,
        .chunk,
        .field,
        .struct_t,
        .object_t,
        .func,
        .typeAlias,
        .enum_t,
        .enumMember,
        .import => {},
        else => {
            log.tracev("{}", .{sym.type});
            return error.Unsupported;
        }
    }
}

fn prepareFunc(c: *cy.Compiler, func: *cy.Func) !void {
    if (func.type == .userLambda) {
        return;
    }
    if (cy.Trace) {
        const symPath = try func.sym.?.head.allocAbsPath(c.alloc);
        defer c.alloc.free(symPath);
        log.tracev("bc prepare func: {s}", .{symPath});
    }
    if (func.type == .hostFunc) {
        const funcSig = c.sema.getFuncSig(func.funcSigId);
        const rtFunc = rt.FuncSymbol.initHostFunc(@ptrCast(func.data.hostFunc.ptr), funcSig.reqCallTypeCheck, func.isMethod, funcSig.numParams(), func.funcSigId);
        _ = try addVmFunc(c, func, rtFunc);
    } else if (func.type == .userFunc) {
        _ = try addVmFunc(c, func, rt.FuncSymbol.initNull());
        // Func is patched later once funcPc and stackSize is obtained.
        // Method entry is also added later.
    } else {
        log.tracev("{}", .{func.type});
        return error.Unsupported;
    }
}

fn addVmFunc(c: *cy.Compiler, func: *cy.Func, rtFunc: rt.FuncSymbol) !u32 {
    const id = try c.vm.addFunc(func.name(), func.funcSigId, rtFunc);
    try c.genSymMap.putNoClobber(c.alloc, func, .{ .func = .{ .id = @intCast(id), .pc = 0 }});
    return @intCast(id);
}

fn genChunk(c: *Chunk) !void {
    genChunkInner(c) catch |err| {
        if (err != error.CompileError) {
            // Wrap all other errors as a CompileError.
            return c.reportErrorFmt("error.{}", &.{v(err)}, c.curNodeId);
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
        const contextStr = try c.encoder.format(nodeId, &cy.tempBuf);
        if (cc.verbose()) {
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
        .ifStmt             => try ifStmt(c, idx, nodeId),
        .ifUnwrapStmt       => try ifUnwrapStmt(c, idx, nodeId),
        .loopStmt           => try loopStmt(c, idx, nodeId),
        .mainBlock          => try mainBlock(c, idx, nodeId),
        .block              => try genBlock(c, idx, nodeId),
        .opSet              => try opSet(c, idx, nodeId),
        .pushDebugLabel     => try pushDebugLabel(c, idx),
        .retExprStmt        => try gen.retExprStmt(c, idx, nodeId),
        .retStmt            => try retStmt(c),
        .setCallObjSymTern  => try setCallObjSymTern(c, idx, nodeId),
        .setCaptured        => try setCaptured(c, idx, nodeId),
        .set_field_dyn      => try setFieldDyn(c, idx, .{}, nodeId),
        .setIndex           => try setIndex(c, idx, nodeId),
        .setLocal           => try irSetLocal(c, idx, nodeId),
        .set_field          => try setField(c, idx, .{}, nodeId),
        .setVarSym          => try setVarSym(c, idx, nodeId),
        .setLocalType       => try setLocalType(c, idx),
        .switchStmt         => try switchStmt(c, idx, nodeId),
        .tryStmt            => try tryStmt(c, idx, nodeId),
        .verbose            => try verbose(c, idx, nodeId),
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
        if (cc.verbose()) {
            rt.logFmt("{}| end {} unw={} ntmp={}", &.{
                cy.fmt.repeat(' ', c.indent * 4), v(@tagName(code)),
                v(c.unwindTempIndexStack.items.len), v(c.rega.nextTemp),
            });
        }
    }

    // Check stack after statement.
    if (c.procs.items.len > 0) {
        if (c.unwindTempIndexStack.items.len != tempRetainedStart) {
            return c.reportErrorFmt("Expected {} unwindable retained temps, got {}",
                &.{v(tempRetainedStart), v(c.unwindTempIndexStack.items.len)}, nodeId);
        }

        if (c.rega.nextTemp != tempStart) {
            return c.reportErrorFmt("Expected {} temp start, got {}",
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
        return c.reportErrorFmt("Remaining gen values: {}", &.{v(c.genValueStack.items.len)}, cy.NullId);
    }
    if (c.unwindTempIndexStack.items.len > 0) {
        return c.reportErrorFmt("Remaining unwind temp index: {}", &.{v(c.unwindTempIndexStack.items.len)}, cy.NullId);
    }
    if (c.unwindTempRegStack.items.len > 0) {
        return c.reportErrorFmt("Remaining unwind temp reg: {}", &.{v(c.unwindTempRegStack.items.len)}, cy.NullId);
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
        const contextStr = try c.encoder.format(nodeId, &cy.tempBuf);
        if (cc.verbose()) {
            rt.logFmt("{}( {}: `{}` {} unw={} ntmp={}", &.{
                cy.fmt.repeat(' ', c.indent * 4), v(@tagName(code)), v(contextStr), v(@tagName(cstr.type)),
                v(c.unwindTempIndexStack.items.len), v(c.rega.nextTemp),
            });
        }
    }

    const res = try switch (code) {
        .box                => genBox(c, idx, cstr, nodeId),
        .captured           => genCaptured(c, idx, cstr, nodeId),
        .cast               => genCast(c, idx, cstr, nodeId),
        .coinitCall         => genCoinitCall(c, idx, cstr, nodeId),
        .coresume           => genCoresume(c, idx, cstr, nodeId),
        .coyield            => genCoyield(c, idx, cstr, nodeId),
        .enumMemberSym      => genEnumMemberSym(c, idx, cstr, nodeId),
        .errorv             => genError(c, idx, cstr, nodeId),
        .falsev             => genFalse(c, cstr, nodeId),
        .fieldDyn           => genFieldDyn(c, idx, cstr, .{}, nodeId),
        .field              => genField(c, idx, cstr, .{}, nodeId),
        .float              => genFloat(c, idx, cstr, nodeId),
        .funcSym            => genFuncSym(c, idx, cstr, nodeId),
        .if_expr            => genIfExpr(c, idx, cstr, nodeId),
        .int                => genInt(c, idx, cstr, nodeId),
        .lambda             => genLambda(c, idx, cstr, nodeId),
        .list               => genList(c, idx, cstr, nodeId),
        .local              => genLocal(c, idx, cstr, nodeId),
        .map                => genMap(c, idx, cstr, nodeId),
        .object_init        => genObjectInit(c, idx, cstr, nodeId),
        .pre                => return error.Unexpected,
        .preBinOp           => genBinOp(c, idx, cstr, .{}, nodeId),
        .preCallDyn         => genCallDyn(c, idx, cstr, nodeId),
        .preCallFuncSym     => genCallFuncSym(c, idx, cstr, nodeId),
        .pre_call_sym_dyn   => genCallSymDyn(c, idx, cstr, nodeId),
        .preCallObjSym      => genCallObjSym(c, idx, cstr, nodeId),
        .preUnOp            => genUnOp(c, idx, cstr, nodeId),
        .range              => genRange(c, idx, cstr, nodeId),
        .string             => genString(c, idx, cstr, nodeId),
        .stringTemplate     => genStringTemplate(c, idx, cstr, nodeId),
        .switchExpr         => genSwitch(c, idx, cstr, nodeId),
        .symbol             => genSymbol(c, idx, cstr, nodeId),
        .type_check         => genTypeCheck(c, idx, cstr, nodeId),
        .typeCheckOption    => genTypeCheckOption(c, idx, cstr, nodeId),
        .throw              => genThrow(c, idx, nodeId),
        .truev              => genTrue(c, cstr, nodeId),
        .tryExpr            => genTryExpr(c, idx, cstr, nodeId),
        .typeSym            => genTypeSym(c, idx, cstr, nodeId),
        .unwrapChoice       => genUnwrapChoice(c, idx, cstr, nodeId),
        .unwrap_or          => genUnwrapOr(c, idx, cstr, nodeId),
        .varSym             => genVarSym(c, idx, cstr, nodeId),
        .blockExpr          => genBlockExpr(c, idx, cstr, nodeId),
        else => {
            rt.printErrorZFmt(c.vm, "{}\n", .{code});
            return error.TODO;
        }
    };
    if (cy.Trace) {
        if (cc.verbose()) {
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
    const rtId = c.compiler.genSymMap.get(func).?.func.id;
    const rtFunc = rt.FuncSymbol.initFunc(funcPc, stackSize, func.numParams, func.funcSigId, func.reqCallTypeCheck, func.isMethod);
    c.compiler.vm.funcSyms.buf[rtId] = rtFunc;

    try popFuncBlockCommon(c, func);
    c.patchJumpToCurPc(skipJump);
}

fn genCoresume(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .coresume);
    const inst = try c.rega.selectForDstInst(cstr, true, nodeId);

    const childv = try genExpr(c, data.expr, Cstr.simpleRetain);

    try c.pushCode(.coresume, &.{childv.reg, inst.dst}, nodeId);

    try popTempAndUnwind(c, childv);

    return finishDstInst(c, inst, true);
}

fn genCoyield(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    _ = idx;
    try c.pushCode(.coyield, &.{c.curBlock.startLocalReg, c.curBlock.nextLocalReg}, nodeId);
    // TODO: return coyield expression.
    return genFalse(c, cstr, nodeId);
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
    } else if (callCode == .preCallDyn) {
        numArgs = data.callDyn.numArgs;
        args = c.ir.getArray(data.callDyn.args, u32, numArgs);

        const temp = try c.rega.consumeNextTemp();
        const val = try genAndPushExpr(c, data.callDyn.callee, Cstr.toTempRetain(temp));
        try pushUnwindValue(c, val);
    } else return error.Unexpected;

    for (args) |argIdx| {
        const temp = try c.rega.consumeNextTemp();
        const val = try genAndPushExpr(c, argIdx, Cstr.toTempRetain(temp));
        try pushUnwindValue(c, val);
    }

    const node = c.ast.node(nodeId);
    const callExprId = node.data.coinit.child;

    var numTotalArgs = numArgs;
    var argDst: u8 = undefined;
    if (callCode == .preCallFuncSym) {
        argDst = 1 + cy.vm.CallArgStart;
    } else if (callCode == .preCallDyn) {
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
    try c.pushCode(.coinit, &.{
        tempStart, @as(u8, @intCast(numTotalArgs)), argDst, 0, @as(u8, @intCast(initialStackSize)), inst.dst }, nodeId);

    try pushFiberBlock(c, @intCast(numTotalArgs), nodeId);

    // Gen func call.
    const callRet: u8 = 1;
    if (callCode == .preCallFuncSym) {
        const rtId = c.compiler.genSymMap.get(data.callFuncSym.func).?.func.id;
        try pushCallSym(c, callRet, numArgs, 1, rtId, callExprId);
    } else if (callCode == .preCallDyn) {
        try pushCall(c, callRet, numArgs, 1, callExprId);
    } else return error.Unexpected;

    try c.pushCode(.coreturn, &.{}, nodeId);
    c.buf.setOpArgs1(coinitPc + 4, @intCast(c.buf.ops.items.len - coinitPc));

    try popFiberBlock(c);

    const argvs = popValues(c, numTotalArgs);
    try popTempAndUnwinds(c, argvs);

    return finishDstInst(c, inst, true);
}

fn genCast(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .cast);

    if (!data.isRtCast) {
        return genExpr(c, data.expr, cstr);
    }

    const inst = try c.rega.selectForDstInst(cstr, false, nodeId);

    // TODO: If inst.dst is a temp, this should have a cstr of localOrExact.
    const childv = try genExpr(c, data.expr, Cstr.simple);
    try pushUnwindValue(c, childv);

    if (types.toRtConcreteType(data.typeId)) |tId| {
        const pc = c.buf.ops.items.len;
        try c.pushFCode(.cast, &.{ childv.reg, 0, 0, inst.dst }, nodeId);
        c.buf.setOpArgU16(pc + 2, @intCast(tId));
    } else {
        // Cast to abstract type.
        const pc = c.buf.ops.items.len;
        try c.pushFCode(.castAbstract, &.{ childv.reg, 0, 0, inst.dst }, nodeId);
        c.buf.setOpArgU16(pc + 2, @intCast(data.typeId));
    }

    try popTempAndUnwind(c, childv);

    return finishDstInst(c, inst, childv.retained);
} 

const FieldOptions = struct {
    recv: ?GenValue = null,
};

fn genFieldDyn(c: *Chunk, idx: usize, cstr: Cstr, opts: FieldOptions, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .fieldDyn);

    const inst = try c.rega.selectForDstInst(cstr, true, nodeId);
    const ownRecv = opts.recv == null;

    var recv: GenValue = undefined;
    if (opts.recv) |recv_| {
        recv = recv_;
    } else {
        recv = try genExpr(c, data.rec, Cstr.simple);
        try pushUnwindValue(c, recv);
    }

    const fieldId = try c.compiler.vm.ensureFieldSym(data.name);
    try pushFieldDyn(c, recv.reg, inst.dst, @intCast(fieldId), nodeId);

    if (ownRecv) {
        try popTempAndUnwind(c, recv);
        try releaseTempValue(c, recv, nodeId);
    }

    return finishDstInst(c, inst, true);
}

fn genField(c: *Chunk, idx: usize, cstr: Cstr, opts: FieldOptions, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .field);

    const inst = try c.rega.selectForDstInst(cstr, true, nodeId);
    const ownRecv = opts.recv == null;

    var recv: GenValue = undefined;
    if (opts.recv) |recv_| {
        recv = recv_;
    } else {
        recv = try genExpr(c, data.rec, Cstr.simple);
        // try pushUnwindValue(c, recv);
    }

    const type_id = c.ir.getExprType(idx).id;
    const isStruct = c.sema.getTypeKind(type_id) == .@"struct";
    var getRef = false;
    var refTemp: RegisterId = undefined;
    if (data.numNestedFields > 0) {
        // get ref.
        refTemp = try c.rega.consumeNextTemp();
        try pushFieldRef(c, recv.reg, data.idx, data.numNestedFields, refTemp, nodeId);
        const fieldsLoc = c.ir.advanceExpr(idx, .field);
        const fields = c.ir.getArray(fieldsLoc, u8, data.numNestedFields);
        try c.pushCodeBytes(fields);
        getRef = true;
    } else {
        if (isStruct) {
            refTemp = try c.rega.consumeNextTemp();
            try pushFieldRef(c, recv.reg, data.idx, 0, refTemp, nodeId);
            getRef = true;
        }
    }

    if (getRef) {
        if (c.sema.getTypeKind(type_id) == .@"struct") {
            const numFields: u8 = @intCast(c.sema.types.items[type_id].data.@"struct".numFields);
            try c.pushCode(.refCopyObj, &.{ refTemp, numFields, inst.dst }, nodeId);
        } else {
            try c.pushCode(.ref, &.{ refTemp, inst.dst }, nodeId);
        }
        try popTemp(c, refTemp);
        try pushRelease(c, refTemp, nodeId);
    } else {
        try pushField(c, recv.reg, data.idx, inst.dst, nodeId);
    }

    if (ownRecv) {
        try popTempValue(c, recv);
        // ARC cleanup.
        try releaseTempValue(c, recv, nodeId);
    }

    const willRetain = c.sema.isRcCandidateType(type_id);
    return finishDstInst(c, inst, willRetain);
}

fn genObjectInit(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .object_init);

    const inst = try c.rega.selectForDstInst(cstr, true, nodeId);

    // TODO: Would it be faster/efficient to copy the fields into contiguous registers
    //       and copy all at once to heap or pass locals into the operands and iterate each and copy to heap?
    //       The current implementation is the former.
    const args = c.ir.getArray(data.args, u32, data.numArgs);
    const argStart = c.rega.getNextTemp();
    for (args) |argIdx| {
        const temp = try c.rega.consumeNextTemp();
        const val = try genAndPushExpr(c, argIdx, Cstr.toTempRetain(temp));
        try pushUnwindValue(c, val);
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
    const data = c.ir.getExprData(idx, .throw);
    const childv = try genExpr(c, data.expr, Cstr.simple);

    try c.pushFCode(.throw, &.{childv.reg}, nodeId);

    try popTempAndUnwind(c, childv);
    return GenValue.initNoValue();
}

fn setCallObjSymTern(c: *Chunk, loc: usize, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(loc, .setCallObjSymTern).callObjSymTern;

    const inst = try beginCall(c, Cstr.none, false, nodeId);

    var args: [4]GenValue = undefined;
    var temp = try c.rega.consumeNextTemp();
    args[0] = try genExpr(c, data.rec, Cstr.toTemp(temp));
    try pushUnwindValue(c, args[0]);
    temp = try c.rega.consumeNextTemp();
    args[1] = try genExpr(c, data.index, Cstr.toTemp(temp));
    try pushUnwindValue(c, args[1]);
    temp = try c.rega.consumeNextTemp();
    args[2] = try genExpr(c, data.right, Cstr.toTemp(temp));
    try pushUnwindValue(c, args[2]);

    const method = try c.compiler.vm.ensureMethod(data.name);
    try pushCallObjSym(c, inst.ret, 3,
        @intCast(method), nodeId);

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
    const recv = try genAndPushExpr(c, data.rec, Cstr.simple);
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
        try pushInlineTernExpr(c, .setIndexList, recv.reg, indexv.reg, rightv.reg, noneRet, nodeId);
    } else if (data.recvT == bt.Map) {
        try pushInlineTernExpr(c, .setIndexMap, recv.reg, indexv.reg, rightv.reg, noneRet, nodeId);
    } else return error.Unexpected;

    const retained = try popTempAndUnwinds2(c, argvs);
    c.rega.freeTemps(1);

    // ARC cleanup.
    try pushReleaseVals(c, retained, nodeId);
}

const SetFieldDynOptions = struct {
    recv: ?GenValue = null,
    right: ?GenValue = null,
};

fn setFieldDyn(c: *Chunk, idx: usize, opts: SetFieldDynOptions, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(idx, .set_field_dyn).set_field_dyn;

    const fieldId = try c.compiler.vm.ensureFieldSym(data.name);
    const ownRecv = opts.recv == null;

    // LHS
    var recv: GenValue = undefined;
    if (opts.recv) |recv_| {
        recv = recv_;
    } else {
        recv = try genExpr(c, data.rec, Cstr.simple);
        try pushUnwindValue(c, recv);
    }

    // RHS
    var rightv: GenValue = undefined;
    if (opts.right) |right| {
        rightv = right;
    } else {
        rightv = try genExpr(c, data.right, Cstr.simpleRetain);
        try pushUnwindValue(c, rightv);
    }

    // Performs runtime type check.
    const pc = c.buf.ops.items.len;
    try c.pushFCode(.setFieldDyn, &.{ recv.reg, 0, 0, rightv.reg, 0, 0, 0, 0, 0, c.rega.nextTemp }, nodeId);
    c.buf.setOpArgU16(pc + 2, @intCast(fieldId));

    try popTempAndUnwind(c, rightv);
    if (ownRecv) {
        try popTempAndUnwind(c, recv);
        // ARC cleanup. Right is not released since it's being assigned to the field.
        try releaseTempValue(c, recv, nodeId);
    }
}

const SetFieldOptions = struct {
    recv: ?GenValue = null,
    right: ?GenValue = null,
};

fn setField(c: *Chunk, idx: usize, opts: SetFieldOptions, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(idx, .set_field).set_field;
    const fieldData = c.ir.getExprData(data.field, .field);

    // Receiver.
    var recv: GenValue = undefined;
    if (opts.recv) |recv_| {
        recv = recv_;
    } else {
        recv = try genExpr(c, fieldData.rec, Cstr.simple);
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

    const type_id = c.ir.getExprType(data.field).id;
    const isStruct = c.sema.getTypeKind(type_id) == .@"struct";
    var getRef = false;
    var refTemp: RegisterId = undefined;
    if (fieldData.numNestedFields > 0) {
        // get ref.
        refTemp = try c.rega.consumeNextTemp();
        try pushFieldRef(c, recv.reg, fieldData.idx, fieldData.numNestedFields, refTemp, nodeId);
        const fieldsLoc = c.ir.advanceExpr(data.field, .field);
        const fields = c.ir.getArray(fieldsLoc, u8, fieldData.numNestedFields);
        try c.pushCodeBytes(fields);
        getRef = true;
    } else {
        if (isStruct) {
            refTemp = try c.rega.consumeNextTemp();
            try pushFieldRef(c, recv.reg, fieldData.idx, 0, refTemp, nodeId);
            getRef = true;
        }
    }

    if (getRef) {
        try c.pushCode(.setRef, &.{ refTemp, rightv.reg }, nodeId);
        try popTemp(c, refTemp);
        try pushRelease(c, refTemp, nodeId);
    } else {
        try c.pushCode(.setField, &.{ recv.reg, fieldData.idx, rightv.reg }, nodeId);
    }

    try popTempAndUnwind(c, rightv);
    const ownRecv = opts.recv == null;
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

    const inst = try c.rega.selectForNoErrNoDepInst(cstr, false, nodeId);
    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, nodeId);
    }
    try genConst(c, constIdx, inst.dst, false, nodeId);
    return finishNoErrNoDepInst(c, inst, false);
}

fn genRange(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .range);

    const inst = try c.rega.selectForDstInst(cstr, true, nodeId);

    var start: GenValue = undefined;
    if (data.start != cy.NullId) {
        // Assume int.
        start = try genExpr(c, data.start, Cstr.simple);
    }

    var end: GenValue = undefined;
    if (data.end != cy.NullId) {
        // Assume int.
        end = try genExpr(c, data.end, Cstr.simple);
    }

    const start_reg = if (data.start != cy.NullId) start.reg else cy.NullU8;
    const end_reg = if (data.end != cy.NullId) end.reg else cy.NullU8;
    try c.pushCode(.range, &.{start_reg, end_reg, @intFromBool(data.inc), inst.dst}, nodeId);

    if (data.end != cy.NullId) try popTempValue(c, end);
    if (data.start != cy.NullId) try popTempValue(c, start);
    return finishDstInst(c, inst, true);
}

fn genSymbol(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .symbol);

    const symId = try c.compiler.vm.ensureSymbol(data.name);
    const inst = try c.rega.selectForNoErrNoDepInst(cstr, false, nodeId);
    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, nodeId);
    }
    try c.buf.pushOp2(.tagLiteral, @intCast(symId), inst.dst);
    return finishNoErrNoDepInst(c, inst, false);
}

fn genTypeCheck(c: *Chunk, loc: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(loc, .type_check);

    const expr = try genExpr(c, data.expr, cstr);
    try pushUnwindValue(c, expr);

    try pushTypeCheck(c, expr.reg, data.exp_type, nodeId);

    _ = try popUnwindValue(c, expr); 
    return expr;
}

fn genTypeCheckOption(c: *Chunk, loc: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(loc, .typeCheckOption);

    const expr = try genExpr(c, data.expr, cstr);
    try pushUnwindValue(c, expr);

    try pushTypeCheckOption(c, expr.reg, nodeId);

    _ = try popUnwindValue(c, expr); 
    return expr;
}

fn genUnwrapChoice(c: *Chunk, loc: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(loc, .unwrapChoice);
    const retain = c.sema.isRcCandidateType(data.payload_t);
    const inst = try c.rega.selectForDstInst(cstr, retain, nodeId);

    const choice = try genExpr(c, data.choice, Cstr.simple);
    try pushUnwindValue(c, choice); 

    try c.pushFCode(.unwrapChoice, &.{ choice.reg, data.tag, data.fieldIdx, inst.dst }, nodeId);

    try popTempAndUnwind(c, choice);
    try releaseTempValue(c, choice, nodeId);
    return finishDstInst(c, inst, retain);
}

fn genTrue(c: *Chunk, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const inst = try c.rega.selectForNoErrNoDepInst(cstr, false, nodeId);
    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, nodeId);
    }
    try c.buf.pushOp1(.true, inst.dst);
    return finishNoErrNoDepInst(c, inst, false);
}

fn genFalse(c: *Chunk, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const inst = try c.rega.selectForNoErrNoDepInst(cstr, false, nodeId);
    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, nodeId);
    }
    try c.buf.pushOp1(.false, inst.dst);
    return finishNoErrNoDepInst(c, inst, false);
}

fn genInt(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .int);

    const inst = try c.rega.selectForNoErrNoDepInst(cstr, false, nodeId);
    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, nodeId);
    }
    _ = try genConstIntExt(c, data.val, inst.dst, c.desc(nodeId));
    return finishNoErrNoDepInst(c, inst, false);
}

fn genFloat(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .float);
    const inst = try c.rega.selectForNoErrNoDepInst(cstr, false, nodeId);
    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, nodeId);
    }
    _ = try genConstFloat(c, data.val, inst.dst, nodeId);
    return finishNoErrNoDepInst(c, inst, false);
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
    const inst = try c.rega.selectForNoErrNoDepInst(cstr, true, nodeId);
    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, nodeId);
    }
    // TODO: Ideally this shouldn't retain. But release ops *might* require them to be.
    try pushStringConst(c, data.raw, inst.dst, nodeId);
    return finishNoErrNoDepInst(c, inst, true);
}

fn pushStringConst(c: *Chunk, str: []const u8, dst: RegisterId, nodeId: cy.NodeId) !void {
    const idx = try c.buf.getOrPushStaticStringConst(str);
    try genConst(c, idx, dst, true, nodeId);
}

fn genUnOp(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .preUnOp).unOp;
    const inst = try c.rega.selectForDstInst(cstr, false, nodeId);

    const canUseDst = !c.isParamOrLocalVar(inst.dst);
    const childv = try genExpr(c, data.expr, Cstr.preferVolatileIf(canUseDst, inst.dst));
    try pushUnwindValue(c, childv);

    switch (data.op) {
        .not => {
            try c.buf.pushOp2(.not, childv.reg, inst.dst);
        },
        .minus,
        .bitwiseNot => {
            if (data.childT == bt.Integer) {
                try pushInlineUnExpr(c, getIntUnaryOpCode(data.op), childv.reg, inst.dst, nodeId);
            } else if (data.childT == bt.Float) {
                try pushInlineUnExpr(c, getFloatUnaryOpCode(data.op), childv.reg, inst.dst, nodeId);
            } else return error.Unexpected;
            // Builtin unary expr do not have retained child.
        },
        else => {
            return c.reportErrorFmt("Unsupported op: {}", &.{v(data.op)}, nodeId);
        }
    }

    if (childv.isTemp()) {
        if (childv.reg != inst.dst) {
            try popTemp(c, childv.reg);
        }
        if (try popUnwindValue(c, childv)) {
            try pushRelease(c, childv.reg, nodeId);
        }
    }

    return finishDstInst(c, inst, false);
}

fn genCallSymDyn(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .pre_call_sym_dyn).call_sym_dyn;

    const inst = try beginCall(c, cstr, false, nodeId);

    // Receiver.
    const argStart = c.rega.nextTemp;

    const args = c.ir.getArray(data.args, u32, data.nargs);
    for (args, 0..) |argIdx, i| {
        const temp = try c.rega.consumeNextTemp();
        if (cy.Trace and temp != argStart + i) return error.Unexpected;
        const val = try genAndPushExpr(c, argIdx, Cstr.toTemp(temp));
        try pushUnwindValue(c, val);
    }

    const rt_id = c.compiler.genSymMap.get(data.sym).?.funcs.id;
    try pushCallSymDyn(c, inst.ret, data.nargs, 1, rt_id, nodeId);

    const argvs = popValues(c, data.nargs);
    try checkArgs(argStart, argvs);

    const retained = try popTempAndUnwinds2(c, argvs);
    try pushReleaseVals(c, retained, nodeId);

    return endCall(c, inst, true);
}

fn genCallObjSym(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .preCallObjSym).callObjSym;

    const inst = try beginCall(c, cstr, false, nodeId);

    // Receiver.
    const argStart = c.rega.nextTemp;
    var temp = try c.rega.consumeNextTemp();
    const recv = try genExpr(c, data.rec, Cstr.toTemp(temp));
    try pushUnwindValue(c, recv);
    try c.genValueStack.append(c.alloc, recv);

    const args = c.ir.getArray(data.args, u32, data.numArgs);
    for (args, 0..) |argIdx, i| {
        temp = try c.rega.consumeNextTemp();
        if (cy.Trace and temp != argStart + 1 + i) return error.Unexpected;
        const val = try genAndPushExpr(c, argIdx, Cstr.toTemp(temp));
        try pushUnwindValue(c, val);
    }

    const method = try c.compiler.vm.ensureMethod(data.name);
    try pushCallObjSym(c, inst.ret, data.numArgs + 1,
        @intCast(method), nodeId);

    const argvs = popValues(c, data.numArgs+1);
    try checkArgs(argStart, argvs);

    const retained = try popTempAndUnwinds2(c, argvs);
    try pushReleaseVals(c, retained, nodeId);

    return endCall(c, inst, true);
}

fn genCallFuncSym(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .preCallFuncSym).callFuncSym;

    if (true) {
        // TODO: Handle specialized. (eg. listIndex, listAppend)
    }

    const inst = try beginCall(c, cstr, false, nodeId);

    const args = c.ir.getArray(data.args, u32, data.numArgs);

    const argStart = c.rega.nextTemp;
    for (args, 0..) |argIdx, i| {
        const temp = try c.rega.consumeNextTemp();
        if (cy.Trace and temp != argStart + i) return error.Unexpected;
        const val = try genAndPushExpr(c, argIdx, Cstr.toTemp(temp));
        try pushUnwindValue(c, val);
    }

    const rtId = c.compiler.genSymMap.get(data.func).?.func.id;
    try pushCallSym(c, inst.ret, data.numArgs, 1, rtId, nodeId);

    const argvs = popValues(c, data.numArgs);
    try checkArgs(argStart, argvs);

    const retained = try popTempAndUnwinds2(c, argvs);
    try pushReleaseVals(c, retained, nodeId);

    const retRetained = c.sema.isRcCandidateType(data.func.retType);
    return endCall(c, inst, retRetained);
}

fn genCallDyn(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .preCallDyn).callDyn;
    const inst = try beginCall(c, cstr, true, nodeId);

    // Callee.
    const argStart = c.rega.nextTemp;
    var temp = try c.rega.consumeNextTemp();
    const calleev = try genExpr(c, data.callee, Cstr.toTemp(temp));
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

    try c.pushFCode(.call, &.{inst.ret, data.numArgs, 1}, nodeId);

    const retained = try popTempAndUnwinds2(c, calleeAndArgvs);
    try pushReleaseVals(c, retained, nodeId);

    return endCall(c, inst, true);
}

const BinOpOptions = struct {
    left: ?GenValue = null,
};

fn genBinOp(c: *Chunk, idx: usize, cstr: Cstr, opts: BinOpOptions, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .preBinOp).binOp;
    log.tracev("binop {} {}", .{data.op, data.leftT});

    if (data.op == .and_op) {
        return gen.andOp(c, data, cstr, nodeId);
    } else if (data.op == .or_op) {
        return gen.orOp(c, data, cstr, nodeId);
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
        leftv = try genExpr(c, data.left, Cstr.preferVolatileIf(prefer.canUseDst, prefer.dst));
        try pushUnwindValue(c, leftv);
    }

    // Rhs.
    const rightv = try genExpr(c, data.right, prefer.nextCstr(leftv));
    try pushUnwindValue(c, rightv);

    var retained = false;
    switch (data.op) {
        .index => {
            if (data.leftT == bt.List) {
                if (data.rightT == bt.Range) {
                    try pushInlineBinExpr(c, .sliceList, leftv.reg, rightv.reg, inst.dst, nodeId);
                } else {
                    try pushInlineBinExpr(c, .indexList, leftv.reg, rightv.reg, inst.dst, nodeId);
                }
            } else if (data.leftT == bt.Tuple) {
                try pushInlineBinExpr(c, .indexTuple, leftv.reg, rightv.reg, inst.dst, nodeId);
            } else if (data.leftT == bt.Map) {
                try pushInlineBinExpr(c, .indexMap, leftv.reg, rightv.reg, inst.dst, nodeId);
            } else return error.Unexpected;
            retained = true;
        },
        .bitwiseAnd,
        .bitwiseOr,
        .bitwiseXor,
        .bitwiseLeftShift,
        .bitwiseRightShift => {
            if (data.leftT == bt.Integer) {
                try pushInlineBinExpr(c, getIntOpCode(data.op), leftv.reg, rightv.reg, inst.dst, nodeId);
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
                try pushInlineBinExpr(c, getFloatOpCode(data.op), leftv.reg, rightv.reg, inst.dst, nodeId);
            } else if (data.leftT == bt.Integer) {
                try pushInlineBinExpr(c, getIntOpCode(data.op), leftv.reg, rightv.reg, inst.dst, nodeId);
            } else return error.Unexpected;
        },
        .equal_equal => {
            try c.pushOptionalDebugSym(nodeId);
            try c.buf.pushOp3Ext(.compare, leftv.reg, rightv.reg, inst.dst, c.desc(nodeId));
        },
        .bang_equal => {
            try c.pushOptionalDebugSym(nodeId);
            try c.buf.pushOp3Ext(.compareNot, leftv.reg, rightv.reg, inst.dst, c.desc(nodeId));
        },
        else => {
            return c.reportErrorFmt("Unsupported op: {}", &.{v(data.op)}, nodeId);
        },
    }

    const tempRight = rightv.isTemp();
    if (tempRight and rightv.reg != inst.dst) {
        try popTemp(c, rightv.reg);
    }
    const retainedRight = try popUnwindValue(c, rightv);

    var retainedLeft = false;
    if (opts.left == null) {
        const tempLeft = leftv.isTemp();
        if (tempLeft and leftv.reg != inst.dst) {
            try popTemp(c, leftv.reg);
        }
        retainedLeft = try popUnwindValue(c, leftv);
    }

    // ARC cleanup.
    try releaseCond2(c, retainedLeft, leftv.reg, retainedRight, rightv.reg, nodeId);

    return finishDstInst(c, inst, retained);
}

fn genBox(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    _ = nodeId;
    // Passthrough, since all values are boxed in the VM.
    const data = c.ir.getExprData(idx, .box);
    return genExpr(c, data.expr, cstr);
}

fn genCaptured(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .captured);

    const inst = try c.rega.selectForNoErrNoDepInst(cstr, true, nodeId);
    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, nodeId);
    }
    try c.buf.pushOp3(.captured, c.curBlock.closureLocal, data.idx, inst.dst);

    return finishNoErrNoDepInst(c, inst, true);
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

fn genValueLocal(c: *Chunk, reg: RegisterId, local: Local, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    if (!cstr.isExact()) {
        // Prefer no copy.
        const retain = cstr.type == .simple and cstr.data.simple.retain;
        if (retain) {
            try c.pushCode(.retain, &.{ reg }, nodeId);
        }
        return regValue(c, reg, retain);
    }
    const inst = try c.rega.selectForDstInst(cstr, true, nodeId);
    const numFields: u8 = @intCast(c.sema.types.items[local.some.type].data.object.numFields);
    try c.pushCode(.copyObj, &.{ reg, numFields, inst.dst }, nodeId);
    return finishDstInst(c, inst, true);
}

fn genLiftedValueLocal(c: *Chunk, reg: RegisterId, local: Local, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const inst = try c.rega.selectForDstInst(cstr, true, nodeId);
    try c.pushCode(.boxValue, &.{ reg, inst.dst }, nodeId);
    const numFields: u8 = @intCast(c.sema.types.items[local.some.type].data.object.numFields);
    try c.pushCode(.copyObj, &.{ inst.dst, numFields, inst.dst }, nodeId);
    return finishDstInst(c, inst, true);
}

fn genLocalReg(c: *Chunk, reg: RegisterId, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const local = getLocalInfo(c, reg);

    if (!local.some.lifted) {
        if (local.some.isStructValue) {
            return genValueLocal(c, reg, local, cstr, nodeId);
        }

        const srcv = regValue(c, reg, false);
        var exact_cstr = cstr;
        switch (cstr.type) {
            .preferVolatile => {
                exact_cstr = Cstr.toLocal(reg, false);
                exact_cstr.data.reg.retain = false;
            },
            .simple => {
                exact_cstr = Cstr.toLocal(reg, false);
                exact_cstr.data.reg.retain = local.some.rcCandidate and cstr.data.simple.retain;
            },
            else => {},
        }
        return genToExact(c, srcv, exact_cstr, nodeId);
    } else {
        if (local.some.isStructValue) {
            return genLiftedValueLocal(c, reg, local, cstr, nodeId);
        }

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
            try c.pushCode(.boxValueRetain, &.{ reg, inst.dst }, nodeId);
        } else {
            try c.pushCode(.boxValue, &.{ reg, inst.dst }, nodeId);
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

    const inst = try c.rega.selectForNoErrNoDepInst(cstr, false, nodeId);
    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, nodeId);
    }

    const val = cy.Value.initEnum(@intCast(data.type), @intCast(data.val));
    const constIdx = try c.buf.getOrPushConst(val);
    try genConst(c, constIdx, inst.dst, false, nodeId);

    return finishNoErrNoDepInst(c, inst, false);
}

fn genVarSym(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .varSym);

    const varId = c.compiler.genSymMap.get(data.sym).?.varSym.id;

    const inst = try c.rega.selectForNoErrNoDepInst(cstr, true, nodeId);
    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, nodeId);
    }

    try c.pushOptionalDebugSym(nodeId);       
    const pc = c.buf.len();
    try c.buf.pushOp3(.staticVar, 0, 0, inst.dst);
    c.buf.setOpArgU16(pc + 1, @intCast(varId));

    return finishNoErrNoDepInst(c, inst, true);
}

fn genFuncSym(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .funcSym);

    const inst = try c.rega.selectForDstInst(cstr, true, nodeId);

    const pc = c.buf.len();
    try c.pushOptionalDebugSym(nodeId);
    try c.buf.pushOp3(.staticFunc, 0, 0, inst.dst);
    const rtId = c.compiler.genSymMap.get(data.func).?.func.id;
    c.buf.setOpArgU16(pc + 1, @intCast(rtId));

    return finishDstInst(c, inst, true);
}

fn genTypeSym(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(idx, .typeSym);

    const inst = try c.rega.selectForDstInst(cstr, true, nodeId);
    try c.buf.pushOp1(.sym, @intFromEnum(cy.heap.MetaTypeKind.object));
    try c.pushCodeBytes(std.mem.asBytes(&data.typeId));
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
                    .defined = true,
                    .rcCandidate = c.sema.isRcCandidateType(param.declType),
                    .lifted = param.lifted,
                    .isDynamic = param.declType == bt.Dynamic,
                    .type = param.declType,
                    .isStructValue = c.sema.getTypeKind(param.declType) == .@"struct",
                },
            };

            // Copy param to local.
            if (param.lifted) {
                // Retain param and box.
                try c.buf.pushOp1(.retain, nextReg);
                try c.pushFCode(.box, &.{nextReg, reg}, c.curBlock.debugNodeId);
            } else {
                try c.pushCode(.copyRetainSrc, &.{ nextReg, reg }, c.curBlock.debugNodeId);
            }

            paramCopyIdx += 1;
        } else {
            c.genIrLocalMapStack.items[c.curBlock.irLocalMapStart + i] = nextReg;
            c.genLocalStack.items[c.curBlock.localStart + 4 + 1 + i] = .{
                .some = .{
                    .owned = false,
                    .defined = true,
                    .rcCandidate = c.sema.isRcCandidateType(param.declType),
                    .lifted = false,
                    .isDynamic = param.declType == bt.Dynamic,
                    .type = param.declType,
                    .isStructValue = c.sema.getTypeKind(param.declType) == .@"struct",
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

fn setVarSym(c: *Chunk, idx: usize, nodeId: cy.NodeId) !void {
    _ = nodeId;
    const data = c.ir.getStmtData(idx, .setVarSym).generic;
    const varSym = c.ir.getExprData(data.left, .varSym);

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

    var cstr = Cstr.toLocal(reg, false);
    if (data.declType != bt.Dynamic and data.initType.dynamic) {
        cstr.data.reg.check_type = data.declType;
    }
    const val = try genExpr(c, data.init, cstr);

    const local = getLocalInfoPtr(c, reg);
    local.some.defined = true;

    if (local.some.lifted) {
        try c.pushOptionalDebugSym(nodeId);
        try c.buf.pushOp2(.box, reg, reg);
    }
    local.some.rcCandidate = val.retained;
    if (local.some.isDynamic) {
        local.some.type = data.declType;
    } else {
        local.some.type = data.initType.id;
    }
    local.some.isStructValue = c.sema.getTypeKind(local.some.type) == .@"struct";

    if (!data.zeroMem) {
        // rhs has generated, increase `nextLocalReg`.
        c.curBlock.nextLocalReg += 1;
    }
    log.tracev("declare {}, rced: {} ", .{val.reg, local.some.rcCandidate});
}

fn declareLocal(c: *Chunk, idx: u32, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(idx, .declareLocal);
    _ = try reserveLocalReg(c, data.id, data.declType, data.lifted, nodeId, true);
}

fn setCaptured(c: *Chunk, idx: usize, nodeId: cy.NodeId) !void {
    _ = nodeId;
    const data = c.ir.getStmtData(idx, .setCaptured).generic;
    const capData = c.ir.getExprData(data.left, .captured);

    // RHS.
    // const dstRetained = c.sema.isRcCandidateType(data.leftT.id);
    _ = try genExpr(c, data.right, Cstr.toCaptured(capData.idx));
}

fn irSetLocal(c: *Chunk, idx: usize, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(idx, .setLocal).generic;
    const localData = c.ir.getExprData(data.left, .local);
    const check_type: ?cy.TypeId = if (!data.left_t.dynamic and data.right_t.dynamic) data.left_t.id else null;
    try setLocal(c, localData, data.right, data.right_t.id, nodeId, .{ .check_type = check_type });
}

const SetLocalOptions = struct {
    rightv: ?GenValue = null,
    extraIdx: ?u32 = null,
    check_type: ?cy.TypeId = null,
};

fn setLocal(c: *Chunk, data: ir.Local, rightIdx: u32, right_t: cy.TypeId, nodeId: cy.NodeId, opts: SetLocalOptions) !void {
    const reg = toLocalReg(c, data.id);
    var local = getLocalInfoPtr(c, reg);

    var dst: Cstr = undefined;
    if (local.some.lifted) {
        dst = Cstr.toLiftedLocal(reg, local.some.rcCandidate);
    } else {
        dst = Cstr.toLocal(reg, local.some.rcCandidate);
        if (opts.check_type) |check_type| {
            dst.data.reg.check_type = check_type;
        }
    }

    var rightv: GenValue = undefined;
    if (opts.rightv) |rightv_| {
        rightv = rightv_;
        if (rightv.reg != reg) {
            // Move to local.
            _ = try genToExactDesc(c, rightv, dst, nodeId, opts.extraIdx);
        }
    } else {
        rightv = try genExpr(c, rightIdx, dst);
    }

    // Update retained state. Refetch local.
    updateRegType(c, reg, right_t);
}

fn updateRegType(c: *Chunk, reg: RegisterId, type_id: cy.TypeId) void {
    const local = getLocalInfoPtr(c, reg);
    local.some.defined = true;
    local.some.rcCandidate = c.sema.isRcCandidateType(type_id);
    local.some.type = type_id;
    local.some.isStructValue = c.sema.getTypeKind(type_id) == .@"struct";
}

fn setLocalType(c: *Chunk, idx: usize) !void {
    const data = c.ir.getStmtData(idx, .setLocalType);
    const reg = toLocalReg(c, data.local);
    updateRegType(c, reg, data.type.id);
}

fn opSet(c: *Chunk, idx: usize, nodeId: cy.NodeId) !void {
    _ = nodeId;
    const data = c.ir.getStmtData(idx, .opSet);
    // TODO: Perform optimizations depending on the next set* code.
    try genStmt(c, data.set_stmt);
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
//         return c.reportErrorFmt("Unsupported op: {}", &.{v(data.op)}, nodeId);
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
//         return c.reportErrorFmt("Unsupported op: {}", &.{v(data.op)}, nodeId);
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

fn orOp(c: *Chunk, data: ir.BinOp, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    if (cstr.isExact()) {
        const leftv = try genExpr(c, data.left, Cstr.simple);

        const jumpFalse = try c.pushEmptyJumpNotCond(leftv.reg);

        // Gen left to finalCstr. Require retain to merge with right.
        const finalLeftv = try genToExact(c, leftv, cstr.toRetained(), nodeId);
        const jumpEnd = try c.pushEmptyJump();

        // RHS, gen to final dst.
        c.patchJumpNotCondToCurPc(jumpFalse);
        try releaseTempValue(c, leftv, nodeId);
        const rightv = try genExpr(c, data.right, cstr);

        c.patchJumpToCurPc(jumpEnd);
        try popTempValue(c, leftv);

        return regValue(c, rightv.reg, finalLeftv.retained or rightv.retained);
    } else {
        // Can use cond as merged result. Require retain to merge with right.
        const cond = try c.rega.consumeNextTemp();
        const condCstr = Cstr.toTempRetain(cond);
        const leftv = try genExpr(c, data.left, condCstr);
        const jumpTrue = try c.pushEmptyJumpCond(cond);

        const rightv = try genExpr(c, data.right, condCstr);
        c.patchJumpCondToCurPc(jumpTrue);

        return regValue(c, cond, leftv.retained or rightv.retained);
    }
}

fn andOp(c: *Chunk, data: ir.BinOp, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    if (cstr.isExact()) {
        const leftv = try genExpr(c, data.left, Cstr.simple);

        const jumpFalse = try c.pushEmptyJumpNotCond(leftv.reg);

        // RHS. Goes to final dst whether true or false.
        const rightv = try genExpr(c, data.right, cstr);
        try releaseTempValue(c, leftv, nodeId);
        const jumpEnd = try c.pushEmptyJump();

        // Copy left to dst. Can assume leftv is a non-rc value.
        c.patchJumpNotCondToCurPc(jumpFalse);
        _ = try genToExact(c, leftv, cstr, nodeId);

        c.patchJumpToCurPc(jumpEnd);

        try popTempAndUnwind(c, leftv);
        return regValue(c, rightv.reg, rightv.retained);
    } else {
        // Merged branch result.
        const res = try c.rega.consumeNextTemp();
        const leftv = try genExpr(c, data.left, Cstr.simple);

        const jumpFalse = try c.pushEmptyJumpNotCond(leftv.reg);
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

    const data = c.ir.getStmtData(idx, .retExprStmt);

    // TODO: If the returned expr is a local, consume the local after copying to reg 0.
    var childv: GenValue = undefined;
    if (c.curBlock.type == .main) {
        // Main block.
        childv = try genExpr(c, data.expr, Cstr.simpleRetain);
    } else {
        childv = try genExpr(c, data.expr, Cstr.ret);
    }

    try popTempAndUnwind(c, childv);

    try genBlockReleaseLocals(c);
    if (c.curBlock.type == .main) {
        try c.buf.pushOp1(.end, @intCast(childv.reg));
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

        /// Whether a value has been assigned to the local.
        defined: bool,

        lifted: bool,

        isStructValue: bool,

        isDynamic: bool,
        type: cy.TypeId,
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
        if (local.some.defined and local.some.owned) {
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

fn finishNoErrNoDepInst(c: *Chunk, inst: cy.register.NoErrInst, retainedToDst: bool) !GenValue {
    return finishInst(c, inst.dst, inst.finalDst, retainedToDst, inst.nodeId);
}

fn shouldRetain(c: *Chunk, retainCstr: bool, src: GenValue) bool {
    if (!retainCstr) return false;

    if (!src.isTemp()) {
        const local = getLocalInfo(c, src.reg);
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

            const retain = shouldRetain(c, reg.retain, src);
            if (src.reg != reg.dst) {
                if (dst.data.reg.check_type) |type_id| {
                    try pushTypeCheck(c, src.reg, type_id, nodeId);
                }

                if (reg.releaseDst) {
                    if (retain) {
                        try c.pushCodeExt(.copyRetainRelease, &.{ src.reg, reg.dst }, nodeId, extraIdx);
                    } else {
                        try c.pushCodeExt(.copyReleaseDst, &.{ src.reg, reg.dst }, nodeId, extraIdx);
                    }
                } else {
                    if (retain) {
                        try c.pushCodeExt(.copyRetainSrc, &.{ src.reg, reg.dst }, nodeId, extraIdx);
                    } else {
                        try c.pushCodeExt(.copy, &.{ src.reg, reg.dst }, nodeId, extraIdx);
                    }
                }
            } else {
                if (retain) {
                    try c.pushCode(.retain, &.{ src.reg }, nodeId);
                } else {
                    // Nop.
                }
            }
            return regValue(c, reg.dst, retain or src.retained);
        },
        .liftedLocal => {
            const lifted = dst.data.liftedLocal;
            if (src.reg == lifted.reg) return error.Unexpected;

            if (shouldRetain(c, true, src)) {
                try c.pushCode(.retain, &.{ src.reg }, nodeId);
            }

            if (lifted.rcCandidate) {
                try c.pushCodeExt(.setBoxValueRelease, &.{ lifted.reg, src.reg }, nodeId, extraIdx);
            } else {
                try c.pushCodeExt(.setBoxValue, &.{ lifted.reg, src.reg }, nodeId, extraIdx);
            }
            return GenValue.initRetained(src.retained);
        },
        .varSym => {
            if (shouldRetain(c, true, src)) {
                try c.pushCode(.retain, &.{ src.reg }, nodeId);
            }

            const pc = c.buf.len();
            try c.pushCodeExt(.setStaticVar, &.{ 0, 0, src.reg }, nodeId, extraIdx);
            c.buf.setOpArgU16(pc + 1, @intCast(dst.data.varSym));
            return GenValue.initRetained(src.retained);
        },
        .captured => {
            if (shouldRetain(c, true, src)) {
                try c.pushCode(.retain, &.{ src.reg }, nodeId);
            }

            const captured = dst.data.captured;
            try c.pushCodeExt(.setCaptured, &.{ c.curBlock.closureLocal, captured.idx, src.reg }, nodeId, extraIdx);
            return GenValue.initRetained(src.retained);
        },
        else => {
            log.tracev("{}", .{dst.type});
            return error.NotExactCstr;
        },
    }
}

fn genToFinalDst(c: *Chunk, val: GenValue, dst: Cstr, nodeId: cy.NodeId) !GenValue {
    log.tracev("genToFinalDst src: {} dst: {s}", .{val.reg, @tagName(dst.type)});

    const res = try genToExact(c, val, dst, nodeId);

    // Check to remove the temp that is used to move to final dst.
    if (val.isTemp()) try popTemp(c, val.reg);
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
    const iterv = try genExpr(c, data.iter, Cstr.toTemp(iterTemp + cy.vm.CallArgStart));

    const node = c.ast.node(nodeId);
    const header = c.ast.node(node.data.forIterStmt.header);
    const iterNodeId = header.data.forIterHeader.iterable;
    const eachNodeId = header.data.forIterHeader.eachClause;

    var extraIdx = try c.fmtExtraDesc("iterator()", .{});
    try pushCallObjSymExt(c, iterTemp, 1,
        @intCast(c.compiler.iteratorMID),
        iterNodeId, extraIdx);

    try releaseIf(c, iterv.retained, iterv.reg, iterNodeId);
    _ = try pushUnwind(c, iterTemp);

    try pushBlock(c, true, nodeId);
    try genStmts(c, data.declHead);

    const bodyPc = c.buf.ops.items.len;

    // next()
    try genIterNext(c, iterTemp, data.countLocal != null, iterNodeId);
    const hasCounter = data.countLocal != null;

    const jump_none = try c.pushEmptyJumpNone(iterTemp + 1);
    if (data.eachLocal) |eachLocal| {
        // extraIdx = try c.fmtExtraDesc("copy next() to local", .{});

        // Unwrap to var.
        const unwrap_reg = toLocalReg(c, eachLocal);
        const unwrap_local = getLocalInfoPtr(c, unwrap_reg);
        try pushField(c, iterTemp + 1, 1, unwrap_reg, nodeId);
        // Mark var defined for ARC.
        unwrap_local.some.defined = true;
    }
    if (data.countLocal) |countLocal| {
        extraIdx = try c.fmtExtraDesc("copy count to local", .{});
        const countTemp = regValue(c, iterTemp - 1, false);
        try setLocal(c, .{ .id = countLocal }, undefined, bt.Integer, eachNodeId, .{ .rightv = countTemp, .extraIdx = extraIdx });
    }

    try pushRelease(c, iterTemp + 1, iterNodeId);

    const jumpStackSave: u32 = @intCast(c.blockJumpStack.items.len);
    defer c.blockJumpStack.items.len = jumpStackSave;

    try genStmts(c, data.bodyHead);

    const b = c.blocks.getLast();
    try genReleaseLocals(c, b.nextLocalReg, b.nodeId);
    // Pop sub block.
    try popLoopBlock(c);

    const contPc = c.buf.ops.items.len;
    try c.pushJumpBackTo(bodyPc);

    c.patchForBlockJumps(jumpStackSave, c.buf.ops.items.len, contPc);

    const skip_release = try c.pushEmptyJump();
    c.patchJumpNoneToCurPc(jump_none);
    try pushRelease(c, iterTemp + 1, iterNodeId);
    c.patchJumpToCurPc(skip_release);

    // TODO: Iter local should be a reserved hidden local (instead of temp) so it can be cleaned up by endLocals when aborting the current fiber.
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

    extraIdx = try c.fmtExtraDesc("next()", .{});
    try pushCallObjSymExt(c, iterTemp + 1, 1,
        @intCast(c.compiler.nextMID),
        iterNodeId, extraIdx);

    if (hasCounter) {
        try pushInlineBinExpr(c, .addInt, iterTemp-1, iterTemp-2, iterTemp-1, iterNodeId);
    }
}

fn loopStmt(c: *cy.Chunk, loc: usize, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(loc, .loopStmt);

    const top_pc = c.buf.ops.items.len;
    const jump_start: u32 = @intCast(c.blockJumpStack.items.len);
    defer c.blockJumpStack.items.len = jump_start;

    {
        try pushBlock(c, true, nodeId);
        try genStmts(c, data.body_head);
        try popLoopBlock(c);
    }
    c.patchForBlockJumps(jump_start, c.buf.ops.items.len, top_pc);
}

fn destrElemsStmt(c: *Chunk, idx: usize, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(idx, .destrElemsStmt);
    const localsIdx = c.ir.advanceStmt(idx, .destrElemsStmt);
    const locals = c.ir.getArray(localsIdx, u8, data.numLocals);

    const rightv = try genExpr(c, data.right, Cstr.simple);

    try c.pushFCode(.seqDestructure, &.{rightv.reg, @as(u8, @intCast(locals.len))}, nodeId);
    const start = c.buf.ops.items.len;
    try c.buf.ops.resize(c.alloc, c.buf.ops.items.len + locals.len);
    for (locals, 0..) |local, i| {
        const reg = toLocalReg(c, local);
        updateRegType(c, reg, bt.Any);
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
    const startv = try genExpr(c, data.start, Cstr.simple);

    // Range end.
    const endv = try genExpr(c, data.end, Cstr.toTemp(rangeEnd));
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
    try c.pushFCode(.forRangeInit, &.{ startv.reg, rangeEnd, @intFromBool(data.increment),
        counter, eachLocal, 0, 0 }, nodeId);

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
    try c.pushCode(.forRange, &.{ counter, rangeEnd, eachLocal, 0, 0 }, nodeId);
    c.buf.setOpArgU16(forRangeOp + 4, jumpBackOffset);

    c.patchForBlockJumps(jumpStackSave, c.buf.ops.items.len, forRangeOp);
    c.blockJumpStack.items.len = jumpStackSave;

    try popTempAndUnwind(c, endv);
    try popTemp(c, counter);
}

fn verbose(c: *cy.Chunk, idx: usize, nodeId: cy.NodeId) !void {
    _ = nodeId;
    const data = c.ir.getStmtData(idx, .verbose);
    cc.setVerbose(data.verbose);
}

fn tryStmt(c: *cy.Chunk, idx: usize, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(idx, .tryStmt);
    const pushTryPc = c.buf.ops.items.len;
    try c.pushCode(.pushTry, &.{0, 0, 0, 0}, nodeId);

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
    try c.pushCode(.pushTry, &.{ 0, 0, 0, 0 }, nodeId);

    const mcstr = try MergeCstr.init(c, cstr);

    // Body expr.
    const childv = try genExpr(c, data.expr, mcstr.cstr);

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

fn genUnwrapOr(c: *Chunk, loc: usize, cstr: Cstr, node: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(loc, .unwrap_or);

    var finalCstr = cstr;
    if (!finalCstr.isExact()) {
        const temp = try c.rega.consumeNextTemp();
        finalCstr = Cstr.toTemp(temp);
    }

    const optv = try genExpr(c, data.opt, Cstr.simple);
    try pushUnwindValue(c, optv);
    const jump_none = try c.pushEmptyJumpNone(optv.reg);

    var retain_some = true;
    const inst = try c.rega.selectForDstInst(cstr, retain_some, node);
    try pushField(c, optv.reg, 1, inst.dst, node);
    const somev = finishDstInst(c, inst, retain_some);
    const jump_end = try c.pushEmptyJump();

    // else.
    c.patchJumpNoneToCurPc(jump_none);
    _ = try genExpr(c, data.default, finalCstr);

    c.patchJumpToCurPc(jump_end);
    try popTempAndUnwind(c, optv);
    try releaseTempValue(c, optv, node);

    return somev;
}

fn genIfExpr(c: *Chunk, idx: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    _ = nodeId;
    const data = c.ir.getExprData(idx, .if_expr);
    const condNodeId = c.ir.getNode(data.cond);

    var finalCstr = cstr;
    if (!finalCstr.isExact()) {
        const temp = try c.rega.consumeNextTemp();
        finalCstr = Cstr.toTemp(temp);
    }

    // Cond.
    const condv = try genExpr(c, data.cond, Cstr.simple);

    const condFalseJump = try c.pushEmptyJumpNotCond(condv.reg);

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

fn ifUnwrapStmt(c: *cy.Chunk, loc: usize, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(loc, .ifUnwrapStmt);

    var opt_nid = c.ir.getNode(data.opt);
    var optv = try genExpr(c, data.opt, Cstr.simple);
    try pushUnwindValue(c, optv);

    var jump_none = try c.pushEmptyJumpNone(optv.reg);
    {
        try pushBlock(c, false, nodeId);
        try genStmts(c, data.decl_head);

        // Unwrap to var.
        const unwrap_reg = toLocalReg(c, data.unwrap_local);
        const unwrap_local = getLocalInfoPtr(c, unwrap_reg);
        try pushField(c, optv.reg, 1, unwrap_reg, nodeId);
        // Mark var defined for ARC.
        unwrap_local.some.defined = true;

        // ARC cleanup for true case.
        try popTempAndUnwind(c, optv);
        try releaseTempValue(c, optv, opt_nid);

        try genStmts(c, data.body_head);
        try popBlock(c);
    }

    var jump_end: u32 = undefined;
    if (data.else_block != cy.NullId) {
        jump_end = try c.pushEmptyJump();
    }

    if (optv.isRetainedTemp()) {
        const skip_release = try c.pushEmptyJump();
        c.patchJumpNoneToCurPc(jump_none);
        try pushRelease(c, optv.reg, opt_nid);
        c.patchJumpToCurPc(skip_release);
    } else {
        c.patchJumpNoneToCurPc(jump_none);
    }

    if (data.else_block != cy.NullId) {
        try elseBlocks(c, data.else_block);
        c.patchJumpToCurPc(jump_end);
    }
}

fn ifStmt(c: *cy.Chunk, idx: usize, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(idx, .ifStmt);

    var condNodeId = c.ir.getNode(data.cond);
    var condv = try genExpr(c, data.cond, Cstr.simple);
    try pushUnwindValue(c, condv);

    var jump_miss = try c.pushEmptyJumpNotCond(condv.reg);

    // ARC cleanup for true case.
    try popTempAndUnwind(c, condv);
    try releaseTempValue(c, condv, condNodeId);

    try pushBlock(c, false, nodeId);
    try genStmts(c, data.body_head);
    try popBlock(c);

    var jump_end: u32 = undefined;
    if (data.else_block != cy.NullId) {
        jump_end = try c.pushEmptyJump();
    }

    c.patchJumpNotCondToCurPc(jump_miss);

    if (data.else_block != cy.NullId) {
        try elseBlocks(c, data.else_block);
        c.patchJumpToCurPc(jump_end);
    }
}

fn elseBlocks(c: *cy.Chunk, else_head: u32) !void {
    const bodyEndJumpsStart = c.listDataStack.items.len;
    defer c.listDataStack.items.len = bodyEndJumpsStart;

    var else_loc = else_head;
    while (else_loc != cy.NullId) {
        const else_nid = c.ir.getNode(else_loc);
        const else_b = c.ir.getExprData(else_loc, .else_block);

        if (else_b.cond != cy.NullId) {
            const condNodeId = c.ir.getNode(else_b.cond);
            const condv = try genExpr(c, else_b.cond, Cstr.simple);
            try pushUnwindValue(c, condv);

            const jump_miss = try c.pushEmptyJumpNotCond(condv.reg);

            // ARC cleanup for true case.
            try popTempAndUnwind(c, condv);
            try releaseTempValue(c, condv, condNodeId);

            try pushBlock(c, false, else_nid);
            try genStmts(c, else_b.body_head);
            try popBlock(c);

            if (else_b.else_block != cy.NullId) {
                const jump_end = try c.pushEmptyJump();
                try c.listDataStack.append(c.alloc, .{ .pc = jump_end });
            }

            c.patchJumpNotCondToCurPc(jump_miss);
        } else {
            try pushBlock(c, false, else_nid);
            try genStmts(c, else_b.body_head);
            try popBlock(c);
        }
        else_loc = else_b.else_block;
    }

    // Jump here from all body ends.
    const bodyEndJumps = c.listDataStack.items[bodyEndJumpsStart..];
    for (bodyEndJumps) |jump| {
        c.patchJumpToCurPc(jump.pc);
    }
}

fn genBlockExpr(c: *Chunk, loc: usize, cstr: Cstr, nodeId: cy.NodeId) !GenValue {
    const data = c.ir.getExprData(loc, .blockExpr);

    // Select merged dst.
    const inst = try c.rega.selectForDstInst(cstr, true, nodeId);

    try pushBlock(c, false, nodeId);
    const b = c.genBlock();
    b.blockExprCstr = Cstr.toTempRetain(inst.dst);

    try genStmts(c, data.bodyHead);

    try popBlock(c);

    return finishDstInst(c, inst, true);
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
                try c.buf.pushOp3Ext(.compare, exprv.reg, condv.reg, temp, c.desc(condNodeId));
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
        _ = try genFalse(c, cstr.?, nodeId);
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
    _ = idx;
    const inst = try c.rega.selectForDstInst(cstr, true, nodeId);
    try c.buf.pushOp1(.map, inst.dst);
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

    try c.pushFCode(.list, &.{argStart, data.numArgs, inst.dst}, nodeId);

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

    if (c.compiler.config.gen_debug_func_markers) {
        try c.compiler.buf.pushDebugFuncStart(func, c.id);
    }

    // `reserveFuncRegs` may emit copy and box insts.
    try reserveFuncRegs(c, maxIrLocals, numParamCopies, params);
}

pub const Sym = union {
    varSym: struct {
        id: u32,
    },
    funcs: struct {
        id: u32,
    },
    func: struct {
        id: u32,

        // Used by jit.
        pc: u32,
    },
    hostFunc: struct {
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

    if (c.compiler.config.gen_debug_func_markers) {
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
        try c.pushCode(.lambda, &.{
            0, 0, func.numParams, stackSize, @intFromBool(func.reqCallTypeCheck), 0, 0, inst.dst }, nodeId);
        c.buf.setOpArgU16(start + 1, offset);
        c.buf.setOpArgU16(start + 6, @intCast(func.funcSigId));
    } else {
        const captures = c.ir.getArray(data.captures, u8, data.numCaptures);
        const start = c.buf.ops.items.len;
        try c.pushCode(.closure, &.{
            0, 0, func.numParams, @as(u8, @intCast(captures.len)), stackSize, 
            0, 0, cy.vm.CalleeStart, @intFromBool(func.reqCallTypeCheck), inst.dst
        }, nodeId);
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

pub fn shouldGenMainScopeReleaseOps(c: *cy.Compiler) bool {
    return !c.vm.config.single_run;
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
            return c.reportErrorFmt("Temp registers were not reset. {} > {}", &.{v(c.rega.nextTemp), v(c.rega.tempStart)}, last.debugNodeId);
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
            if (argv.reg != start + i) {
                log.tracev("Expected arg[{}] at {}, got {}.", .{i, start+i, argv.reg});
                return error.Unexpected;
            }
        }
    }
}

fn exprStmt(c: *Chunk, idx: usize, nodeId: cy.NodeId) !void {
    const data = c.ir.getStmtData(idx, .exprStmt);

    if (data.isBlockResult) {
        const inMain = c.curBlock.sBlockDepth == 0;
        if (inMain) {
            const exprv = try genExpr(c, data.expr, Cstr.simpleRetain);
            c.curBlock.endLocal = exprv.reg;
            try popTempValue(c, exprv);
        } else {
            // Return from block expression.
            const b = c.blocks.getLast();
            _ = try genExpr(c, data.expr, b.blockExprCstr);
        }
    } else {
        const exprv = try genExpr(c, data.expr, Cstr.simple);

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
        try popTemp(c, val.reg);
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
        try c.pushUnwindTemp(val.reg);
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
        try popUnwind(c, val.reg);
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
    local,
    temp,
};

pub const GenValue = struct {
    type: GenValueType,

    reg: RegisterId,

    /// Whether this value was retained by 1 refcount.
    retained: bool,

    data: union {
        jitCondFlag: struct {
            type: jitgen.JitCondFlagType,
        },
        constant: struct {
            val: cy.Value,
        },
    } = undefined,

    pub fn initConstant(val: cy.Value) GenValue {
        return .{ .type = .constant, 
            .reg = undefined, .retained = false,
            .data = .{ .constant = .{
                .val = val,
            }
        }};
    }

    fn initJitCondFlag(condt: jitgen.JitCondFlagType) GenValue {
        return .{ .type = .jitCondFlag,
            .reg = undefined, .retained = false,
            .data = .{ .jitCondFlag = .{
                .type = condt,
            }},
        };
    }

    pub fn initRetained(retained: bool) GenValue {
        return .{
            .type = .generic,
            .reg = 255,
            .retained = retained,
        };
    }

    fn initNoValue() GenValue {
        return .{
            .type = .generic,
            .reg = 255,
            .retained = false,
        };
    }

    pub fn initLocalValue(reg: RegisterId, retained: bool) GenValue {
        return .{
            .type = .local,
            .reg = reg,
            .retained = retained,
        };
    }

    pub fn initTempValue(reg: RegisterId, retained: bool) GenValue {
        return .{
            .type = .temp,
            .reg = reg,
            .retained = retained,
        };
    }

    pub fn isTemp(self: GenValue) bool {
        return self.type == .temp;
    }

    pub fn isRetainedTemp(self: GenValue) bool {
        return self.type == .temp and self.retained;
    }

    pub fn isRetainedLocal(self: GenValue) bool {
        return self.type == .local and self.retained;
    }
};

fn pushReleaseVals(self: *Chunk, vals: []const GenValue, debugNodeId: cy.NodeId) !void {
    if (vals.len > 1) {
        try self.pushOptionalDebugSym(debugNodeId);
        try self.buf.pushOp1(.releaseN, @intCast(vals.len));

        const start = self.buf.ops.items.len;
        try self.buf.ops.resize(self.alloc, self.buf.ops.items.len + vals.len);
        for (vals, 0..) |val, i| {
            self.buf.ops.items[start+i] = .{ .val = val.reg };
        }
    } else if (vals.len == 1) {
        try pushRelease(self, vals[0].reg, debugNodeId);
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
        try pushReleases(c, &.{ a.reg, b.reg }, debugId);
    } else {
        if (releaseA) {
            try pushRelease(c, a.reg, debugId);
        }
        if (releaseB) {
            try pushRelease(c, b.reg, debugId);
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
        try pushRelease(c, val.reg, nodeId);
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

fn pushTypeCheckOption(c: *cy.Chunk, local: RegisterId, nodeId: cy.NodeId) !void {
    try c.pushFCode(.typeCheckOption, &.{ local }, nodeId);
}

fn pushTypeCheck(c: *cy.Chunk, local: RegisterId, typeId: cy.TypeId, nodeId: cy.NodeId) !void {
    const start = c.buf.ops.items.len;
    try c.pushFCode(.typeCheck, &.{ local, 0, 0, }, nodeId);
    c.buf.setOpArgU16(start + 2, @intCast(typeId));
}

fn pushCallSym(c: *cy.Chunk, startLocal: u8, numArgs: u32, numRet: u8, symId: u32, nodeId: cy.NodeId) !void {
    const start = c.buf.ops.items.len;
    try c.pushFCode(.callSym, &.{ startLocal, @as(u8, @intCast(numArgs)), numRet, 0, 0, 0, 0, 0, 0, 0, 0 }, nodeId);
    c.buf.setOpArgU16(start + 4, @intCast(symId));
}

fn pushCallSymDyn(c: *cy.Chunk, startLocal: u8, numArgs: u32, numRet: u8, symId: u32, nodeId: cy.NodeId) !void {
    const start = c.buf.ops.items.len;
    try c.pushFCode(.call_sym_dyn, &.{ startLocal, @as(u8, @intCast(numArgs)), numRet, 0, 0, 0, 0, 0, 0, 0, 0 }, nodeId);
    c.buf.setOpArgU16(start + 4, @intCast(symId));
}

fn pushCall(c: *cy.Chunk, ret: u8, numArgs: u32, numRet: u8, nodeId: cy.NodeId) !void {
    try c.pushFCode(.call, &.{ret, @as(u8, @intCast(numArgs)), numRet}, nodeId);
}

fn reserveLocalRegAt(c: *Chunk, irLocalId: u8, declType: types.TypeId, lifted: bool, reg: u8, nodeId: cy.NodeId) !void {
    // Stacks are always big enough because of pushProc.
    log.tracev("reserve irId={} reg={} {*} {} {} {} {s}", .{ irLocalId, reg, c.curBlock, c.curBlock.irLocalMapStart, c.curBlock.localStart, declType, c.sema.getTypeBaseName(declType) });
    log.tracev("local stacks {} {}", .{ c.genIrLocalMapStack.items.len, c.genLocalStack.items.len });
    c.genIrLocalMapStack.items[c.curBlock.irLocalMapStart + irLocalId] = reg;
    c.genLocalStack.items[c.curBlock.localStart + reg] = .{
        .some = .{
            .owned = true,

            // Not yet initialized, so it does not have a refcount.
            .defined = false,

            .lifted = lifted,
            .isDynamic = declType == bt.Dynamic,
            .rcCandidate = undefined,
            .isStructValue = undefined,
            .type = undefined,
        },
    };
    updateRegType(c, reg, declType);

    if (cy.Trace) {
        const nodeStr = try c.encoder.format(nodeId, &cy.tempBuf);
        log.tracev("reserve {} {}: {s}", .{c.curBlock.localStart, reg, nodeStr});
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

fn pushCallObjSym(c: *cy.Chunk, ret: u8, numArgs: u8, method: u16, nodeId: cy.NodeId) !void {
    try pushCallObjSymExt(c, ret, numArgs, method, nodeId, cy.NullId);
}

fn pushCallObjSymExt(c: *cy.Chunk, ret: u8, numArgs: u8, method: u16, nodeId: cy.NodeId, extraIdx: u32) !void {
    try c.pushFailableDebugSym(nodeId);
    const start = c.buf.ops.items.len;
    try c.buf.pushOpSliceExt(.callObjSym, &.{
        ret, numArgs, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    }, c.descExtra(nodeId, extraIdx));
    c.buf.setOpArgU16(start + 4, method);
}

fn pushInlineUnExpr(c: *cy.Chunk, code: cy.OpCode, child: u8, dst: u8, nodeId: cy.NodeId) !void {
    try c.pushFCode(code, &.{ child, dst, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }, nodeId);
}

fn pushInlineBinExpr(c: *cy.Chunk, code: cy.OpCode, left: u8, right: u8, dst: u8, nodeId: cy.NodeId) !void {
    try c.pushFCode(code, &.{ left, right, dst, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }, nodeId);
}

fn pushInlineTernExpr(c: *cy.Chunk, code: cy.OpCode, a: u8, b: u8, c_: u8, dst: u8, nodeId: cy.NodeId) !void {
    try c.pushFCode(code, &.{ a, b, c_, dst, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }, nodeId);
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
        const start = c.buf.ops.items.len;
        try c.pushCode(.objectSmall, &.{ 0, 0, startLocal, numFields, dst }, debugNodeId);
        c.buf.setOpArgU16(start + 1, @intCast(typeId)); 
    } else {
        const start = c.buf.ops.items.len;
        try c.pushFCode(.object, &.{ 0, 0, startLocal, numFields, dst }, debugNodeId);
        c.buf.setOpArgU16(start + 1, @intCast(typeId)); 
    }
}

fn pushFieldDyn(c: *cy.Chunk, recv: u8, dst: u8, fieldId: u16, debugNodeId: cy.NodeId) !void {
    const start = c.buf.ops.items.len;
    try c.pushFCode(.fieldDyn, &.{ recv, dst, 0, 0, 0, 0, 0, c.rega.nextTemp }, debugNodeId);
    c.buf.setOpArgU16(start + 3, fieldId);
}

fn pushField(c: *cy.Chunk, recv: u8, fieldIdx: u8, dst: u8, debugNodeId: cy.NodeId) !void {
    try c.pushCode(.field, &.{ recv, fieldIdx, dst }, debugNodeId);
}

fn pushFieldRef(c: *cy.Chunk, recv: u8, fieldIdx: u8, numNestedFields: u8, dst: u8, debugNodeId: cy.NodeId) !void {
    try c.pushCode(.fieldRef, &.{ recv, fieldIdx, numNestedFields, dst }, debugNodeId);
}

pub const PreferDst = struct {
    dst: RegisterId,
    canUseDst: bool,

    pub fn nextCstr(self: *PreferDst, val: GenValue) Cstr {
        switch (val.type) {
            .generic => {
                if (val.reg == self.dst) {
                    self.canUseDst = false;
                }
            },
            .temp => {
                if (val.reg == self.dst) {
                    self.canUseDst = false;
                }
            },
            else => {},
        }
        return Cstr.preferVolatileIf(self.canUseDst, self.dst);
    }
};