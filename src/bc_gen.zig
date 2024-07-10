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
const ast = cy.ast;
const bc = @This();
const Root = @This();

pub const SlotId = u8;
const TypeId = types.TypeId;

const Chunk = cy.chunk.Chunk;

pub const VtableKey = packed struct {
    type: cy.TypeId,
    trait: cy.TypeId,
};

pub fn genAll(c: *cy.Compiler) !void {
    // Constants.
    c.vm.emptyString = try c.buf.getOrPushStaticAstring("");
    c.vm.emptyArray = try cy.heap.allocArray(c.vm, "");
    try c.vm.staticObjects.append(c.alloc, c.vm.emptyArray.asHeapObject());

    // Prepare types.
    for (c.newTypes(), c.type_start..) |stype, typeId| {
        if (stype.kind == .null) {
            // Skip placeholders.
            continue;
        }
        log.tracev("prep type: {s}", .{stype.sym.name()});
        const sym = stype.sym;

        if (stype.info.ct_infer or stype.info.ct_ref) {
            continue;
        }

        switch (sym.type) {
            .object_t => {
                const obj = sym.cast(.object_t);
                for (obj.fields[0..obj.numFields], 0..) |field, i| {
                    const fieldSymId = try c.vm.ensureFieldSym(field.sym.head.name());
                    try c.vm.addFieldSym(@intCast(typeId), fieldSymId, @intCast(i), field.type);
                }
            },
            .struct_t => {
                const obj = sym.cast(.struct_t);
                for (obj.fields[0..obj.numFields], 0..) |field, i| {
                    const fieldSymId = try c.vm.ensureFieldSym(field.sym.head.name());
                    try c.vm.addFieldSym(@intCast(typeId), fieldSymId, @intCast(i), field.type);
                }
            },
            .dummy_t,
            .trait_t,
            .int_t,
            .float_t,
            .bool_t,
            .custom_t,
            .enum_t => {},
            else => {
                log.tracev("{}", .{sym.type});
                return error.Unsupported;
            },
        }
    }

    for (c.newChunks()) |chunk| {
        log.tracev("prep chunk", .{});
        for (chunk.syms.items) |sym| {
            try prepareSym(c, sym);
        }
        for (chunk.variantFuncSyms.items) |func| {
            try prepareFunc(c, null, func);
        }
    }

    if (!c.cont) {
        // test_int.
        try c.vm.c.getContextVars().append(c.alloc, .{ .value = cy.Value.initInt(123) });
    }

    for (c.newChunks()) |chunk| {
        for (chunk.syms.items) |sym| {
            switch (sym.type) {
                .object_t => {
                    // rt funcs have been reserved. Create impl vtables.
                    const object_t = sym.cast(.object_t);
                    for (object_t.impls()) |impl| {
                        const vtable = try c.alloc.alloc(u32, impl.funcs.len);
                        for (impl.funcs, 0..) |func, i| {
                            vtable[i] = c.genSymMap.get(func).?.func.id;
                        }
                        const vtable_idx = c.vm.vtables.len;
                        try c.vm.vtables.append(c.alloc, vtable);
                        try c.gen_vtables.put(c.alloc, VtableKey{ .type = object_t.type, .trait = impl.trait.type }, @intCast(vtable_idx));
                    }
                },
                else => {},
            }
        }
    }

    // Bind the rest that aren't in sema.
    try @call(.never_inline, cy.bindings.bindCore, .{c.vm});

    for (c.newChunks()) |chunk| {
        log.tracev("Perform codegen for chunk{}: {s}", .{chunk.id, chunk.srcUri});
        chunk.buf = &c.buf;
        try genChunk(chunk);
        log.tracev("Done. performChunkCodegen {s}", .{chunk.srcUri});
    }

    // Merge inst and const buffers.
    const reqLen = c.buf.ops.items.len + c.buf.consts.items.len * @sizeOf(cy.Value) + @alignOf(cy.Value) - 1;
    if (c.buf.ops.capacity < reqLen) {
        try c.buf.ops.ensureTotalCapacityPrecise(c.alloc, reqLen);
    }
    const constAddr = std.mem.alignForward(usize, @intFromPtr(c.buf.ops.items.ptr) + c.buf.ops.items.len, @alignOf(cy.Value));
    const constDst = @as([*]cy.Value, @ptrFromInt(constAddr))[0..c.buf.consts.items.len];
    @memcpy(constDst, c.buf.consts.items);
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
    log.tracev("prep sym: {s}", .{sym.name()});
    switch (sym.type) {
        .hostVar => {
            const id = c.vm.c.varSyms_len;
            const rtVar = rt.VarSym.init(sym.cast(.hostVar).val);
            cy.arc.retain(c.vm, rtVar.value);
            try c.vm.c.getVarSyms().append(c.alloc, rtVar);
            try c.vm.varSymExtras.append(c.alloc, sym);
            try c.genSymMap.putNoClobber(c.alloc, sym, .{ .varSym = .{ .id = @intCast(id) }});
        },
        .userVar => {
            const id = c.vm.c.varSyms_len;
            const rtVar = rt.VarSym.init(cy.Value.initInt(0));
            try c.vm.c.getVarSyms().append(c.alloc, rtVar);
            try c.vm.varSymExtras.append(c.alloc, sym);
            try c.genSymMap.putNoClobber(c.alloc, sym, .{ .varSym = .{ .id = @intCast(id) }});
        },
        .func => {
            const func_sym = sym.cast(.func);
            if (func_sym.first.type == .trait) {
                return;
            }
            const name = func_sym.head.name();

            const group = try c.vm.addFuncGroup();
            var cur: ?*cy.Func = func_sym.first;
            var has_method = false;
            while (cur) |func| {
                if (func.isMethod()) {
                    has_method = true;
                }
                try prepareFunc(c, group, func);
                cur = func.next;
            }
            try c.genSymMap.putNoClobber(c.alloc, sym, .{ .func_sym = .{ .group = @intCast(group) }});
            if (has_method) {
                const parentT = func_sym.head.parent.?.getStaticType().?;
                try c.vm.setMethodGroup(parentT, name, @intCast(group));
            }
        },
        .context_var,
        .template,
        .custom_t,
        .bool_t,
        .int_t,
        .float_t,
        .chunk,
        .field,
        .struct_t,
        .object_t,
        .trait_t,
        .typeAlias,
        .distinct_t,
        .placeholder,
        .enum_t,
        .enumMember,
        .use_alias,
        .module_alias => {},
        else => {
            log.tracev("{}", .{sym.type});
            return error.Unsupported;
        }
    }
}

fn prepareFunc(c: *cy.Compiler, opt_group: ?rt.FuncGroupId, func: *cy.Func) !void {
    switch (func.type) {
        .template,
        .trait,
        .userLambda => return,

        .hostFunc => {
            if (cy.Trace) {
                const symPath = try cy.sym.allocSymName(&c.sema, c.alloc, @ptrCast(func.sym.?), .{});
                defer c.alloc.free(symPath);
                log.tracev("prep func: {s}", .{symPath});
            }
            const funcSig = c.sema.getFuncSig(func.funcSigId);
            const rtFunc = rt.FuncSymbol.initHostFunc(@ptrCast(func.data.hostFunc.ptr), funcSig.reqCallTypeCheck, func.isMethod(), funcSig.numParams(), func.funcSigId);
            if (opt_group) |group| {
                _ = try addGroupFunc(c, group, func, rtFunc);
            } else {
                _ = try addFunc(c, func, rtFunc);
            }
        },
        .userFunc => {
            if (cy.Trace) {
                const symPath = try cy.sym.allocSymName(&c.sema, c.alloc, @ptrCast(func.sym.?), .{});
                defer c.alloc.free(symPath);
                log.tracev("prep func: {s}", .{symPath});
            }
            if (opt_group) |group| {
                _ = try addGroupFunc(c, group, func, rt.FuncSymbol.initNull());
            } else {
                _ = try addFunc(c, func, rt.FuncSymbol.initNull());
            }
            // Func is patched later once funcPc and stackSize is obtained.
        },
    }
}

fn addGroupFunc(c: *cy.Compiler, group: rt.FuncGroupId, func: *cy.Func, rtFunc: rt.FuncSymbol) !u32 {
    const id = try c.vm.addGroupFunc(group, func.name(), func.funcSigId, rtFunc);
    try c.genSymMap.putNoClobber(c.alloc, func, .{ .func = .{ .id = @intCast(id), .pc = 0 }});
    return @intCast(id);
}

fn addFunc(c: *cy.Compiler, func: *cy.Func, rtFunc: rt.FuncSymbol) !u32 {
    const id = try c.vm.addFunc(func.name(), func.funcSigId, rtFunc);
    try c.genSymMap.putNoClobber(c.alloc, func, .{ .func = .{ .id = @intCast(id), .pc = 0 }});
    return @intCast(id);
}

fn genChunk(c: *Chunk) !void {
    genChunkInner(c) catch |err| {
        if (err != error.CompileError) {
            // Wrap all other errors as a CompileError.
            return c.reportErrorFmt("error.{}", &.{v(err)}, c.curNode);
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
    const node = c.ir.getNode(idx);
    c.curNode = node;
    if (cy.Trace) {
        const contextStr = try c.encoder.format(node, &cy.tempBuf);
        if (cc.verbose()) {
            rt.logFmt("{}| {}: `{}` unw={} nslots={}", &.{
                cy.fmt.repeat(' ', c.indent * 4), v(@tagName(code)), v(contextStr),
                v(c.unwind_stack.items.len), v(c.slot_stack.items.len),
            });
        }
    }

    var exp_unwind_index_start: usize = undefined;
    var exp_slot_count: usize = undefined;
    if (c.proc_stack.items.len > 0) {
        exp_unwind_index_start = @intCast(c.unwind_stack.items.len);
        exp_slot_count = c.slot_stack.items.len;
    }

    switch (code) {
        .breakStmt          => try breakStmt(c, node),
        .contStmt           => try contStmt(c, node),
        .declareLocal       => {
            try declareLocal(c, idx, node);
            exp_slot_count += 1;
        },
        .declareLocalInit   => {
            const slot = try declareLocalInit(c, idx, node);
            if (getSlot(c, slot).boxed) {
                exp_unwind_index_start += 1;
            }
            exp_slot_count += 1;
        },
        .exprStmt           => try exprStmt(c, idx, node),
        .forIterStmt        => try forIterStmt(c, idx, node),
        .forRangeStmt       => try forRangeStmt(c, idx, node),
        .funcBlock          => try funcBlock(c, idx, node),
        .ifStmt             => try ifStmt(c, idx, node),
        .loopStmt           => try loopStmt(c, idx, node),
        .mainBlock          => try mainBlock(c, idx, node),
        .block              => try genBlock(c, idx, node),
        .opSet              => try opSet(c, idx, node),
        .pushDebugLabel     => try pushDebugLabel(c, idx),
        .retExprStmt        => try bc.retExprStmt(c, idx, node),
        .retStmt            => try retStmt(c),
        .setCaptured        => try setCaptured(c, idx, node),
        .set_field_dyn      => try setFieldDyn(c, idx, .{}, node),
        .setIndex           => try setIndex(c, idx, node),
        .setLocal           => try irSetLocal(c, idx, node),
        .set_field          => try setField(c, idx, .{}, node),
        .setVarSym          => try setVarSym(c, idx, node),
        .switchStmt         => try switchStmt(c, idx, node),
        .tryStmt            => try tryStmt(c, idx, node),
        .verbose            => try verbose(c, idx, node),
        // TODO: Specialize op assign.
        // .opSetLocal => try opSetLocal(c, getData(pc, .opSetLocal), node),
        // .opSetObjectField => try opSetObjectField(c, getData(pc, .opSetObjectField), node),
        // .opSetField => try opSetField(c, getData(pc, .opSetField), node),
        //         .dumpBytecode => {
        //             try cy.debug.dumpBytecode(c.compiler.vm, null);
        //         },
        else => {
            return error.TODO;
        }
    }

    if (cy.Trace) {
        if (cc.verbose()) {
            rt.logFmt("{}| end {} unw={} nslots={}", &.{
                cy.fmt.repeat(' ', c.indent * 4), v(@tagName(code)),
                v(c.unwind_stack.items.len), v(numSlots(c)),
            });
        }
    }

    // Check stack after statement.
    if (c.proc_stack.items.len > 0) {
        if (c.unwind_stack.items.len != exp_unwind_index_start) {
            return c.reportErrorFmt("Expected {} arc unwind index, found {}.",
                &.{v(exp_unwind_index_start), v(c.unwind_stack.items.len)}, node);
        }

        if (c.slot_stack.items.len != exp_slot_count) {
            return c.reportErrorFmt("Expected {} slots, found {}.",
                &.{v(exp_slot_count), v(c.slot_stack.items.len)}, node);
        }
    }
}

fn genChunkInner(c: *Chunk) !void {
    c.dataStack.clearRetainingCapacity();
    c.dataU8Stack.clearRetainingCapacity();
    c.listDataStack.clearRetainingCapacity();

    c.indent = 0;

    const code = c.ir.getStmtCode(0);
    if (code != .root) return error.Unexpected;

    const data = c.ir.getStmtData(0, .root);
    try genStmts(c, data.bodyHead);

    // Ensure that all cstr and values were accounted for.
    if (c.unwind_stack.items.len > 0) {
        return c.reportErrorFmt("Remaining arc unwind index: {}", &.{v(c.unwind_stack.items.len)}, null);
    }
}

fn genExpr(c: *Chunk, idx: usize, cstr: Cstr) anyerror!GenValue {
    const code = c.ir.getExprCode(idx);
    const node = c.ir.getNode(idx);
    if (cy.Trace) {
        c.indent += 1;
        const contextStr = try c.encoder.format(node, &cy.tempBuf);
        if (cc.verbose()) {
            rt.logFmt("{}( {}: `{}` {} unw={} nslots={}", &.{
                cy.fmt.repeat(' ', c.indent * 4), v(@tagName(code)), v(contextStr), v(@tagName(cstr.type)),
                v(c.unwind_stack.items.len), v(numSlots(c)),
            });
        }
    }

    const res = try switch (code) {
        .await_expr         => genAwait(c, idx, cstr, node),
        .box                => genBox(c, idx, cstr, node),
        .captured           => genCaptured(c, idx, cstr, node),
        .cast               => genCast(c, idx, cstr, node),
        .coinitCall         => genCoinitCall(c, idx, cstr, node),
        .context            => genContext(c, idx, cstr, node),
        .coresume           => genCoresume(c, idx, cstr, node),
        .coyield            => genCoyield(c, idx, cstr, node),
        .enumMemberSym      => genEnumMemberSym(c, idx, cstr, node),
        .errorv             => genError(c, idx, cstr, node),
        .falsev             => genFalse(c, cstr, node),
        .fieldDyn           => genFieldDyn(c, idx, cstr, .{}, node),
        .field              => genField(c, idx, cstr, .{}, node),
        .float              => genFloat(c, idx, cstr, node),
        .funcSym            => genFuncSym(c, idx, cstr, node),
        .if_expr            => genIfExpr(c, idx, cstr, node),
        .int                => genInt(c, idx, cstr, node),
        .lambda             => genLambda(c, idx, cstr, node),
        .list               => genList(c, idx, cstr, node),
        .local              => genLocal(c, idx, cstr, node),
        .map                => genMap(c, idx, cstr, node),
        .none               => genNone(c, idx, cstr, node),
        .object_init        => genObjectInit(c, idx, cstr, node),
        .pre                => return error.Unexpected,
        .preBinOp           => genBinOp(c, idx, cstr, .{}, node),
        .preCallDyn         => genCallDyn(c, idx, cstr, node),
        .preCallFuncSym     => genCallFuncSym(c, idx, cstr, node),
        .pre_call_sym_dyn   => genCallSymDyn(c, idx, cstr, node),
        .pre_call_trait     => genCallTrait(c, idx, cstr, node),
        .preCallObjSym      => genCallObjSym(c, idx, cstr, node),
        .preUnOp            => genUnOp(c, idx, cstr, node),
        .range              => genRange(c, idx, cstr, node),
        .string             => genString(c, idx, cstr, node),
        .stringTemplate     => genStringTemplate(c, idx, cstr, node),
        .switchExpr         => genSwitch(c, idx, cstr, node),
        .symbol             => genSymbol(c, idx, cstr, node),
        .tag_lit            => genTagLit(c, idx, cstr, node),
        .type_check         => genTypeCheck(c, idx, cstr, node),
        .typeCheckOption    => genTypeCheckOption(c, idx, cstr, node),
        .throw              => genThrow(c, idx, node),
        .trait              => genTrait(c, idx, cstr, node),
        .truev              => genTrue(c, cstr, node),
        .tryExpr            => genTryExpr(c, idx, cstr, node),
        .typeSym            => genTypeSym(c, idx, cstr, node),
        .unbox              => genUnbox(c, idx, cstr, node),
        .unwrapChoice       => genUnwrapChoice(c, idx, cstr, node),
        .unwrap_or          => genUnwrapOr(c, idx, cstr, node),
        .varSym             => genVarSym(c, idx, cstr, node),
        .blockExpr          => genBlockExpr(c, idx, cstr, node),
        else => {
            rt.errZFmt(c.vm, "{}\n", .{code});
            return error.TODO;
        }
    };
    if (cy.Trace) {
        if (cc.verbose()) {
            rt.logFmt("{}) end {} unw={} nslots={}", &.{
                cy.fmt.repeat(' ', c.indent * 4), v(@tagName(code)),
                v(c.unwind_stack.items.len), v(numSlots(c)),
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

fn mainBlock(c: *Chunk, idx: usize, node: *ast.Node) !void {
    const data = c.ir.getStmtData(idx, .mainBlock);
    log.tracev("main block: {}", .{data.maxLocals});

    try pushProc(c, .main, "main", node);

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
    try popUnwindBoundary(c, node);
}

fn funcBlock(c: *Chunk, idx: usize, node: *ast.Node) !void {
    const data = c.ir.getStmtData(idx, .funcBlock);
    const func = data.func;
    const paramsIdx = c.ir.advanceStmt(idx, .funcBlock);
    const params = c.ir.getArray(paramsIdx, ir.FuncParam, func.numParams);

    // Reserve jump to skip the body.
    const skipJump = try c.pushEmptyJump();

    const funcPc = c.buf.ops.items.len;

    try pushFuncBlock(c, data, params, node);

    try genStmts(c, data.bodyHead);

    // Get stack size.
    const stackSize = c.getMaxUsedRegisters();

    // Patch empty func sym slot.
    const rtId = c.compiler.genSymMap.get(func).?.func.id;
    const rtFunc = rt.FuncSymbol.initFunc(funcPc, stackSize, func.numParams, func.funcSigId, func.reqCallTypeCheck, func.isMethod());
    c.compiler.vm.funcSyms.buf[rtId] = rtFunc;

    try popFuncBlockCommon(c, func);
    c.patchJumpToCurPc(skipJump);
}

fn genAwait(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .await_expr);
    const ret_t = c.ir.getExprType(idx).id;
    const inst = try bc.selectForDstInst(c, cstr, ret_t, true, node);
    const childv = try genExpr(c, data.expr, Cstr.simple);

    try c.pushCode(.await_op, &.{childv.reg}, node);
    try c.pushCode(.future_value, &.{childv.reg, inst.dst }, node);
    try popTempValue(c, childv, node);

    return finishDstInst(c, inst, true);
}

fn genCoresume(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .coresume);
    const inst = try bc.selectForDstInst(c, cstr, bt.Any, true, node);

    const childv = try genExpr(c, data.expr, Cstr.simpleRetain);

    try c.pushCode(.coresume, &.{childv.reg, inst.dst}, node);
    try popTempValue(c, childv, node);

    return finishDstInst(c, inst, true);
}

fn genCoyield(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = idx;
    try c.pushFCode(.coyield, &.{0, 0}, node);
    // TODO: return coyield expression.
    return genFalse(c, cstr, node);
}

fn genCoinitCall(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const callIdx = c.ir.advanceExpr(idx, .coinitCall);
    const callCode: ir.ExprCode = @enumFromInt(c.ir.buf.items[callIdx]);
    const data = c.ir.getExprData(callIdx, .pre);

    const inst = try bc.selectForDstInst(c, cstr, bt.Fiber, true, node);

    const tempStart = numSlots(c);
    var numArgs: u32 = 0;
    var args: []align(1) const u32 = undefined;
    if (callCode == .preCallFuncSym) {
        numArgs = data.callFuncSym.numArgs;

        const argsIdx = c.ir.advanceExpr(callIdx, .preCallFuncSym);
        args = c.ir.getArray(argsIdx, u32, numArgs);
    } else if (callCode == .preCallDyn) {
        numArgs = data.callDyn.numArgs;
        args = c.ir.getArray(data.callDyn.args, u32, numArgs);

        const temp = try bc.reserveTemp(c, bt.Dyn);
        _ = try genExpr(c, data.callDyn.callee, Cstr.toTempRetain(temp));
    } else return error.Unexpected;

    for (args) |argIdx| {
        const arg_t = c.ir.getExprType(argIdx).id;
        const temp = try bc.reserveTemp(c, arg_t);
        _ = try genExpr(c, argIdx, Cstr.toTempRetain(temp));
    }

    const callExprId = node.cast(.coinit).child;

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
        @intCast(tempStart), @as(u8, @intCast(numTotalArgs)), argDst, 0, @as(u8, @intCast(initialStackSize)), inst.dst }, node);

    try pushFiberBlock(c, @intCast(numTotalArgs), node);

    // Gen func call.
    const callRet: u8 = 1;
    if (callCode == .preCallFuncSym) {
        const rtId = c.compiler.genSymMap.get(data.callFuncSym.func).?.func.id;
        try pushCallSym(c, callRet, numArgs, 1, rtId, @ptrCast(callExprId));
    } else if (callCode == .preCallDyn) {
        try pushCall(c, callRet, numArgs, 1, @ptrCast(callExprId));
    } else return error.Unexpected;

    try c.pushCode(.coreturn, &.{}, node);
    c.buf.setOpArgs1(coinitPc + 4, @intCast(c.buf.ops.items.len - coinitPc));

    try popFiberBlock(c);

    try popTemps(c, numTotalArgs, node);

    return finishDstInst(c, inst, true);
}

fn genCast(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .cast);
    const ret_t = c.ir.getExprType(idx).id;

    if (!data.isRtCast) {
        return genExpr(c, data.expr, cstr);
    }

    const inst = try bc.selectForDstInst(c, cstr, ret_t, false, node);

    // TODO: If inst.dst is a temp, this should have a cstr of localOrExact.
    const childv = try genExpr(c, data.expr, Cstr.simple);

    if (types.toRtConcreteType(data.typeId)) |tId| {
        const pc = c.buf.ops.items.len;
        try c.pushFCode(.cast, &.{ childv.reg, 0, 0, inst.dst }, node);
        c.buf.setOpArgU16(pc + 2, @intCast(tId));
    } else {
        // Cast to abstract type.
        const pc = c.buf.ops.items.len;
        try c.pushFCode(.castAbstract, &.{ childv.reg, 0, 0, inst.dst }, node);
        c.buf.setOpArgU16(pc + 2, @intCast(data.typeId));
    }

    try popTempValue(c, childv, node);

    return finishDstInst(c, inst, childv.retained);
} 

const FieldOptions = struct {
    recv: ?GenValue = null,
};

fn genFieldDyn(c: *Chunk, idx: usize, cstr: Cstr, opts: FieldOptions, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .fieldDyn);

    const inst = try bc.selectForDstInst(c, cstr, bt.Dyn, true, node);
    const ownRecv = opts.recv == null;

    var recv: GenValue = undefined;
    if (opts.recv) |recv_| {
        recv = recv_;
    } else {
        recv = try genExpr(c, data.rec, Cstr.simple);
    }

    const fieldId = try c.compiler.vm.ensureFieldSym(data.name);
    try pushFieldDyn(c, recv.reg, inst.dst, @intCast(fieldId), node);

    if (ownRecv) {
        try popTempValue(c, recv, node);
    }
    if (inst.dst_owned) {
        try initSlot(c, inst.dst, true, node);
    }
    return finishDstInst(c, inst, true);
}

fn genField(c: *Chunk, idx: usize, cstr: Cstr, opts: FieldOptions, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .field);
    const ret_t = c.ir.getExprType(idx).id;

    const inst = try bc.selectForDstInst(c, cstr, ret_t, true, node);
    const ownRecv = opts.recv == null;

    var recv: GenValue = undefined;
    if (opts.recv) |recv_| {
        recv = recv_;
    } else {
        recv = try genExpr(c, data.rec, Cstr.simple);
    }

    const isStruct = c.sema.getTypeKind(ret_t) == .@"struct";
    var getRef = false;
    var refTemp: SlotId = undefined;
    if (data.numNestedFields > 0) {
        // get ref.
        refTemp = try bc.reserveTemp(c, bt.Pointer);
        try pushFieldRef(c, recv.reg, data.idx, data.numNestedFields, refTemp, node);
        const fieldsLoc = c.ir.advanceExpr(idx, .field);
        const fields = c.ir.getArray(fieldsLoc, u8, data.numNestedFields);
        try c.pushCodeBytes(fields);
        getRef = true;
    } else {
        if (isStruct) {
            refTemp = try bc.reserveTemp(c, bt.Pointer);
            try pushFieldRef(c, recv.reg, data.idx, 0, refTemp, node);
            getRef = true;
        }
    }

    if (getRef) {
        if (c.sema.getTypeKind(ret_t) == .@"struct") {
            const numFields: u8 = @intCast(c.sema.types.items[ret_t].data.@"struct".numFields);
            try c.pushCode(.refCopyObj, &.{ refTemp, numFields, inst.dst }, node);
        } else {
            try c.pushCode(.ref, &.{ refTemp, inst.dst }, node);
        }
        try popTemp(c, refTemp, node);
    } else {
        const retain = !types.isUnboxedType(ret_t);
        try pushField(c, recv.reg, data.idx, retain, inst.dst, node);
    }

    if (ownRecv) {
        try popTempValue(c, recv, node);
    }
    const willRetain = c.sema.isRcCandidateType(ret_t);
    if (inst.dst_owned) {
        try initSlot(c, inst.dst, willRetain, node);
    }

    return finishDstInst(c, inst, willRetain);
}

fn genObjectInit(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .object_init);
    const ret_t = c.ir.getExprType(idx).id;

    const inst = try bc.selectForDstInst(c, cstr, ret_t, true, node);

    // TODO: Would it be faster/efficient to copy the fields into contiguous registers
    //       and copy all at once to heap or pass locals into the operands and iterate each and copy to heap?
    //       The current implementation is the former.
    const args = c.ir.getArray(data.args, u32, data.numArgs);
    const argStart = numSlots(c);
    for (args) |argIdx| {
        const arg_t = c.ir.getExprType(argIdx).id;
        const temp = try bc.reserveTemp(c, arg_t);
        const argv = try genExpr(c, argIdx, Cstr.toTempRetain(temp));
        try initSlot(c, temp, argv.retained, node);
    }

    try pushObjectInit(c, data.typeId, @intCast(argStart), @intCast(data.numArgs), inst.dst, node);
    for (argStart..argStart+args.len) |temp| {
        const slot = getSlot(c, temp);
        if (slot.temp_retained) {
            try consumeTemp(c, @intCast(temp), node);
        }
    }
    try popTemps(c, args.len, node);
    if (inst.dst_owned) {
        try initSlot(c, inst.dst, true, node);
    }

    return finishDstInst(c, inst, true);
}

fn breakStmt(c: *Chunk, node: *ast.Node) !void {
    // Release from startLocal of the first parent loop block.
    var idx = c.blocks.items.len-1;
    while (true) {
        const b = c.blocks.items[idx];
        if (b.isLoopBlock) {
            try genReleaseLocals(c, c.curBlock.slot_start, b.slot_off, node);
            break;
        }
        idx -= 1;
    }

    const pc = try c.pushEmptyJumpExt(null);
    try c.blockJumpStack.append(c.alloc, .{ .jumpT = .brk, .pc = pc });
}

fn contStmt(c: *Chunk, node: *ast.Node) !void {
    // Release from startLocal of the first parent loop block.
    var idx = c.blocks.items.len-1;
    while (true) {
        const b = c.blocks.items[idx];
        if (b.isLoopBlock) {
            try genReleaseLocals(c, c.curBlock.slot_start, b.slot_off, node);
            break;
        }
        idx -= 1;
    }

    const pc = try c.pushEmptyJumpExt(null);
    try c.blockJumpStack.append(c.alloc, .{ .jumpT = .cont, .pc = pc });
}

fn genThrow(c: *Chunk, idx: usize, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .throw);
    const childv = try genExpr(c, data.expr, Cstr.simple);

    try c.pushFCode(.throw, &.{childv.reg}, node);

    try popTempValue(c, childv, node);
    return GenValue.initNoValue();
}

fn setIndex(c: *Chunk, idx: usize, node: *ast.Node) !void {
    const data = c.ir.getStmtData(idx, .setIndex).index;
    if (data.recvT != bt.ListDyn and data.recvT != bt.Map) {
        return error.Unexpected;
    }

    // None of the operands force a retain since it must mimic the generic $setIndex.

    const noneRet = try bc.reserveTemp(c, bt.Void);

    const recv = try genExpr(c, data.rec, Cstr.simple);
    const indexv = try genExpr(c, data.index, Cstr.simple);
    const rightv = try genExpr(c, data.right, Cstr.simple);

    if (data.recvT == bt.ListDyn) {
        try pushInlineTernExpr(c, .setIndexList, recv.reg, indexv.reg, rightv.reg, noneRet, node);
    } else if (data.recvT == bt.Map) {
        try pushInlineTernExpr(c, .setIndexMap, recv.reg, indexv.reg, rightv.reg, noneRet, node);
    } else return error.Unexpected;

    try popTempValue(c, rightv, node);
    try popTempValue(c, indexv, node);
    try popTempValue(c, recv, node);
    try popTemp(c, noneRet, node);
}

const SetFieldDynOptions = struct {
    recv: ?GenValue = null,
    right: ?GenValue = null,
};

fn setFieldDyn(c: *Chunk, idx: usize, opts: SetFieldDynOptions, node: *ast.Node) !void {
    const data = c.ir.getStmtData(idx, .set_field_dyn).set_field_dyn;

    const fieldId = try c.compiler.vm.ensureFieldSym(data.name);
    const ownRecv = opts.recv == null;

    // LHS
    var recv: GenValue = undefined;
    if (opts.recv) |recv_| {
        recv = recv_;
    } else {
        recv = try genExpr(c, data.rec, Cstr.simple);
    }

    // RHS
    var rightv: GenValue = undefined;
    if (opts.right) |right| {
        rightv = right;
    } else {
        rightv = try genExpr(c, data.right, Cstr.simpleRetain);
    }

    // Performs runtime type check.
    const pc = c.buf.ops.items.len;
    try c.pushFCode(.setFieldDyn, &.{ recv.reg, 0, 0, rightv.reg, 0, 0, 0, 0, 0 }, node);
    c.buf.setOpArgU16(pc + 2, @intCast(fieldId));

    try consumeTempValue(c, rightv, node);
    try popTempValue(c, rightv, node);
    if (ownRecv) {
        try popTempValue(c, recv, node);
    }
}

const SetFieldOptions = struct {
    recv: ?GenValue = null,
    right: ?GenValue = null,
};

fn setField(c: *Chunk, idx: usize, opts: SetFieldOptions, node: *ast.Node) !void {
    const data = c.ir.getStmtData(idx, .set_field).set_field;
    const fieldData = c.ir.getExprData(data.field, .field);

    // Receiver.
    var recv: GenValue = undefined;
    if (opts.recv) |recv_| {
        recv = recv_;
    } else {
        recv = try genExpr(c, fieldData.rec, Cstr.simple);
    } 

    // Right.
    var rightv: GenValue = undefined;
    if (opts.right) |right| {
        rightv = right;
    } else {
        rightv = try genExpr(c, data.right, Cstr.simpleRetain);
    }

    const type_id = c.ir.getExprType(data.field).id;
    const isStruct = c.sema.getTypeKind(type_id) == .@"struct";
    var getRef = false;
    var refTemp: SlotId = undefined;
    if (fieldData.numNestedFields > 0) {
        // get ref.
        refTemp = try bc.reserveTemp(c, bt.Pointer);
        try pushFieldRef(c, recv.reg, fieldData.idx, fieldData.numNestedFields, refTemp, node);
        const fieldsLoc = c.ir.advanceExpr(data.field, .field);
        const fields = c.ir.getArray(fieldsLoc, u8, fieldData.numNestedFields);
        try c.pushCodeBytes(fields);
        getRef = true;
    } else {
        if (isStruct) {
            refTemp = try bc.reserveTemp(c, bt.Pointer);
            try pushFieldRef(c, recv.reg, fieldData.idx, 0, refTemp, node);
            getRef = true;
        }
    }

    if (getRef) {
        try c.pushCode(.setRef, &.{ refTemp, rightv.reg }, node);
        try popTemp(c, refTemp, node);
    } else {
        try c.pushCode(.setField, &.{ recv.reg, fieldData.idx, rightv.reg }, node);
    }

    try consumeTempValue(c, rightv, node);
    try popTempValue(c, rightv, node);

    const ownRecv = opts.recv == null;
    if (ownRecv) {
        try popTempValue(c, recv, node);
    }
}

fn genError(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .errorv);

    const symId = try c.compiler.vm.ensureSymbol(data.name);
    const errval = cy.Value.initErrorSymbol(@intCast(symId));
    const constIdx = try c.buf.getOrPushConst(errval);

    const inst = try bc.selectForNoErrNoDepInst(c, cstr, bt.Error, false, node);
    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, node);
    }
    try genConst(c, constIdx, inst.dst, false, null);
    if (inst.dst_owned) {
        try initSlot(c, inst.dst, false, node);
    }

    return finishNoErrNoDepInst(c, inst, false);
}

fn genRange(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .range);

    const inst = try bc.selectForDstInst(c, cstr, bt.Range, true, node);

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
    try c.pushCode(.range, &.{start_reg, end_reg, @intFromBool(data.inc), inst.dst}, node);

    if (data.end != cy.NullId) try popTempValue(c, end, node);
    if (data.start != cy.NullId) try popTempValue(c, start, node);
    return finishDstInst(c, inst, true);
}

fn genTagLit(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .tag_lit);

    const sym = try c.compiler.vm.ensureSymbol(data.name);
    const inst = try bc.selectForNoErrNoDepInst(c, cstr, bt.TagLit, false, node);
    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, node);
    }
    try c.buf.pushOp2(.tag_lit, @intCast(sym), inst.dst);
    return finishNoErrNoDepInst(c, inst, false);
}

fn genSymbol(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .symbol);

    const symId = try c.compiler.vm.ensureSymbol(data.name);
    const inst = try bc.selectForNoErrNoDepInst(c, cstr, bt.Symbol, false, node);
    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, node);
    }
    try c.buf.pushOp2(.symbol, @intCast(symId), inst.dst);
    if (inst.dst_owned) {
        try initSlot(c, inst.dst, false, node);
    }
    return finishNoErrNoDepInst(c, inst, false);
}

fn genTypeCheck(c: *Chunk, loc: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(loc, .type_check);

    const expr = try genExpr(c, data.expr, cstr);
    try pushTypeCheck(c, expr.reg, data.exp_type, node);
    return expr;
}

fn genTypeCheckOption(c: *Chunk, loc: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(loc, .typeCheckOption);

    const expr = try genExpr(c, data.expr, cstr);
    try pushTypeCheckOption(c, expr.reg, node);
    return expr;
}

fn genUnwrapChoice(c: *Chunk, loc: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(loc, .unwrapChoice);
    const ret_t = c.ir.getExprType(loc).id;
    const retain = c.sema.isRcCandidateType(ret_t);
    const inst = try bc.selectForDstInst(c, cstr, ret_t, retain, node);

    const choice = try genExpr(c, data.choice, Cstr.simple);

    try c.pushFCode(.unwrapChoice, &.{ choice.reg, data.tag, data.fieldIdx, inst.dst }, node);

    try popTempValue(c, choice, node);
    return finishDstInst(c, inst, retain);
}

fn genTrue(c: *Chunk, cstr: Cstr, node: *ast.Node) !GenValue {
    const inst = try bc.selectForNoErrNoDepInst(c, cstr, bt.Boolean, false, node);
    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, node);
    }

    try c.buf.pushOp1(.true, inst.dst);
    if (inst.dst_owned) {
        try initSlot(c, inst.dst, false, node);
    }
    return finishNoErrNoDepInst(c, inst, false);
}

fn genFalse(c: *Chunk, cstr: Cstr, node: *ast.Node) !GenValue {
    const inst = try bc.selectForNoErrNoDepInst(c, cstr, bt.Boolean, false, node);
    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, node);
    }

    try c.buf.pushOp1(.false, inst.dst);
    if (inst.dst_owned) {
        try initSlot(c, inst.dst, false, node);
    }

    return finishNoErrNoDepInst(c, inst, false);
}

fn genInt(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .int);

    const inst = try bc.selectForNoErrNoDepInst(c, cstr, bt.Integer, false, node);
    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, node);
    }
    _ = try genConstIntExt(c, data.val, inst.dst, null);
    return finishNoErrNoDepInst(c, inst, false);
}

fn genFloat(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .float);
    const inst = try bc.selectForNoErrNoDepInst(c, cstr, bt.Float, false, node);
    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, node);
    }
    _ = try genConstFloat(c, data.val, inst.dst, node);
    if (inst.dst_owned) {
        try initSlot(c, inst.dst, false, node);
    }
    return finishNoErrNoDepInst(c, inst, false);
}

fn genNone(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .none);
    const ret_t = c.ir.getExprType(idx).id;

    const childv = try genExpr(c, data.child, Cstr.simple);

    const inst = try bc.selectForDstInst(c, cstr, ret_t, false, node);
    try c.pushCode(.none, &.{childv.reg, inst.dst}, node);
    try popTempValue(c, childv, node);
    if (inst.dst_owned) {
        try initSlot(c, inst.dst, false, node);
    }

    return finishDstInst(c, inst, false);
}

fn genStringTemplate(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .stringTemplate);
    const strsIdx = c.ir.advanceExpr(idx, .stringTemplate);
    const strs = c.ir.getArray(strsIdx, []const u8, data.numExprs+1);
    const args = c.ir.getArray(data.args, u32, data.numExprs);

    const inst = try bc.selectForDstInst(c, cstr, bt.String, true, node); 
    const argStart = numSlots(c);

    for (args, 0..) |argIdx, i| {
        const temp = try bc.reserveTemp(c, bt.Any);
        if (cy.Trace and temp != argStart + i) return error.Unexpected;
        _ = try genExpr(c, argIdx, Cstr.toTemp(temp));
    }
    if (cy.Trace and numSlots(c) != argStart + data.numExprs) return error.Unexpected;

    try c.pushOptionalDebugSym(node);
    try c.buf.pushOp3(.stringTemplate, @intCast(argStart), data.numExprs, inst.dst);

    // Append const str indexes.
    const start = try c.buf.reserveData(strs.len);
    for (strs, 0..) |str, i| {
        const ustr = try c.unescapeString(str);
        const constIdx = try c.buf.getOrPushStaticStringConst(ustr);
        c.buf.ops.items[start + i].val = @intCast(constIdx);
    }

    try popTemps(c, args.len, node);
    return finishDstInst(c, inst, true);
}

fn genString(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .string);
    const inst = try bc.selectForNoErrNoDepInst(c, cstr, bt.String, true, node);
    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, node);
    }
    // TODO: Ideally this shouldn't retain. But release ops *might* require them to be.
    try pushStringConst(c, data.raw, inst.dst, node);

    if (inst.dst_owned) {
        try initSlot(c, inst.dst, true, node);
    }
    return finishNoErrNoDepInst(c, inst, true);
}

fn pushStringConst(c: *Chunk, str: []const u8, dst: SlotId, node: *ast.Node) !void {
    _ = node;

    const idx = try c.buf.getOrPushStaticStringConst(str);
    try genConst(c, idx, dst, true, null);
}

fn genUnOp(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .preUnOp).unOp;
    const ret_t = c.ir.getExprType(idx).id;
    const inst = try bc.selectForDstInst(c, cstr, ret_t, false, node);

    const childv = try genExpr(c, data.expr, Cstr.simple);

    switch (data.op) {
        .not => {
            try c.buf.pushOp2(.not, childv.reg, inst.dst);
        },
        .minus,
        .bitwiseNot => {
            if (data.childT == bt.Integer) {
                try pushInlineUnExpr(c, getIntUnaryOpCode(data.op), childv.reg, inst.dst, node);
            } else if (data.childT == bt.Float) {
                try pushInlineUnExpr(c, getFloatUnaryOpCode(data.op), childv.reg, inst.dst, node);
            } else return error.Unexpected;
            // Builtin unary expr do not have retained child.
        },
        else => {
            return c.reportErrorFmt("Unsupported op: {}", &.{v(data.op)}, node);
        }
    }

    if (childv.isTemp()) {
        if (childv.reg != inst.dst) {
            try popTemp(c, childv.reg, node);
        }
    }

    if (inst.dst_owned) {
        try initSlot(c, inst.dst, false, node);
    }

    return finishDstInst(c, inst, false);
}

fn genCallSymDyn(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .pre_call_sym_dyn).call_sym_dyn;

    const inst = try beginCall(c, cstr, bt.Dyn, false, node);

    const argStart = numSlots(c);
    const args = c.ir.getArray(data.args, u32, data.nargs);
    for (args, 0..) |argIdx, i| {
        const temp = try bc.reserveTemp(c, bt.Dyn);
        if (cy.Trace and temp != argStart + i) return error.Unexpected;
        const argv = try genExpr(c, argIdx, Cstr.toTemp(temp));
        try initSlot(c, temp, argv.retained, node);
    }

    const sym = c.compiler.genSymMap.get(data.sym).?;
    const group = c.vm.func_groups.buf[sym.func_sym.group];
    try pushCallSymDyn(c, inst.ret, data.nargs, 1, group.id, node);

    try popTemps(c, args.len, node);
    if (inst.ret_owned) {
        try initSlot(c, inst.ret, true, node);
    }

    return endCall(c, inst, true);
}

fn genCallObjSym(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .preCallObjSym).callObjSym;

    const inst = try beginCall(c, cstr, bt.Dyn, false, node);

    // Receiver.
    const argStart = numSlots(c);
    var temp = try bc.reserveTemp(c, bt.Dyn);
    const recv = try genExpr(c, data.rec, Cstr.toTemp(temp));
    try initSlot(c, temp, recv.retained, node);

    const args = c.ir.getArray(data.args, u32, data.numArgs);
    for (args, 0..) |argIdx, i| {
        temp = try bc.reserveTemp(c, bt.Dyn);
        if (cy.Trace and temp != argStart + 1 + i) return error.Unexpected;
        const argv = try genExpr(c, argIdx, Cstr.toTemp(temp));
        try initSlot(c, temp, argv.retained, node);
    }

    const method = try c.compiler.vm.ensureMethod(data.name);
    try pushCallObjSym(c, inst.ret, data.numArgs + 1,
        @intCast(method), node);

    try popTemps(c, args.len + 1, node);
    if (inst.ret_owned) {
        try initSlot(c, inst.ret, true, node);
    }

    return endCall(c, inst, true);
}

fn genCallTrait(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .pre_call_trait).call_trait;

    const ret_t = c.ir.getExprType(idx).id;
    const inst = try beginCall(c, cstr, ret_t, true, node);

    // Trait ref.
    const argStart = numSlots(c);
    var temp = try bc.reserveTemp(c, bt.Any);
    _ = try genExpr(c, data.trait, Cstr.toTemp(temp));

    // Reserve slot for unwrapped impl.
    _ = try bc.reserveTemp(c, bt.Any);

    // Args. Skip impl placeholder.
    const args = c.ir.getArray(data.args, u32, data.nargs)[1..];
    for (args, 2..) |argIdx, i| {
        const arg_t = c.ir.getExprType(argIdx).id;
        temp = try bc.reserveTemp(c, arg_t);
        if (cy.Trace and temp != argStart + i) return error.Unexpected;
        _ = try genExpr(c, argIdx, Cstr.toTemp(temp));
    }

    const start = c.buf.ops.items.len;
    try c.pushFCode(.call_trait, &.{inst.ret, data.nargs, 1, 0, 0, 0, 0, 0, 0, 0, 0 }, node);
    c.buf.setOpArgU16(start + 4, @intCast(data.vtable_idx));

    try popTemps(c, args.len + 2, node);

    return endCall(c, inst, true);
}

fn genCallFuncSym(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .preCallFuncSym).callFuncSym;

    if (true) {
        // TODO: Handle specialized. (eg. listIndex, listAppend)
    }

    const ret_t = c.ir.getExprType(idx).id;
    const inst = try beginCall(c, cstr, ret_t, false, node);

    const args = c.ir.getArray(data.args, u32, data.numArgs);

    const argStart = numSlots(c);
    for (args, 0..) |argIdx, i| {
        const arg_t = c.ir.getExprType(argIdx).id;
        const temp = try bc.reserveTemp(c, arg_t);
        if (cy.Trace and temp != argStart + i) return error.Unexpected;
        const argv = try genExpr(c, argIdx, Cstr.toTemp(temp));
        try initSlot(c, temp, argv.retained, node);
    }

    const rtId = c.compiler.genSymMap.get(data.func).?.func.id;
    try pushCallSym(c, inst.ret, data.numArgs, 1, rtId, node);

    try popTemps(c, args.len, node);
    if (inst.ret_owned) {
        try initSlot(c, inst.ret, !types.isUnboxedType(ret_t), node);
    }

    const retRetained = c.sema.isRcCandidateType(data.func.retType);
    return endCall(c, inst, retRetained);
}

fn genCallDyn(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .preCallDyn).callDyn;
    const inst = try beginCall(c, cstr, bt.Dyn, true, node);

    // Callee.
    const argStart = numSlots(c);
    var temp = try bc.reserveTemp(c, bt.Dyn);
    const callee = try genExpr(c, data.callee, Cstr.toTemp(temp));
    try initSlot(c, temp, callee.retained, node);

    // Args.
    const args = c.ir.getArray(data.args, u32, data.numArgs);
    for (args, 0..) |argIdx, i| {
        temp = try bc.reserveTemp(c, bt.Dyn);
        if (cy.Trace and temp != argStart + 1 + i) return error.Unexpected;
        const argv = try genExpr(c, argIdx, Cstr.toTemp(temp));
        try initSlot(c, temp, argv.retained, node);
    }

    try c.pushFCode(.call, &.{inst.ret, data.numArgs, 1}, node);
    try c.pushCode(.ret_dyn, &.{ data.numArgs }, node);

    try popTemps(c, args.len + 1, node);
    if (inst.ret_owned) {
        try initSlot(c, inst.ret, true, node);
    }

    return endCall(c, inst, true);
}

const BinOpOptions = struct {
    left: ?GenValue = null,
};

fn genBinOp(c: *Chunk, idx: usize, cstr: Cstr, opts: BinOpOptions, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .preBinOp).binOp;
    const ret_t = c.ir.getExprType(idx).id;
    log.tracev("binop {} {}", .{data.op, data.leftT});

    if (data.op == .and_op) {
        return bc.andOp(c, data, cstr, node);
    } else if (data.op == .or_op) {
        return bc.orOp(c, data, cstr, node);
    }

    // Most builtin binOps do not retain.
    var willRetain = false;
    switch (data.op) {
        .index => {
            willRetain = true;
        },
        else => {},
    }
    const inst = try bc.selectForDstInst(c, cstr, ret_t, willRetain, node);

    // Lhs.
    var leftv: GenValue = undefined;
    if (opts.left) |left| {
        leftv = left;
    } else {
        leftv = try genExpr(c, data.left, Cstr.simple);
    }

    // Rhs.
    const rightv = try genExpr(c, data.right, Cstr.simple);

    var retained = false;
    switch (data.op) {
        .index => {
            if (data.leftT == bt.ListDyn) {
                if (data.rightT == bt.Range) {
                    try pushInlineBinExpr(c, .sliceList, leftv.reg, rightv.reg, inst.dst, node);
                } else {
                    try pushInlineBinExpr(c, .indexList, leftv.reg, rightv.reg, inst.dst, node);
                }
            } else if (data.leftT == bt.Tuple) {
                try pushInlineBinExpr(c, .indexTuple, leftv.reg, rightv.reg, inst.dst, node);
            } else if (data.leftT == bt.Map) {
                try pushInlineBinExpr(c, .indexMap, leftv.reg, rightv.reg, inst.dst, node);
            } else return error.Unexpected;
            retained = true;
        },
        .bitwiseAnd,
        .bitwiseOr,
        .bitwiseXor,
        .bitwiseLeftShift,
        .bitwiseRightShift => {
            if (data.leftT == bt.Integer) {
                try pushInlineBinExpr(c, getIntOpCode(data.op), leftv.reg, rightv.reg, inst.dst, node);
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
                try pushInlineBinExpr(c, getFloatOpCode(data.op), leftv.reg, rightv.reg, inst.dst, node);
            } else if (data.leftT == bt.Integer) {
                try pushInlineBinExpr(c, getIntOpCode(data.op), leftv.reg, rightv.reg, inst.dst, node);
            } else return error.Unexpected;
        },
        .equal_equal => {
            try c.pushOptionalDebugSym(node);
            try c.buf.pushOp3Ext(.compare, leftv.reg, rightv.reg, inst.dst, null);
        },
        .bang_equal => {
            try c.pushOptionalDebugSym(node);
            try c.buf.pushOp3Ext(.compareNot, leftv.reg, rightv.reg, inst.dst, null);
        },
        else => {
            return c.reportErrorFmt("Unsupported op: {}", &.{v(data.op)}, node);
        },
    }

    const tempRight = rightv.isTemp();
    if (tempRight and rightv.reg != inst.dst) {
        try popTemp(c, rightv.reg, node);
    }

    if (opts.left == null) {
        const tempLeft = leftv.isTemp();
        if (tempLeft and leftv.reg != inst.dst) {
            try popTemp(c, leftv.reg, node);
        }
    }

    if (inst.dst_owned) {
        try initSlot(c, inst.dst, retained, node);
    }

    return finishDstInst(c, inst, retained);
}

fn genTrait(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .trait);

    const inst = try bc.selectForDstInst(c, cstr, bt.Any, true, node);
    const exprv = try genExpr(c, data.expr, Cstr.simpleRetain);

    const key = VtableKey{ .type = data.expr_t, .trait = data.trait_t };
    const vtable_idx = c.compiler.gen_vtables.get(key).?;

    const pc = c.buf.len();
    try c.pushFCode(.trait, &.{ exprv.reg, 0, 0, 0, 0, inst.dst }, node);
    c.buf.setOpArgU16(pc + 2, @intCast(data.trait_t));
    c.buf.setOpArgU16(pc + 4, @intCast(vtable_idx));

    try popTempValue(c, exprv, node);
    return finishDstInst(c, inst, true);
}

fn genUnbox(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .unbox);
    const ret_t = c.ir.getExprType(idx).id;
    if (ret_t == bt.Integer) {
        const inst = try bc.selectForDstInst(c, cstr, ret_t, false, node);
        const childv = try genExpr(c, data.expr, Cstr.simple);
        const pc = c.buf.len();
        try c.pushFCode(.unbox, &.{ childv.reg, 0, 0, inst.dst }, node);
        c.buf.setOpArgU16(pc + 2, @intCast(ret_t));

        try popTempValue(c, childv, node);
        return finishDstInst(c, inst, false);
    } else {
        // Passthrough.
        return genExpr(c, data.expr, cstr);
    }
}

fn genBox(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .box);
    const ret_t = c.ir.getExprType(data.expr);
    if (ret_t.id == bt.Integer) {
        const inst = try bc.selectForDstInst(c, cstr, bt.Any, true, node);
        const childv = try genExpr(c, data.expr, Cstr.simple);
        const pc = c.buf.len();
        try c.pushFCode(.box, &.{ childv.reg, 0, 0, inst.dst }, node);
        c.buf.setOpArgU16(pc + 2, @intCast(ret_t.id));
        try popTempValue(c, childv, node);
        if (inst.dst_owned) {
            try initSlot(c, inst.dst, true, node);
        }
        return finishDstInst(c, inst, true);
    } else {
        // Passthrough.
        return genExpr(c, data.expr, cstr);
    }
}

fn genCaptured(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .captured);

    const value_t = c.ir.getExprType(idx).id;
    const retain = !types.isUnboxedType(value_t);
    const inst = try bc.selectForNoErrNoDepInst(c, cstr, value_t, retain, node);
    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, node);
    }

    try c.pushCode(.captured, &.{ c.curBlock.closureLocal, data.idx, @intFromBool(retain), inst.dst }, node);
    if (inst.dst_owned) {
        try initSlot(c, inst.dst, retain, node);
    }

    return finishNoErrNoDepInst(c, inst, retain);
}

/// Registers a slot for ARC tracking.
pub fn initSlot(c: *Chunk, slot_id: SlotId, retained_val: bool, node: *ast.Node) !void {
    const slot = &c.slot_stack.items[c.curBlock.slot_start + slot_id];
    if (slot.type == .null) {
        return c.reportErrorFmt("Expected non null slot {}.", &.{v(slot_id)}, node);
    }
    if (slot.type == .ret) {
        return;
    }
    if (!slot.boxed) {
        return;
    }
    if (slot.boxed_init) {
        return c.reportErrorFmt("Slot {} already inited.", &.{v(slot_id)}, node);
    }
    slot.boxed_init = true;

    if (slot.type == .temp) {
        if (retained_val) {
            slot.temp_retained = true;
            try pushUnwindSlot(c, slot_id);
        }
    } else {
        try pushUnwindSlot(c, slot_id);
    }
}

pub fn getSlotPtr(c: *const Chunk, idx: usize) *Slot {
    return &c.slot_stack.items[c.curBlock.slot_start + idx];
}

pub fn getSlot(c: *const Chunk, idx: usize) Slot {
    return c.slot_stack.items[c.curBlock.slot_start + idx];
}

pub fn setSlot(c: *Chunk, idx: usize, slot: Slot) void {
    c.slot_stack.items[c.curBlock.slot_start + idx] = slot;
}

pub fn toLocalReg(c: *Chunk, irVarId: u8) SlotId {
    return c.genIrLocalMapStack.items[c.curBlock.irLocalMapStart + irVarId];
}

fn genValueLocal(c: *Chunk, reg: SlotId, local: Slot, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = c;
    _ = reg;
    _ = local;
    _ = cstr;
    _ = node;

    // if (!cstr.isExact()) {
    //     // Prefer no copy.
    //     const retain = cstr.type == .simple and cstr.data.simple.retain;
    //     if (retain) {
    //         try c.pushCode(.retain, &.{ reg }, node);
    //     }
    //     return regValue(c, reg, retain);
    // }
    // const inst = try c.rega.selectForDstInst(cstr, true, node);
    // const numFields: u8 = @intCast(c.sema.types.items[local.some.type].data.object.numFields);
    // try c.pushCode(.copyObj, &.{ reg, numFields, inst.dst }, node);
    // return finishDstInst(c, inst, true);
    return error.TODO;
}

fn genLiftedValueLocal(c: *Chunk, reg: SlotId, local: Slot, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = c;
    _ = reg;
    _ = local;
    _ = cstr;
    _ = node;

    // const inst = try c.rega.selectForDstInst(cstr, true, node);
    // try c.pushCode(.up_value, &.{ reg, inst.dst }, node);
    // const numFields: u8 = @intCast(c.sema.types.items[local.some.type].data.object.numFields);
    // try c.pushCode(.copyObj, &.{ inst.dst, numFields, inst.dst }, node);
    // return finishDstInst(c, inst, true);
    return error.TODO;
}

fn genLocalReg(c: *Chunk, reg: SlotId, slot_t: cy.TypeId, cstr: Cstr, node: *ast.Node) !GenValue {
    const slot = getSlot(c, reg);

    if (!slot.boxed_up) {
        const type_e = c.sema.getType(slot_t);
        if (type_e.kind == .@"struct") {
            return genValueLocal(c, reg, slot, cstr, node);
        }

        const srcv = regValue(c, reg, false);
        var exact_cstr = cstr;
        switch (cstr.type) {
            .simple => {
                exact_cstr = Cstr.toLocal(reg, false);
                exact_cstr.data.slot.retain = slot.boxed and cstr.data.simple.retain;
            },
            else => {},
        }
        return genToExact(c, srcv, exact_cstr, node);
    } else {
        const type_e = c.sema.getType(slot_t);
        if (type_e.kind == .@"struct") {
            return genLiftedValueLocal(c, reg, slot, cstr, node);
        }

        // Special case when src local is an UpValue.

        var retain_src = false;
        const boxed_child = !cy.types.isUnboxedType(slot_t);
        if (boxed_child) {
            switch (cstr.type) {
                .liftedLocal,
                .localReg => {
                    retain_src = true;
                },
                .simple => {
                    if (cstr.data.simple.retain) {
                        retain_src = true;
                    }
                },
                .tempReg => {
                    if (cstr.data.slot.retain) {
                        retain_src = true;
                    }
                },
                else => {},
            }
        }
        const inst = try bc.selectForDstInst(c, cstr, slot_t, retain_src, node);

        try c.pushCode(.up_value, &.{ reg, inst.dst }, node);
        if (retain_src) {
            try c.pushCode(.retain, &.{ inst.dst }, node);
        }

        return finishDstInst(c, inst, retain_src);
    }
}

fn genLocal(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .local);
    const reg = toLocalReg(c, data.id);
    const slot_t = c.ir.getExprType(idx).id;
    return genLocalReg(c, reg, slot_t, cstr, node);
}

fn genEnumMemberSym(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .enumMemberSym);
    const ret_t = c.ir.getExprType(idx).id;

    const inst = try bc.selectForNoErrNoDepInst(c, cstr, ret_t, false, node);
    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, node);
    }

    const val = cy.Value.initEnum(@intCast(data.type), @intCast(data.val));
    const constIdx = try c.buf.getOrPushConst(val);
    try genConst(c, constIdx, inst.dst, false, null);

    return finishNoErrNoDepInst(c, inst, false);
}

fn genContext(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .context);
    const ret_t = c.ir.getExprType(idx).id;

    const inst = try bc.selectForNoErrNoDepInst(c, cstr, ret_t, true, node);
    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, node);
    }

    try c.pushOptionalDebugSym(node);       
    try c.buf.pushOp2(.context, data.sym.idx, inst.dst);

    return finishNoErrNoDepInst(c, inst, true);
}

fn genVarSym(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .varSym);
    const ret_t = c.ir.getExprType(idx).id;

    const varId = c.compiler.genSymMap.get(data.sym).?.varSym.id;

    const inst = try bc.selectForNoErrNoDepInst(c, cstr, ret_t, true, node);
    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, node);
    }

    try c.pushOptionalDebugSym(node);       
    const pc = c.buf.len();
    try c.buf.pushOp3(.staticVar, 0, 0, inst.dst);
    c.buf.setOpArgU16(pc + 1, @intCast(varId));

    return finishNoErrNoDepInst(c, inst, true);
}

fn genFuncSym(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .funcSym);

    const inst = try bc.selectForDstInst(c, cstr, bt.Any, true, node);

    const pc = c.buf.len();
    try c.pushOptionalDebugSym(node);
    try c.buf.pushOp3(.staticFunc, 0, 0, inst.dst);
    const rtId = c.compiler.genSymMap.get(data.func).?.func.id;
    c.buf.setOpArgU16(pc + 1, @intCast(rtId));
    if (inst.dst_owned) {
        try initSlot(c, inst.dst, true, node);
    }
    return finishDstInst(c, inst, true);
}

fn genTypeSym(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .typeSym);

    const inst = try bc.selectForDstInst(c, cstr, bt.MetaType, true, node);
    const pc = c.buf.len();
    try c.pushCode(.metatype, &.{@intFromEnum(cy.heap.MetaTypeKind.object), 0, 0, 0, 0, inst.dst}, node);
    c.buf.setOpArgU32(pc + 2, data.typeId);
    if (inst.dst_owned) {
        try initSlot(c, inst.dst, true, node);
    }

    return finishDstInst(c, inst, true);
}

/// Reserve params and captured vars.
/// Function stack layout:
/// [retLocal] [retInfo] [retAddress] [prevFramePtr] [callee] [params...] [var locals...] [temp locals...]
/// `callee` is reserved so that function values can call static functions with the same call convention.
/// Captured vars are accessed from the closure in the callee slot.
fn reserveFuncSlots(c: *Chunk, maxIrLocals: u8, numParamCopies: u8, params: []align(1) const ir.FuncParam) !void {
    const numParams = params.len;
    try c.genIrLocalMapStack.resize(c.alloc, c.curBlock.irLocalMapStart + maxIrLocals);

    c.curBlock.num_pre_slots = @intCast(4 + 1 + numParams);
    c.genBlock().num_locals += numParamCopies;

    const maxLocalRegs = 4 + 1 + numParams + numParamCopies + (maxIrLocals - numParams);
    log.tracev("reserveFuncRegs {} {}", .{c.curBlock.slot_start, maxLocalRegs});
    _ = try reserveRetSlot(c);
    try reserveSlots(c, c.curBlock.num_pre_slots + numParamCopies - 1);

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
            const slot: SlotId = @intCast(4 + 1 + numParams + paramCopyIdx);
            c.genIrLocalMapStack.items[c.curBlock.irLocalMapStart + i] = slot;

            const boxed = !cy.types.isUnboxedType(param.declType) or param.lifted;
            c.slot_stack.items[c.curBlock.slot_start + slot] = .{
                .type = .local,
                .boxed = boxed,
                .owned = true,
                .boxed_up = param.lifted,
                .boxed_init = false,
                .temp_retained = false,
            };

            // Copy param to local.
            if (param.lifted) {
                // Retain param and box.
                try c.buf.pushOp1(.retain, nextReg);
                try c.pushFCode(.up, &.{nextReg, slot}, c.curBlock.debugNode);
            } else {
                try c.pushCode(.copyRetainSrc, &.{ nextReg, slot }, c.curBlock.debugNode);
            }

            try initSlot(c, slot, true, c.curBlock.debugNode);

            paramCopyIdx += 1;
        } else {
            c.genIrLocalMapStack.items[c.curBlock.irLocalMapStart + i] = nextReg;

            const boxed = !cy.types.isUnboxedType(param.declType);
            c.slot_stack.items[c.curBlock.slot_start + 4 + 1 + i] = .{
                .type = .local,
                .boxed = boxed,
                .owned = false,
                .boxed_up = false,
                .boxed_init = false,
                .temp_retained = false,
            };
        }
        log.tracev("reserve param {}: slot={}", .{i, nextReg});
        nextReg += 1;
    }

    c.curBlock.startLocalReg = nextReg;
    nextReg += numParamCopies;
}

fn setVarSym(c: *Chunk, idx: usize, node: *ast.Node) !void {
    _ = node;
    const data = c.ir.getStmtData(idx, .setVarSym).generic;
    const varSym = c.ir.getExprData(data.left, .varSym);

    const id = c.compiler.genSymMap.get(varSym.sym).?.varSym.id;
    _ = try genExpr(c, data.right, Cstr.toVarSym(id));
}

fn declareLocalInit(c: *Chunk, idx: u32, node: *ast.Node) !SlotId {
    const data = c.ir.getStmtData(idx, .declareLocalInit);

    const slot = try reserveLocal(c, data.declType, data.lifted, node);
    c.genIrLocalMapStack.items[c.curBlock.irLocalMapStart + data.id] = slot;
    c.genBlock().num_locals += 1;
    if (data.zeroMem) {
        try c.buf.pushOp2Ext(.constIntV8, 0, slot, null);
    }

    var cstr = Cstr.toLocal(slot, false);
    if (data.declType != bt.Dyn and data.initType.dynamic) {
        cstr.data.slot.check_type = data.declType;
    }
    const val = try genExpr(c, data.init, cstr);

    if (data.lifted) {
        try c.pushOptionalDebugSym(node);
        try c.buf.pushOp2(.up, slot, slot);
    }

    const local = getSlot(c, slot);
    try initSlot(c, slot, local.boxed, node);
    log.tracev("declare {}, type: {} ", .{val.reg, local.type});
    return slot;
}

fn declareLocal(c: *Chunk, idx: u32, node: *ast.Node) !void {
    const data = c.ir.getStmtData(idx, .declareLocal);
    const slot = try reserveLocal(c, data.declType, data.lifted, node);
    c.genIrLocalMapStack.items[c.curBlock.irLocalMapStart + data.id] = slot;
    c.genBlock().num_locals += 1;
}

fn setCaptured(c: *Chunk, idx: usize, node: *ast.Node) !void {
    _ = node;
    const data = c.ir.getStmtData(idx, .setCaptured).generic;
    const capData = c.ir.getExprData(data.left, .captured);

    // RHS.
    // const dstRetained = c.sema.isRcCandidateType(data.leftT.id);
    _ = try genExpr(c, data.right, Cstr.toCaptured(capData.idx));
}

fn irSetLocal(c: *Chunk, idx: usize, node: *ast.Node) !void {
    const data = c.ir.getStmtData(idx, .setLocal).generic;
    const localData = c.ir.getExprData(data.left, .local);
    const check_type: ?cy.TypeId = if (!data.left_t.dynamic and data.right_t.dynamic) data.left_t.id else null;
    try setLocal(c, localData, data.right, node, .{ .check_type = check_type });
}

const SetLocalOptions = struct {
    rightv: ?GenValue = null,
    extraIdx: ?u32 = null,
    check_type: ?cy.TypeId = null,
};

fn setLocal(c: *Chunk, data: ir.Local, rightIdx: u32, node: *ast.Node, opts: SetLocalOptions) !void {
    const reg = toLocalReg(c, data.id);
    const local = getSlot(c, reg);

    var dst: Cstr = undefined;
    if (local.boxed_up) {
        const right_t = c.ir.getExprType(rightIdx).id;
        const boxed_child = !cy.types.isUnboxedType(right_t);
        dst = Cstr.toLiftedLocal(reg, boxed_child);
    } else {
        dst = Cstr.toLocal(reg, local.boxed);
        if (opts.check_type) |check_type| {
            dst.data.slot.check_type = check_type;
        }
    }

    var rightv: GenValue = undefined;
    if (opts.rightv) |rightv_| {
        rightv = rightv_;
        if (rightv.reg != reg) {
            // Move to local.
            _ = try genToExactDesc(c, rightv, dst, node, opts.extraIdx);
        }
    } else {
        rightv = try genExpr(c, rightIdx, dst);
    }
}

fn opSet(c: *Chunk, idx: usize, node: *ast.Node) !void {
    _ = node;
    const data = c.ir.getStmtData(idx, .opSet);
    // TODO: Perform optimizations depending on the next set* code.
    try genStmt(c, data.set_stmt);
}

// fn opSetField(c: *Chunk, data: ir.OpSet, node: *ast.Node) !void {
//     try pushIrData(c, .{ .opSet = data });
//     try pushBasic(c, node);

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
//     try preCallGenericBinOp(c, .{ .op = data.op, .funcSigId = funcSigId }, info.node);
//     try fieldDynamic(c, .{ .name = data.set.field.name }, .{ .recv = recv }, info.node);
// }

// fn opSetFieldRightEnd(c: *Chunk) !void {
//     const data = popIrData(c).opSet;
//     const info = getLastTaskInfo(c).basic;

//     getTop(c).type = .opSetFieldEnd;
//     const val = popValue(c);
//     const recv = popValue(c);
//     try setField(c, data.set.field, .{ .recv = recv, .right = val }, info.node);
// }

// fn opSetFieldEnd(c: *Chunk) !void {
//     const info = popTaskInfo(c).basic;

//     const retainedTemps = c.popUnwindTempsFrom(info.retainedStart);
//     try pushReleases(c, retainedTemps, info.node);

//     c.rega.setNextTemp(info.tempStart);
//     removeTop(c);
// }

// fn opSetObjectField(c: *Chunk, data: ir.OpSet, node: *ast.Node) !void {
//     const opStrat = getOpStrat(data.op, data.leftT) orelse {
//         return c.reportErrorFmt("Unsupported op: {}", &.{v(data.op)}, node);
//     };

//     try pushIrData(c, .{ .opSet = data });
//     try pushBasic(c, node);
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
//             try binOp(c, .{ .op = data.op, .leftT = data.leftT }, .{ .left = binLeftCstr }, info.node);
//             try fieldStatic(c, .{ .typeId = data.leftT, .idx = fieldIdx }, .{ .recv = recv }, info.node);
//         },
//         .callObjSym => {
//             top.type = .opSetObjectFieldRightEnd;
//             try pushCstr(c, Cstr.initBc(RegisterCstr.exact(dst)));
//             const funcSigId = try sema.ensureFuncSig(c.compiler, &.{ bt.Any, bt.Any }, bt.Any);
//             try preCallGenericBinOp(c, .{ .op = data.op, .funcSigId = funcSigId }, info.node);
//             try fieldStatic(c, .{ .typeId = data.leftT, .idx = fieldIdx }, .{ .recv = recv }, info.node);
//         },
//     }
// }

// fn opSetObjectFieldRightEnd(c: *Chunk) !void {
//     const data = popIrData(c).opSet;
//     const info = getLastTaskInfo(c).basic;

//     getTop(c).type = .opSetObjectFieldEnd;
//     const val = popValue(c);
//     const recv = popValue(c);
//     try setObjectField(c, data.set.objectField, .{ .recv = recv, .right = val }, info.node);
// }

// fn opSetObjectFieldEnd(c: *Chunk) !void {
//     const info = popTaskInfo(c).basic;

//     const retainedTemps = c.popUnwindTempsFrom(info.retainedStart);
//     try pushReleases(c, retainedTemps, info.node);

//     c.rega.setNextTemp(info.tempStart);
//     removeTop(c);
// }

// fn opSetLocal(c: *Chunk, data: ir.OpSet, node: *ast.Node) !void {
//     const opStrat = getOpStrat(data.op, data.leftT) orelse {
//         return c.reportErrorFmt("Unsupported op: {}", &.{v(data.op)}, node);
//     };

//     const dst = toLocalReg(c, data.set.local.id);
//     try pushCustom(c, .popValue);
//     try pushCstr(c, Cstr.initBc(RegisterCstr.exact(dst)));
//     switch (opStrat) {
//         .inlineOp => {
//             const local = Task.initRes(regValue(c, dst, false));
//             try genBinOp(c, .{ .op = data.op, .leftT = data.leftT }, .{ .left = local }, node);
//         },
//         .callObjSym => {
//             const funcSigId = try sema.ensureFuncSig(c.compiler, &.{ bt.Any, bt.Any }, bt.Any);
//             try genCallObjSymBinOp(c, .{ .op = data.op, .funcSigId = funcSigId }, node);
//             try genLocal(c, .{ .id = data.set.local.id }, node);
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

fn orOp(c: *Chunk, data: ir.BinOp, cstr: Cstr, node: *ast.Node) !GenValue {
    const cond = try reserveTemp(c, bt.Boolean);
    var res = regValue(c, cond, false);

    const leftv = try genExpr(c, data.left, Cstr.toTemp(cond));
    const jump_true = try c.pushEmptyJumpCond(cond);

    _ = try genExpr(c, data.right, Cstr.toTemp(cond));
    c.patchJumpCondToCurPc(jump_true);

    try initSlot(c, cond, false, node);
    if (cstr.isExact()) {
        res = try genToExact(c, leftv, cstr, node);
        try popTemp(c, cond, node);
    }

    return res;
}

fn andOp(c: *Chunk, data: ir.BinOp, cstr: Cstr, node: *ast.Node) !GenValue {
    if (cstr.isExact()) {
        const leftv = try genExpr(c, data.left, Cstr.simple);

        const jumpFalse = try c.pushEmptyJumpNotCond(leftv.reg);

        // RHS. Goes to final dst whether true or false.
        const rightv = try genExpr(c, data.right, cstr);
        const jumpEnd = try c.pushEmptyJump();

        // Copy left to dst. Can assume leftv is a non-rc value.
        c.patchJumpNotCondToCurPc(jumpFalse);
        _ = try genToExact(c, leftv, cstr, node);

        c.patchJumpToCurPc(jumpEnd);

        try popTempValue(c, leftv, node);
        return regValue(c, rightv.reg, rightv.retained);
    } else {
        // Merged branch result.
        const res = try bc.reserveTemp(c, bt.Boolean);
        const leftv = try genExpr(c, data.left, Cstr.simple);

        const jumpFalse = try c.pushEmptyJumpNotCond(leftv.reg);

        // RHS. Goes to res whether true or false.
        const rightv = try genExpr(c, data.right, Cstr.toTempRetain(res));
        const jumpEnd = try c.pushEmptyJump();

        c.patchJumpNotCondToCurPc(jumpFalse);
        // Require retain to merge with right.
        _ = try genToExact(c, leftv, Cstr.toTempRetain(res), node);

        c.patchJumpToCurPc(jumpEnd);

        try popTempValue(c, leftv, node);

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
fn retExprStmt(c: *Chunk, idx: usize, node: *ast.Node) !void {
    const data = c.ir.getStmtData(idx, .retExprStmt);

    // TODO: If the returned expr is a local, consume the local after copying to reg 0.
    var childv: GenValue = undefined;
    if (c.curBlock.type == .main) {
        // Main block.
        childv = try genExpr(c, data.expr, Cstr.simpleRetain);
    } else {
        childv = try genExpr(c, data.expr, Cstr.ret);
    }

    try popTempValue(c, childv, node);

    try genBlockReleaseLocals(c);
    if (c.curBlock.type == .main) {
        try c.buf.pushOp1(.end, @intCast(childv.reg));
    } else {
        try c.buf.pushOp(.ret1);
    }
}

pub const SlotType = enum {
    null,
    ret,
    local,

    /// Temps have a very short life-time and are either moved or released after an expression.
    temp,
};

pub const Slot = struct {
    type: SlotType,

    /// Whether the slot is owned by the block. eg. Read-only func params would not be owned.
    owned: bool,

    boxed: bool,

    /// Whether the slot has been inited with a value. Eligible for tracking.
    boxed_init: bool,

    /// Holds an `UpValue`.
    boxed_up: bool,

    /// Whether this temp slot still retains a value. If true, `boxed` should also be true.
    temp_retained: bool,
};

pub fn numSlots(c: *Chunk) usize {
    return c.slot_stack.items.len - c.curBlock.slot_start;
}

fn genBlockReleaseLocals(c: *Chunk) !void {
    const block = c.curBlock;
    try genReleaseLocals(c, c.curBlock.slot_start, block.startLocalReg, block.debugNode);
}

/// Only the locals that are alive at this moment are considered for release.
fn genReleaseLocals(c: *Chunk, slot_base: usize, slot_off: usize, debugNode: *ast.Node) !void {
    const start = c.operandStack.items.len;
    defer c.operandStack.items.len = start;

    const slots = c.slot_stack.items[slot_base + slot_off..];
    log.tracev("Generate release for slots[{}..{}]", .{slot_off, slot_off + slots.len});
    for (slots, slot_off..) |slot, i| {
        if (slot.boxed and slot.boxed_init and slot.type == .local) {
            try c.operandStack.append(c.alloc, @intCast(i));
        }
    }
    
    const regs = c.operandStack.items[start..];
    if (regs.len > 0) {
        try pushReleases(c, regs, debugNode);
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

fn reserveFiberCallRegs(c: *Chunk, numArgs: u8) !void {
    var nextReg: u8 = 1;

    if (numArgs > 0) {
        // Advance 1 + prelude + numArgs.
        nextReg += 5 + numArgs;
    }

    c.curBlock.startLocalReg = nextReg;
    c.curBlock.num_pre_slots = nextReg;

    try c.genIrLocalMapStack.resize(c.alloc, c.curBlock.irLocalMapStart);
    try reserveSlots(c, nextReg);
}

pub fn reserveMainRegs(c: *Chunk, maxLocals: u8) !void {
    log.tracev("reserveMainRegs maxLocals={}\n", .{maxLocals});
    var nextReg: u8 = 0;

    // Reserve the first slot for BC main frame check and the JIT return addr.
    _ = try reserveSlot(c);
    nextReg += 1;

    c.curBlock.startLocalReg = nextReg;
    c.curBlock.num_pre_slots = nextReg;

    nextReg += maxLocals;

    try c.genIrLocalMapStack.resize(c.alloc, c.curBlock.irLocalMapStart + maxLocals);
}

pub const CallInst = struct {
    ret: SlotId,
    ret_owned: bool,
    numPreludeTemps: u8,
    cstr: Cstr,
    has_final_dst: bool,
    node: *ast.Node,
};

/// Returns gen strategy and advances the temp local.
pub fn beginCall(c: *Chunk, cstr: Cstr, ret_t: cy.TypeId, hasCalleeValue: bool, node: *ast.Node) !CallInst {
    var ret: SlotId = undefined;
    var ret_owned: bool = undefined;
    var allocTempRet = true;

    // Optimization: Check to use dst cstr as ret.
    if (cstr.type == .tempReg or cstr.type == .localReg) {
        if (!cstr.data.slot.releaseDst and cstr.data.slot.dst + 1 == numSlots(c)) {
            if (cstr.data.slot.dst != 0) {
                ret = cstr.data.slot.dst;
                ret_owned = false;
                allocTempRet = false;
            }
        }
    }

    if (allocTempRet) {
        log.tracev("alloc ret temp", .{});
        ret = try bc.reserveTemp(c, ret_t);
        ret_owned = true;
        // Assumes nextTemp is never 0.
        if (cy.Trace and ret == 0) return error.Unexpected;
    }
    const tempStart = c.slot_stack.items.len;

    // Reserve registers for return value and return info.
    _ = try bc.reserveRtSlot(c);
    _ = try bc.reserveRtSlot(c);
    _ = try bc.reserveRtSlot(c);

    if (!hasCalleeValue) {
        // Reserve callee reg.
        _ = try bc.reserveRtSlot(c);
    }

    // Compute the number of preludes to be unwinded after the call inst.
    const numPreludeTemps: u8 = @intCast(c.slot_stack.items.len - tempStart);
    var has_final_dst = false;
    switch (cstr.type) {
        .tempReg,
        .localReg => {
            if (cstr.data.slot.dst != ret) {
                has_final_dst = true;
            }
        },
        .varSym,
        .liftedLocal => {
            has_final_dst = true;
        },
        else => {},
    }
    return .{
        .ret = ret,
        .ret_owned = ret_owned,
        .cstr = cstr,
        .has_final_dst = has_final_dst,
        .numPreludeTemps = numPreludeTemps,
        .node = node,
    };
}

fn finishInst(c: *Chunk, cstr: Cstr, dst: SlotId, has_final_dst: bool, retainedToDst: bool, node: *ast.Node) !GenValue {
    if (has_final_dst) {
        const val = regValue(c, dst, retainedToDst);
        return try genToFinalDst(c, val, cstr, node);
    } else {
        return regValue(c, dst, retainedToDst);
    }
}

fn finishCallInst(c: *Chunk, inst: CallInst, retainedToDst: bool) !GenValue {
    return finishInst(c, inst.cstr, inst.ret, inst.has_final_dst, retainedToDst, inst.node);
}

fn finishDstInst(c: *Chunk, inst: DstInst, retainedToDst: bool) !GenValue {
    return finishInst(c, inst.cstr, inst.dst, inst.has_final_dst, retainedToDst, inst.node);
}

fn finishNoErrNoDepInst(c: *Chunk, inst: NoErrInst, retainedToDst: bool) !GenValue {
    return finishInst(c, inst.cstr, inst.dst, inst.has_final_dst, retainedToDst, inst.node);
}

fn shouldRetain(c: *Chunk, src: SlotId, dst: SlotId, retain_override: bool) bool {
    const src_s = getSlot(c, src);
    const dst_s = getSlot(c, dst);
    if (!src_s.boxed) {
        return false;
    }
    if (src_s.type == .local) {
        if (dst_s.type == .temp) {
            return false or retain_override;
        } else {
            return true;
        }
    } else {
        return false or retain_override;
    }
}

fn genToExact(c: *Chunk, src: GenValue, dst: Cstr, node: *ast.Node) !GenValue {
    return genToExactDesc(c, src, dst, node, null);
}

/// Assumes `src` is a register and `dst` is an exact constraint.
fn genToExactDesc(c: *Chunk, src: GenValue, dst: Cstr, node: *ast.Node, extraIdx: ?u32) !GenValue {
    switch (dst.type) {
        .localReg,
        .tempReg => {
            const reg = dst.data.slot;

            if (src.reg != reg.dst) {
                const retain = shouldRetain(c, src.reg, reg.dst, dst.data.slot.retain);
                if (dst.data.slot.check_type) |type_id| {
                    try pushTypeCheck(c, src.reg, type_id, node);
                }

                if (reg.releaseDst) {
                    if (retain) {
                        try c.pushCodeExt(.copyRetainRelease, &.{ src.reg, reg.dst }, node, extraIdx);
                    } else {
                        try c.pushCodeExt(.copyReleaseDst, &.{ src.reg, reg.dst }, node, extraIdx);
                    }
                } else {
                    if (retain) {
                        try c.pushCodeExt(.copyRetainSrc, &.{ src.reg, reg.dst }, node, extraIdx);
                    } else {
                        try c.pushCodeExt(.copy, &.{ src.reg, reg.dst }, node, extraIdx);
                    }
                }

                if (!retain) {
                    // Shared ownership was moved from temp.
                    try consumeTempValue(c, src, node);
                }
                return regValue(c, reg.dst, retain or src.retained);
            } else {
                const src_s = getSlot(c, src.reg);
                const retain = src_s.boxed and dst.data.slot.retain;
                if (retain) {
                    try c.pushCode(.retain, &.{ src.reg }, node);
                } else {
                    // Nop.
                }
                return regValue(c, reg.dst, retain or src.retained);
            }
        },
        .liftedLocal => {
            const lifted = dst.data.liftedLocal;
            if (src.reg == lifted.reg) return error.Unexpected;

            const src_s = getSlot(c, src.reg);
            if (src_s.boxed and src_s.type == .local) {
                try c.pushCode(.retain, &.{ src.reg }, node);
            }

            const release: u8 = @intFromBool(lifted.rcCandidate);
            try c.pushCodeExt(.set_up_value, &.{ lifted.reg, src.reg, release }, node, extraIdx);
            return GenValue.initRetained(src.retained);
        },
        .varSym => {
            const src_s = getSlot(c, src.reg);
            if (src_s.boxed and src_s.type == .local) {
                try c.pushCode(.retain, &.{ src.reg }, node);
            }

            const pc = c.buf.len();
            try c.pushCodeExt(.setStaticVar, &.{ 0, 0, src.reg }, node, extraIdx);
            c.buf.setOpArgU16(pc + 1, @intCast(dst.data.varSym));
            return GenValue.initRetained(src.retained);
        },
        .captured => {
            const src_s = getSlot(c, src.reg);
            if (src_s.boxed and src_s.type == .local) {
                try c.pushCode(.retain, &.{ src.reg }, node);
            }

            const captured = dst.data.captured;
            try c.pushCodeExt(.setCaptured, &.{ c.curBlock.closureLocal, captured.idx, src.reg }, node, extraIdx);
            return GenValue.initRetained(src.retained);
        },
        else => {
            log.tracev("{}", .{dst.type});
            return error.NotExactCstr;
        },
    }
}

fn genToFinalDst(c: *Chunk, val: GenValue, dst: Cstr, node: *ast.Node) !GenValue {
    log.tracev("genToFinalDst src: {} dst: {s}", .{val.reg, @tagName(dst.type)});

    const res = try genToExact(c, val, dst, node);

    // Check to remove the temp that is used to move to final dst.
    if (val.isTemp()) {
        try popTemp(c, val.reg, node);
    }
    return res;
}

pub fn endCall(c: *Chunk, inst: CallInst, retained: bool) !GenValue {
    popNullSlots(c, inst.numPreludeTemps);
    return finishCallInst(c, inst, retained);
}

fn forIterStmt(c: *Chunk, idx: usize, node: *ast.Node) !void {
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

    const block = node.cast(.forIterStmt);

    var extraIdx = try c.fmtExtraDesc("iterator()", .{});
    try pushCallObjSymExt(c, iterTemp, 1,
        @intCast(c.compiler.iteratorMID),
        block.iterable, extraIdx);

    try releaseIf(c, iterv.retained, iterv.reg, block.iterable);
    _ = try pushUnwind(c, iterTemp);

    try pushBlock(c, true, node);
    try genStmts(c, data.declHead);

    const bodyPc = c.buf.ops.items.len;

    // next()
    try genIterNext(c, iterTemp, data.countLocal != null, block.iterable);
    const hasCounter = data.countLocal != null;

    const jump_none = try c.pushEmptyJumpNone(iterTemp + 1);
    if (data.eachLocal) |eachLocal| {
        // extraIdx = try c.fmtExtraDesc("copy next() to local", .{});

        // Unwrap to var.
        const unwrap_reg = toLocalReg(c, eachLocal);
        const unwrap_local = getLocalInfoPtr(c, unwrap_reg);
        try pushField(c, iterTemp + 1, 1, unwrap_reg, node);
        // Mark var defined for ARC.
        unwrap_local.some.defined = true;
    }
    if (data.countLocal) |countLocal| {
        extraIdx = try c.fmtExtraDesc("copy count to local", .{});
        const countTemp = regValue(c, iterTemp - 1, false);
        try setLocal(c, .{ .id = countLocal }, undefined, bt.Integer, block.each.?, .{ .rightv = countTemp, .extraIdx = extraIdx });
    }

    try pushRelease(c, iterTemp + 1, block.iterable);

    const jumpStackSave: u32 = @intCast(c.blockJumpStack.items.len);
    defer c.blockJumpStack.items.len = jumpStackSave;

    try genStmts(c, data.bodyHead);

    const b = c.blocks.getLast();
    try genReleaseLocals(c, b.nextLocalReg, b.node);
    // Pop sub block.
    try popLoopBlock(c);

    const contPc = c.buf.ops.items.len;
    try c.pushJumpBackTo(bodyPc);

    c.patchForBlockJumps(jumpStackSave, c.buf.ops.items.len, contPc);

    const skip_release = try c.pushEmptyJump();
    c.patchJumpNoneToCurPc(jump_none);
    try pushRelease(c, iterTemp + 1, block.iterable);
    c.patchJumpToCurPc(skip_release);

    // TODO: Iter local should be a reserved hidden local (instead of temp) so it can be cleaned up by endLocals when aborting the current fiber.
    try pushRelease(c, iterTemp, block.iterable);

    try popUnwind(c, iterTemp);

    if (hasCounter) {
        c.rega.nextTemp -= 2;
    }
    c.rega.nextTemp -= 1;
}

fn genIterNext(c: *Chunk, iterTemp: u8, hasCounter: bool, iterNodeId: *ast.Node) !void {
    var desc = try c.fmtExtraDesc("push iterator arg", .{});
    try c.buf.pushOp2Ext(.copy, iterTemp, iterTemp + cy.vm.CallArgStart + 1, desc);

    desc = try c.fmtExtraDesc("next()", .{});
    try pushCallObjSymExt(c, iterTemp + 1, 1,
        @intCast(c.compiler.nextMID),
        iterNodeId, desc);

    if (hasCounter) {
        try pushInlineBinExpr(c, .addInt, iterTemp-1, iterTemp-2, iterTemp-1, iterNodeId);
    }
}

fn loopStmt(c: *cy.Chunk, loc: usize, node: *ast.Node) !void {
    const data = c.ir.getStmtData(loc, .loopStmt);

    const top_pc = c.buf.ops.items.len;
    const jump_start: u32 = @intCast(c.blockJumpStack.items.len);
    defer c.blockJumpStack.items.len = jump_start;

    {
        try pushBlock(c, true, node);
        try genStmts(c, data.body_head);
        try popLoopBlock(c);
    }
    c.patchForBlockJumps(jump_start, c.buf.ops.items.len, top_pc);
}

fn forRangeStmt(c: *Chunk, idx: usize, node: *ast.Node) !void {
    const data = c.ir.getStmtData(idx, .forRangeStmt);

    // Reserve vars until end of block, hidden from user.
    const counter = try bc.reserveTemp(c, bt.Integer);
    const rangeEnd = try bc.reserveTemp(c, bt.Integer);

    // Range start.
    const startv = try genExpr(c, data.start, Cstr.simple);

    // Range end.
    const endv = try genExpr(c, data.end, Cstr.toTemp(rangeEnd));
    _ = endv;

    // Begin sub-block.
    try pushBlock(c, true, node);

    // Copy counter to itself if no each clause
    var eachLocal: SlotId = counter;
    if (data.eachLocal) |irVar| {
        try genStmt(c, data.declHead);
        eachLocal = toLocalReg(c, irVar);
    }

    const initPc = c.buf.ops.items.len;
    try c.pushCode(.forRangeInit, &.{ startv.reg, rangeEnd, @intFromBool(data.increment),
        counter, eachLocal, 0, 0 }, node);

    const bodyPc = c.buf.ops.items.len;
    const jumpStackSave = c.blockJumpStack.items.len;
    try genStmts(c, data.bodyHead);

    const b = c.blocks.getLast();
    try genReleaseLocals(c, c.curBlock.slot_start, b.slot_off, b.node);

    // End sub-block.
    try popLoopBlock(c);

    // Perform counter update and perform check against end range.
    const jumpBackOffset: u16 = @intCast(c.buf.ops.items.len - bodyPc);
    const forRangeOp = c.buf.ops.items.len;
    // The forRange op is patched by forRangeInit at runtime.
    c.buf.setOpArgU16(initPc + 6, @intCast(c.buf.ops.items.len - initPc));
    try c.pushCode(.forRange, &.{ counter, rangeEnd, eachLocal, 0, 0 }, node);
    c.buf.setOpArgU16(forRangeOp + 4, jumpBackOffset);

    c.patchForBlockJumps(jumpStackSave, c.buf.ops.items.len, forRangeOp);
    c.blockJumpStack.items.len = jumpStackSave;

    try popTempValue(c, startv, node);
    try popTemp(c, rangeEnd, node);
    try popTemp(c, counter, node);
}

fn verbose(c: *cy.Chunk, idx: usize, node: *ast.Node) !void {
    _ = node;
    const data = c.ir.getStmtData(idx, .verbose);
    cc.setVerbose(data.verbose);
}

fn tryStmt(c: *cy.Chunk, idx: usize, node: *ast.Node) !void {
    const data = c.ir.getStmtData(idx, .tryStmt);

    try pushUnwindTry(c);

    try pushBlock(c, false, node);
    try genStmts(c, data.bodyHead);
    try popBlock(c);

    const catch_pc = c.buf.ops.items.len;
    popUnwindTry(c, catch_pc);
    try c.pushCode(.catch_op, &.{0, 0, 0, 0}, node);

    try pushBlock(c, false, node);
    try genStmts(c, data.catchBodyHead);
    var errReg: SlotId = cy.NullU8;
    if (data.hasErrLocal) {
        errReg = toLocalReg(c, data.errLocal);
    }
    try popBlock(c);

    // Patch pushTry with errReg.
    c.buf.setOpArgs1(catch_pc + 3, errReg);

    c.buf.setOpArgU16(catch_pc + 1, @intCast(c.buf.ops.items.len - catch_pc));
}

fn genTryExpr(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .tryExpr);

    const type_id = c.ir.getExprType(idx).id;
    const temp = try reserveTemp(c, type_id);

    // Body expr.
    try pushUnwindTry(c);
    const childv = try genExpr(c, data.expr, Cstr.toTemp(temp));
    const catch_pc = c.buf.ops.items.len;
    popUnwindTry(c, catch_pc);

    try c.pushCode(.catch_op, &.{ 0, 0, 0, 0 }, node);

    var retained = childv.retained;
    if (data.catchBody != cy.NullId) {
        // Error is not copied anywhere.
        c.buf.setOpArgs1(catch_pc + 3, cy.NullU8);

        // Catch expr.
        const catchv = try genExpr(c, data.catchBody, Cstr.toTemp(temp));
        if (catchv.retained) {
            retained = true;
        }
    } else {
        // const inst = try bc.selectForDstInst(c, merge_cstr, bt.Any, false, node);

        // Runtime will copy error to `temp`.
        // c.buf.setOpArgs1(catch_pc + 3, inst.dst);
        c.buf.setOpArgs1(catch_pc + 3, temp);
        c.buf.setOpArgs1(catch_pc + 4, @intFromBool(false));

        // _ = try finishDstInst(c, inst, false);
    }
    c.buf.setOpArgU16(catch_pc + 1, @intCast(c.buf.ops.items.len - catch_pc));

    try initSlot(c, temp, retained, node);
    if (cstr.isExact()) {
        const val = try genToExact(c, childv, cstr, node);
        try popTemp(c, temp, node);
        return val;
    } else {
        return regValue(c, temp, retained);
    }
}

fn genUnwrapOr(c: *Chunk, loc: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(loc, .unwrap_or);
    const ret_t = c.ir.getExprType(loc).id;

    var finalCstr = cstr;
    if (!finalCstr.isExact()) {
        const temp = try bc.reserveTemp(c, ret_t);
        finalCstr = Cstr.toTemp(temp);
    }

    const optv = try genExpr(c, data.opt, Cstr.simple);

    const cond = try bc.reserveTemp(c, bt.Boolean);
    try c.pushCode(.none, &.{optv.reg, cond}, node);
    const jump_cond = try c.pushEmptyJumpCond(cond);

    const retain_some = true;
    const inst = try bc.selectForDstInst(c, finalCstr, ret_t, retain_some, node);
    const retain = !types.isUnboxedType(ret_t);
    try pushField(c, optv.reg, 1, retain, inst.dst, node);
    const somev = finishDstInst(c, inst, retain_some);
    const jump_end = try c.pushEmptyJump();

    // else.
    c.patchJumpCondToCurPc(jump_cond);
    _ = try genExpr(c, data.default, finalCstr);

    c.patchJumpToCurPc(jump_end);

    try popTemp(c, cond, node);
    try popTempValue(c, optv, node);

    return somev;
}

fn genIfExpr(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = node;
    const data = c.ir.getExprData(idx, .if_expr);
    const condNodeId = c.ir.getNode(data.cond);
    const type_id = c.ir.getExprType(idx).id;

    var finalCstr = cstr;
    if (!finalCstr.isExact()) {
        const temp = try bc.reserveTemp(c, type_id);
        finalCstr = Cstr.toTemp(temp);
    }

    // Cond.
    const condv = try genExpr(c, data.cond, Cstr.simple);

    const condFalseJump = try c.pushEmptyJumpNotCond(condv.reg);

    try popTempValue(c, condv, condNodeId);

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

fn ifStmt(c: *cy.Chunk, idx: usize, node: *ast.Node) !void {
    const data = c.ir.getStmtData(idx, .ifStmt);

    const condNodeId = c.ir.getNode(data.cond);
    const condv = try genExpr(c, data.cond, Cstr.simple);

    const jump_miss = try c.pushEmptyJumpNotCond(condv.reg);

    try pushBlock(c, false, node);
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

    try popTempValue(c, condv, condNodeId);
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

            const jump_miss = try c.pushEmptyJumpNotCond(condv.reg);

            // ARC cleanup for true case.
            try popTempValue(c, condv, condNodeId);

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

fn genBlockExpr(c: *Chunk, loc: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(loc, .blockExpr);
    const ret_t = c.ir.getExprType(loc).id;

    // Select merged dst.
    const inst = try bc.selectForDstInst(c, cstr, ret_t, true, node);

    try pushBlock(c, false, node);
    const b = c.genBlock();
    b.blockExprCstr = Cstr.toTempRetain(inst.dst);

    try genStmts(c, data.bodyHead);

    try popBlock(c);

    return finishDstInst(c, inst, true);
}

fn switchStmt(c: *Chunk, idx: usize, node: *ast.Node) !void {
    const switchLoc = c.ir.advanceStmt(idx, .switchStmt);
    _ = try genSwitch(c, switchLoc, null, node);
}

fn genSwitch(c: *Chunk, idx: usize, cstr: ?Cstr, node: *ast.Node) !GenValue {
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

                const temp = try bc.reserveTemp(c, bt.Boolean);
                const condv = try genExpr(c, condIdx, Cstr.simple);

                try c.pushOptionalDebugSym(condNodeId);
                try c.buf.pushOp3Ext(.compare, exprv.reg, condv.reg, temp, null);
                try popTempValue(c, condv, node);

                const condMissJump = try c.pushEmptyJumpNotCond(temp);

                try releaseTempValue(c, condv, condNodeId);

                const condMatchJump = try c.pushEmptyJump();
                try c.listDataStack.append(c.alloc, .{ .pc = condMatchJump });
                c.patchJumpNotCondToCurPc(condMissJump);
                // Miss continues to next cond.

                try releaseTempValue(c, condv, condNodeId);

                try popTemp(c, temp, node);
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
        _ = try genFalse(c, cstr.?, node);
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
    try popTempValue(c, exprv, node);

    // Complete with no value since assign statement doesn't do anything with it.
    return GenValue.initNoValue();
}

fn genMap(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = idx;
    const inst = try bc.selectForDstInst(c, cstr, bt.Map, true, node);
    try c.buf.pushOp1(.map, inst.dst);
    return finishDstInst(c, inst, true);
}

fn genList(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .list);
    const argsIdx = c.ir.advanceExpr(idx, .list);
    const args = c.ir.getArray(argsIdx, u32, data.numArgs);
    const ret_t = c.ir.getExprType(idx).id;

    const inst = try bc.selectForDstInst(c, cstr, ret_t, true, node);
    const argStart = numSlots(c);

    for (args) |argIdx| {
        const arg_t = c.ir.getExprType(argIdx).id;
        const temp = try bc.reserveTemp(c, arg_t);
        _ = try genExpr(c, argIdx, Cstr.toTempRetain(temp));
    }

    if (ret_t == bt.ListDyn) {
        try c.pushFCode(.list_dyn, &.{@intCast(argStart), data.numArgs, inst.dst}, node);
    } else {
        const start = c.buf.ops.items.len;
        try c.pushFCode(.list, &.{@intCast(argStart), data.numArgs, 0, 0, inst.dst}, node);
        c.buf.setOpArgU16(start + 3, @intCast(ret_t)); 
    }
    try popTemps(c, args.len, node);

    return finishDstInst(c, inst, true);
}

const ProcType = enum(u8) {
    main,
    fiber,
    func,
};

fn pushFiberBlock(c: *Chunk, numArgs: u8, node: *ast.Node) !void {
    log.tracev("push fiber block: {}", .{numArgs});

    try pushProc(c, .fiber, "comain", node);
    try reserveFiberCallRegs(c, numArgs);
}

fn popFiberBlock(c: *Chunk) !void {
    try genBlockReleaseLocals(c);

    // Pop boundary index.
    try popUnwindBoundary(c, c.curBlock.debugNode);

    try popProc(c);
}

pub const Sym = union {
    varSym: struct {
        id: u32,
    },
    func_sym: struct {
        group: u32,
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

pub fn pushFuncBlock(c: *Chunk, data: ir.FuncBlock, params: []align(1) const ir.FuncParam, node: *ast.Node) !void {
    log.tracev("push func block: {}, {}, {}, {}", .{data.func.numParams, data.maxLocals, data.func.isMethod(), node});
    try pushFuncBlockCommon(c, data.maxLocals, data.numParamCopies, params, data.func, node);
}

fn pushFuncBlockCommon(c: *Chunk, maxIrLocals: u8, numParamCopies: u8, params: []align(1) const ir.FuncParam, func: *cy.Func, node: *ast.Node) !void {
    try pushProc(c, .func, func.name(), node);

    if (c.compiler.config.gen_debug_func_markers) {
        try c.compiler.buf.pushDebugFuncStart(func, c.id);
    }

    // `reserveFuncRegs` may emit copy and box insts.
    try reserveFuncSlots(c, maxIrLocals, numParamCopies, params);
}

pub fn popFuncBlockCommon(c: *Chunk, func: *cy.Func) !void {
    // TODO: Check last statement to skip adding ret.
    try genFuncEnd(c);

    if (c.compiler.config.gen_debug_func_markers) {
        try c.compiler.buf.pushDebugFuncEnd(func, c.id);
    }

    try popProc(c);

    // Pop the null boundary index.
    try popUnwindBoundary(c, c.curBlock.debugNode);
}

fn genLambda(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .lambda);
    const func = data.func;
    const paramsIdx = c.ir.advanceExpr(idx, .lambda);
    const params = c.ir.getArray(paramsIdx, ir.FuncParam, func.numParams);

    const inst = try bc.selectForDstInst(c, cstr, bt.Any, true, node);

    // Prepare jump to skip the body.
    const skipJump = try c.pushEmptyJump();

    log.tracev("push lambda block: {}, {}", .{func.numParams, data.maxLocals});
    const funcPc = c.buf.ops.items.len;
    try pushFuncBlockCommon(c, data.maxLocals, data.numParamCopies, params, func, node);

    try genStmts(c, data.bodyHead);

    const stackSize = c.getMaxUsedRegisters();
    try popFuncBlockCommon(c, func);
    c.patchJumpToCurPc(skipJump);
    const offset: u16 = @intCast(c.buf.ops.items.len - funcPc);

    if (data.numCaptures == 0) {
        const start = c.buf.ops.items.len;
        try c.pushCode(.lambda, &.{
            0, 0, func.numParams, stackSize, @intFromBool(func.reqCallTypeCheck), 0, 0, inst.dst }, node);
        c.buf.setOpArgU16(start + 1, offset);
        c.buf.setOpArgU16(start + 6, @intCast(func.funcSigId));
    } else {
        const captures = c.ir.getArray(data.captures, u8, data.numCaptures);
        const start = c.buf.ops.items.len;
        try c.pushCode(.closure, &.{
            0, 0, func.numParams, @as(u8, @intCast(captures.len)), stackSize, 
            0, 0, cy.vm.CalleeStart, @intFromBool(func.reqCallTypeCheck), inst.dst
        }, node);
        c.buf.setOpArgU16(start + 1, offset);
        c.buf.setOpArgU16(start + 6, @intCast(func.funcSigId));

        const operandStart = try c.buf.reserveData(captures.len);
        for (captures, 0..) |irVar, i| {
            const reg = toLocalReg(c, irVar);
            c.buf.ops.items[operandStart + i].val = reg;
        }
    }

    if (inst.dst_owned) {
        try initSlot(c, inst.dst, true, node);
    }
    return finishDstInst(c, inst, true);
}

pub fn shouldGenMainScopeReleaseOps(c: *cy.Compiler) bool {
    return !c.vm.config.single_run;
}

fn genBlock(c: *Chunk, idx: usize, node: *ast.Node) !void {
    const data = c.ir.getStmtData(idx, .block);

    try pushBlock(c, false, node);
    try genStmts(c, data.bodyHead);
    try popBlock(c);
}

pub fn pushProc(c: *Chunk, btype: ProcType, name: []const u8, debugNode: *ast.Node) !void {
    const id = c.procs.items.len;
    try c.procs.append(c.alloc, name);
    try c.proc_stack.append(c.alloc, Proc.init(btype, id));
    c.curBlock = &c.proc_stack.items[c.proc_stack.items.len-1];
    c.curBlock.irLocalMapStart = @intCast(c.genIrLocalMapStack.items.len);
    c.curBlock.slot_start = @intCast(c.slot_stack.items.len);
    c.curBlock.debugNode = debugNode;

    try pushUnwindBoundary(c);
    try pushBlock(c, false, debugNode);
}

pub fn popProc(c: *Chunk) !void {
    log.tracev("pop gen block", .{});
    c.genIrLocalMapStack.items.len = c.curBlock.irLocalMapStart;

    try popBlock(c);
    c.slot_stack.items.len -= c.curBlock.num_pre_slots;

    var last = c.proc_stack.pop();

    last.deinit(c.alloc);
    if (c.proc_stack.items.len > 0) {
        c.curBlock = &c.proc_stack.items[c.proc_stack.items.len-1];
    }
}

pub fn pushBlock(c: *Chunk, isLoop: bool, node: *ast.Node) !void {
    log.tracev("push block {}", .{c.curBlock.block_depth});
    c.curBlock.block_depth += 1;

    const idx = c.blocks.items.len;
    try c.blocks.append(c.alloc, .{
        .node = node,
        .num_locals = 0,
        .isLoopBlock = isLoop,
        .slot_off = @intCast(c.slot_stack.items.len - c.curBlock.slot_start),
        .blockExprCstr = undefined,
    });

    if (cy.Trace) {
        c.blocks.items[idx].retainedTempStart = @intCast(getUnwindLen(c));
        c.blocks.items[idx].slot_count = @intCast(numSlots(c));
        c.indent += 1;
    }
}

pub fn popBlock(c: *Chunk) !void {
    log.tracev("pop block {}", .{c.curBlock.block_depth});

    const b = c.blocks.pop();

    try genReleaseLocals(c, c.curBlock.slot_start, b.slot_off, b.node);
    try popLocals(c, b.num_locals, b.node);

    c.curBlock.block_depth -= 1;

    if (cy.Trace) {
        c.indent -= 1;
    }
}

pub fn popLoopBlock(c: *Chunk) !void {
    const b = c.blocks.pop();

    try popLocals(c, b.num_locals, b.node);

    c.curBlock.block_depth -= 1;

    if (cy.Trace) {
        c.indent -= 1;
    }
}

pub const Block = struct {
    node: *ast.Node,

    // Offset from `Proc.slot_start`.
    slot_off: u8,

    isLoopBlock: bool,

    /// Locals declared in this block.
    num_locals: u8,

    /// Dst for block expression.
    blockExprCstr: Cstr,

    // Used to check the stack state after each stmt.
    retainedTempStart: if (cy.Trace) u32 else void = undefined,
    slot_count: if (cy.Trace) u32 else void = undefined,
};

pub const Proc = struct {
    type: ProcType,

    /// Index into `Chunk.procs`.
    id: u32,

    /// Whether codegen should create an ending that returns 1 arg.
    /// Otherwise `ret0` is generated.
    requiresEndingRet1: bool,

    /// If the function body belongs to a closure, this local
    /// contains the closure's value which is then used to perform captured var lookup.
    closureLocal: u8,

    /// Starts after the prelude registers.
    startLocalReg: u8,

    irLocalMapStart: u32,

    num_pre_slots: u32,
    slot_start: u32,

    /// Track the max number of slots used to know the stack size required by the block.
    max_slots: u8,

    debugNode: *ast.Node,

    block_depth: u32,

    /// If the last stmt is an expr stmt, return the local instead of releasing it. (Only for main block.)
    endLocal: u8 = cy.NullU8,

    /// LLVM
    // funcRef: if (cy.hasJIT) llvm.ValueRef else void = undefined,

    fn init(btype: ProcType, id: usize) Proc {
        return .{
            .id = @intCast(id),
            .type = btype,
            .requiresEndingRet1 = false,
            .closureLocal = cy.NullU8,
            .irLocalMapStart = 0,
            .slot_start = 0,
            .num_pre_slots = 0,
            .max_slots = 0,
            .startLocalReg = 0,
            .debugNode = undefined,
            .block_depth = 0,
        };
    }

    fn deinit(self: *Proc, alloc: std.mem.Allocator) void {
        _ = self;
        _ = alloc;
    }
};

fn genConstFloat(c: *Chunk, val: f64, dst: LocalId, node: *ast.Node) !GenValue {
    _ = node;
    const idx = try c.buf.getOrPushConst(cy.Value.initF64(val));
    try genConst(c, idx, dst, false, null);
    return regValue(c, dst, false);
}

fn genConst(c: *cy.Chunk, idx: usize, dst: u8, retain: bool, desc: ?u32) !void {
    const pc = c.buf.len();
    if (retain) {
        try c.buf.pushOp3Ext(.constRetain, 0, 0, dst, desc);
    } else {
        try c.buf.pushOp3Ext(.constOp, 0, 0, dst, desc);
    }
    c.buf.setOpArgU16(pc + 1, @intCast(idx));
}

fn exprStmt(c: *Chunk, idx: usize, node: *ast.Node) !void {
    const data = c.ir.getStmtData(idx, .exprStmt);

    if (data.isBlockResult) {
        const inMain = c.curBlock.block_depth == 1;
        if (inMain) {
            const exprv = try genExpr(c, data.expr, Cstr.simpleRetain);
            c.curBlock.endLocal = exprv.reg;
            try popTempValue(c, exprv, node);
        } else {
            // Return from block expression.
            const b = c.blocks.getLast();
            _ = try genExpr(c, data.expr, b.blockExprCstr);
        }
    } else {
        const exprv = try genExpr(c, data.expr, Cstr.simple);

        // TODO: Merge with previous release inst.
        try popTempValue(c, exprv, node);
    }
}

const LocalId = u8;

fn pushValue(c: *Chunk, reg: SlotId, retained: bool) !GenValue {
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
    if (isTempSlot(c, local)) {
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

    reg: SlotId,

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

    pub fn initLocalValue(reg: SlotId, retained: bool) GenValue {
        return .{
            .type = .local,
            .reg = reg,
            .retained = retained,
        };
    }

    pub fn initTempValue(reg: SlotId, retained: bool) GenValue {
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

fn pushReleaseVals(c: *Chunk, vals: []const GenValue, debugNode: *ast.Node) !void {
    if (vals.len > 1) {
        try c.pushOptionalDebugSym(debugNode);
        try c.buf.pushOp1(.releaseN, @intCast(vals.len));

        const start = c.buf.ops.items.len;
        try c.buf.ops.resize(c.alloc, c.buf.ops.items.len + vals.len);
        for (vals, 0..) |val, i| {
            c.buf.ops.items[start+i] = .{ .val = val.reg };
        }
    } else if (vals.len == 1) {
        try pushRelease(c, vals[0].reg, debugNode);
    }
}

fn pushReleases(c: *Chunk, regs: []const u8, debugNode: *ast.Node) !void {
    if (regs.len > 1) {
        try c.pushOptionalDebugSym(debugNode);
        try c.buf.pushOp1(.releaseN, @intCast(regs.len));
        try c.buf.pushOperands(regs);
    } else if (regs.len == 1) {
        try pushRelease(c, regs[0], debugNode);
    }
}

fn releaseCond2(c: *Chunk, releaseA: bool, a: SlotId, releaseB: bool, b: SlotId, debugId: *ast.Node) !void {
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

fn releaseTempValue2(c: *Chunk, a: GenValue, b: GenValue, debugId: *ast.Node) !void {
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

fn releaseIf(c: *Chunk, cond: bool, reg: SlotId, node: *ast.Node) !void {
    if (cond) {
        try pushRelease(c, reg, node);
    }
}

fn releaseTempValue(c: *Chunk, val: GenValue, node: *ast.Node) !void {
    if (val.isRetainedTemp()) {
        const slot = getSlot(c, val.reg);
        if (!slot.boxed or slot.type != .temp or !slot.temp_retained) {
            std.debug.panic("Expected retained temp {}.", .{val.reg});
        }
        try pushRelease(c, val.reg, node);
    }
}

fn pushRelease(c: *Chunk, slot: u8, node: *ast.Node) !void {
    log.tracev("release: {}", .{slot});
    try c.pushOptionalDebugSym(node);
    try c.buf.pushOp1Ext(.release, slot, null);
}

fn pushReleaseExt(c: *Chunk, slot: u8, node: *ast.Node, desc: u32) !void {
    log.tracev("release: {}", .{slot});
    try c.pushOptionalDebugSym(node);
    try c.buf.pushOp1Ext(.release, slot, desc);
}

fn genConstIntExt(c: *Chunk, val: u48, dst: LocalId, desc: ?u32) !GenValue {
    // TODO: Can be constU8.
    if (val <= std.math.maxInt(i8)) {
        try c.buf.pushOp2Ext(.constI8, @bitCast(@as(i8, @intCast(val))), dst, desc);
        return regValue(c, dst, false);
    }
    const idx = try c.buf.getOrPushConst(cy.Value.initInt(@intCast(val)));
    try genConst(c, idx, dst, false, desc);
    return regValue(c, dst, false);
}

fn pushTypeCheckOption(c: *cy.Chunk, local: SlotId, node: *ast.Node) !void {
    try c.pushFCode(.typeCheckOption, &.{ local }, node);
}

fn pushTypeCheck(c: *cy.Chunk, local: SlotId, typeId: cy.TypeId, node: *ast.Node) !void {
    const start = c.buf.ops.items.len;
    try c.pushFCode(.typeCheck, &.{ local, 0, 0, }, node);
    c.buf.setOpArgU16(start + 2, @intCast(typeId));
}

fn pushCallSym(c: *cy.Chunk, ret: u8, numArgs: u32, numRet: u8, symId: u32, node: *ast.Node) !void {
    const start = c.buf.ops.items.len;
    try c.pushFCode(.callSym, &.{ ret, @as(u8, @intCast(numArgs)), numRet, 0, 0, 0, 0, 0, 0, 0, 0 }, node);
    c.buf.setOpArgU16(start + 4, @intCast(symId));
}

fn pushCallSymDyn(c: *cy.Chunk, ret: u8, numArgs: u32, numRet: u8, symId: u32, node: *ast.Node) !void {
    const start = c.buf.ops.items.len;
    try c.pushFCode(.call_sym_dyn, &.{ ret, @as(u8, @intCast(numArgs)), numRet, 0, 0, 0, 0, 0, 0, 0, 0 }, node);
    c.buf.setOpArgU16(start + 4, @intCast(symId));
}

fn pushCall(c: *cy.Chunk, ret: u8, numArgs: u32, numRet: u8, node: *ast.Node) !void {
    try c.pushFCode(.call, &.{ret, @as(u8, @intCast(numArgs)), numRet}, node);
}

pub fn reserveLocal(c: *Chunk, declType: types.TypeId, lifted: bool, node: *ast.Node) !SlotId {
    const slot = try reserveSlot(c);

    // Stacks are always big enough because of pushProc.
    log.tracev("reserve slot={} {*} {} {} {s}", .{ slot, c.curBlock, c.curBlock.slot_start, declType, c.sema.getTypeBaseName(declType) });

    const boxed = !cy.types.isUnboxedType(declType) or lifted;
    setSlot(c, slot, .{
        .type = .local,
        .boxed = boxed,
        .owned = true,
        .boxed_up = lifted,
        .boxed_init = false,
        .temp_retained = false,
    });

    if (cy.Trace) {
        const nodeStr = try c.encoder.format(node, &cy.tempBuf);
        log.tracev("reserved from {s}", .{nodeStr});
    }

    return slot;
}

fn reserveRtSlot(c: *Chunk) !u8 {
    const slot = try reserveSlot(c);
    log.tracev("reserve rt slot={}", .{ slot });
    return slot;
}

fn reserveRetSlot(c: *Chunk) !u8 {
    const slot = try reserveSlot(c);
    log.tracev("reserve ret slot={}", .{ slot });
    c.slot_stack.items[c.curBlock.slot_start + slot].type = .ret;
    return slot;
}

fn reserveSlots(c: *Chunk, n: usize) !void {
    try c.slot_stack.resize(c.alloc, c.slot_stack.items.len + n);
    const zero = Slot{
        .type = .null,
        .owned = undefined,
        .boxed_up = undefined,
        .boxed = undefined,
        .boxed_init = undefined,
        .temp_retained = undefined,
    };
    @memset(c.slot_stack.items[c.slot_stack.items.len - n..], zero);
}

fn reserveSlot(c: *Chunk) !u8 {
    const slot = c.slot_stack.items.len - c.curBlock.slot_start;
    if (slot > std.math.maxInt(u8)) {
        return c.reportError("Exceeded max locals.", null);
    }
    try c.slot_stack.append(c.alloc, .{
        .type = .null,
        .owned = undefined,
        .boxed = undefined,
        .boxed_init = undefined,
        .boxed_up = undefined,
        .temp_retained = undefined, 
    });
    if (slot+1 > c.curBlock.max_slots) {
        c.curBlock.max_slots = @intCast(slot+1);
    }
    return @intCast(slot);
}

pub fn reserveTemp(c: *Chunk, type_id: cy.TypeId) !u8 {
    const slot = try reserveSlot(c);
    log.tracev("reserve temp: {}", .{slot});
    const boxed = !cy.types.isUnboxedType(type_id);
    setSlot(c, slot, .{
        .type = .temp,
        .boxed = boxed,
        .owned = true,
        .boxed_init = false,
        .boxed_up = false,
        .temp_retained = false,
    });
    return slot;
}

fn pushCallObjSym(c: *cy.Chunk, ret: u8, numArgs: u8, method: u16, node: *ast.Node) !void {
    try pushCallObjSymExt(c, ret, numArgs, method, node, cy.NullId);
}

fn pushCallObjSymExt(c: *cy.Chunk, ret: u8, numArgs: u8, method: u16, node: *ast.Node, desc: u32) !void {
    try c.pushFailableDebugSym(node);
    const start = c.buf.ops.items.len;
    try c.buf.pushOpSliceExt(.callObjSym, &.{
        ret, numArgs, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    }, desc);
    c.buf.setOpArgU16(start + 4, method);

    try c.pushCode(.ret_dyn, &.{ numArgs }, node);
}

fn pushInlineUnExpr(c: *cy.Chunk, code: cy.OpCode, child: u8, dst: u8, node: *ast.Node) !void {
    try c.pushFCode(code, &.{ child, dst, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }, node);
}

fn pushInlineBinExpr(c: *cy.Chunk, code: cy.OpCode, left: u8, right: u8, dst: u8, node: *ast.Node) !void {
    try c.pushFCode(code, &.{ left, right, dst, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }, node);
}

fn pushInlineTernExpr(c: *cy.Chunk, code: cy.OpCode, a: u8, b: u8, c_: u8, dst: u8, node: *ast.Node) !void {
    try c.pushFCode(code, &.{ a, b, c_, dst, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }, node);
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

fn pushObjectInit(c: *cy.Chunk, typeId: cy.TypeId, startLocal: u8, numFields: u8, dst: SlotId, debugNode: *ast.Node) !void {
    if (numFields <= 4) {
        const start = c.buf.ops.items.len;
        try c.pushCode(.objectSmall, &.{ 0, 0, startLocal, numFields, dst }, debugNode);
        c.buf.setOpArgU16(start + 1, @intCast(typeId)); 
    } else {
        const start = c.buf.ops.items.len;
        try c.pushFCode(.object, &.{ 0, 0, startLocal, numFields, dst }, debugNode);
        c.buf.setOpArgU16(start + 1, @intCast(typeId)); 
    }
}

fn pushFieldDyn(c: *cy.Chunk, recv: u8, dst: u8, fieldId: u16, debugNode: *ast.Node) !void {
    const start = c.buf.ops.items.len;
    try c.pushFCode(.fieldDyn, &.{ recv, dst, 0, 0, 0, 0, 0 }, debugNode);
    c.buf.setOpArgU16(start + 3, fieldId);
}

fn pushField(c: *cy.Chunk, recv: u8, fieldIdx: u8, retain: bool, dst: u8, debugNode: *ast.Node) !void {
    try c.pushCode(.field, &.{ recv, fieldIdx, @intFromBool(retain), dst }, debugNode);
}

fn pushFieldRef(c: *cy.Chunk, recv: u8, fieldIdx: u8, numNestedFields: u8, dst: u8, debugNode: *ast.Node) !void {
    try c.pushCode(.fieldRef, &.{ recv, fieldIdx, numNestedFields, dst }, debugNode);
}

/// Selecting for a non local inst with a dst operand.
/// A required dst can not be retained or a temp register is allocated and `requiresCopyRelease` will be set true.
///
/// Handles the case where the inst depends and assigns to the same local:
/// > let node = [Node ...]
/// > node = node.val
/// `node` rec gets +1 retain and saves to temp since the `node` cstr has `releaseDst=true`.
pub fn selectForDstInst(c: *cy.Chunk, cstr: Cstr, type_id: cy.TypeId, instCouldRetain: bool, node: *ast.Node) !DstInst {
    switch (cstr.type) {
        .varSym => {
            return .{
                .dst = try bc.reserveTemp(c, type_id),
                .dst_owned = true,
                .cstr = cstr,
                .has_final_dst = true,
                .node = node,
            };
        },
        .captured => {
            return .{
                .dst = try bc.reserveTemp(c, type_id),
                .dst_owned = true,
                .cstr = cstr,
                .has_final_dst = true,
                .node = node,
            };
        },
        .localReg,
        .tempReg => {
            if (cstr.data.slot.releaseDst or (cstr.data.slot.retain and !instCouldRetain)) {
                return .{
                    .dst = try bc.reserveTemp(c, type_id),
                    .dst_owned = true,
                    .cstr = cstr,
                    .has_final_dst = true,
                    .node = node,
                };
            } else {
                return .{
                    .dst = cstr.data.slot.dst,
                    .dst_owned = false,
                    .cstr = cstr,
                    .has_final_dst = false,
                    .node = node,
                };
            }
        },
        .liftedLocal => {
            return .{
                .dst = try bc.reserveTemp(c, type_id),
                .dst_owned = true,
                .cstr = cstr,
                .has_final_dst = true,
                .node = node,
            };
        },
        .simple => {
            return .{
                .dst = try bc.reserveTemp(c, type_id),
                .dst_owned = true,
                .cstr = cstr,
                .has_final_dst = false,
                .node = node,
            };
        },
        .none => return error.Unsupported,
    }
}

/// Selecting for a non local inst that can not fail.
/// A required dst can be retained but `requiresPreRelease` will be set to true.
pub fn selectForNoErrNoDepInst(c: *Chunk, cstr: Cstr, type_id: cy.TypeId, instCouldRetain: bool, node: *ast.Node) !NoErrInst {
    _ = instCouldRetain;
    switch (cstr.type) {
        .varSym => {
            return .{
                .dst = try bc.reserveTemp(c, type_id),
                .dst_owned = true,
                .requiresPreRelease = false,
                .cstr = cstr,
                .has_final_dst = true,
                .node = node,
            };
        },
        .localReg,
        .tempReg => {
            return .{
                .dst = cstr.data.slot.dst,
                .dst_owned = false,
                .requiresPreRelease = cstr.data.slot.releaseDst,
                .cstr = cstr,
                .has_final_dst = false,
                .node = node,
            };
        },
        .liftedLocal => {
            return .{
                .dst = try bc.reserveTemp(c, type_id),
                .dst_owned = true,
                .requiresPreRelease = false,
                .cstr = cstr,
                .has_final_dst = true,
                .node = node,
            };
        },
        .captured => {
            return .{
                .dst = try bc.reserveTemp(c, type_id),
                .dst_owned = true,
                .requiresPreRelease = false,
                .cstr = cstr,
                .has_final_dst = true,
                .node = node,
            };
        },
        .simple => {
            return .{
                .dst = try bc.reserveTemp(c, type_id),
                .dst_owned = true,
                .requiresPreRelease = false,
                .cstr = cstr,
                .has_final_dst = false,
                .node = node,
            };
        },
        .none => return error.Unsupported,
    }
}

pub const DstInst = struct {
    dst: SlotId,
    dst_owned: bool,
    cstr: Cstr,
    has_final_dst: bool,
    node: *ast.Node,
};

pub const NoErrInst = struct {
    dst: SlotId,
    dst_owned: bool,
    requiresPreRelease: bool,
    cstr: Cstr,
    has_final_dst: bool,
    node: *ast.Node,
};

const CstrType = enum(u8) {
    /// 1. Prefers local register first.
    /// 2. Allocates the next temp.
    simple,

    /// To a temp register.
    tempReg,

    /// To a local register.
    localReg,

    /// Var sym.
    varSym,

    /// Lifted local.
    liftedLocal,

    /// Captured.
    captured,

    /// No register selection.
    none,
};

/// Constraints on where a value should go to.
///
/// Some constraints have a `retain` flag.
/// If `retain` is true, the caller expects the value to have increased it's RC by 1.
///     * If the value already has a +1 retain (eg. map literal), the requirement is satisfied.
///     * If the value comes from a local, then a retain copy is generated.
///     * If the value does not have a RC, the requirement is satisfied.
/// If `retain` is false, the value can still be retained by +1 to keep it alive.
pub const Cstr = struct {
    type: CstrType,

    data: union {
        simple: struct {
            retain: bool,
        },
        slot: struct {
            dst: SlotId,
            retain: bool,
            releaseDst: bool,

            /// Whether to check src type before copy to dst.
            check_type: ?cy.TypeId,
        },
        // Runtime id.
        varSym: u32,
        liftedLocal: struct {
            reg: SlotId,
            /// This shouldn't change after initialization.
            rcCandidate: bool,
        },
        captured: struct {
            idx: u8,
        },
        uninit: void,
    } = .{ .uninit = {} },

    /// Only relevant for constraints that allow temps: exact, simple
    jitPreferCondFlag: bool = false,
    jitPreferConstant: bool = false,

    /// TODO: provide hint whether the allocated reill be used or not.
    /// eg. For expr statements, the top level expr reg isn't used.
    /// If it's not used and the current expr does not produce side-effects, it can omit generating its code.

    pub const none = Cstr{
        .type = .none,
    };

    pub const simple = Cstr{ .type = .simple, .data = .{ .simple = .{
        .retain = false,
    }}};

    pub const simpleRetain = Cstr{ .type = .simple, .data = .{ .simple = .{
        .retain = true,
    }}};

    pub const ret = Cstr{ .type = .localReg, .data = .{ .slot = .{
        .dst = 0,
        .retain = true,
        .releaseDst = false,
        .check_type = null,
    }}};

    pub fn toCaptured(idx: u8) Cstr {
        return .{ .type = .captured, .data = .{ .captured = .{
            .idx = idx,
        }}};
    }

    pub fn toVarSym(id: u32) Cstr {
        return .{ .type = .varSym, .data = .{
            .varSym = id
        }};
    }

    pub fn toRetained(self: Cstr) Cstr {
        switch (self.type) {
            .simple => {
                var res = self;
                res.data.simple.retain = true;
                return res;
            },
            .localReg, 
            .tempReg => {
                var res = self;
                res.data.slot.retain = true;
                return res;
            },
            else => {
                return self;
            },
        }
    }

    pub fn initLocalOrTemp(mustRetain: bool) Cstr {
        return .{
            .type = .localOrTemp,
            .mustRetain = mustRetain,
            .data = .{ .localOrTemp = {} }
        };
    }

    pub fn toTempRetain(reg: u8) Cstr {
        return .{ .type = .tempReg, .data = .{ .slot = .{
            .dst = reg,
            .retain = true,
            .releaseDst = false,
            .check_type = null,
        }}};
    }

    pub fn toTemp(reg: u8) Cstr {
        return .{ .type = .tempReg, .data = .{ .slot = .{
            .dst = reg,
            .retain = false,
            .releaseDst = false,
            .check_type = null,
        }}};
    }

    pub fn toRetainedTemp(reg: u8) Cstr {
        return .{ .type = .tempReg, .data = .{ .slot = .{
            .dst = reg,
            .retain = true,
            .releaseDst = true,
            .check_type = null,
        }}};
    }

    pub fn toLocal(reg: u8, rcCandidate: bool) Cstr {
        return .{ .type = .localReg, .data = .{ .slot = .{
            .dst = reg,
            .retain = true,
            .releaseDst = rcCandidate,
            .check_type = null,
        }}};
    }

    pub fn toLiftedLocal(reg: u8, rcCandidate: bool) Cstr {
        return .{ .type = .liftedLocal, .data = .{ .liftedLocal = .{
            .reg = reg,
            .rcCandidate = rcCandidate,
        }}};
    }

    pub fn isExact(self: Cstr) bool {
        return self.type != .simple;
    }
};

fn consumeTemp(c: *Chunk, slot: SlotId, node: *ast.Node) !void {
    getSlotPtr(c, slot).temp_retained = false;
    try popUnwindSlot(c, slot, node);
}

fn consumeTempValue(c: *Chunk, val: GenValue, node: *ast.Node) !void {
    if (val.isRetainedTemp()) {
        try consumeTemp(c, val.reg, node);
    }
}

pub fn popTempValue(c: *Chunk, val: GenValue, node: *ast.Node) !void {
    if (val.type == .temp) {
        try popTemp(c, val.reg, node);
    }
}

pub fn popTemp(c: *Chunk, slot_id: SlotId, node: *ast.Node) !void {
    log.tracev("pop temp: {} -{}", .{numSlots(c), 1});
    if (slot_id + 1 != numSlots(c)) {
        return c.reportErrorFmt("Pop temp {}, but found {}.", &.{v(slot_id), v(numSlots(c)-1)}, node);
    }

    const slot = getSlot(c, slot_id);
    if (slot.type != .temp) {
        return c.reportErrorFmt("Expected temp slot {}.", &.{v(slot_id)}, node);
    }

    if (slot.boxed) {
        if (!slot.boxed_init) {
            return c.reportErrorFmt("Expected boxed init for slot {}.", &.{v(slot_id)}, node);
        }
        if (slot.temp_retained) {
            try pushRelease(c, slot_id, node);
            try popUnwindSlot(c, slot_id, node);
        }
    }
    c.slot_stack.items.len -= 1;
}

pub fn popTemps(c: *cy.Chunk, n: usize, node: *ast.Node) !void {
    log.tracev("pop temps: {} -{}", .{numSlots(c), n});
    const nslots = numSlots(c);
    const slots = c.slot_stack.items[c.slot_stack.items.len-n..];
    for (0..slots.len) |i| {
        const slot = slots[slots.len-1-i];
        if (slot.type != .temp) {
            return c.reportError("Expected temp slot.", node);
        }
        if (slot.boxed) {
            if (slot.temp_retained) {
                const slot_id: SlotId = @intCast(nslots - 1 - i);
                try pushRelease(c, slot_id, node);
                try popUnwindSlot(c, slot_id, node);
            }
        }
    }
    c.slot_stack.items.len -= n;
}

pub fn popLocals(c: *cy.Chunk, n: usize, node: *ast.Node) !void {
    log.tracev("pop locals: {} -{}", .{numSlots(c), n});
    const nslots = numSlots(c);
    const slots = c.slot_stack.items[c.slot_stack.items.len-n..];
    for (0..slots.len) |i| {
        const slot = slots[slots.len-1-i];
        if (slot.type != .local) {
            @panic("Expected local slot.");
        }
        if (slot.boxed) {
            const slot_id: SlotId = @intCast(nslots-1-i);
            try popUnwindSlot(c, slot_id, node);
        }
    }
    c.slot_stack.items.len -= n;
}

pub fn popLocal(c: *cy.Chunk, slot: SlotId, node: *ast.Node) !void {
    if (slot+1 != c.slot_stack.items.len) {
        std.debug.panic("Expected slot {}, found {}.", .{slot, c.slot_stack.items.len-1});
    }
    const last = c.slot_stack.items[c.slot_stack.items.len-1];
    if (last.type != .local) {
        std.debug.panic("Expected local at {}.", .{slot});
    }

    if (last.boxed) {
        try popUnwindSlot(c, slot, node);
    }
    c.slot_stack.items.len -= 1;
}

pub fn popNullSlots(c: *cy.Chunk, n: u8) void {
    log.tracev("free null slots: {} -{}", .{numSlots(c), n});
    const slots = c.slot_stack.items[c.slot_stack.items.len-n..];
    for (slots) |slot| {
        if (slot.type != .null) {
            @panic("Expected null slot.");
        }
    }
    c.slot_stack.items.len -= n;
}

pub inline fn isTempSlot(c: *const cy.Chunk, slot: SlotId) bool {
    return c.slot_stack.items[c.curBlock.slot_start + slot].type == .temp;
}

pub fn popUnwindBoundary(c: *Chunk, node: *ast.Node) !void {
    log.tracev("-pop unwind boundary: stack={}", .{c.unwind_stack.items.len});
    const entry = c.unwind_stack.pop();
    if (entry.type != .boundary) {
        return c.reportErrorFmt("Expected unwind boundary, found {}.", &.{v(entry.type)}, node);
    }
}

pub fn popUnwindSlot(c: *Chunk, slot: SlotId, node: *ast.Node) !void {
    log.tracev("-pop unwind slot: stack={} slot={}", .{c.unwind_stack.items.len, slot});
    const entry = c.unwind_stack.pop();
    if (entry.payload != slot) {
        return c.reportErrorFmt("Pop unwind at {}, found {}.", &.{v(slot), v(entry.payload)}, node);
    }
}

pub fn pushUnwindSlot(c: *Chunk, slot: u8) !void {
    log.tracev("+push unwind: stack={} slot={}", .{c.unwind_stack.items.len, slot});
    try c.unwind_stack.append(c.alloc, .{
        .payload = slot,
        .created = false,
        .type = .slot,
    });
}

pub fn popUnwindTry(c: *Chunk, catch_pc: usize) void {
    log.tracev("-pop unwind try: stack={} catch={}", .{c.unwind_stack.items.len, catch_pc});
    const entry = c.unwind_stack.pop();
    if (entry.created) {
        c.buf.unwind_trys.items[entry.rt_idx].catch_pc = @intCast(catch_pc);
    }
}

pub fn pushUnwindTry(c: *Chunk) !void {
    log.tracev("+push unwind try: stack={}", .{c.unwind_stack.items.len});
    try c.unwind_stack.append(c.alloc, .{
        .created = false,
        .type = .try_e,
    });
}

const UnwindEntryType = enum(u2) {
    boundary,
    try_e,
    slot,
};

pub const UnwindEntry = packed struct {
    // `payload` can refer to catch pc for a try entry, or slot id for a slot entry.
    payload: u32 = undefined,
    rt_idx: u29 = undefined,
    created: bool,
    type: UnwindEntryType,
};

pub fn getUnwindLen(c: *Chunk) usize {
    return c.unwind_stack.items.len;
}

pub fn pushUnwindBoundary(c: *Chunk) !void {
    try c.unwind_stack.append(c.alloc, .{
        .created = false,
        .type = .boundary,
    });
}

pub fn getLastUnwindKey(self: *Chunk) !cy.fiber.UnwindKey {
    const entry = &self.unwind_stack.items[self.unwind_stack.items.len-1];
    if (entry.type == .boundary) {
        return cy.fiber.UnwindKey.initNull();
    }
    if (entry.created) {
        return cy.fiber.UnwindKey.fromCreatedEntry(entry.*);
    }
    var prev = cy.fiber.UnwindKey.initNull();
    if (self.unwind_stack.items.len > 1) {
        const prev_e = self.unwind_stack.items[self.unwind_stack.items.len-2];
        if (prev_e.type != .boundary) {
            if (!prev_e.created) {
                // Multiple uncreated temp indexes. 

                // Find first uncreated.
                var first = self.unwind_stack.items.len-2;
                while (first > 0) {
                    first -= 1;
                    if (self.unwind_stack.items[first].type == .boundary) {
                        // Block boundary.
                        prev = cy.fiber.UnwindKey.initNull();
                        first += 1;
                        break;
                    }
                    if (self.unwind_stack.items[first].created) {
                        // Created.
                        prev = cy.fiber.UnwindKey.fromCreatedEntry(self.unwind_stack.items[first]);
                        first += 1;
                        break;
                    }
                }

                for (first..self.unwind_stack.items.len-1) |i| {
                    const e = &self.unwind_stack.items[i];
                    if (e.type == .try_e) {
                        const rt_idx = self.buf.unwind_trys.items.len;
                        try self.buf.unwind_trys.append(self.alloc, .{ .catch_pc = e.payload, .prev = prev });
                        prev = cy.fiber.UnwindKey{ .idx = @intCast(rt_idx), .is_try = true, .is_null = false };
                        e.created = true;
                        e.rt_idx = @intCast(rt_idx);
                    } else {
                        const rt_idx = self.buf.unwind_slots.items.len;
                        try self.buf.unwind_slots.append(self.alloc, @intCast(e.payload));
                        try self.buf.unwind_slot_prevs.append(self.alloc, prev);
                        prev = cy.fiber.UnwindKey{ .idx = @intCast(rt_idx), .is_try = false, .is_null = false };
                        e.created = true;
                        e.rt_idx = @intCast(rt_idx);
                    }
                }
            } else {
                // Previous index is already created.
                prev = cy.fiber.UnwindKey.fromCreatedEntry(prev_e);
            }
        }
    }

    // Insert unwind temp now that it's needed by a failable inst.
    if (entry.type == .try_e) {
        const rt_idx = self.buf.unwind_trys.items.len;
        try self.buf.unwind_trys.append(self.alloc, .{ .catch_pc = entry.payload, .prev = prev });
        entry.created = true;
        entry.rt_idx = @intCast(rt_idx);
    } else {
        const rt_idx = self.buf.unwind_slots.items.len;
        try self.buf.unwind_slots.append(self.alloc, @intCast(entry.payload));
        try self.buf.unwind_slot_prevs.append(self.alloc, prev);
        entry.created = true;
        entry.rt_idx = @intCast(rt_idx);
    }
    return cy.fiber.UnwindKey.fromCreatedEntry(entry.*);
}
