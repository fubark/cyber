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
                    const field_id = try c.vm.ensureField(field.sym.head.name());
                    try c.vm.addTypeField(@intCast(typeId), field_id, @intCast(i), field.type);
                }
            },
            .struct_t => {
                const obj = sym.cast(.struct_t);
                for (obj.fields[0..obj.numFields], 0..) |field, i| {
                    const field_id = try c.vm.ensureField(field.sym.head.name());
                    try c.vm.addTypeField(@intCast(typeId), field_id, @intCast(i), field.type);
                }
            },
            .dummy_t,
            .trait_t,
            .hostobj_t,
            .type,
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
        for (chunk.funcs.items) |func| {
            if (func.type == .userLambda) {
                try prepareFunc(c, null, func);
            }
        }
        for (chunk.deferred_funcs.items) |func| {
            // prepareSym will handle deferred funcs that have a parent func sym.
            if (func.variant == null) continue;

            try prepareFunc(c, null, func);
        }
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

    if (!c.cont) {
        const context_vars = c.vm.c.getContextVars();

        const core = c.chunks.items[0].sym.getMod();

        const DefaultMemoryT = core.getSym("DefaultMemory").?.getStaticType().?;
        const impl = try c.vm.allocObjectSmall(DefaultMemoryT, &.{});
        const IMemoryT = core.getSym("IMemory").?.getStaticType().?;
        const vtable_idx = c.gen_vtables.get(bc.VtableKey{ .type = DefaultMemoryT, .trait = IMemoryT }).?;
        const imem = try c.vm.allocTrait(IMemoryT, @intCast(vtable_idx), impl);
        const mem = try c.vm.allocObjectSmall(bt.Memory, &.{ imem });
        try context_vars.append(c.alloc, .{ .value = mem });

        // test_int.
        try context_vars.append(c.alloc, .{ .value = cy.Value.initInt(123) });
    }

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
        .hostobj_t,
        .type,
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

pub fn prepareFunc(c: *cy.Compiler, opt_group: ?rt.FuncGroupId, func: *cy.Func) !void {
    switch (func.type) {
        .template,
        .trait => return,
        .userLambda => {
            if (cy.Trace) {
                const symPath = try cy.sym.allocSymName(&c.sema, c.alloc, func.parent, .{});
                defer c.alloc.free(symPath);
                log.tracev("prep lambda in: {s}", .{symPath});
            }
            if (opt_group != null) {
                return error.Unexpected;
            }
            _ = try reserveFunc(c, func);
        },
        .hostFunc => {
            if (cy.Trace) {
                const symPath = try cy.sym.allocSymName(&c.sema, c.alloc, func.parent, .{});
                defer c.alloc.free(symPath);
                log.tracev("prep func: {s}", .{symPath});
            }
            const funcSig = c.sema.getFuncSig(func.funcSigId);
            const rtFunc = rt.FuncSymbol.initHostFunc(@ptrCast(func.data.hostFunc.ptr), funcSig.reqCallTypeCheck, func.isMethod(), funcSig.numParams(), func.funcSigId);
            if (opt_group) |group| {
                _ = try addGroupFunc(c, group, func, rtFunc);
            } else {
                const id = try reserveFunc(c, func);
                if (c.vm.funcSyms.buf[id].type == .null) {
                    completeFunc(c, id, func, rtFunc);
                }
            }
        },
        .userFunc => {
            if (cy.Trace) {
                const symPath = try cy.sym.allocSymName(&c.sema, c.alloc, func.parent, .{});
                defer c.alloc.free(symPath);
                log.tracev("prep func: {s}", .{symPath});
            }
            if (opt_group) |group| {
                _ = try addGroupFunc(c, group, func, rt.FuncSymbol.initNull());
            } else {
                _ = try reserveFunc(c, func);
            }
            // Func is patched later once funcPc and stackSize is obtained.
        },
    }
}

fn addGroupFunc(c: *cy.Compiler, group: rt.FuncGroupId, func: *cy.Func, rtFunc: rt.FuncSymbol) !u32 {
    const id = try c.vm.addGroupFunc(group, func.name(), func.funcSigId, rtFunc);
    try c.genSymMap.put(c.alloc, func, .{ .func = .{ .id = @intCast(id), .pc = 0 }});
    return @intCast(id);
}

fn reserveFunc(c: *cy.Compiler, func: *cy.Func) !u32 {
    const res = try c.genSymMap.getOrPut(c.alloc, func);
    if (!res.found_existing) {
        const id = try c.vm.addFunc(func.name(), func.funcSigId, rt.FuncSymbol.initNull());
        res.value_ptr.* = .{ .func = .{ .id = id, .pc = 0 }};
    }
    return res.value_ptr.func.id;
}

fn completeFunc(c: *cy.Compiler, id: u32, func: *cy.Func, sym: rt.FuncSymbol) void {
    log.tracev("complete func gen: {s} {}", .{func.name(), id});
    if (c.vm.funcSyms.buf[id].type != .null) {
        std.debug.panic("Func already completed: {s} {}", .{func.name(), id});
    }
    c.vm.funcSyms.buf[id] = sym;
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
            const slot = try declareLocal(c, idx, node);
            if (getSlot(c, slot).boxed) {
                exp_unwind_index_start += 1;
            }
            exp_slot_count += 1;
        },
        .declareLocalInit   => {
            const slot = try declareLocalInit(c, idx, node);
            if (getSlot(c, slot).boxed) {
                exp_unwind_index_start += 1;
            }
            exp_slot_count += 1;
        },
        .exprStmt           => try genExprStmt(c, idx, node),
        .forRangeStmt       => try forRangeStmt(c, idx, node),
        .funcBlock          => try funcBlock(c, idx, node),
        .ifStmt             => try genIfStmt(c, idx, node),
        .loopStmt           => try loopStmt(c, idx, node),
        .mainBlock          => try mainBlock(c, idx, node),
        .block              => try genBlock(c, idx, node),
        .opSet              => try opSet(c, idx, node),
        .pushDebugLabel     => try pushDebugLabel(c, idx),
        .retExprStmt        => try bc.retExprStmt(c, idx, node),
        .retStmt            => try genRetStmt(c),
        .setCaptured        => try setCaptured(c, idx, node),
        .set_field_dyn      => try setFieldDyn(c, idx, .{}, node),
        .setIndex           => try setIndex(c, idx, node),
        .setLocal           => try irSetLocal(c, idx, node),
        .set_field          => try setField(c, idx, node),
        .set_deref          => try genSetDeref(c, idx, node),
        .setVarSym          => try setVarSym(c, idx, node),
        .switchStmt         => try genSwitchStmt(c, idx, node),
        .tryStmt            => try genTryStmt(c, idx, node),
        .verbose            => try verbose(c, idx, node),
        // TODO: Specialize op assign.
        // .opSetLocal => try opSetLocal(c, getData(pc, .opSetLocal), node),
        // .opSetObjectField => try opSetObjectField(c, getData(pc, .opSetObjectField), node),
        // .opSetField => try opSetField(c, getData(pc, .opSetField), node),
        //         .dumpBytecode => {
        //             try cy.debug.dumpBytecode(c.compiler.vm, null);
        //         },
        else => {
            rt.errZFmt(c.vm, "{}\n", .{code});
            return error.TODO;
        }
    }

    if (cy.Trace) {
        if (cc.verbose()) {
            rt.logFmt("{}| end {} unw={} nslots={}", &.{
                cy.fmt.repeat(' ', c.indent * 4), v(@tagName(code)),
                v(c.unwind_stack.items.len), v(c.slot_stack.items.len),
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

    for (c.ir.func_blocks.items) |block| {
        try genStmt(c, block);
    }

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
        .address_of         => genAddressOf(c, idx, cstr, node),
        .array              => genArray(c, idx, cstr, node),
        .await_expr         => genAwait(c, idx, cstr, node),
        .box                => genBox(c, idx, cstr, node),
        .byte               => genByte(c, idx, cstr, node),
        .captured           => genCaptured(c, idx, cstr, node),
        .cast               => genCast(c, idx, cstr, node),
        .coinitCall         => genCoinitCall(c, idx, cstr, node),
        .context            => genContext(c, idx, cstr, node),
        .coresume           => genCoresume(c, idx, cstr, node),
        .coyield            => genCoyield(c, idx, cstr, node),
        .deref              => genDeref(c, idx, cstr, node),
        .enumMemberSym      => genEnumMemberSym(c, idx, cstr, node),
        .errorv             => genError(c, idx, cstr, node),
        .falsev             => genFalse(c, cstr, node),
        .fieldDyn           => genFieldDyn(c, idx, cstr, node),
        .field              => genField(c, idx, cstr, node),
        .float              => genFloat(c, idx, cstr, node),
        .func_ptr           => genFuncPtr(c, idx, cstr, node),
        .func_union         => genFuncUnion(c, idx, cstr, node),
        .if_expr            => genIfExpr(c, idx, cstr, node),
        .int                => genInt(c, idx, cstr, node),
        .lambda             => genLambda(c, idx, cstr, node),
        .list               => genList(c, idx, cstr, node),
        .local              => genLocal(c, idx, cstr, node),
        .map                => genMap(c, idx, cstr, node),
        .none               => genNone(c, idx, cstr, node),
        .object_init        => genObjectInit(c, idx, cstr, node),
        .pre                => return error.Unexpected,
        .preBinOp           => genBinOp(c, idx, cstr, node),
        .call_dyn           => genCallDyn(c, idx, cstr, node),
        .call_sym           => genCallFuncSym(c, idx, cstr, node),
        .call_sym_dyn       => genCallSymDyn(c, idx, cstr, node),
        .call_trait         => genCallTrait(c, idx, cstr, node),
        .call_obj_sym       => genCallObjSym(c, idx, cstr, node),
        .preUnOp            => genUnOp(c, idx, cstr, node),
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
        .type               => genType(c, idx, cstr, node),
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

    const main_pc = c.buf.ops.items.len;

    var child = data.bodyHead;
    while (child != cy.NullId) {
        try genStmt(c, child);
        child = c.ir.getStmtNext(child);
    }

    if (shouldGenMainScopeReleaseOps(c.compiler)) {
        try genReleaseBlock(c);
    }
    if (c.curBlock.endLocal != cy.NullU8) {
        try mainEnd(c, c.curBlock.endLocal);
    } else {
        try mainEnd(c, null);
    }
    try popProc(c);

    c.buf.mainStackSize = c.getMaxUsedRegisters();
    c.buf.main_pc = @intCast(main_pc);

    // Pop boundary index.
    try popUnwindBoundary(c, node);
}

pub fn funcBlock(c: *Chunk, idx: usize, node: *ast.Node) !void {
    const data = c.ir.getStmtData(idx, .funcBlock);
    const func = data.func;

    // Skip if marked as skip (usually for one-off compile-time functions)
    if (data.skip) {
        return;
    }

    // Skip if func has already been generated by compile-time.
    const id = try reserveFunc(c.compiler, func);
    if (c.vm.funcSyms.buf[id].type != .null) {
        return;
    }

    const params = c.ir.getArray(data.params, ir.FuncParam, func.numParams);

    const funcPc = c.buf.ops.items.len;

    try pushFuncBlock(c, data, params, node);

    try genStmts(c, data.bodyHead);

    const stackSize = c.getMaxUsedRegisters();

    const rt_func = rt.FuncSymbol.initFunc(funcPc, @intCast(stackSize), func.numParams, func.funcSigId, func.reqCallTypeCheck, func.isMethod());
    completeFunc(c.compiler, id, func, rt_func);
    try popFuncBlockCommon(c, func);
}

fn genAwait(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .await_expr);
    const ret_t = c.ir.getExprType(idx).id;
    const inst = try bc.selectForDstInst(c, cstr, ret_t, true, node);
    const childv = try genExpr(c, data.expr, Cstr.simple);
    try initTempValue(c, childv, node);

    try c.pushCode(.await_op, &.{childv.reg}, node);
    try c.pushCode(.future_value, &.{childv.reg, inst.dst }, node);
    try popTempValue(c, childv, node);

    return finishDstInst(c, inst, true);
}

fn genCoresume(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .coresume);
    const inst = try bc.selectForDstInst(c, cstr, bt.Any, true, node);

    const childv = try genExpr(c, data.expr, Cstr.simpleRetain);
    try initTempValue(c, childv, node);

    try c.pushCode(.coresume, &.{childv.reg, inst.dst}, node);
    try popTempValue(c, childv, node);
    if (inst.own_dst) {
        try initSlot(c, inst.dst, true, node);
    }

    return finishDstInst(c, inst, true);
}

fn genCoyield(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = idx;
    try c.pushFCode(.coyield, &.{0, 0}, node);
    // TODO: return coyield expression.
    return genFalse(c, cstr, node);
}

fn genCoinitCall(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const call = c.ir.getExprData(idx, .coinitCall).call;
    const callCode: ir.ExprCode = @enumFromInt(c.ir.buf.items[call]);

    const inst = try bc.selectForDstInst(c, cstr, bt.Fiber, true, node);

    const tempStart = numSlots(c);
    var numArgs: u32 = 0;
    var args: []align(1) const u32 = undefined;
    if (callCode == .call_sym) {
        const data = c.ir.getExprData(call, .call_sym);
        numArgs = data.numArgs;

        args = c.ir.getArray(data.args, u32, numArgs);
    } else if (callCode == .call_dyn) {
        const data = c.ir.getExprData(call, .call_dyn);
        numArgs = data.numArgs;
        args = c.ir.getArray(data.args, u32, numArgs);

        const temp = try bc.reserveTemp(c, bt.Dyn);
        _ = try genExpr(c, data.callee, Cstr.toTempRetain(temp));
    } else return error.Unexpected;

    for (args) |argIdx| {
        const arg_t = c.ir.getExprType(argIdx).id;
        const temp = try bc.reserveTemp(c, arg_t);
        _ = try genExpr(c, argIdx, Cstr.toTempRetain(temp));
    }

    const callExprId = node.cast(.coinit).child;

    var numTotalArgs = numArgs;
    var argDst: u8 = undefined;
    if (callCode == .call_sym) {
        argDst = 1 + cy.vm.CallArgStart;
    } else if (callCode == .call_dyn) {
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
    if (callCode == .call_sym) {
        const data = c.ir.getExprData(call, .call_sym);
        const rtId = c.compiler.genSymMap.get(data.func).?.func.id;
        try pushCallSym(c, callRet, numArgs, 1, rtId, @ptrCast(callExprId));

        const coreturn_pc = c.buf.ops.items.len;
        const call_ret_t = c.ir.getExprType(call).id;
        const box = c.sema.isUnboxedType(call_ret_t);
        try c.pushCode(.coreturn, &.{ 0, 0, @intFromBool(box) }, node);
        c.buf.setOpArgU16(coreturn_pc + 1, @intCast(call_ret_t));
    } else if (callCode == .call_dyn) {
        try pushCall(c, callRet, numArgs, 1, @ptrCast(callExprId));
        try c.pushCode(.ret_dyn, &.{ @intCast(numArgs) }, @ptrCast(callExprId));
        try c.pushCode(.coreturn, &.{ 0, 0, 0 }, node);
    } else return error.Unexpected;

    c.buf.setOpArgs1(coinitPc + 4, @intCast(c.buf.ops.items.len - coinitPc));

    try popFiberBlock(c);

    try popTemps(c, numTotalArgs, node);
    if (inst.own_dst) {
        try initSlot(c, inst.dst, true, node);
    }

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
    try initTempValue(c, childv, node);

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

    try consumeTempValue(c, childv, node);
    try popTempValue(c, childv, node);
    if (inst.own_dst) {
        try initSlot(c, inst.dst, childv.retained, node);
    }

    return finishDstInst(c, inst, childv.retained);
} 

fn genFieldDyn(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .fieldDyn);

    const inst = try bc.selectForDstInst(c, cstr, bt.Dyn, true, node);

    const recv = try genExpr(c, data.rec, Cstr.simple);
    try initTempValue(c, recv, node);

    const field_id = try c.compiler.vm.ensureField(data.name);
    try pushFieldDyn(c, recv.reg, inst.dst, @intCast(field_id), node);

    try popTempValue(c, recv, node);
    if (inst.own_dst) {
        try initSlot(c, inst.dst, true, node);
    }
    return finishDstInst(c, inst, true);
}

fn genAddressOf(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .address_of);
    return genAddressOf2(c, data.expr, cstr, node);
}

fn genAddressOf2(c: *Chunk, expr: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const code = c.ir.getExprCode(expr);
    switch (code) {
        .local => {
            const data = c.ir.getExprData(expr, .local);
            const local_slot = toLocalReg(c, data.id);
            const slot = getSlot(c, local_slot);

            const inst = try bc.selectForDstInst(c, cstr, bt.Integer, false, node);

            const expr_t = c.ir.getExprType(expr).id;
            const type_sym = c.sema.getTypeSym(expr_t);
            var is_ptr = false;
            if (type_sym.getVariant()) |variant| {
                if (variant.getSymTemplate() == c.sema.pointer_tmpl) {
                    is_ptr = true;
                }
            }
            if (is_ptr) {
                try c.pushCode(.copy, &.{ local_slot, inst.dst }, node);
            } else {
                if (slot.boxed_up) {
                    const temp = try bc.reserveTemp(c, bt.Integer);
                    const lifted_struct = c.sema.isStructType(expr_t) or c.sema.isArrayType(expr_t);
                    try c.pushCode(.addr_local, &.{ local_slot, @intFromBool(lifted_struct), temp }, node);
                    try initSlot(c, temp, false, node);

                    try c.pushCode(.copy, &.{ temp, inst.dst }, node);
                    try popTemp(c, temp, node);
                } else {
                    try c.pushCode(.addr_local, &.{ local_slot, 0, inst.dst }, node);
                }
            }
            if (inst.own_dst) {
                try initSlot(c, inst.dst, false, node);
            }
            return finishDstInst(c, inst, false);
        },
        .field => {
            const data = c.ir.getExprData(expr, .field);
            const inst = try bc.selectForDstInst(c, cstr, bt.Integer, false, node);
            const childv = try genAddressOf2(c, data.rec, Cstr.simple, node);
            try initTempValue(c, childv, node);

            const fields = c.sema.getTypeSym(data.parent_t).cast(.struct_t).getFields();
            try pushAddrConstIndex(c, childv.reg, @intCast(fields[data.idx].offset), inst.dst, node);
            try popTempValue(c, childv, node);
            if (inst.own_dst) {
                try initSlot(c, inst.dst, false, node);
            }
            return finishDstInst(c, inst, false);
        },
        .deref => {
            // Given: `a.*.b`
            // Ideally, `a.*` should deref and create a new temporary value but doing so will also cause it
            // to be freed before the expression completes (due to release ops after each expression).
            // So the copy is ignored and deref mimics C's deref.
            const deref = c.ir.getExprData(expr, .deref);
            return genExpr(c, deref.expr, cstr);
        },
        .call_sym => {
            const ret_t = c.ir.getExprType(expr).id;
            if (c.sema.isPointerType(ret_t)) {
                return genCallFuncSym(c, expr, cstr, node);
            }
            return error.TODO;
        },
        else => {
            log.tracev("{}", .{code});
            return error.TODO;
        },
    }
}

fn genDeref(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .deref);

    const ret_t = c.ir.getExprType(idx).id;
    const ret_te = c.sema.getType(ret_t);
    const ret_is_struct = ret_te.kind == .struct_t;
    const retain = ret_is_struct or c.sema.isRcCandidateType(ret_t);

    const inst = try bc.selectForDstInst(c, cstr, ret_t, ret_is_struct, node);
    const srcv = try genExpr(c, data.expr, Cstr.simple);
    try initTempValue(c, srcv, node);

    if (ret_is_struct) {
        const nfields: u8 = @intCast(c.sema.types.items[ret_t].data.struct_t.nfields);
        const start = c.buf.ops.items.len;
        try c.pushCode(.deref_struct, &.{ srcv.reg, 0, 0, nfields, inst.dst }, node);
        c.buf.setOpArgU16(start + 2, @intCast(ret_t));
    } else {
        try c.pushCode(.deref, &.{ srcv.reg, @intFromBool(retain), inst.dst }, node);
    }
    try popTempValue(c, srcv, node);
    if (inst.own_dst) {
        try initSlot(c, inst.dst, retain, node);
    }
    return finishDstInst(c, inst, retain);
}

fn genField(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .field);
    const ret_t = c.ir.getExprType(idx).id;

    const inst = try bc.selectForDstInst(c, cstr, ret_t, true, node);

    const rec_t = c.ir.getExprType(data.rec).id;
    const rec_te = c.sema.getType(rec_t);
    const rec_is_pointer = c.sema.isPointerType(rec_t);
    const willRetain = c.sema.isRcCandidateType(ret_t);
    if (rec_te.kind == .struct_t or rec_is_pointer) {
        const addrv = try genAddressOf2(c, idx, Cstr.simple, node);
        try initTempValue(c, addrv, node);

        const ret_te = c.sema.getType(ret_t);
        if (ret_te.kind == .struct_t) {
            const numFields: u8 = @intCast(c.sema.types.items[ret_t].data.struct_t.nfields);
            const start = c.buf.ops.items.len;
            try c.pushCode(.deref_struct, &.{ addrv.reg, 0, 0, numFields, inst.dst }, node);
            c.buf.setOpArgU16(start + 2, @intCast(ret_t));
        } else {
            try c.pushCode(.deref, &.{ addrv.reg, @intFromBool(willRetain), inst.dst }, node);
        }
        try popTempValue(c, addrv, node);
    } else {
        const recv = try genExpr(c, data.rec, Cstr.simple);
        try initTempValue(c, recv, node);

        const retain = !c.sema.isUnboxedType(ret_t);
        try pushField(c, recv.reg, data.idx, retain, inst.dst, node);
        try popTempValue(c, recv, node);
    }

    if (inst.own_dst) {
        try initSlot(c, inst.dst, willRetain, node);
    }

    return finishDstInst(c, inst, willRetain);
}

fn genStructInit(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .object_init);
    const ret_t = c.ir.getExprType(idx).id;

    const inst = try bc.selectForDstInst(c, cstr, ret_t, true, node);

    const args = c.ir.getArray(data.args, u32, data.numArgs);
    const argStart = numSlots(c);
    for (args) |argIdx| {
        const arg_t = c.ir.getExprType(argIdx).id;
        const temp = try bc.reserveTemp(c, arg_t);
        const argv = try genExpr(c, argIdx, Cstr.toTempRetain(temp));
        try initSlot(c, temp, argv.retained, node);
    }

    const ret_te = c.sema.types.items[ret_t];
    const ret_ts = ret_te.sym.cast(.struct_t);
    const nfields = ret_te.data.struct_t.nfields;
    try pushStructInit(c, data.typeId, @intCast(argStart), @intCast(nfields), ret_ts.getFields(), inst.dst, node);
    for (0..args.len) |i| {
        const slot_id = argStart + args.len - i - 1;
        const slot = getSlot(c, slot_id);
        if (slot.boxed_retains) {
            try consumeTemp(c, @intCast(slot_id), node);
        }
    }
    try popTemps(c, args.len, node);
    if (inst.own_dst) {
        try initSlot(c, inst.dst, true, node);
    }

    return finishDstInst(c, inst, true);
}

fn genObjectInit(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const ret_t = c.ir.getExprType(idx).id;
    const ret_te = c.sema.getType(ret_t);
    if (ret_te.kind == .struct_t) {
        return genStructInit(c, idx, cstr, node);
    }

    const data = c.ir.getExprData(idx, .object_init);

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
    for (0..args.len) |i| {
        const slot_id = argStart + args.len - i - 1;
        const slot = getSlot(c, slot_id);
        if (slot.boxed_retains) {
            try consumeTemp(c, @intCast(slot_id), node);
        }
    }
    try popTemps(c, args.len, node);
    if (inst.own_dst) {
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
            try genReleaseSlots(c, c.curBlock.slot_start, b.slot_off, node);
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
            try genReleaseSlots(c, c.curBlock.slot_start, b.slot_off, node);
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
    if (childv.type == .temp) {
        try initSlot(c, childv.reg, childv.retained, node);
    }

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

    const field_id = try c.compiler.vm.ensureField(data.name);
    const ownRecv = opts.recv == null;

    // LHS
    var recv: GenValue = undefined;
    if (opts.recv) |recv_| {
        recv = recv_;
    } else {
        recv = try genExpr(c, data.rec, Cstr.simple);
        try initTempValue(c, recv, node);
    }

    // RHS
    var rightv: GenValue = undefined;
    if (opts.right) |right| {
        rightv = right;
    } else {
        rightv = try genExpr(c, data.right, Cstr.simpleRetain);
        try initTempValue(c, rightv, node);
    }

    // Performs runtime type check.
    const pc = c.buf.ops.items.len;
    try c.pushFCode(.setFieldDyn, &.{ recv.reg, 0, 0, rightv.reg, 0, 0, 0, 0, 0 }, node);
    c.buf.setOpArgU16(pc + 2, @intCast(field_id));

    try consumeTempValue(c, rightv, node);
    try popTempValue(c, rightv, node);
    if (ownRecv) {
        try popTempValue(c, recv, node);
    }
}

fn genSetDeref(c: *Chunk, idx: usize, node: *ast.Node) !void {
    const data = c.ir.getStmtData(idx, .set_deref);

    const ptrv = try genExpr(c, data.ptr, Cstr.simple);
    try initTempValue(c, ptrv, node);

    const rightv = try genExpr(c, data.right, Cstr.simple);
    try initTempValue(c, rightv, node);

    const right_t = c.ir.getExprType(data.right).id;
    const right_te = c.sema.types.items[right_t];
    if (right_te.kind == .struct_t) {
        try c.pushCode(.set_deref_struct, &.{ ptrv.reg, @intCast(right_te.data.struct_t.nfields), rightv.reg }, node);
    } else {
        try c.pushCode(.set_deref, &.{ ptrv.reg, rightv.reg }, node);
    }
    try consumeTempValue(c, rightv, node);
    try popTempValue(c, rightv, node);
    try popTempValue(c, ptrv, node);
}

fn setField(c: *Chunk, idx: usize, node: *ast.Node) !void {
    const data = c.ir.getStmtData(idx, .set_field).set_field;
    const fieldData = c.ir.getExprData(data.field, .field);

    const type_id = c.ir.getExprType(fieldData.rec).id;
    const type_e = c.sema.getType(type_id);
    // const rec_is_pointer = c.sema.isPointerType(type_id);

    // Receiver.
    if (type_e.kind == .struct_t) {
        const addrv = try genAddressOf2(c, data.field, Cstr.simple, node);
        try initTempValue(c, addrv, node);

        const rightv = try genExpr(c, data.right, Cstr.simpleRetain);
        try initTempValue(c, rightv, node);

        const right_te = c.sema.getType(c.ir.getExprType(data.right).id);
        if (right_te.kind == .struct_t) {
            try c.pushCode(.set_deref_struct, &.{ addrv.reg, @intCast(right_te.data.struct_t.nfields), rightv.reg }, node);
        } else {
            try c.pushCode(.set_deref, &.{ addrv.reg, rightv.reg }, node);
        }

        try consumeTempValue(c, rightv, node);
        try popTempValue(c, rightv, node);
        try popTempValue(c, addrv, node);
    } else {
        const recv = try genExpr(c, fieldData.rec, Cstr.simple);
        try initTempValue(c, recv, node);

        const rightv = try genExpr(c, data.right, Cstr.simpleRetain);
        try initTempValue(c, rightv, node);

        try c.pushCode(.setField, &.{ recv.reg, fieldData.idx, rightv.reg }, node);

        try consumeTempValue(c, rightv, node);
        try popTempValue(c, rightv, node);
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
    try genConst(c, constIdx, inst.dst, false, node);
    if (inst.own_dst) {
        try initSlot(c, inst.dst, false, node);
    }

    return finishNoErrNoDepInst(c, inst, false);
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
    if (inst.own_dst) {
        try initSlot(c, inst.dst, false, node);
    }
    return finishNoErrNoDepInst(c, inst, false);
}

fn genTypeCheck(c: *Chunk, loc: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(loc, .type_check);

    const inst = try selectForDstInst(c, cstr, data.exp_type, false, node);

    const exprv = try genExpr(c, data.expr, Cstr.simple);
    try initTempValue(c, exprv, node);

    try pushTypeCheck(c, exprv.reg, data.exp_type, inst.dst, node);
    try consumeTempValue(c, exprv, node);
    try popTempValue(c, exprv, node);
    if (inst.own_dst) {
        try initSlot(c, inst.dst, exprv.retained, node);
    }
    return finishDstInst(c, inst, exprv.retained);
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
    try initTempValue(c, choice, node);

    try c.pushFCode(.unwrapChoice, &.{ choice.reg, data.tag, data.fieldIdx, inst.dst }, node);

    try popTempValue(c, choice, node);
    if (inst.own_dst) {
        try initSlot(c, inst.dst, retain, node);
    }
    return finishDstInst(c, inst, retain);
}

fn genTrue(c: *Chunk, cstr: Cstr, node: *ast.Node) !GenValue {
    const inst = try bc.selectForNoErrNoDepInst(c, cstr, bt.Boolean, false, node);
    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, node);
    }

    try c.buf.pushOp1(.true, inst.dst);
    if (inst.own_dst) {
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
    if (inst.own_dst) {
        try initSlot(c, inst.dst, false, node);
    }

    return finishNoErrNoDepInst(c, inst, false);
}

fn genByte(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .byte);

    const inst = try bc.selectForNoErrNoDepInst(c, cstr, bt.Byte, false, node);
    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, node);
    }
    try c.pushCode(.const_byte, &.{ data.val, inst.dst }, node);
    if (inst.own_dst) {
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
    _ = try genConstIntExt(c, data.val, inst.dst, node);
    if (inst.own_dst) {
        try initSlot(c, inst.dst, false, node);
    }
    return finishNoErrNoDepInst(c, inst, false);
}

fn genFloat(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .float);
    const inst = try bc.selectForNoErrNoDepInst(c, cstr, bt.Float, false, node);
    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, node);
    }
    _ = try genConstFloat(c, data.val, inst.dst, node);
    if (inst.own_dst) {
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
    if (inst.own_dst) {
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
        const argv = try genExpr(c, argIdx, Cstr.toTemp(temp));
        try initSlot(c, temp, argv.retained, node);
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
    if (inst.own_dst) {
        try initSlot(c, inst.dst, true, node);
    }

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

    if (inst.own_dst) {
        try initSlot(c, inst.dst, true, node);
    }
    return finishNoErrNoDepInst(c, inst, true);
}

fn pushStringConst(c: *Chunk, str: []const u8, dst: SlotId, node: *ast.Node) !void {
    const idx = try c.buf.getOrPushStaticStringConst(str);
    try genConst(c, idx, dst, true, node);
}

fn genUnOp(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .preUnOp).unOp;
    const ret_t = c.ir.getExprType(idx).id;
    const inst = try bc.selectForDstInst(c, cstr, ret_t, false, node);

    const childv = try genExpr(c, data.expr, Cstr.simple);
    try initTempValue(c, childv, node);

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

    if (inst.own_dst) {
        try initSlot(c, inst.dst, false, node);
    }

    return finishDstInst(c, inst, false);
}

fn genCallSymDyn(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .call_sym_dyn);

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
    if (inst.own_ret) {
        try initSlot(c, inst.ret, true, node);
    }

    return endCall(c, inst, true);
}

fn genCallObjSym(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .call_obj_sym);

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
    if (inst.own_ret) {
        try initSlot(c, inst.ret, true, node);
    }

    return endCall(c, inst, true);
}

fn genCallTrait(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .call_trait);

    const ret_t = c.ir.getExprType(idx).id;
    const inst = try beginCall(c, cstr, ret_t, true, node);

    // Trait ref.
    const argStart = numSlots(c);
    var temp = try bc.reserveTemp(c, bt.Any);
    const trait = try genExpr(c, data.trait, Cstr.toTemp(temp));
    try initTempValue(c, trait, node);

    // Reserve slot for unwrapped impl.
    const placeholder = try bc.reserveTemp(c, bt.Any);
    try initSlot(c, placeholder, false, node);

    // Args. Skip impl placeholder.
    const args = c.ir.getArray(data.args, u32, data.nargs)[1..];
    for (args, 2..) |argIdx, i| {
        const arg_t = c.ir.getExprType(argIdx).id;
        temp = try bc.reserveTemp(c, arg_t);
        if (cy.Trace and temp != argStart + i) return error.Unexpected;
        const argv = try genExpr(c, argIdx, Cstr.toTemp(temp));
        try initSlot(c, temp, argv.retained, node);
    }

    const start = c.buf.ops.items.len;
    try c.pushFCode(.call_trait, &.{inst.ret, data.nargs, 1, 0, 0, 0, 0, 0, 0, 0, 0 }, node);
    c.buf.setOpArgU16(start + 4, @intCast(data.vtable_idx));

    try popTemps(c, args.len + 2, node);
    if (inst.own_ret) {
        try initSlot(c, inst.ret, !c.sema.isUnboxedType(ret_t), node);
    }

    return endCall(c, inst, true);
}

fn genCallFuncSym(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .call_sym);

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

    const rt_id = try reserveFunc(c.compiler, data.func);
    try pushCallSym(c, inst.ret, data.numArgs, 1, rt_id, node);

    try popTemps(c, args.len, node);
    if (inst.own_ret) {
        try initSlot(c, inst.ret, !c.sema.isUnboxedType(ret_t), node);
    }

    const retRetained = c.sema.isRcCandidateType(data.func.retType);
    return endCall(c, inst, retRetained);
}

fn genCallDyn(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .call_dyn);
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
    if (inst.own_ret) {
        try initSlot(c, inst.ret, true, node);
    }

    return endCall(c, inst, true);
}

fn genBinOp(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
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
    const leftv = try genExpr(c, data.left, Cstr.simple);
    try initTempValue(c, leftv, node);

    // Rhs.
    const rightv = try genExpr(c, data.right, Cstr.simple);
    try initTempValue(c, rightv, node);

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

    const tempLeft = leftv.isTemp();
    if (tempLeft and leftv.reg != inst.dst) {
        try popTemp(c, leftv.reg, node);
    }

    if (inst.own_dst) {
        try initSlot(c, inst.dst, retained, node);
    }

    return finishDstInst(c, inst, retained);
}

fn genTrait(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .trait);

    const inst = try bc.selectForDstInst(c, cstr, bt.Any, true, node);
    const exprv = try genExpr(c, data.expr, Cstr.simpleRetain);
    try initTempValue(c, exprv, node);

    const key = VtableKey{ .type = data.expr_t, .trait = data.trait_t };
    const vtable_idx = c.compiler.gen_vtables.get(key).?;

    const pc = c.buf.len();
    try c.pushFCode(.trait, &.{ exprv.reg, 0, 0, 0, 0, inst.dst }, node);
    c.buf.setOpArgU16(pc + 2, @intCast(data.trait_t));
    c.buf.setOpArgU16(pc + 4, @intCast(vtable_idx));

    try consumeTempValue(c, exprv, node);
    try popTempValue(c, exprv, node);
    if (inst.own_dst) {
        try initSlot(c, inst.dst, true, node);
    }
    return finishDstInst(c, inst, true);
}

fn genUnbox(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .unbox);
    const ret_t = c.ir.getExprType(idx).id;
    if (c.sema.isUnboxedType(ret_t)) {
        const inst = try bc.selectForDstInst(c, cstr, ret_t, false, node);
        const childv = try genExpr(c, data.expr, Cstr.simple);
        try initTempValue(c, childv, node);

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
    const expr_t = c.ir.getExprType(data.expr);
    if (c.sema.isUnboxedType(expr_t.id)) {
        const inst = try bc.selectForDstInst(c, cstr, bt.Any, true, node);
        const childv = try genExpr(c, data.expr, Cstr.simple);
        const pc = c.buf.len();
        try c.pushFCode(.box, &.{ childv.reg, 0, 0, inst.dst }, node);
        c.buf.setOpArgU16(pc + 2, @intCast(expr_t.id));
        try popTempValue(c, childv, node);
        if (inst.own_dst) {
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
    const retain = !c.sema.isUnboxedType(value_t);
    const inst = try bc.selectForNoErrNoDepInst(c, cstr, value_t, retain, node);
    if (inst.requiresPreRelease) {
        try pushRelease(c, inst.dst, node);
    }

    try c.pushCode(.captured, &.{ c.curBlock.closureLocal, data.idx, @intFromBool(retain), inst.dst }, node);
    if (inst.own_dst) {
        try initSlot(c, inst.dst, retain, node);
    }

    return finishNoErrNoDepInst(c, inst, retain);
}

fn initTempValue(c: *Chunk, val: GenValue, node: *ast.Node) !void {
    if (val.type == .temp) {
        try initSlot(c, val.reg, val.retained, node);
    }
}

/// Registers a slot for ARC tracking.
pub fn initSlot(c: *Chunk, slot_id: SlotId, retained_val: bool, node: *ast.Node) !void {
    // log.tracev("initSlot {}", .{slot_id});
    const slot = getSlotPtr(c, slot_id);
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
            slot.boxed_retains = true;
            try pushUnwindSlot(c, slot_id);
        }
    } else {
        slot.boxed_retains = true;
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

fn genValueLocal(c: *Chunk, reg: SlotId, cstr: Cstr, node: *ast.Node) !GenValue {
    if (!cstr.isExact()) {
        // Prefer no copy.
        const retain = cstr.type == .simple and cstr.data.simple.retain;
        if (retain) {
            try c.pushCode(.retain, &.{ reg }, node);
        }
        return regValue(c, reg, retain);
    }
    const inst = try selectForDstInst(c, cstr, bt.Any, true, node);
    try c.pushCode(.copy_struct, &.{ reg, inst.dst }, node);
    if (inst.own_dst) {
        try initSlot(c, inst.dst, true, node);
    }
    return finishDstInst(c, inst, true);
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
    // try c.pushCode(.copy_struct, &.{ inst.dst, numFields, inst.dst }, node);
    // return finishDstInst(c, inst, true);
    return error.TODO;
}

fn genLocalReg(c: *Chunk, reg: SlotId, slot_t: cy.TypeId, cstr: Cstr, node: *ast.Node) !GenValue {
    const slot = getSlot(c, reg);

    if (!slot.boxed_up) {
        const type_e = c.sema.getType(slot_t);
        if (type_e.kind == .struct_t) {
            return genValueLocal(c, reg, cstr, node);
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
        if (type_e.kind == .struct_t) {
            return genLiftedValueLocal(c, reg, slot, cstr, node);
        }

        // Special case when src local is an UpValue.

        var retain_src = false;
        const boxed_child = !c.sema.isUnboxedType(slot_t);
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
    try genConst(c, constIdx, inst.dst, false, node);

    if (inst.own_dst) {
        try initSlot(c, inst.dst, false, node);
    }

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

    if (inst.own_dst) {
        try initSlot(c, inst.dst, true, node);
    }

    return finishNoErrNoDepInst(c, inst, true);
}

fn genFuncPtr(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .func_ptr);

    const inst = try bc.selectForDstInst(c, cstr, bt.Any, true, node);

    const pc = c.buf.len();
    try c.pushCode(.func_ptr, &.{ 0, 0, 0, 0, inst.dst }, node);
    const rtId = c.compiler.genSymMap.get(data.func).?.func.id;
    c.buf.setOpArgU16(pc + 1, @intCast(rtId));
    c.buf.setOpArgU16(pc + 3, @intCast(c.ir.getExprType(idx).id));
    if (inst.own_dst) {
        try initSlot(c, inst.dst, true, node);
    }
    return finishDstInst(c, inst, true);
}

fn genFuncUnion(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .func_union);

    const inst = try bc.selectForDstInst(c, cstr, bt.Any, true, node);
    const childv = try genExpr(c, data.expr, Cstr.simple);
    try initTempValue(c, childv, node);

    const pc = c.buf.len();
    try c.pushCode(.func_union, &.{ childv.reg, 0, 0, inst.dst }, node);
    c.buf.setOpArgU16(pc + 2, @intCast(c.ir.getExprType(idx).id));

    try consumeTempValue(c, childv, node);
    try popTempValue(c, childv, node);
    if (inst.own_dst) {
        try initSlot(c, inst.dst, true, node);
    }
    return finishDstInst(c, inst, true);
}

fn genType(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .type);

    const inst = try bc.selectForDstInst(c, cstr, if (data.expr_type) bt.ExprType else bt.Type, true, node);
    const pc = c.buf.len();

    try c.pushCode(.type, &.{0, 0, 0, 0, @intFromBool(data.expr_type), inst.dst}, node);
    c.buf.setOpArgU32(pc + 1, data.typeId);
    if (inst.own_dst) {
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

            const boxed = !c.sema.isUnboxedType(param.declType) or param.lifted;
            c.slot_stack.items[c.curBlock.slot_start + slot] = .{
                .type = .local,
                .boxed = boxed,
                .owned = true,
                .boxed_up = param.lifted,
                .boxed_init = false,
                .boxed_retains = false,
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

            const boxed = !c.sema.isUnboxedType(param.declType);
            c.slot_stack.items[c.curBlock.slot_start + 4 + 1 + i] = .{
                .type = .local,
                .boxed = boxed,
                .owned = false,
                .boxed_up = false,
                .boxed_init = false,
                .boxed_retains = false,
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

    const cstr = Cstr.toLocal(slot, false);
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

fn declareLocal(c: *Chunk, idx: u32, node: *ast.Node) !SlotId {
    const data = c.ir.getStmtData(idx, .declareLocal);
    const slot = try reserveLocal(c, data.declType, data.lifted, node);
    c.genIrLocalMapStack.items[c.curBlock.irLocalMapStart + data.id] = slot;
    c.genBlock().num_locals += 1;

    const local = getSlot(c, slot);
    try initSlot(c, slot, local.boxed, node);
    return slot;
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
    const data = c.ir.getStmtData(idx, .setLocal).local;
    try setLocal(c, data, node, .{ .check_type = null });
}

const SetLocalOptions = struct {
    rightv: ?GenValue = null,
    extraIdx: ?u32 = null,
    check_type: ?cy.TypeId = null,
};

fn setLocal(c: *Chunk, data: ir.SetLocal, node: *ast.Node, opts: SetLocalOptions) !void {
    const reg = toLocalReg(c, data.id);
    const local = getSlot(c, reg);

    var dst: Cstr = undefined;
    if (local.boxed_up) {
        const right_t = c.ir.getExprType(data.right).id;
        const boxed_child = !c.sema.isUnboxedType(right_t);
        dst = Cstr.toLiftedLocal(reg, boxed_child);
    } else {
        dst = Cstr.toLocal(reg, local.boxed);
    }

    var rightv: GenValue = undefined;
    if (opts.rightv) |rightv_| {
        rightv = rightv_;
        if (rightv.reg != reg) {
            // Move to local.
            _ = try genToExactDesc(c, rightv, dst, node, opts.extraIdx);
        }
    } else {
        rightv = try genExpr(c, data.right, dst);
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

/// `or` and `and` are not simplified to an opcode because we want to prevent
/// the execution of the right expr if the left expr is enough to return the result.
fn orOp(c: *Chunk, data: ir.BinOp, cstr: Cstr, node: *ast.Node) !GenValue {
    const merged_cstr = try toMergedDst(c, cstr, bt.Boolean);
    const cond = try reserveTemp(c, bt.Boolean);
    try initSlot(c, cond, false, node);

    const leftv = try genExpr(c, data.left, Cstr.toTemp(cond));
    const jump_false = try c.pushEmptyJumpNotCond(cond);
    _ = try genToExact(c, leftv, merged_cstr, node);
    try popTemp(c, cond, node);
    const jump_end = try c.pushEmptyJump();

    c.patchJumpCondToCurPc(jump_false);
    _ = try genExpr(c, data.right, merged_cstr);
    c.patchJumpToCurPc(jump_end);

    return merged_cstr.toMergedValue(c, bt.Boolean);
}

fn andOp(c: *Chunk, data: ir.BinOp, cstr: Cstr, node: *ast.Node) !GenValue {
    const merged_cstr = try toMergedDst(c, cstr, bt.Boolean);
    const cond = try reserveTemp(c, bt.Boolean);
    try initSlot(c, cond, false, node);

    const leftv = try genExpr(c, data.left, Cstr.toTemp(cond));
    const jump_true = try c.pushEmptyJumpCond(cond);
    _ = try genToExact(c, leftv, merged_cstr, node);
    try popTemp(c, cond, node);
    const jump_end = try c.pushEmptyJump();

    c.patchJumpCondToCurPc(jump_true);
    _ = try genExpr(c, data.right, merged_cstr);
    c.patchJumpToCurPc(jump_end);

    return merged_cstr.toMergedValue(c, bt.Boolean);
}

fn genRetStmt(c: *Chunk) !void {
    if (c.curBlock.type == .main) {
        try genReleaseBlock(c);
        try c.buf.pushOp1(.end, 255);
    } else {
        try genReleaseBlock(c);
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
        try initTempValue(c, childv, node);
        try consumeTempValue(c, childv, node);
    } else {
        childv = try genExpr(c, data.expr, Cstr.ret);
    }
    try popTempValue(c, childv, node);

    try genReleaseBlock(c);
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

    boxed_retains: bool,
};

pub fn numSlots(c: *Chunk) usize {
    return c.slot_stack.items.len - c.curBlock.slot_start;
}

fn genReleaseBlock(c: *Chunk) !void {
    const block = c.curBlock;
    try genReleaseSlots(c, c.curBlock.slot_start, block.startLocalReg, block.debugNode);
}

/// Only the locals that are alive at this moment are considered for release.
fn genReleaseSlots(c: *Chunk, slot_base: usize, slot_off: usize, debugNode: *ast.Node) !void {
    const start = c.operandStack.items.len;
    defer c.operandStack.items.len = start;

    const slots = c.slot_stack.items[slot_base + slot_off..];
    log.tracev("Generate release for slots[{}..{}]", .{slot_off, slot_off + slots.len});
    for (slots, slot_off..) |slot, i| {
        if (slot.boxed and slot.boxed_init and slot.boxed_retains) {
            try c.operandStack.append(c.alloc, @intCast(i));
        }
    }
    
    const regs = c.operandStack.items[start..];
    if (regs.len > 0) {
        try pushReleases(c, regs, debugNode);
    }
}

fn genFuncEnd(c: *Chunk) !void {
    try genReleaseBlock(c);
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
    own_ret: bool,
    numPreludeTemps: u8,
    cstr: Cstr,
    has_final_dst: bool,
    node: *ast.Node,
};

/// Returns gen strategy and advances the temp local.
pub fn beginCall(c: *Chunk, cstr: Cstr, ret_t: cy.TypeId, hasCalleeValue: bool, node: *ast.Node) !CallInst {
    var ret: SlotId = undefined;
    var own_ret: bool = undefined;
    var allocTempRet = true;

    // Optimization: Check to use dst cstr as ret.
    if (cstr.type == .tempReg or cstr.type == .localReg) {
        if (!cstr.data.slot.releaseDst and cstr.data.slot.dst + 1 == numSlots(c)) {
            if (cstr.data.slot.dst != 0) {
                ret = cstr.data.slot.dst;
                own_ret = false;
                allocTempRet = false;
            }
        }
    }

    if (allocTempRet) {
        ret = try bc.reserveTemp(c, ret_t);
        own_ret = cstr.type != .simple;
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
        .captured,
        .varSym,
        .liftedLocal => {
            has_final_dst = true;
        },
        else => {},
    }
    return .{
        .ret = ret,
        .own_ret = own_ret,
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
            const data = dst.data.slot;

            if (src.reg != data.dst) {
                const retain = shouldRetain(c, src.reg, data.dst, dst.data.slot.retain);
                if (data.releaseDst) {
                    if (retain) {
                        try c.pushCodeExt(.copyRetainRelease, &.{ src.reg, data.dst }, node, extraIdx);
                    } else {
                        try c.pushCodeExt(.copyReleaseDst, &.{ src.reg, data.dst }, node, extraIdx);
                    }
                } else {
                    if (retain) {
                        try c.pushCodeExt(.copyRetainSrc, &.{ src.reg, data.dst }, node, extraIdx);
                    } else {
                        try c.pushCodeExt(.copy, &.{ src.reg, data.dst }, node, extraIdx);
                    }
                }

                if (!retain) {
                    // Ownership was moved from temp.
                    try consumeTempValue(c, src, node);
                }
                return regValue(c, data.dst, retain or src.retained);
            } else {
                const src_s = getSlot(c, src.reg);
                const retain = src_s.boxed and dst.data.slot.retain;
                if (retain) {
                    try c.pushCode(.retain, &.{ src.reg }, node);
                } else {
                    // Nop.
                }
                return regValue(c, data.dst, retain or src.retained);
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

            // Ownership was moved from temp.
            try consumeTempValue(c, src, node);

            return GenValue.initRetained(src.retained);
        },
        .captured => {
            const src_s = getSlot(c, src.reg);
            if (src_s.boxed and src_s.type == .local) {
                try c.pushCode(.retain, &.{ src.reg }, node);
            }

            const captured = dst.data.captured;
            try c.pushCodeExt(.setCaptured, &.{ c.curBlock.closureLocal, captured.idx, src.reg }, node, extraIdx);
            
            // Ownership was moved from temp.
            try consumeTempValue(c, src, node);

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
    try genReleaseSlots(c, c.curBlock.slot_start, b.slot_off, b.node);

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

fn genTryStmt(c: *cy.Chunk, idx: usize, node: *ast.Node) !void {
    const data = c.ir.getStmtData(idx, .tryStmt);

    try pushUnwindTry(c);

    try pushBlock(c, false, node);
    try genStmts(c, data.bodyHead);
    try popBlock(c);

    const catch_pc = c.buf.ops.items.len;
    try popUnwindTry(c, catch_pc, node);
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

    const dst_cstr = try toMergedDst(c, cstr, type_id);

    // Body expr.
    try pushUnwindTry(c);
    const childv = try genExpr(c, data.expr, Cstr.simple);
    try initTempValue(c, childv, node);
    const retv = try genToExact(c, childv, dst_cstr, node);
    try popTempValue(c, childv, node);
    const catch_pc = c.buf.ops.items.len;
    try popUnwindTry(c, catch_pc, node);

    try c.pushCode(.catch_op, &.{ 0, 0, 0, 0 }, node);

    var retained = childv.retained;
    if (data.catchBody != cy.NullId) {
        // Error is not copied anywhere.
        c.buf.setOpArgs1(catch_pc + 3, cy.NullU8);

        // Catch expr.
        const catchv = try genExpr(c, data.catchBody, dst_cstr);
        if (catchv.retained) {
            retained = true;
        }
    } else {
        const inst = try bc.selectForDstInst(c, dst_cstr, type_id, false, node);
        c.buf.setOpArgs1(catch_pc + 3, inst.dst);
        c.buf.setOpArgs1(catch_pc + 4, @intFromBool(false));
        if (inst.own_dst) {
            try initSlot(c, inst.dst, false, node);
        }
        _ = try finishDstInst(c, inst, false);
    }
    c.buf.setOpArgU16(catch_pc + 1, @intCast(c.buf.ops.items.len - catch_pc));

    return retv;
}

fn genUnwrapOr(c: *Chunk, loc: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(loc, .unwrap_or);
    const ret_t = c.ir.getExprType(loc).id;

    const merged_cstr = try toMergedDst(c, cstr, ret_t);

    const optv = try genExpr(c, data.opt, Cstr.simple);
    try initTempValue(c, optv, node);

    const cond = try bc.reserveTemp(c, bt.Boolean);
    try c.pushCode(.none, &.{optv.reg, cond}, node);
    try initSlot(c, cond, false, node);
    const jump_cond = try c.pushEmptyJumpCond(cond);

    const retain_some = true;
    const inst = try bc.selectForDstInst(c, merged_cstr, ret_t, retain_some, node);
    const retain = !c.sema.isUnboxedType(ret_t);
    try pushField(c, optv.reg, 1, retain, inst.dst, node);
    if (inst.own_dst) {
        try initSlot(c, inst.dst, retain, node);
    }
    const somev = finishDstInst(c, inst, retain_some);
    const jump_end = try c.pushEmptyJump();

    // else.
    c.patchJumpCondToCurPc(jump_cond);
    _ = try genExpr(c, data.default, merged_cstr);

    c.patchJumpToCurPc(jump_end);

    try popTemp(c, cond, node);
    try popTempValue(c, optv, node);

    return somev;
}

fn genIfExpr(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .if_expr);
    const condNodeId = c.ir.getNode(data.cond);
    const type_id = c.ir.getExprType(idx).id;

    const merged_cstr = try toMergedDst(c, cstr, type_id);

    // Cond.
    const condv = try genExpr(c, data.cond, Cstr.simple);
    try initTempValue(c, condv, node);

    const condFalseJump = try c.pushEmptyJumpNotCond(condv.reg);

    // If body.
    const bodyv = try genExpr(c, data.body, merged_cstr);
    var retained = bodyv.retained;
    const bodyEndJump = try c.pushEmptyJump();

    // Else body.
    // Don't need to free cond since it evaluated to false.
    c.patchJumpNotCondToCurPc(condFalseJump);
    const elsev = try genExpr(c, data.elseBody, merged_cstr);
    retained = retained or elsev.retained;

    // End.
    c.patchJumpToCurPc(bodyEndJump);

    try popTempValue(c, condv, condNodeId);

    return GenValue.initRetained(retained);
}

fn genIfStmt(c: *cy.Chunk, idx: usize, node: *ast.Node) !void {
    const data = c.ir.getStmtData(idx, .ifStmt);

    const condNodeId = c.ir.getNode(data.cond);
    const condv = try genExpr(c, data.cond, Cstr.simple);
    try initTempValue(c, condv, node);

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
            try initTempValue(c, condv, condNodeId);

            const jump_miss = try c.pushEmptyJumpNotCond(condv.reg);

            try pushBlock(c, false, else_nid);
            try genStmts(c, else_b.body_head);
            try popBlock(c);

            if (else_b.else_block != cy.NullId) {
                const jump_end = try c.pushEmptyJump();
                try c.listDataStack.append(c.alloc, .{ .pc = jump_end });
            }

            c.patchJumpNotCondToCurPc(jump_miss);

            try popTempValue(c, condv, condNodeId);
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
    const retain = !c.sema.isUnboxedType(ret_t);
    const inst = try bc.selectForDstInst(c, cstr, ret_t, retain, node);

    try pushBlock(c, false, node);
    const b = c.genBlock();
    b.blockExprCstr = Cstr.toTempRetain(inst.dst);

    try genStmts(c, data.bodyHead);
    try popBlock(c);

    if (inst.own_dst) {
        try initSlot(c, inst.dst, retain, node);
    }

    return finishDstInst(c, inst, true);
}

fn genSwitchStmt(c: *Chunk, idx: usize, node: *ast.Node) !void {
    const switchLoc = c.ir.advanceStmt(idx, .switchStmt);
    _ = try genSwitch(c, switchLoc, null, node);
}

fn genSwitch(c: *Chunk, idx: usize, cstr: ?Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .switchExpr);

    const ret_t = c.ir.getExprType(idx).id;
    var merged_cstr: Cstr = undefined;
    var childBreakJumpsStart: u32 = undefined;
    if (!data.is_expr) {
        childBreakJumpsStart = @intCast(c.blockJumpStack.items.len);
    } else {
        merged_cstr = try toMergedDst(c, cstr.?, ret_t);
    }

    const casesIdx = c.ir.advanceExpr(idx, .switchExpr);
    const cases = c.ir.getArray(casesIdx, u32, data.numCases);

    // Expr.
    const exprv = try genExpr(c, data.expr, Cstr.simple);
    try initTempValue(c, exprv, node);

    const caseBodyEndJumpsStart = c.listDataStack.items.len;

    var prevCaseMissJump: u32 = cy.NullId;
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
                const cond_n = c.ir.getNode(condIdx);

                const temp = try bc.reserveTemp(c, bt.Boolean);
                const condv = try genExpr(c, condIdx, Cstr.simple);
                try initTempValue(c, condv, cond_n);

                try c.pushCode(.compare, &.{exprv.reg, condv.reg, temp}, cond_n);
                try popTempValue(c, condv, cond_n);
                try initSlot(c, temp, false, cond_n);

                const condMissJump = try c.pushEmptyJumpNotCond(temp);

                const condMatchJump = try c.pushEmptyJump();
                try c.listDataStack.append(c.alloc, .{ .pc = condMatchJump });
                c.patchJumpNotCondToCurPc(condMissJump);
                // Miss continues to next cond.

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
            prevCaseMissJump = cy.NullId;
        }

        if (case.bodyIsExpr) {
            _ = try genExpr(c, case.bodyHead, merged_cstr);
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

    // Jump here from case body ends.
    for (c.listDataStack.items[caseBodyEndJumpsStart..]) |pc| {
        c.patchJumpToCurPc(pc.jumpToEndPc);
    }
    c.listDataStack.items.len = caseBodyEndJumpsStart;

    // Jump here from nested child breaks.
    if (!data.is_expr) {
        const newLen = c.patchBreaks(childBreakJumpsStart, c.buf.ops.items.len);
        c.blockJumpStack.items.len = newLen;
    }

    // Unwind switch expr.
    try popTempValue(c, exprv, node);

    // Complete with no value since assign statement doesn't do anything with it.
    if (data.is_expr) {
        return merged_cstr.toMergedValue(c, ret_t);
    } else {
        return GenValue.initNoValue();
    }
}

fn genMap(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    _ = idx;
    const inst = try bc.selectForDstInst(c, cstr, bt.Map, true, node);
    try c.buf.pushOp1(.map, inst.dst);
    if (inst.own_dst) {
        try initSlot(c, inst.dst, true, node);
    }
    return finishDstInst(c, inst, true);
}

fn genList(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .list);
    const args = c.ir.getArray(data.args, u32, data.nargs);
    const ret_t = c.ir.getExprType(idx).id;

    const inst = try bc.selectForDstInst(c, cstr, ret_t, true, node);
    const argStart = numSlots(c);

    for (args) |argIdx| {
        const arg_t = c.ir.getExprType(argIdx).id;
        const temp = try bc.reserveTemp(c, arg_t);
        const argv = try genExpr(c, argIdx, Cstr.toTempRetain(temp));
        try initSlot(c, temp, argv.retained, node);
    }

    if (ret_t == bt.ListDyn) {
        try c.pushFCode(.list_dyn, &.{@intCast(argStart), data.nargs, inst.dst}, node);
    } else {
        const start = c.buf.ops.items.len;
        try c.pushFCode(.list, &.{@intCast(argStart), data.nargs, 0, 0, inst.dst}, node);
        c.buf.setOpArgU16(start + 3, @intCast(ret_t)); 
    }

    for (0..args.len) |i| {
        const slot_id = argStart + args.len - i - 1;
        const slot = getSlot(c, slot_id);
        if (slot.boxed_retains) {
            try consumeTemp(c, @intCast(slot_id), node);
        }
    }
    try popTemps(c, args.len, node);
    if (inst.own_dst) {
        try initSlot(c, inst.dst, true, node);
    }

    return finishDstInst(c, inst, true);
}

fn genArray(c: *Chunk, idx: usize, cstr: Cstr, node: *ast.Node) !GenValue {
    const data = c.ir.getExprData(idx, .array);
    const args = c.ir.getArray(data.args, u32, data.nargs);
    const ret_t = c.ir.getExprType(idx).id;

    const inst = try bc.selectForDstInst(c, cstr, ret_t, true, node);
    const argStart = numSlots(c);

    for (args) |argIdx| {
        const arg_t = c.ir.getExprType(argIdx).id;
        const temp = try bc.reserveTemp(c, arg_t);
        const argv = try genExpr(c, argIdx, Cstr.toTempRetain(temp));
        try initSlot(c, temp, argv.retained, node);
    }

    const start = c.buf.ops.items.len;
    try c.pushFCode(.array, &.{@intCast(argStart), data.nargs, 0, 0, inst.dst}, node);
    c.buf.setOpArgU16(start + 3, @intCast(ret_t)); 

    for (0..args.len) |i| {
        const slot_id = argStart + args.len - i - 1;
        const slot = getSlot(c, slot_id);
        if (slot.boxed_retains) {
            try consumeTemp(c, @intCast(slot_id), node);
        }
    }
    try popTemps(c, args.len, node);
    if (inst.own_dst) {
        try initSlot(c, inst.dst, true, node);
    }

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
    try genReleaseBlock(c);

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
    log.tracev("push func block: params={}, max_locals={}, method={}, {}", .{data.func.numParams, data.maxLocals, data.func.isMethod(), node});
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
    const params = c.ir.getArray(data.params, ir.FuncParam, func.numParams);

    const inst = try bc.selectForDstInst(c, cstr, bt.Any, true, node);

    // Prepare jump to skip the body.
    const skipJump = try c.pushEmptyJump();

    log.tracev("push lambda block: {}, {}", .{func.numParams, data.maxLocals});
    const funcPc = c.buf.ops.items.len;
    try pushFuncBlockCommon(c, data.maxLocals, data.numParamCopies, params, func, node);

    try genStmts(c, data.bodyHead);

    const stackSize = c.getMaxUsedRegisters();
    const rt_func = rt.FuncSymbol.initFunc(funcPc, @intCast(stackSize), func.numParams, func.funcSigId, func.reqCallTypeCheck, func.isMethod());
    const rt_id = try reserveFunc(c.compiler, func);
    completeFunc(c.compiler, rt_id, func, rt_func);

    try popFuncBlockCommon(c, func);
    c.patchJumpToCurPc(skipJump);

    if (data.numCaptures == 0) {
        if (data.ct) {
            const start = c.buf.ops.items.len;
            try c.pushCode(.func_sym, &.{
                0, 0, 0, 0, 0, 0, 0, 0, inst.dst }, node);
            c.buf.setOpArgU16(start + 1, @intCast(c.ir.getExprType(idx).id));
            c.buf.setOpArgU48(start + 3, @intCast(@intFromPtr(func)));
        } else {
            const start = c.buf.ops.items.len;
            try c.pushCode(.lambda, &.{ 0, 0, 0, 0, inst.dst }, node);
            c.buf.setOpArgU16(start + 1, @intCast(rt_id));
            c.buf.setOpArgU16(start + 3, @intCast(c.ir.getExprType(idx).id));
        }
    } else {
        const captures = c.ir.getArray(data.captures, u8, data.numCaptures);
        const start = c.buf.ops.items.len;
        try c.pushCode(.closure, &.{
            0, 0, 0, 0, @as(u8, @intCast(captures.len)), cy.vm.CalleeStart, inst.dst
        }, node);
        c.buf.setOpArgU16(start + 1, @intCast(rt_id));
        c.buf.setOpArgU16(start + 3, @intCast(c.ir.getExprType(idx).id));

        const operandStart = try c.buf.reserveData(captures.len);
        for (captures, 0..) |irVar, i| {
            const reg = toLocalReg(c, irVar);
            c.buf.ops.items[operandStart + i].val = reg;
        }
    }

    if (inst.own_dst) {
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

    try genReleaseSlots(c, c.curBlock.slot_start, b.slot_off, b.node);
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
    const idx = try c.buf.getOrPushConst(cy.Value.initF64(val));
    try genConst(c, idx, dst, false, node);
    return regValue(c, dst, false);
}

fn genConst(c: *cy.Chunk, idx: usize, dst: u8, retain: bool, node: *ast.Node) !void {
    const pc = c.buf.len();
    if (retain) {
        try c.pushCode(.constRetain, &.{ 0, 0, dst }, node);
    } else {
        try c.pushCode(.constOp, &.{0, 0, dst}, node);
    }
    c.buf.setOpArgU16(pc + 1, @intCast(idx));
}

fn genExprStmt(c: *Chunk, idx: usize, node: *ast.Node) !void {
    const data = c.ir.getStmtData(idx, .exprStmt);

    if (data.isBlockResult) {
        const inMain = c.curBlock.block_depth == 1;
        if (inMain) {
            const exprv = try genExpr(c, data.expr, Cstr.simpleRetain);
            try initTempValue(c, exprv, node);
            c.curBlock.endLocal = exprv.reg;
            try consumeTempValue(c, exprv, node);
            try popTempValue(c, exprv, node);
        } else {
            // Return from block expression.
            const b = c.blocks.getLast();
            _ = try genExpr(c, data.expr, b.blockExprCstr);
        }
    } else {
        const exprv = try genExpr(c, data.expr, Cstr.simple);
        try initTempValue(c, exprv, node);

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
        if (!slot.boxed or slot.type != .temp or !slot.has_unwind) {
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

fn genConstIntExt(c: *Chunk, val: i64, dst: LocalId, node: *ast.Node) !GenValue {
    // TODO: Can be constU8.
    if (val >= 0 and val <= std.math.maxInt(i8)) {
        try c.pushCode(.constIntV8, &.{ @bitCast(@as(i8, @intCast(val))), dst }, node);
        return regValue(c, dst, false);
    }
    const idx = try c.buf.getOrPushConst(cy.Value.initInt(@intCast(val)));
    try genConst(c, idx, dst, false, node);
    return regValue(c, dst, false);
}

fn pushTypeCheckOption(c: *cy.Chunk, local: SlotId, node: *ast.Node) !void {
    try c.pushFCode(.typeCheckOption, &.{ local }, node);
}

fn pushTypeCheck(c: *cy.Chunk, local: SlotId, typeId: cy.TypeId, dst: SlotId, node: *ast.Node) !void {
    const start = c.buf.ops.items.len;
    try c.pushFCode(.typeCheck, &.{ local, 0, 0, dst }, node);
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

    const boxed = !c.sema.isUnboxedType(declType) or lifted;
    setSlot(c, slot, .{
        .type = .local,
        .boxed = boxed,
        .owned = true,
        .boxed_up = lifted,
        .boxed_init = false,
        .boxed_retains = false,
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
        .boxed_retains = undefined,
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
        .boxed_retains = undefined,
    });
    if (slot+1 > c.curBlock.max_slots) {
        c.curBlock.max_slots = @intCast(slot+1);
    }
    return @intCast(slot);
}

pub fn reserveTemp(c: *Chunk, type_id: cy.TypeId) !u8 {
    const slot = try reserveSlot(c);
    log.tracev("reserve temp: {}", .{slot});
    const boxed = !c.sema.isUnboxedType(type_id);
    setSlot(c, slot, .{
        .type = .temp,
        .boxed = boxed,
        .owned = true,
        .boxed_init = false,
        .boxed_up = false,
        .boxed_retains = false,
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

fn pushStructInit(c: *cy.Chunk, typeId: cy.TypeId, startLocal: u8, val_size: u8, fields: []const cy.sym.FieldInfo, dst: SlotId, debugNode: *ast.Node) !void {
    const start = c.buf.ops.items.len;
    if (val_size <= 4) {
        try c.pushCode(.struct_small, &.{ 0, 0, startLocal, @intCast(fields.len), dst }, debugNode);
    } else {
        try c.pushFCode(.struct_init, &.{ 0, 0, startLocal, @intCast(fields.len), dst }, debugNode);
    }
    c.buf.setOpArgU16(start + 1, @intCast(typeId)); 
    for (fields) |field| {
        const type_e = c.sema.types.items[field.type];
        if (type_e.kind == .struct_t) {
            try c.buf.pushOperand(@intCast(type_e.data.struct_t.nfields));
        } else {
            try c.buf.pushOperand(0);
        }
    }
}

fn pushFieldDyn(c: *cy.Chunk, recv: u8, dst: u8, fieldId: u16, debugNode: *ast.Node) !void {
    const start = c.buf.ops.items.len;
    try c.pushFCode(.fieldDyn, &.{ recv, dst, 0, 0, 0, 0, 0, 0, 0, 0 }, debugNode);
    c.buf.setOpArgU16(start + 3, fieldId);
}

fn pushField(c: *cy.Chunk, recv: u8, fieldIdx: u8, retain: bool, dst: u8, debugNode: *ast.Node) !void {
    try c.pushCode(.field, &.{ recv, fieldIdx, @intFromBool(retain), dst }, debugNode);
}

fn pushAddrConstIndex(c: *cy.Chunk, recv: u8, offset: u8, dst: u8, debugNode: *ast.Node) !void {
    try c.pushCode(.addr_const_index, &.{ recv, offset, dst }, debugNode);
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
                .own_dst = true,
                .cstr = cstr,
                .has_final_dst = true,
                .node = node,
            };
        },
        .captured => {
            return .{
                .dst = try bc.reserveTemp(c, type_id),
                .own_dst = true,
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
                    .own_dst = true,
                    .cstr = cstr,
                    .has_final_dst = true,
                    .node = node,
                };
            } else {
                return .{
                    .dst = cstr.data.slot.dst,
                    .own_dst = false,
                    .cstr = cstr,
                    .has_final_dst = false,
                    .node = node,
                };
            }
        },
        .liftedLocal => {
            return .{
                .dst = try bc.reserveTemp(c, type_id),
                .own_dst = true,
                .cstr = cstr,
                .has_final_dst = true,
                .node = node,
            };
        },
        .simple => {
            return .{
                .dst = try bc.reserveTemp(c, type_id),
                .own_dst = false,
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
                .own_dst = true,
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
                .own_dst = false,
                .requiresPreRelease = cstr.data.slot.releaseDst,
                .cstr = cstr,
                .has_final_dst = false,
                .node = node,
            };
        },
        .liftedLocal => {
            return .{
                .dst = try bc.reserveTemp(c, type_id),
                .own_dst = true,
                .requiresPreRelease = false,
                .cstr = cstr,
                .has_final_dst = true,
                .node = node,
            };
        },
        .captured => {
            return .{
                .dst = try bc.reserveTemp(c, type_id),
                .own_dst = true,
                .requiresPreRelease = false,
                .cstr = cstr,
                .has_final_dst = true,
                .node = node,
            };
        },
        .simple => {
            return .{
                .dst = try bc.reserveTemp(c, type_id),
                .own_dst = false,
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
    own_dst: bool,
    cstr: Cstr,
    has_final_dst: bool,
    node: *ast.Node,
};

pub const NoErrInst = struct {
    dst: SlotId,
    own_dst: bool,
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
        }}};
    }

    pub fn toTemp(reg: u8) Cstr {
        return .{ .type = .tempReg, .data = .{ .slot = .{
            .dst = reg,
            .retain = false,
            .releaseDst = false,
        }}};
    }

    pub fn toRetainedTemp(reg: u8) Cstr {
        return .{ .type = .tempReg, .data = .{ .slot = .{
            .dst = reg,
            .retain = true,
            .releaseDst = true,
        }}};
    }

    pub fn toLocal(reg: u8, rcCandidate: bool) Cstr {
        return .{ .type = .localReg, .data = .{ .slot = .{
            .dst = reg,
            .retain = true,
            .releaseDst = rcCandidate,
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

    pub fn isSlot(self: Cstr) bool {
        return self.type == .localReg or self.type == .tempReg or self.type == .simple;
    }

    /// Assumes merged cstr.
    pub fn toMergedValue(self: Cstr, c: *cy.Chunk, ret_t: cy.TypeId) GenValue {
        const retain = !c.sema.isUnboxedType(ret_t);
        if (self.type == .tempReg) {
            return GenValue.initTempValue(self.data.slot.dst, retain);
        } else if (self.type == .localReg) {
            return GenValue.initLocalValue(self.data.slot.dst, retain);
        } else {
            return GenValue.initRetained(retain);
        }
    }
};

/// Selects a slot to be used as the result of different control flow paths.
/// Retains boxed values to ensure the same value type for the parent consumer.
pub fn toMergedDst(c: *Chunk, cstr: Cstr, type_id: cy.TypeId) !Cstr {
    if (cstr.type == .localReg or cstr.type == .tempReg) {
        var new = cstr;
        new.data.slot.retain = true;
        return new;
    } else if (cstr.type == .simple) {
        const slot = try reserveTemp(c, type_id);
        const new = Cstr.toTempRetain(slot);
        return new;
    } else {
        return cstr;
    }
}

fn consumeTemp(c: *Chunk, slot: SlotId, node: *ast.Node) !void {
    getSlotPtr(c, slot).boxed_retains = false;
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
        if (slot.boxed_retains) {
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
            if (slot.boxed_retains) {
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

pub fn popUnwindTry(c: *Chunk, catch_pc: usize, node: *ast.Node) !void {
    log.tracev("-pop unwind try: stack={} catch={}", .{c.unwind_stack.items.len, catch_pc});
    const entry = c.unwind_stack.pop();
    if (entry.type != .try_e) {
        return c.reportErrorFmt("Pop try unwind, found {}.", &.{v(entry.type)}, node);
    }
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
