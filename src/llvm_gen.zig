const std = @import("std");
const cy = @import("cyber.zig");
const sema = cy.sema;
const types = cy.types;
const bt = types.BuiltinTypes;
const fmt = @import("fmt.zig");
const v = fmt.v;
const log = cy.log.scoped(.llvm_gen);
const llvm = @import("llvm.zig");

/// Gen LLVM IR -> LLVM -> Binary.
pub fn genNativeBinary(self: *cy.VMcompiler) !void {
    const ctx = llvm.ContextCreate();
    const mod = llvm.ModuleCreateWithNameInContext("Module", ctx);
    const builder = llvm.CreateBuilderInContext(ctx);

    for (self.chunks.items) |*chunk| {
        chunk.mod = mod;
        chunk.builder = builder;
        chunk.ctx = ctx;

        // Attach LLVM func data.
        chunk.llvmFuncs = try self.alloc.alloc(cy.chunk.LLVM_Func, chunk.semaFuncDecls.items.len);

        try genChunk(self, chunk.id);
    }

    llvm.DumpModule(mod);

    // Prepare target.
    _ = llvm.InitializeNativeTarget();
    llvm.InitializeAllAsmPrinters();
    const triple = llvm.GetDefaultTargetTriple();
    var target: llvm.TargetRef = undefined;
    var err: [*:0]u8 = undefined;
    if (llvm.GetTargetFromTriple(triple, &target, @ptrCast(&err)) == llvm.True) {
        try self.setErrorAt(cy.NullId, cy.NullId, "{}", &.{v(err)});
        return error.CompileError;
    }
    const cpu = "generic";
    const features = "";
    const targetm = llvm.CreateTargetMachine(target, triple, cpu, features,
        llvm.CodeGenLevelDefault, llvm.RelocDefault, llvm.CodeModelDefault);

    // Apply target to module.
    llvm.SetTarget(mod, triple);
    const dataLayoutStr = llvm.CopyStringRepOfTargetData(llvm.CreateTargetDataLayout(targetm));
    llvm.SetDataLayout(mod, dataLayoutStr);
    llvm.DisposeMessage(dataLayoutStr);

    // Perform optimizations.
    const passm = llvm.CreatePassManager();
    _ = llvm.RunPassManager(passm, mod);

    llvm.DumpModule(mod);

    // Dump object file.
    if (llvm.TargetMachineEmitToFile(targetm, mod, "./cy.o", llvm.CodeGenFileType.ObjectFile, @ptrCast(&err)) == llvm.True) {
        try self.setErrorAt(cy.NullId, cy.NullId, "{}", &.{v(err)});
        return error.CompileError;
    }
}

fn genChunk(self: *cy.VMcompiler, id: cy.ChunkId) !void {
    const chunk = &self.chunks.items[id];
    log.tracev(self.vm, "Perform codegen for chunk{}: {s}", .{id, chunk.srcUri});

    if (id == 0) {
        // Main script performs gen for decls and the main block.
        // try gen.initVarLocals(chunk);
        // const jumpStackStart = chunk.blockJumpStack.items.len;
        const root = chunk.nodes[0];

        // try chunk.unwindTempIndexStack.append(self.alloc, @bitCast(@as(u32, cy.NullId)));
        // defer chunk.popUnwindTempIndex();

        try genStatements(chunk, root.head.root.headStmt, true);
        // chunk.patchBlockJumps(jumpStackStart);
        // chunk.blockJumpStack.items.len = jumpStackStart;
        // self.buf.mainStackSize = chunk.getMaxUsedRegisters();
        // chunk.popBlock();
    } else {
        // Modules perform gen for only the top level declarations.
        const root = chunk.nodes[0];
        try genTopDeclStatements(chunk, root.head.root.headStmt);
        // chunk.popBlock();
    }
}

fn genStatements(self: *cy.Chunk, head: cy.NodeId, comptime attachEnd: bool) anyerror!void {
    _ = attachEnd;
    if (head == cy.NullId) {
        // try self.buf.pushOp1(.end, 255);
        return;
    }
    
    var curId = head;
    var node = self.nodes[curId];

    while (node.next != cy.NullId) {
        try genStatement(self, curId);
        curId = node.next;
        node = self.nodes[curId];
    }

    // // Check for last expression statement.
    // if (node.node_t == .expr_stmt) {
    //     if (attachEnd) {
    //         const local = try exprStmt(self, cur_id, true);
    //         if (shouldGenMainScopeReleaseOps(self.compiler)) {
    //             self.curBlock.endLocalsPc = @intCast(self.buf.ops.items.len);
    //             self.nodes[0].head.root.genEndLocalsPc = self.curBlock.endLocalsPc;
    //             try self.endLocals();
    //         }
    //         try self.buf.pushOp1(.end, local);
    //     } else {
    //         _ = try statement(self, cur_id);
    //     }
    // } else {
    //     if (attachEnd) {
    //         try statement(self, cur_id);
    //         if (shouldGenMainScopeReleaseOps(self.compiler)) {
    //             self.curBlock.endLocalsPc = @intCast(self.buf.ops.items.len);
    //             self.nodes[0].head.root.genEndLocalsPc = self.curBlock.endLocalsPc;
    //             try self.endLocals();
    //         }
    //         try self.buf.pushOp1(.end, 255);
    //     } else {
    //         try statement(self, cur_id);
    //     }
    // }
    try genStatement(self, curId);
}

fn genTopDeclStatements(c: *cy.Chunk, head: cy.NodeId) !void {
    var nodeId = head;
    while (nodeId != cy.NullId) {
        const node = c.nodes[nodeId];
        switch (node.node_t) {
            .objectDecl => {
                try genStatement(c, nodeId);
            },
            .funcDecl => {
                try genStatement(c, nodeId);
            },
            else => {},
        }
        nodeId = node.next;
    }
}

fn genStatement(c: *cy.Chunk, nodeId: cy.NodeId) !void {
    const node = c.nodes[nodeId];
    // log.gtracev("gen stmt {}", .{node.node_t});
    c.curNodeId = nodeId;

    // const tempStart = c.rega.getNextTemp();
    // defer c.rega.setNextTemp(tempStart);

    switch (node.node_t) {
        // .pass_stmt => {
        //     return;
        // },
        // .breakStmt => {
        //     const pc = try c.pushEmptyJump();
        //     try c.subBlockJumpStack.append(c.alloc, .{ .jumpT = .brk, .pc = pc });
        // },
        // .continueStmt => {
        //     const pc = try c.pushEmptyJump();
        //     try c.subBlockJumpStack.append(c.alloc, .{ .jumpT = .cont, .pc = pc });
        // },
        .expr_stmt => {
            _ = try genExprStmt(c, nodeId, false);
        },
        // .localDecl => {
        //     if (node.head.localDecl.right != cy.NullId) {
        //         // Can assume left is .ident from sema.
        //         const varSpec = c.nodes[node.head.localDecl.varSpec];
        //         try assignExprToLocalVar(c, varSpec.head.varSpec.name, node.head.localDecl.right);
        //     }
        // },
        // .assign_stmt => {
        //     try assignStmt(c, nodeId);
        // },
        // .opAssignStmt => {
        //     try opAssignStmt(c, nodeId);
        // },
        // .importStmt => {
        //     const ident = c.nodes[node.head.left_right.left];
        //     const name = c.getNodeString(ident);

        //     const spec = c.nodes[node.head.left_right.right];
        //     const specPath = c.getNodeString(spec);

        //     _ = name;
        //     _ = specPath;

        //     // const modId = try self.getOrLoadModule(specPath);
        //     // const leadSym = try self.ensureSemaSym(name);
        //     // try self.semaSymToMod.put(self.alloc, leadSym, modId);
        // },
        // .funcDeclInit => {
        //     // Nop. Func declaration initializer are hoisted and initialized at the start of the program.
        // },
        .funcDecl => {
            try genFuncDecl(c, c.semaRootSymId, nodeId);
        },
        // .enumDecl => {
        //     // Nop.
        // },
        // .typeAliasDecl => {
        //     // Nop.
        // },
        // .objectDecl => {
        //     const nameN = c.nodes[node.head.objectDecl.name];
        //     const name = c.getNodeString(nameN);
        //     const nameId = try sema.ensureNameSym(c.compiler, name);
        //     const crObjSymId = nameN.head.ident.sema_csymId;
        //     const robjSymId = crObjSymId.id;
        //     const sid = try c.compiler.vm.ensureObjectType(c.semaRootSymId, nameId, robjSymId);

        //     const body = c.nodes[node.head.objectDecl.body];
        //     var funcId = body.head.objectDeclBody.funcsHead;
        //     var func: cy.Node = undefined;
        //     while (funcId != cy.NullId) : (funcId = func.next) {
        //         func = c.nodes[funcId];
        //         const decl = c.semaFuncDecls.items[func.head.func.semaDeclId];

        //         const funcName = decl.getName(c);
        //         if (decl.numParams > 0) {
        //             const param = c.nodes[decl.paramHead];
        //             const paramName = c.getNodeString(c.nodes[param.head.funcParam.name]);
        //             if (std.mem.eql(u8, paramName, "self")) {
        //                 // Object method.
        //                 if (func.node_t == .funcDecl) {
        //                     try genMethodDecl(c, sid, func, decl, funcName);
        //                 }
        //                 continue;
        //             }
        //         }

        //         // const detail = cy.FuncSymDetail{
        //         //     .name = try c.alloc.dupe(u8, funcName),
        //         // };
        //         // try c.compiler.vm.funcSymDetails.append(c.alloc, detail);
        //         if (func.node_t == .funcDecl) {
        //             try funcDecl(c, robjSymId, funcId);
        //         }
        //     }
        // },
        .if_stmt => {
            try genIfStmt(c, nodeId);
        },
        // .matchBlock => {
        //     _ = try matchBlock(c, nodeId, null);
        // },
        // .tryStmt => {
        //     // push try block.
        //     var errorVarLocal: u8 = cy.NullU8;
        //     if (node.head.tryStmt.errorVar != cy.NullId) {
        //         const errorVarN = c.nodes[node.head.tryStmt.errorVar];
        //         errorVarLocal = c.genGetVar(errorVarN.head.ident.semaVarId).?.local;
        //     }
        //     const pushTryPc = c.buf.ops.items.len;
        //     try c.buf.pushOp3(.pushTry, errorVarLocal, 0, 0);

        //     // try body.
        //     c.nextSemaSubBlock();
        //     try genStatements(c, node.head.tryStmt.tryFirstStmt, false);
        //     c.prevSemaSubBlock();

        //     // pop try block.
        //     const popTryPc = c.buf.ops.items.len;
        //     try c.buf.pushOp2(.popTry, 0, 0);
        //     c.buf.setOpArgU16(pushTryPc + 2, @intCast(c.buf.ops.items.len - pushTryPc));

        //     // catch body.
        //     c.nextSemaSubBlock();
        //     try genStatements(c, node.head.tryStmt.catchFirstStmt, false);
        //     c.prevSemaSubBlock();

        //     c.buf.setOpArgU16(popTryPc + 1, @intCast(c.buf.ops.items.len - popTryPc));
        // },
        // .staticDecl => {
        //     // Nop. Static variables are hoisted and initialized at the start of the program.
        // },
        // .whileOptStmt => {
        //     try whileOptStmt(c, nodeId);
        // },
        // .whileInfStmt => {
        //     c.nextSemaSubBlock();

        //     const pcSave: u32 = @intCast(c.buf.ops.items.len);
        //     const jumpStackSave: u32 = @intCast(c.subBlockJumpStack.items.len);
        //     defer c.subBlockJumpStack.items.len = jumpStackSave;

        //     // TODO: generate gas meter checks.
        //     // if (c.opts.gas_meter != .none) {
        //     //     try c.indent();
        //     //     if (c.opts.gas_meter == .error_interrupt) {
        //     //         _ = try c.writer.write("__interrupt_count += 1; if (__interrupt_count > __interrupt_max) throw globalThis._internal.interruptSym;\n");
        //     //     } else if (c.opts.gas_meter == .yield_interrupt) {
        //     //         _ = try c.writer.write("__interrupt_count += 1; if (__interrupt_count > __interrupt_max) yield globalThis._internal.interruptSym;\n");
        //     //     }
        //     // }

        //     try genStatements(c, node.head.child_head, false);
        //     try c.pushJumpBackTo(pcSave);

        //     c.patchForBlockJumps(jumpStackSave, c.buf.ops.items.len, pcSave);
        //     c.prevSemaSubBlock();
        // },
        // .whileCondStmt => {
        //     c.nextSemaSubBlock();

        //     const topPc: u32 = @intCast(c.buf.ops.items.len);
        //     const jumpStackSave: u32 = @intCast(c.subBlockJumpStack.items.len);
        //     defer c.subBlockJumpStack.items.len = jumpStackSave;

        //     const condv = try expression(c, node.head.whileCondStmt.cond, RegisterCstr.simple);
        //     const condLocal = condv.local;

        //     var jumpPc = try c.pushEmptyJumpNotCond(condLocal);

        //     // Enter while body.

        //     // ARC cleanup.
        //     try releaseIfRetainedTemp(c, condv);

        //     c.rega.setNextTemp(tempStart);

        //     try genStatements(c, node.head.whileCondStmt.bodyHead, false);
        //     try c.pushJumpBackTo(topPc);

        //     c.patchJumpNotCondToCurPc(jumpPc);

        //     c.patchForBlockJumps(jumpStackSave, c.buf.ops.items.len, topPc);
        //     c.prevSemaSubBlock();
        // },
        // .for_range_stmt => {
        //     try forRangeStmt(c, nodeId);
        // },
        // .for_iter_stmt => {
        //     try forIterStmt(c, nodeId);
        // },
        // .return_stmt => {
        //     if (c.blocks.items.len == 1) {
        //         try c.endLocals();
        //         try c.buf.pushOp1(.end, 255);
        //     } else {
        //         try c.endLocals();
        //         try c.buf.pushOp(.ret0);
        //     }
        // },
        .return_expr_stmt => {
            const val = try genExpr(c, node.head.child_head);
            // try c.endLocals();
            sema.curSubBlock(c).endReachable = false;
            _ = llvm.BuildRet(c.builder, val.val);
        },
        // .comptimeStmt => {
        //     try comptimeStmt(c, nodeId);
        // },
        else => {
            return c.reportErrorAt("Unsupported statement: {}", &.{v(node.node_t)}, nodeId);
        }
    }
}

fn genFuncDecl(self: *cy.Chunk, parentSymId: sema.SymbolId, nodeId: cy.NodeId) !void {
    _ = parentSymId;
    const node = self.nodes[nodeId];
    const func = &self.semaFuncDecls.items[node.head.func.semaDeclId];
    const name = func.getName(self);
    // const nameId = try sema.ensureNameSym(self.compiler, name);
    // const symId = try self.compiler.vm.ensureFuncSym(parentSymId, nameId, func.funcSigId);

    const funcTypeRef = try toLLVM_FunctionType(self, func.funcSigId);
    const nameZ = try toTempStrZ(self, name);
    const funcRef = llvm.AddFunction(self.mod, nameZ, funcTypeRef);
    self.llvmFuncs[node.head.func.semaDeclId] = .{
        .typeRef = funcTypeRef,
        .funcRef = funcRef,
    };

    const bb = llvm.AppendBasicBlockInContext(self.ctx, funcRef, "entry");
    llvm.PositionBuilderAtEnd(self.builder, bb);

    // const jumpPc = try self.pushEmptyJump();

    try self.pushSemaBlock(func.semaBlockId);
    // if (self.compiler.config.genDebugFuncMarkers) {
    //     try self.compiler.buf.pushDebugFuncStart(node.head.func.semaDeclId, self.id);
    // }
    // self.curBlock.frameLoc = nodeId;
    self.curBlock.funcRef = funcRef;

    // try self.unwindTempIndexStack.append(self.alloc, @bitCast(@as(u32, cy.NullId)));
    // defer self.popUnwindTempIndex();

    // const jumpStackStart = self.blockJumpStack.items.len;

    // const opStart: u32 = @intCast(self.buf.ops.items.len);
    // try self.reserveFuncParams(func.numParams);
    // try initVarLocals(self);
    try genStatements(self, node.head.func.bodyHead, false);
    // // TODO: Check last statement to skip adding ret.
    // try self.genBlockEnding();
    // func.genEndLocalsPc = self.curBlock.endLocalsPc;

    // // Reserve another local for the call return info.
    // const sblock = sema.curBlock(self);
    // const numCaptured: u8 = @intCast(sblock.captures.items.len);
    // std.debug.assert(numCaptured == 0);

    // self.patchJumpToCurPc(jumpPc);

    // self.patchBlockJumps(jumpStackStart);
    // self.blockJumpStack.items.len = jumpStackStart;

    // const stackSize = self.getMaxUsedRegisters();
    self.popSemaBlock();

    // if (self.compiler.config.genDebugFuncMarkers) {
    //     try self.compiler.buf.pushDebugFuncEnd(node.head.func.semaDeclId, self.id);
    // }
    
    // const rtSym = rt.FuncSymbol.initFunc(opStart, @intCast(stackSize), func.numParams, func.funcSigId);
    // self.compiler.vm.setFuncSym(symId, rtSym);

    if (llvm.VerifyFunction(funcRef, llvm.PrintMessageAction) == llvm.True) {
        llvm.DumpValue(funcRef);
        return self.reportErrorAt("LLVM verify function error.", &.{}, nodeId);
    }
}

fn genPred(c: *cy.Chunk, val: Value) !Value {
    switch (val.typeId) {
        bt.Boolean => {
            // Already boolean. Nop.
            return val;
        },
        else => {
            const name = types.getTypeName(c.compiler, val.typeId);
            return c.reportError("Unsupported type: {}, {}", &.{v(name), v(val.typeId)});
        }
    }
}

fn genIfStmt(c: *cy.Chunk, nodeId: cy.NodeId) !void {
    // const tempStart = c.rega.getNextTemp();

    const node = c.nodes[nodeId];
    const condv = try genExpr(c, node.head.left_right.left);

    const predv = try genPred(c, condv);

    const thenBlock = llvm.AppendBasicBlockInContext(c.ctx, c.curBlock.funcRef, "then");
    const mergeBlock = llvm.CreateBasicBlockInContext(c.ctx, "ifmerge");
    _ = llvm.BuildCondBr(c.builder, predv.val, thenBlock, mergeBlock);

    // var lastCondJump = try c.pushEmptyJumpNotCond(condv.local);

    // // Enter if body.

    // // ARC cleanup.
    // try releaseIfRetainedTemp(c, condv);

    // c.rega.setNextTemp(tempStart);

    c.nextSemaSubBlock();
    llvm.PositionBuilderAtEnd(c.builder, thenBlock);
    try genStatements(c, node.head.left_right.right, false);

    if (sema.curSubBlock(c).endReachable) {
        _ = llvm.BuildBr(c.builder, mergeBlock);
    }
    c.prevSemaSubBlock();

    llvm.AppendExistingBasicBlock(c.curBlock.funcRef, mergeBlock);
    llvm.PositionBuilderAtEnd(c.builder, mergeBlock);

    // var elseClauseId = node.head.left_right.extra;
    // if (elseClauseId != cy.NullId) {
    //     var jumpsStart = c.subBlockJumpStack.items.len;
    //     defer c.subBlockJumpStack.items.len = jumpsStart;

    //     var endsWithElse = false;
    //     while (elseClauseId != cy.NullId) {
    //         const pc = try c.pushEmptyJump();
    //         try c.subBlockJumpStack.append(c.alloc, .{ .jumpT = .subBlockBreak, .pc = pc });

    //         c.patchJumpNotCondToCurPc(lastCondJump);

    //         const elseClause = c.nodes[elseClauseId];
    //         if (elseClause.head.else_clause.cond == cy.NullId) {
    //             c.nextSemaSubBlock();
    //             try genStatements(c, elseClause.head.else_clause.body_head, false);
    //             c.prevSemaSubBlock();
    //             endsWithElse = true;
    //             break;
    //         } else {
    //             const tempStart2 = c.rega.getNextTemp();
    //             const elseCondv = try expression(c, elseClause.head.else_clause.cond, RegisterCstr.simple);
    //             lastCondJump = try c.pushEmptyJumpNotCond(elseCondv.local);

    //             // Enter else-if body.
    
    //             // ARC cleanup.
    //             try releaseIfRetainedTemp(c, elseCondv);

    //             c.rega.setNextTemp(tempStart2);

    //             c.nextSemaSubBlock();
    //             try genStatements(c, elseClause.head.else_clause.body_head, false);
    //             c.prevSemaSubBlock();
    //             elseClauseId = elseClause.head.else_clause.else_clause;
    //         }
    //     }

    //     if (!endsWithElse) {
    //         c.patchJumpNotCondToCurPc(lastCondJump);
    //     }
    //     jumpsStart = c.patchSubBlockBreakJumps(jumpsStart, c.buf.ops.items.len);
    // } else {
    //     c.patchJumpNotCondToCurPc(lastCondJump);
    // }
}

fn genExpr(c: *cy.Chunk, nodeId: cy.NodeId) !Value {

    // When NullId is popped, it triggers the postOrder visit.
    try c.exprStack.append(c.alloc, .{
        .nodeId = nodeId,
        .previsited = false,
    });

    while (c.exprStack.items.len > 0) {
        const cur = &c.exprStack.items[c.exprStack.items.len-1];
        if (!cur.previsited) {
            const hasChildren = try pushExprChildren(c, cur.nodeId);
            if (hasChildren) {
                cur.previsited = true;
                continue;
            }
        }
        const val = try postExpr(c, cur.nodeId);
        try c.exprResStack.append(c.alloc, val);
        c.exprStack.items.len -= 1;
    }

    return c.exprResStack.items[0];
}

fn pushExprChildren(c: *cy.Chunk, nodeId: cy.NodeId) !bool {
    const top = c.exprStack.items.len;
    const node = c.nodes[nodeId];
    switch (node.node_t) {
        .number,
        .ident => return false,
        .callExpr => {
            const callee = c.nodes[node.head.callExpr.callee];
            if (callee.node_t == .accessExpr) {
                return error.Unsupported;
            } else if (callee.node_t == .ident) {
                if (callee.head.ident.sema_csymId.isPresent()) {
                    // Symbol.
                    try c.exprStack.resize(c.alloc, top + node.head.callExpr.numArgs);
                    var curId = node.head.callExpr.arg_head;
                    var i: usize = 1;
                    while (curId != cy.NullId) : (i += 1) {
                        c.exprStack.items[c.exprStack.items.len - i] = .{
                            .nodeId = curId,
                            .previsited = false,
                        };
                        const cur = c.nodes[curId];
                        curId = cur.next;
                    }
                } else {
                    return error.Unsupported;
                }
            } else {
                return error.Unsupported;
            }
            return true;
        },
        .binExpr => {
            try c.exprStack.resize(c.alloc, top + 2);
            c.exprStack.items[top] = .{ .nodeId = node.head.binExpr.right, .previsited = false };
            c.exprStack.items[top + 1] = .{ .nodeId = node.head.binExpr.left, .previsited = false };
            return true;
        },
        else => {
            return c.reportErrorAt("Unsupported expression: {}", &.{v(node.node_t)}, nodeId);
        }
    }
}

fn postExpr(c: *cy.Chunk, nodeId: cy.NodeId) !Value {
    // // log.gtracev("gen expr: {}", .{node.node_t});
    // c.curNodeId = nodeId;
    const node = c.nodes[nodeId];
    switch (node.node_t) {
        .ident => {
            return genIdent(c, nodeId);
        },
    //     .true_literal => {
    //         const dst = try c.rega.selectFromNonLocalVar(cstr, false);
    //         try c.buf.pushOp1(.true, dst);
    //         return c.initGenValue(dst, bt.Boolean, false);
    //     },
    //     .false_literal => {
    //         const dst = try c.rega.selectFromNonLocalVar(cstr, false);
    //         try c.buf.pushOp1(.false, dst);
    //         return c.initGenValue(dst, bt.Boolean, false);
    //     },
        .number => {
            const literal = c.getNodeString(node);
            if (c.nodeTypes[nodeId] == bt.Integer) {
                const ival = try std.fmt.parseInt(u64, literal, 10);
                const val = llvm.ConstInt(llvm.IntTypeInContext(c.ctx, 48), ival, llvm.False);
                return Value.init(bt.Integer, val);
            } else {
                const fval = try std.fmt.parseFloat(f64, literal);
                const val = llvm.ConstReal(llvm.DoubleType(), fval);
                return Value.init(bt.Float, val);
            }
        },
    //     .float => {
    //         const literal = c.getNodeString(node);
    //         const val = try std.fmt.parseFloat(f64, literal);

    //         const dst = try c.rega.selectFromNonLocalVar(cstr, false);
    //         return constNumber(c, val, dst);
    //     },
    //     .nonDecInt => {
    //         const fval: f64 = @floatFromInt(node.head.nonDecInt.semaNumberVal);
    //         const dst = try c.rega.selectFromNonLocalVar(cstr, false);
    //         return try constInt(c, fval, dst);
    //     },
    //     .symbolLit => {
    //         const name = c.getNodeString(node);
    //         const symId = try c.compiler.vm.ensureSymbol(name);
    //         const dst = try c.rega.selectFromNonLocalVar(cstr, false);
    //         try c.buf.pushOp2(.tagLiteral, @intCast(symId), dst);
    //         return c.initGenValue(dst, bt.Symbol, false);
    //     },
    //     .errorSymLit => {
    //         const symN = c.nodes[node.head.errorSymLit.symbol];
    //         const name = c.getNodeString(symN);
    //         const symId = try c.compiler.vm.ensureSymbol(name);
    //         const val = cy.Value.initErrorSymbol(@intCast(symId));
    //         const idx = try c.buf.pushConst(cy.Const.init(val.val));
    //         const dst = try c.rega.selectFromNonLocalVar(cstr, false);
    //         try constOp(c, idx, dst);
    //         return c.initGenValue(dst, bt.Error, false);
    //     },
    //     .string => {
    //         const literal = c.getNodeString(node);
    //         const str = try c.unescapeString(literal);
    //         const dst = try c.rega.selectFromNonLocalVar(cstr, false);
    //         return string(c, str, dst);
    //     },
    //     .stringTemplate => {
    //         return stringTemplate(c, nodeId, cstr);
    //     },
    //     .none => {
    //         const dst = try c.rega.selectFromNonLocalVar(cstr, false);
    //         return genNone(c, dst);
    //     },
    //     .group => {
    //         return expression(c, node.head.child_head, cstr);
    //     },
    //     .map_literal => {
    //         return mapInit(c, nodeId, cstr);
    //     },
    //     .arr_literal => {
    //         return listInit(c, nodeId, cstr);
    //     },
    //     .if_expr => {
    //         return ifExpr(c, nodeId, cstr);
    //     },
    //     .unary_expr => {
    //         const op = node.head.unary.op;
    //         switch (op) {
    //             .not => {
    //                 const dst = try c.rega.selectFromNonLocalVar(cstr, false);
    //                 const child = try expression(c, node.head.unary.child, RegisterCstr.exact(dst));
    //                 try c.buf.pushOp1(.not, dst);
    //                 try releaseIfRetainedTemp(c, child);
    //                 return c.initGenValue(dst, bt.Boolean, false);
    //             },
    //             .minus,
    //             .bitwiseNot => {
    //                 const childT = c.nodeTypes[node.head.unary.child];
    //                 switch (node.head.unary.semaGenStrat) {
    //                     .none => cy.fatal(),
    //                     .specialized => {
    //                         const dst = try c.rega.selectFromNonLocalVar(cstr, false);

    //                         const tempStart = c.rega.getNextTemp();
    //                         defer c.rega.setNextTemp(tempStart);

    //                         const canUseDst = !c.isParamOrLocalVar(dst);
    //                         const childv = try expression(c, node.head.unary.child, RegisterCstr.preferIf(dst, canUseDst));

    //                         if (childT == bt.Integer) {
    //                             try pushInlineUnaryExpr(c, getIntUnaryOpCode(op), childv.local, dst, nodeId);
    //                         } else if (childT == bt.Float) {
    //                             try pushInlineUnaryExpr(c, getFloatUnaryOpCode(op), childv.local, dst, nodeId);
    //                         } else {
    //                             cy.fatal();
    //                         }

    //                         // ARC cleanup.
    //                         try releaseIfRetainedTemp(c, childv);

    //                         return c.initGenValue(dst, childT, false);
    //                     },
    //                     .generic => {
    //                         if (op == .minus) {
    //                             return callObjSymUnaryExpr(c, c.compiler.vm.@"prefix-MGID", node.head.unary.child, cstr, nodeId);
    //                         } else {
    //                             return callObjSymUnaryExpr(c, c.compiler.vm.@"prefix~MGID", node.head.unary.child, cstr, nodeId);
    //                         }
    //                     }
    //                 }
    //             },
    //         }
    //     },
        .binExpr => {
            return genBinExpr(c, nodeId);
        },
    //     .lambda_multi => {
    //         return lambdaMulti(c, nodeId, cstr);
    //     },
    //     .lambda_expr => {
    //         return lambdaExpr(c, nodeId, cstr);
    //     },
        .callExpr => {
            return genCallExpr(c, nodeId, false, {});
        },
    //     .matchBlock => {
    //         return matchBlock(c, nodeId, cstr);
    //     },
    //     .coresume => {
    //         const dst = try c.rega.selectFromNonLocalVar(cstr, true);
    //         const tempStart = c.rega.getNextTemp();
    //         defer c.rega.setNextTemp(tempStart);

    //         const fiber = try expression(c, node.head.child_head, RegisterCstr.tempMustRetain);
    //         try c.buf.pushOp2(.coresume, fiber.local, dst);
    //         return c.initGenValue(dst, bt.Any, true);
    //     },
    //     .coyield => {
    //         const pc = c.buf.ops.items.len;
    //         try c.buf.pushOp2(.coyield, 0, 0);
    //         try c.blockJumpStack.append(c.alloc, .{ .jumpT = .jumpToEndLocals, .pc = @intCast(pc), .pcOffset = 1 });

    //         // TODO: return coyield expression.
    //         const dst = try c.rega.selectFromNonLocalVar(cstr, false);
    //         return genNone(c, dst);
    //     },
    //     .coinit => {
    //         return callExpr(c, node.head.child_head, cstr, true, nodeId);
    //     },
    //     .sliceExpr => {
    //         return sliceExpr(c, nodeId, cstr);
    //     },
    //     .indexExpr => {
    //         return indexExpr(c, nodeId, null, null, cstr);
    //     },
    //     .accessExpr => {
    //         return accessExpr(c, nodeId, cstr);
    //     },
    //     .objectInit => {
    //         return objectInit(c, nodeId, cstr);
    //     },
    //     .castExpr => {
    //         const child = try expression(c, node.head.castExpr.expr, cstr);
    //         const tSymId = node.head.castExpr.semaTypeSymId;

    //         const tSym = c.compiler.sema.getSymbol(tSymId);
    //         if (tSym.symT == .object) {
    //             const typeId = tSym.getObjectTypeId(c.compiler.vm).?;
    //             try c.pushFailableDebugSym(nodeId);
    //             const pc = c.buf.ops.items.len;
    //             try c.buf.pushOpSlice(.cast, &.{ child.local, 0, 0 });
    //             c.buf.setOpArgU16(pc + 2, @intCast(typeId));
    //         } else if (tSym.symT == .builtinType) {
    //             if (types.toRtConcreteType(tSymId)) |typeId| {
    //                 try c.pushFailableDebugSym(nodeId);
    //                 const pc = c.buf.ops.items.len;
    //                 try c.buf.pushOpSlice(.cast, &.{ child.local, 0, 0 });
    //                 c.buf.setOpArgU16(pc + 2, @intCast(typeId));
    //             } else {
    //                 // Cast to abstract type.
    //                 try c.pushFailableDebugSym(nodeId);
    //                 const pc = c.buf.ops.items.len;
    //                 try c.buf.pushOpSlice(.castAbstract, &.{ child.local, 0, 0 });
    //                 c.buf.setOpArgU16(pc + 2, @intCast(tSymId));
    //             }
    //         }

    //         // ARC cleanup.
    //         if (!cstr.mustRetain) {
    //             try releaseIfRetainedTemp(c, child);
    //         }

    //         return c.initGenValue(child.local, tSymId, child.retained);
    //     },
    //     .throwExpr => {
    //         const child = try expression(c, node.head.child_head, cstr);

    //         try c.pushFailableDebugSym(nodeId);
    //         try c.buf.pushOp1(.throw, child.local);

    //         // ARC cleanup.
    //         if (!cstr.mustRetain) {
    //             try releaseIfRetainedTemp(c, child);
    //         }

    //         return child;
    //     },
    //     .tryExpr => {
    //         const dst = try c.rega.selectFromNonLocalVar(cstr, true);

    //         const pushTryPc = c.buf.ops.items.len;
    //         try c.buf.pushOp3(.pushTry, 0, 0, 0);

    //         const child = try expression(c, node.head.tryExpr.expr, RegisterCstr.initExact(dst, cstr.mustRetain));

    //         const popTryPc = c.buf.ops.items.len;
    //         try c.buf.pushOp2(.popTry, 0, 0);
    //         c.buf.setOpArgU16(pushTryPc + 2, @intCast(c.buf.ops.items.len - pushTryPc));

    //         // Generate else clause.
    //         var elsev: GenValue = undefined;
    //         if (node.head.tryExpr.elseExpr != cy.NullId) {
    //             // Error is not copied anywhere.
    //             c.buf.setOpArgs1(pushTryPc + 1, cy.NullU8);
    //             elsev = try expression(c, node.head.tryExpr.elseExpr, RegisterCstr.initExact(dst, cstr.mustRetain));
    //         } else {
    //             // Error goes directly to dst.
    //             c.buf.setOpArgs1(pushTryPc + 1, dst);
    //             elsev = c.initGenValue(dst, bt.Error, false);
    //         }

    //         c.buf.setOpArgU16(popTryPc + 1, @intCast(c.buf.ops.items.len - popTryPc));

    //         return c.initGenValue(dst, bt.Any, child.retained or elsev.retained);
    //     },
        else => {
            return c.reportErrorAt("Unsupported expression: {}", &.{v(node.node_t)}, nodeId);
        }
    }
}

fn genIdent(c: *cy.Chunk, nodeId: cy.NodeId) !Value {
    const node = c.nodes[nodeId];
    if (c.genGetVar(node.head.ident.semaVarId)) |svar| {
        // csymId would be active instead.
        cy.dassert(svar.type != .staticAlias);

        if (svar.type == .param) {
            const val = llvm.GetParam(c.curBlock.funcRef, svar.inner.paramIdx);
            return Value.init(svar.vtype, val);
        } else {
            return error.Unsupported;
        }

        // var dst: RegisterId = undefined;
        // if (svar.type == .objectMemberAlias) {
        //     dst = try c.rega.selectFromNonLocalVar(cstr, cstr.mustRetain);
        //     const sblock = sema.curBlock(c);
        //     const selfLocal: u8 = @intCast(4 + sblock.params.items.len - 1);
        //     const name = c.getNodeString(node);
        //     const fieldId = try c.compiler.vm.ensureFieldSym(name);
        //     if (cstr.mustRetain) {
        //         try pushFieldRetain(c, selfLocal, dst, @intCast(fieldId));
        //         return c.initGenValue(dst, bt.Any, true);
        //     } else {
        //         const pc = c.compiler.buf.len();
        //         try c.compiler.buf.pushOpSlice(.field, &.{ selfLocal, dst, 0, 0, 0, 0, 0 });
        //         c.compiler.buf.setOpArgU16(pc + 3, @intCast(fieldId));
        //         return c.initGenValue(dst, bt.Any, false);
        //     }
        // } else if (svar.type == .parentObjectMemberAlias) {
        //     dst = try c.rega.selectFromNonLocalVar(cstr, cstr.mustRetain);
        //     try c.buf.pushOp3(.captured, c.curBlock.closureLocal, svar.capturedIdx, dst);
        //     try c.buf.pushOp2(.boxValue, dst, dst);
        //     const name = c.getNodeString(node);
        //     const fieldId = try c.compiler.vm.ensureFieldSym(name);
        //     if (cstr.mustRetain) {
        //         try pushFieldRetain(c, dst, dst, @intCast(fieldId));
        //         return c.initGenValue(dst, bt.Any, true);
        //     } else {
        //         const pc = c.buf.len();
        //         try c.buf.pushOpSlice(.field, &.{ dst, dst, 0, 0, 0, 0, 0 });
        //         c.buf.setOpArgU16(pc + 3, @intCast(fieldId));
        //         return c.initGenValue(dst, bt.Any, false);
        //     }
        // } else if (!svar.isParentLocalAlias()) {
        //     dst = try c.rega.selectFromLocalVar(cstr, svar.local);
        // } else {
        //     dst = try c.rega.selectFromNonLocalVar(cstr, cstr.mustRetain);
        // }
        // if (cstr.mustRetain and types.isRcCandidateType(c.compiler, svar.vtype)) {
        //     // Ensure retain +1.
        //     if (svar.isBoxed) {
        //         if (svar.isParentLocalAlias()) {
        //             try c.buf.pushOp3(.captured, c.curBlock.closureLocal, svar.capturedIdx, dst);
        //             try c.buf.pushOp2(.boxValueRetain, dst, dst);
        //         } else {
        //             try c.buf.pushOp2(.boxValueRetain, svar.local, dst);
        //         }
        //     } else {
        //         if (dst == svar.local) {
        //             try c.buf.pushOp1(.retain, svar.local);
        //         } else {
        //             try c.buf.pushOp2(.copyRetainSrc, svar.local, dst);
        //         }
        //     }
        //     return c.initGenValue(dst, svar.vtype, true);
        // } else {
        //     if (svar.isBoxed) {
        //         if (svar.isParentLocalAlias()) {
        //             try c.buf.pushOp3(.captured, c.curBlock.closureLocal, svar.capturedIdx, dst);
        //             try c.buf.pushOp2(.boxValue, dst, dst);
        //         } else {
        //             try c.buf.pushOp2(.boxValue, svar.local, dst);
        //         }
        //     } else {
        //         if (dst == svar.local) {
        //             // Nop.
        //         } else {
        //             try c.buf.pushOp2(.copy, svar.local, dst);
        //         }
        //     }
        //     return c.initGenValue(dst, svar.vtype, false);
        // }
    } else {
        return genSymbol(c, node.head.ident.sema_csymId);
    }
}

fn genSymbol(self: *cy.Chunk, csymId: sema.CompactSymbolId) !Value {
    _ = self;
    _ = csymId;
    return error.Unsupported;
    // if (csymId.isFuncSymId) {
    //     const rFuncSym = self.compiler.sema.getFuncSym(csymId.id);
    //     const symId = rFuncSym.getSymbolId();
    //     const rSym = self.compiler.sema.getSymbol(symId);

    //     const rtSymId = try self.compiler.vm.ensureFuncSym(rSym.key.resolvedSymKey.parentSymId, rSym.key.resolvedSymKey.nameId, rFuncSym.getFuncSigId());
    //     const pc = self.buf.len();

    //     const dst = try self.rega.selectFromNonLocalVar(req, true);

    //     try self.pushOptionalDebugSym(self.curNodeId);
    //     try self.buf.pushOp3(.staticFunc, 0, 0, dst);
    //     self.buf.setOpArgU16(pc + 1, @intCast(rtSymId));
    //     return self.initGenValue(dst, bt.Any, true);
    // } else {
    //     const rSym = self.compiler.sema.getSymbol(csymId.id);
    //     switch (rSym.symT) {
    //         .variable => {
    //             const key = rSym.key.resolvedSymKey;
    //             const varId = try self.compiler.vm.ensureVarSym(key.parentSymId, key.nameId);

    //             const dst = try self.rega.selectFromNonLocalVar(req, true);
    //             try self.pushOptionalDebugSym(self.curNodeId);       
    //             const pc = self.buf.len();
    //             try self.buf.pushOp3(.staticVar, 0, 0, dst);
    //             self.buf.setOpArgU16(pc + 1, @intCast(varId));

    //             const stype = rSym.inner.variable.rTypeSymId;
    //             return self.initGenValue(dst, stype, true);
    //         },
    //         .builtinType => {
    //             const typeId = rSym.inner.builtinType.typeId;
    //             const dst = try self.rega.selectFromNonLocalVar(req, true);
    //             try self.buf.pushOp1(.sym, @intFromEnum(cy.heap.MetaTypeKind.object));
    //             try self.buf.pushOperandsRaw(std.mem.asBytes(&typeId));
    //             try self.buf.pushOperand(dst);
    //             return self.initGenValue(dst, bt.MetaType, true);
    //         },
    //         .object => {
    //             const typeId = rSym.inner.object.typeId;
    //             const dst = try self.rega.selectFromNonLocalVar(req, true);
    //             try self.buf.pushOp1(.sym, @intFromEnum(cy.heap.MetaTypeKind.object));
    //             try self.buf.pushOperandsRaw(std.mem.asBytes(&typeId));
    //             try self.buf.pushOperand(dst);
    //             return self.initGenValue(dst, bt.MetaType, true);
    //         },
    //         else => {
    //             const name = sema.getName(self.compiler, rSym.key.resolvedSymKey.nameId);
    //             return self.reportError("Can't use symbol `{}` as a value.", &.{v(name)});
    //         }
    //     }
    // }
}

fn genBinExpr(c: *cy.Chunk, nodeId: cy.NodeId) !Value {
    const node = c.nodes[nodeId];
    return genBinExpr2(c, .{
        .leftId = node.head.binExpr.left,
        .rightId = node.head.binExpr.right,
        .debugNodeId = nodeId,
        .op = node.head.binExpr.op,
        .genStrat = node.head.binExpr.semaGenStrat,
    });
}

const GenBinExprOptions = struct {
    leftId: cy.NodeId,
    rightId: cy.NodeId,
    debugNodeId: cy.NodeId,
    op: cy.BinaryExprOp,

    /// Whether to use an existing leftv managed by the caller.
    leftv: ?Value = null,
    rightv: ?Value = null,
};

/// Abstracted to allow assign op to share codegen with binExpr.
fn genBinExpr2(c: *cy.Chunk, opts: GenBinExprOptions) !Value {
    switch (opts.op) {
        // .equal_equal => {
        //     return binExprGeneric(c, .compare, bt.Boolean, opts);
        // },
        // .bang_equal => {
        //     return binExprGeneric(c, .compareNot, bt.Boolean, opts);
        // },
        // .bitwiseAnd,
        // .bitwiseOr,
        // .bitwiseXor,
        // .bitwiseLeftShift,
        // .bitwiseRightShift,
        // .percent,
        // .slash,
        // .star,
        // .caret,
        .minus,
        .plus => {
            const leftT = c.nodeTypes[opts.leftId];
            switch (opts.genStrat) {
                .none => cy.unexpected(),
                .specialized => {
                    const leftv = c.exprResStack.items[c.exprResStack.items.len-2];
                    const rightv = c.exprResStack.items[c.exprResStack.items.len-1];
                    c.exprResStack.items.len -= 2;

                    if (leftT == bt.Float) {
                        if (rightv.typeId != bt.Float) {
                            return error.Unsupported;
                        }
                        return error.Unsupported;
                    } else if (leftT == bt.Integer) {
                        if (rightv.typeId != bt.Integer) {
                            return error.Unsupported;
                        }
                        var val: llvm.ValueRef = undefined;
                        switch (opts.op) {
                            .plus => val = llvm.BuildAdd(c.builder, leftv.val, rightv.val, "add"),
                            .minus => val = llvm.BuildSub(c.builder, leftv.val, rightv.val, "sub"),
                            else => {
                                return error.Unsupported;
                            }
                        }
                        return Value.init(bt.Integer, val);
                    } else {
                        cy.unexpected();
                    }

                    // ARC cleanup.
                    // try releaseIfRetainedTemp(c, rightv);
                },
                .generic => {
                    return error.Unsupported;
                    // return callObjSymBinExpr(c, getInfixMGID(c, opts.op), opts);
                }
            }
        },
        .less_equal,
        .greater,
        .greater_equal,
        .less => {
            const leftT = c.nodeTypes[opts.leftId];
            switch (opts.genStrat) {
                .none => cy.unexpected(),
                .specialized => {
                    const leftv = c.exprResStack.items[c.exprResStack.items.len-2];
                    const rightv = c.exprResStack.items[c.exprResStack.items.len-1];
                    c.exprResStack.items.len -= 2;

                    if (leftT == bt.Float) {
                        if (rightv.typeId != bt.Float) {
                            return error.Unsupported;
                        }
                        const val = llvm.BuildFCmp(c.builder, toRealPredicate(opts.op), leftv.val, rightv.val, "fcmp");
                        return Value.init(bt.Boolean, val);
                    } else if (leftT == bt.Integer) {
                        if (rightv.typeId != bt.Integer) {
                            return error.Unsupported;
                        }
                        const val = llvm.BuildICmp(c.builder, toIntPredicate(opts.op), leftv.val, rightv.val, "icmp");
                        return Value.init(bt.Boolean, val);
                    } else {
                        cy.unexpected();
                    }

                    // ARC cleanup.
                    // try releaseIfRetainedTemp(c, rightv);
                },
                .generic => {
                    // return callObjSymBinExpr(c, getInfixMGID(c, opts.op), opts);
                    return error.Unsupported;
                }
            }
        },
        // .and_op => {
        //     const dst = try c.rega.selectFromNonLocalVar(opts.cstr, true);

        //     var leftv: GenValue = undefined;
        //     if (opts.leftv) |_leftv| {
        //         leftv = _leftv;
        //     } else {
        //         leftv = try expression(c, opts.leftId, RegisterCstr.initExact(dst, opts.cstr.mustRetain));
        //     }

        //     const jumpPc = try c.pushEmptyJumpNotCond(leftv.local);

        //     // ARC cleanup. First operand is not needed anymore.
        //     try releaseIfRetainedTemp(c, leftv);

        //     const rightv = try expression(c, opts.rightId, RegisterCstr.initExact(dst, opts.cstr.mustRetain));
        //     c.patchJumpNotCondToCurPc(jumpPc);

        //     if (leftv.vtype  == rightv.vtype) {
        //         return c.initGenValue(dst, leftv.vtype, leftv.retained or rightv.retained);
        //     } else {
        //         return c.initGenValue(dst, bt.Any, leftv.retained or rightv.retained);
        //     }
        // },
        // .or_op => {
        //     const dst = try c.rega.selectFromNonLocalVar(opts.cstr, true);

        //     var leftv: GenValue = undefined;
        //     if (opts.leftv) |_leftv| {
        //         leftv = _leftv;
        //     } else {
        //         leftv = try expression(c, opts.leftId, RegisterCstr.initExact(dst, opts.cstr.mustRetain));
        //     }
        //     const jumpPc = try c.pushEmptyJumpCond(leftv.local);

        //     // ARC cleanup. First operand is not needed anymore.
        //     try releaseIfRetainedTemp(c, leftv);

        //     const rightv = try expression(c, opts.rightId, RegisterCstr.initExact(dst, opts.cstr.mustRetain));
        //     c.patchJumpCondToCurPc(jumpPc);

        //     if (leftv.vtype == rightv.vtype) {
        //         return c.initGenValue(dst, leftv.vtype, leftv.retained or rightv.retained);
        //     } else {
        //         return c.initGenValue(dst, bt.Any, leftv.retained or rightv.retained);
        //     }
        // },
        else => return c.reportErrorAt("Unsupported binary op: {}", &.{v(opts.op)}, opts.debugNodeId),
    }
}

fn genCallExpr(c: *cy.Chunk, nodeId: cy.NodeId, comptime startFiber: bool, coinitNodeId: if (startFiber) cy.NodeId else void) !Value {
    _ = coinitNodeId;
    // var fiberStartLocal: u8 = undefined;
    // var fiberDst: u8 = undefined;
    // if (startFiber) {
    //     fiberDst = try self.rega.selectFromNonLocalVar(req, true);
    // }
    // var callStartLocal: u8 = undefined;

    // var tempStart = self.rega.getNextTemp();
    // defer {
    //     if (startFiber) {
    //         self.rega.setNextTempAfterOr(fiberDst, tempStart);
    //     } else {
    //         endCall(self, callStartLocal);
    //     }
    // }

    // if (startFiber) {
    //     fiberStartLocal = tempStart;
    //     callStartLocal = 1;
    // } else {
    //     callStartLocal = try beginCall(self, req);
    // }

    const node = c.nodes[nodeId];
    const callee = c.nodes[node.head.callExpr.callee];
    if (!node.head.callExpr.has_named_arg) {
        if (callee.node_t == .accessExpr) {
            // if (callee.head.accessExpr.sema_csymId.isPresent()) {
            //     const csymId = callee.head.accessExpr.sema_csymId;
            //     if (csymId.isFuncSymId) {
            //         const funcSym = self.compiler.sema.getFuncSym(csymId.id);
            //         const rsym = self.compiler.sema.getSymbol(funcSym.getSymbolId());

            //         const funcSigId = funcSym.getFuncSigId();

            //         const start = self.operandStack.items.len;
            //         defer self.operandStack.items.len = start;

            //         // Func sym.
            //         var callArgsRes: CallArgsResult = undefined;
            //         if (funcSym.declId != cy.NullId) {
            //             const rFuncSig = self.compiler.sema.resolvedFuncSigs.items[funcSigId];
            //             callArgsRes = try callArgs2(self, rFuncSig, node.head.callExpr.arg_head);
            //         } else {
            //             callArgsRes = try callArgs(self, node.head.callExpr.arg_head);
            //         }

            //         if (callArgsRes.hasDynamicArgs) {
            //             try genCallTypeCheck(self, callStartLocal + 4, node.head.callExpr.numArgs, funcSigId, nodeId);
            //         }

            //         const key = rsym.key.resolvedSymKey;
            //         const symId = try self.compiler.vm.ensureFuncSym(key.parentSymId, key.nameId, funcSigId);

            //         const retainedTemps = self.operandStack.items[start..];
            //         defer self.popUnwindTempIndexN(@intCast(retainedTemps.len));

            //         try pushCallSym(self, callStartLocal, node.head.callExpr.numArgs, 1, symId, nodeId);
            //         try pushReleases(self, retainedTemps, nodeId);

            //         return GenValue.initTempValue(callStartLocal, funcSym.retType, true);
            //     } else {
            //         return genFuncValueCallExpr(self, nodeId, fiberDst,
            //             if (startFiber) fiberStartLocal else callStartLocal, startFiber);
            //     }
            // } else {
            //     // Dynamic method call.
            //     // Assume left child is a valid reference from sema. Generate callObjSym.
            //     const right = self.nodes[callee.head.accessExpr.right];
            //     std.debug.assert(right.node_t == .ident);
            //     return callObjSym(self, callStartLocal, nodeId);
            // }
            return error.Unsupported;
        } else if (callee.node_t == .ident) {
            if (callee.head.ident.sema_csymId.isPresent()) {
                // Symbol.
                const csymId = callee.head.ident.sema_csymId;
                if (csymId.isFuncSymId) {
                    const numArgs = node.head.callExpr.numArgs;

                    const argvs = c.exprResStack.items[c.exprResStack.items.len-numArgs..];
                    c.exprResStack.items.len -= numArgs;

                    try c.tempValueRefs.resize(c.alloc, argvs.len);
                    for (argvs, 0..) |argv, i| {
                        c.tempValueRefs.items[i] = argv.val;
                    }

                    const funcSym = c.compiler.sema.getFuncSym(csymId.id);
                    const llvmFunc = c.llvmFuncs[funcSym.declId];

                    const val = llvm.BuildCall2(c.builder, llvmFunc.typeRef, llvmFunc.funcRef,
                        c.tempValueRefs.items.ptr, numArgs, "callsym");
                    return Value.init(funcSym.retType, val);
                } else {
                    return error.Unsupported;
                }

                // stdx.debug.dassert(csymId.isPresent());
                // var rFuncSym: ?sema.FuncSym = null;

                // const start = self.operandStack.items.len;
                // defer self.operandStack.items.len = start;

                // if (csymId.isFuncSymId) {
                //     const funcSym = self.compiler.sema.getFuncSym(csymId.id);
                //     const funcSigId = funcSym.getFuncSigId();
                //     rFuncSym = funcSym;

                //     var callArgsRes: CallArgsResult = undefined;
                //     if (funcSym.declId != cy.NullId) {
                //         const rFuncSig = self.compiler.sema.resolvedFuncSigs.items[funcSigId];
                //         callArgsRes = try callArgs2(self, rFuncSig, node.head.callExpr.arg_head);
                //     } else {
                //         callArgsRes = try callArgs(self, node.head.callExpr.arg_head);
                //     }
            
                //     if (callArgsRes.hasDynamicArgs) {
                //         try genCallTypeCheck(self, callStartLocal + 4, node.head.callExpr.numArgs, funcSigId, nodeId);
                //     }
                // } else {
                //     const rsym = self.compiler.sema.getSymbol(csymId.id);
                //     if (rsym.symT == .variable) {
                //         return genFuncValueCallExpr(self, nodeId, fiberDst,
                //             if (startFiber) fiberStartLocal else callStartLocal, startFiber);
                //     } else {
                //         return self.reportErrorAt("Unsupported callee", &.{}, nodeId);
                //     }
                // }

                // const coinitPc = self.buf.ops.items.len;
                // var frameLocSave: cy.NodeId = undefined;
                // if (startFiber) {
                //     // Precompute first arg local since coinit doesn't need the startLocal.
                //     // numArgs + 4 (ret slots) + 1 (min call start local for main block)
                //     var initialStackSize = node.head.callExpr.numArgs + 4 + 1;
                //     if (initialStackSize < 16) {
                //         initialStackSize = 16;
                //     }
                //     try self.pushOptionalDebugSym(nodeId);
                //     try self.buf.pushOpSlice(.coinit, &[_]u8{ fiberStartLocal, node.head.callExpr.numArgs, 0, @intCast(initialStackSize), fiberDst });

                //     // TODO: Have an actual block for fiber.
                //     frameLocSave = self.curBlock.frameLoc;
                //     self.curBlock.frameLoc = coinitNodeId;
                // }

                // if (csymId.isFuncSymId) {
                //     const rtSymId = try self.genEnsureRtFuncSym(csymId.id);
                //     const retainedTemps = self.operandStack.items[start..];
                //     defer self.popUnwindTempIndexN(@intCast(retainedTemps.len));

                //     if (startFiber) {
                //         // Separate stack for fiber block.
                //         try self.unwindTempIndexStack.append(self.alloc, @bitCast(@as(u32, cy.NullId)));
                //         for (retainedTemps) |*operand| {
                //             operand.* = operand.* - fiberStartLocal + callStartLocal + 4;
                //             try self.pushUnwindIndex(operand.*);
                //         }
                //     }
                //     defer if (startFiber) self.popUnwindTempIndexN(@intCast(retainedTemps.len + 1));

                //     try pushCallSym(self, callStartLocal, node.head.callExpr.numArgs, 1, rtSymId, nodeId);
                //     try pushReleases(self, retainedTemps, nodeId);
                // } else {
                //     return self.reportError("Unsupported coinit func call.", &.{});
                // }

                // if (startFiber) {
                //     try self.buf.pushOp(.coreturn);
                //     self.buf.setOpArgs1(coinitPc + 3, @intCast(self.buf.ops.items.len - coinitPc));

                //     self.curBlock.frameLoc = frameLocSave;
                //     return GenValue.initTempValue(fiberDst, bt.Fiber, true);
                // } else {
                //     if (csymId.isPresent()) {
                //         if (rFuncSym) |funcSym| {
                //             const retained = types.isRcCandidateType(self.compiler, funcSym.retType);
                //             return GenValue.initTempValue(callStartLocal, funcSym.retType, retained);
                //         } else {
                //             return GenValue.initTempValue(callStartLocal, bt.Any, true);
                //         }
                //     } else {
                //         return GenValue.initTempValue(callStartLocal, bt.Any, true);
                //     }
                // }
            } else {
                // if (self.genGetVar(callee.head.ident.semaVarId)) |_| {
                //     return genFuncValueCallExpr(self, nodeId, fiberDst,
                //         if (startFiber) fiberStartLocal else callStartLocal, startFiber);

                // }
                return error.Unsupported;
            }
        } else {
            // // All other callees are treated as function value calls.
            // return genFuncValueCallExpr(self, nodeId, fiberDst,
            //     if (startFiber) fiberStartLocal else callStartLocal, startFiber);
            return error.Unsupported;
        }
    } else return c.reportError("Unsupported named args", &.{});
}

fn genExprStmt(c: *cy.Chunk, stmtId: cy.NodeId, retain: bool) !void {
    _ = retain;
    const stmt = c.nodes[stmtId];

    _ = try genExpr(c, stmt.head.child_head);

    // // ARC cleanup.
    // if (!retain) {
    //     try releaseIfRetainedTemp(c, val);
    // }
}

fn toIntPredicate(op: cy.BinaryExprOp) llvm.RealPredicate {
    return switch (op) {
        .less => llvm.IntSLT,
        .greater => llvm.IntSGT,
        .less_equal => llvm.IntSLE,
        .greater_equal => llvm.IntSGE,
        else => cy.fatal(),
    };
}

fn toRealPredicate(op: cy.BinaryExprOp) llvm.RealPredicate {
    return switch (op) {
        .less => llvm.RealULT,
        .greater => llvm.RealUGT,
        .less_equal => llvm.RealULE,
        .greater_equal => llvm.RealUGE,
        else => cy.fatal(),
    };
}

fn toLLVM_FunctionType(self: *cy.Chunk, funcSigId: sema.FuncSigId) !llvm.TypeRef {
    const funcSig = self.compiler.sema.getFuncSig(funcSigId);
    const pTypeIds = funcSig.params();

    try self.tempTypeRefs.resize(self.alloc, pTypeIds.len);
    for (pTypeIds, 0..) |pTypeId, i| {
        const typeRef = try toLLVM_Type(self, pTypeId);
        self.tempTypeRefs.items[i] = typeRef;
    }

    const ret = try toLLVM_Type(self, funcSig.retSymId);
    return llvm.FunctionType(ret, self.tempTypeRefs.items.ptr, @intCast(self.tempTypeRefs.items.len), llvm.False);
}

fn toLLVM_Type(self: *cy.Chunk, typeId: types.TypeId) !llvm.TypeRef {
    switch (typeId) {
        bt.Integer => return llvm.IntTypeInContext(self.ctx, 48),
        else => {
            const name = sema.getSymName(self.compiler, typeId);
            return self.reportError("Unsupported type: {s}, {}", &.{v(name), v(typeId)});
        }
    }
}

fn toTempStrZ(self: *cy.Chunk, str: []const u8) ![*:0]const u8 {
    try self.tempBufU8.resize(self.alloc, str.len + 1);
    @memcpy(self.tempBufU8.items[0..str.len], str);
    self.tempBufU8.items[str.len] = 0;
    return @ptrCast(self.tempBufU8.items.ptr);
}

pub const Value = struct {
    typeId: types.TypeId,
    val: llvm.ValueRef,
    // isTempLocal: bool,

    // /// Whether this value was retained by 1 refcount.
    // retained: bool,

    fn init(typeId: types.TypeId, val: llvm.ValueRef) Value {
        return .{
            .typeId = typeId,
            .val = val,
        };
    }
};