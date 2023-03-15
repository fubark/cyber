const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const cy = @import("cyber.zig");
const sema = cy.sema;
const CompileChunk = cy.CompileChunk;
const fmt = @import("fmt.zig");
const v = fmt.v;
const log = stdx.log.scoped(.codegen);

const LocalId = u8;

pub const GenValue = struct {
    vtype: sema.Type,
    local: LocalId,
    isTempLocal: bool,

    /// Whether this value was retained by 1 refcount.
    retained: bool,

    fn initNoValue() GenValue {
        return .{
            .vtype = undefined,
            .local = 0,
            .isTempLocal = false,
            .retained = false,
        };
    }

    pub fn initLocalValue(local: LocalId, vtype: sema.Type, retained: bool) GenValue {
        return .{
            .vtype = vtype,
            .local = local,
            .isTempLocal = false,
            .retained = retained,
        };
    }

    pub fn initTempValue(local: LocalId, vtype: sema.Type, retained: bool) GenValue {
        return .{
            .vtype = vtype,
            .local = local,
            .isTempLocal = true,
            .retained = retained,
        };
    }

    fn initSemaVar(svar: sema.LocalVar) GenValue {
        return .{
            .vtype = svar.vtype,
            .local = svar.local,
            .isTempLocal = false,
            .retained = false,
        };
    }
};

fn genIdent(self: *CompileChunk, nodeId: cy.NodeId, dst: LocalId, retain: bool) !GenValue {
    const node = self.nodes[nodeId];
    if (self.genGetVar(node.head.ident.semaVarId)) |svar| {
        if (svar.isStaticAlias) {
            return genSymbolTo(self, svar.inner.staticAlias.crSymId, dst, retain);
        } else {
            if (dst == svar.local) {
                if (retain) {
                    stdx.panic("Unexpected retainEscapeTop.");
                }
                return GenValue.initSemaVar(svar);
            } else {
                if (retain and svar.vtype.rcCandidate) {
                    if (svar.isBoxed) {
                        try self.buf.pushOp2(.boxValueRetain, svar.local, dst);
                    } else {
                        try self.buf.pushOp2(.copyRetainSrc, svar.local, dst);
                    }
                    return self.initGenValue(dst, svar.vtype, true);
                } else {
                    if (svar.isBoxed) {
                        try self.buf.pushOp2(.boxValue, svar.local, dst);
                    } else {
                        try self.buf.pushOp2(.copy, svar.local, dst);
                    }
                    return self.initGenValue(dst, svar.vtype, false);
                }
            }
        }
    } else {
        return genSymbolTo(self, node.head.ident.sema_crSymId, dst, retain);
    }
}

fn genSymbolTo(self: *CompileChunk, crSymId: sema.CompactResolvedSymId, dst: LocalId, retain: bool) !GenValue {
    _ = retain;
    if (crSymId.isFuncSymId) {
        const rFuncSym = self.compiler.sema.getResolvedFuncSym(crSymId.id);
        const rSymId = rFuncSym.getResolvedSymId();
        const rSym = self.compiler.sema.getResolvedSym(rSymId);

        const rtSymId = try self.compiler.vm.ensureFuncSym(rSym.key.absResolvedSymKey.rParentSymId, rSym.key.absResolvedSymKey.nameId, rFuncSym.getResolvedFuncSigId());
        try self.buf.pushOp2(.staticFunc, @intCast(u8, rtSymId), dst);
        return self.initGenValue(dst, sema.AnyType, true);
    } else {
        const rSym = self.compiler.sema.getResolvedSym(crSymId.id);
        if (rSym.symT == .variable) {
            const key = rSym.key.absResolvedSymKey;
            const varId = try self.compiler.vm.ensureVarSym(key.rParentSymId, key.nameId);

            try self.pushOptionalDebugSym(self.curNodeId);       
            try self.buf.pushOp2(.staticVar, @intCast(u8, varId), dst);
            return self.initGenValue(dst, sema.AnyType, true);
        } else if (rSym.symT == .object) {
            const typeId = rSym.getObjectTypeId(self.compiler.vm).?;
            try self.buf.pushOp1(.sym, @enumToInt(cy.heap.SymbolType.object));
            try self.buf.pushOperandsRaw(std.mem.asBytes(&typeId));
            try self.buf.pushOperand(dst);
            return self.initGenValue(dst, sema.AnyType, true);
        } else {
            const name = sema.getName(self.compiler, rSym.key.absResolvedSymKey.nameId);
            return self.reportError("Can't use symbol `{}` as a value.", &.{v(name)});
        }
    }
}

fn genStringTemplate(self: *CompileChunk, nodeId: cy.NodeId, dst: LocalId, comptime dstIsUsed: bool) !GenValue {
    const node = self.nodes[nodeId];
    const operandStart = self.operandStack.items.len;
    defer self.operandStack.items.len = operandStart;

    const startTempLocal = self.curBlock.firstFreeTempLocal;
    defer self.computeNextTempLocalFrom(startTempLocal);

    const argStartLocal = self.advanceNextTempLocalPastReservedTemps();

    var expStringPart = true;
    var curId = node.head.stringTemplate.partsHead;
    var numExprs: u32 = 0;
    while (curId != cy.NullId) {
        const cur = self.nodes[curId];
        if (expStringPart) {
            if (dstIsUsed) {
                const raw = self.getNodeTokenString(cur);
                const str = try self.unescapeString(raw);
                const idx = try self.buf.getOrPushStringConst(str);
                try self.operandStack.append(self.alloc, cy.OpData.initArg(@intCast(u8, idx)));
            }
        } else {
            _ = try self.genRetainedTempExpr(curId, !dstIsUsed);
            numExprs += 1;
        }
        curId = cur.next;
        expStringPart = !expStringPart;
    }

    if (dstIsUsed) {
        try self.buf.pushOp3(.stringTemplate, argStartLocal, @intCast(u8, numExprs), dst);
        try self.buf.pushOperands(self.operandStack.items[operandStart..]);
        return self.initGenValue(dst, sema.StringType, true);
    } else {
        return GenValue.initNoValue();
    }
}

fn genObjectInit(self: *CompileChunk, nodeId: cy.NodeId, dst: LocalId, retain: bool, comptime dstIsUsed: bool) !GenValue {
    _ = retain;
    const node = self.nodes[nodeId];
    const stype = self.nodes[node.head.objectInit.name];

    if (node.head.objectInit.sema_rSymId != cy.NullId) {
        const rSym = self.compiler.sema.getResolvedSym(node.head.objectInit.sema_rSymId);
        if (rSym.symT == .object) {
            const typeId = rSym.getObjectTypeId(self.compiler.vm).?;
            const initializer = self.nodes[node.head.objectInit.initializer];

            // TODO: Would it be faster/efficient to copy the fields into contiguous registers
            //       and copy all at once to heap or pass locals into the operands and iterate each and copy to heap?
            //       The current implementation is the former.
            // TODO: Have sema sort the fields so eval can handle default values easier.

            // Push props onto stack.

            const numFields = self.compiler.vm.structs.buf[typeId].numFields;
            // Repurpose stack for sorting fields.
            const sortedFieldsStart = self.assignedVarStack.items.len;
            try self.assignedVarStack.resize(self.alloc, self.assignedVarStack.items.len + numFields);
            defer self.assignedVarStack.items.len = sortedFieldsStart;

            // Initially set to NullId so leftovers are defaulted to `none`.
            const initFields = self.assignedVarStack.items[sortedFieldsStart..];
            std.mem.set(u32, initFields, cy.NullId);

            const startTempLocal = self.curBlock.firstFreeTempLocal;
            defer self.computeNextTempLocalFrom(startTempLocal);

            const argStartLocal = self.advanceNextTempLocalPastReservedTemps();

            var i: u32 = 0;
            var entryId = initializer.head.child_head;
            // First iteration to sort the initializer fields.
            while (entryId != cy.NullId) : (i += 1) {
                const entry = self.nodes[entryId];
                const prop = self.nodes[entry.head.mapEntry.left];
                const fieldName = self.getNodeTokenString(prop);
                const fieldIdx = self.compiler.vm.getStructFieldIdx(typeId, fieldName) orelse {
                    const objectName = self.compiler.vm.structs.buf[typeId].name;
                    return self.reportErrorAt("Missing field `{}` in `{}`.", &.{v(fieldName), v(objectName)}, entry.head.mapEntry.left);
                };
                initFields[fieldIdx] = entryId;
                entryId = entry.next;
            }

            i = 0;
            while (i < numFields) : (i += 1) {
                entryId = self.assignedVarStack.items[sortedFieldsStart + i];
                if (entryId == cy.NullId) {
                    if (dstIsUsed) {
                        // Push none.
                        const local = try self.nextFreeTempLocal();
                        try self.buf.pushOp1(.none, local);
                    }
                } else {
                    const entry = self.nodes[entryId];
                    _ = try self.genRetainedTempExpr(entry.head.mapEntry.right, !dstIsUsed);
                }
            }

            if (dstIsUsed) {
                if (self.compiler.vm.structs.buf[typeId].numFields <= 4) {
                    try self.pushOptionalDebugSym(nodeId);
                    try self.buf.pushOpSlice(.objectSmall, &.{ @intCast(u8, typeId), argStartLocal, @intCast(u8, numFields), dst });
                } else {
                    try self.buf.pushOpSlice(.object, &.{ @intCast(u8, typeId), argStartLocal, @intCast(u8, numFields), dst });
                }
                return GenValue.initTempValue(dst, sema.AnyType, true);
            } else {
                return GenValue.initNoValue();
            }
        }
    }
    const oname = self.getNodeTokenString(stype);
    return self.reportErrorAt("Expected object type: `{}`", &.{v(oname)}, nodeId);
}

fn genMapInit(self: *CompileChunk, nodeId: cy.NodeId, dst: LocalId, comptime dstIsUsed: bool) !GenValue {
    const node = self.nodes[nodeId];
    const operandStart = self.operandStack.items.len;
    defer self.operandStack.items.len = operandStart;

    const startTempLocal = self.curBlock.firstFreeTempLocal;
    defer self.computeNextTempLocalFrom(startTempLocal);

    const argStartLocal = self.advanceNextTempLocalPastReservedTemps();

    var i: u32 = 0;
    var entry_id = node.head.child_head;
    while (entry_id != cy.NullId) : (i += 1) {
        var entry = self.nodes[entry_id];
        const key = self.nodes[entry.head.mapEntry.left];

        if (dstIsUsed) {
            switch (key.node_t) {
                .ident => {
                    const name = self.getNodeTokenString(key);
                    const idx = try self.buf.getOrPushStringConst(name);
                    try self.operandStack.append(self.alloc, cy.OpData.initArg(@intCast(u8, idx)));
                },
                .string => {
                    const name = self.getNodeTokenString(key);
                    const idx = try self.buf.getOrPushStringConst(name);
                    try self.operandStack.append(self.alloc, cy.OpData.initArg(@intCast(u8, idx)));
                },
                else => stdx.panicFmt("unsupported key {}", .{key.node_t}),
            }
        }

        _ = try self.genRetainedTempExpr(entry.head.mapEntry.right, !dstIsUsed);
        entry_id = entry.next;
    }

    if (dstIsUsed) {
        if (i == 0) {
            try self.buf.pushOp1(.mapEmpty, dst);
        } else {
            try self.buf.pushOp3(.map, argStartLocal, @intCast(u8, i), dst);
            try self.buf.pushOperands(self.operandStack.items[operandStart..]);
        }
        return self.initGenValue(dst, sema.MapType, true);
    } else {
        return GenValue.initNoValue();
    }
}

fn genPushBinOp(self: *CompileChunk, code: cy.OpCode, left: cy.NodeId, right: cy.NodeId, vtype: sema.Type, dst: LocalId, comptime discardTopExprReg: bool) !GenValue {
    const startTempLocal = self.curBlock.firstFreeTempLocal;
    defer self.computeNextTempLocalFrom(startTempLocal);

    var usedDstAsTemp = !self.canUseLocalAsTemp(dst);
    const leftv = try self.genExprToDestOrTempLocal(left, dst, &usedDstAsTemp, discardTopExprReg);
    const rightv = try self.genExprToDestOrTempLocal(right, dst, &usedDstAsTemp, discardTopExprReg);
    if (!discardTopExprReg) {
        switch (code) {
            .mod,
            .pow,
            .div,
            .mul => {
                try self.pushDebugSym(left);
            },
            else => {},
        }
        try self.buf.pushOp3(code, leftv.local, rightv.local, dst);

        // ARC cleanup.
        try genReleaseIfRetainedTemp(self, leftv);
        try genReleaseIfRetainedTemp(self, rightv);

        return self.initGenValue(dst, vtype, false);
    } else return GenValue.initNoValue();
}

fn genBinExpr(self: *CompileChunk, nodeId: cy.NodeId, dst: LocalId, requestedType: sema.Type, retain: bool, comptime dstIsUsed: bool) !GenValue {
    const node = self.nodes[nodeId];
    const left = node.head.binExpr.left;
    const right = node.head.binExpr.right;

    const op = node.head.binExpr.op;
    switch (op) {
        .slash => {
            return genPushBinOp(self, .div, left, right, sema.NumberType, dst, !dstIsUsed);
        },
        .percent => {
            return genPushBinOp(self, .mod, left, right, sema.NumberType, dst, !dstIsUsed);
        },
        .caret => {
            return genPushBinOp(self, .pow, left, right, sema.NumberType, dst, !dstIsUsed);
        },
        .star => {
            return genPushBinOp(self, .mul, left, right, sema.NumberType, dst, !dstIsUsed);
        },
        .plus => {
            const startTempLocal = self.curBlock.firstFreeTempLocal;
            defer self.computeNextTempLocalFrom(startTempLocal);

            var usedDstAsTemp = !self.canUseLocalAsTemp(dst);
            const leftv = try self.genExprToDestOrTempLocal2(requestedType, left, dst, &usedDstAsTemp, !dstIsUsed);
            const rightv = try self.genExprToDestOrTempLocal2(requestedType, right, dst, &usedDstAsTemp, !dstIsUsed);
            if (dstIsUsed) {
                if (leftv.vtype.typeT == .int and rightv.vtype.typeT == .int) {
                    try self.buf.pushOp3(.addInt, leftv.local, rightv.local, dst);
                    return self.initGenValue(dst, sema.IntegerType, false);
                }
                try self.pushDebugSym(nodeId);
                try self.buf.pushOp3(.add, leftv.local, rightv.local, dst);
                return self.initGenValue(dst, sema.NumberType, false);
            } else {
                return GenValue.initNoValue();
            }
        },
        .minus => {
            const startTempLocal = self.curBlock.firstFreeTempLocal;
            defer self.computeNextTempLocalFrom(startTempLocal);

            var usedDstAsTemp = !self.canUseLocalAsTemp(dst);
            const leftv = try self.genExprToDestOrTempLocal2(requestedType, left, dst, &usedDstAsTemp, !dstIsUsed);
            const rightv = try self.genExprToDestOrTempLocal2(requestedType, right, dst, &usedDstAsTemp, !dstIsUsed);
            if (dstIsUsed) {
                if (leftv.vtype.typeT == .int and rightv.vtype.typeT == .int) {
                    try self.buf.pushOp3(.subInt, leftv.local, rightv.local, dst);
                    return self.initGenValue(dst, sema.IntegerType, false);
                }
                try self.pushDebugSym(nodeId);
                try self.buf.pushOp3(.sub, leftv.local, rightv.local, dst);
                return self.initGenValue(dst, sema.NumberType, false);
            } else return GenValue.initNoValue();
        },
        .equal_equal => {
            return genPushBinOp(self, .compare, left, right, sema.BoolType, dst, !dstIsUsed);
        },
        .bang_equal => {
            return genPushBinOp(self, .compareNot, left, right, sema.BoolType, dst, !dstIsUsed);
        },
        .less => {
            const startTempLocal = self.curBlock.firstFreeTempLocal;
            defer self.computeNextTempLocalFrom(startTempLocal);

            var usedDstAsTemp = !self.canUseLocalAsTemp(dst);
            var leftv: GenValue = undefined;
            var rightv: GenValue = undefined;
            if (node.head.binExpr.semaCanRequestIntegerOperands) {
                leftv = try self.genExprToDestOrTempLocal2(sema.IntegerType, left, dst, &usedDstAsTemp, !dstIsUsed);
                rightv = try self.genExprToDestOrTempLocal2(sema.IntegerType, right, dst, &usedDstAsTemp, !dstIsUsed);
            } else {
                leftv = try self.genExprToDestOrTempLocal(left, dst, &usedDstAsTemp, !dstIsUsed);
                rightv = try self.genExprToDestOrTempLocal(right, dst, &usedDstAsTemp, !dstIsUsed);
            }
            if (dstIsUsed) {
                if (node.head.binExpr.semaCanRequestIntegerOperands) {
                    try self.buf.pushOp3(.lessInt, leftv.local, rightv.local, dst);
                    return self.initGenValue(dst, sema.BoolType, false);
                }

                try self.buf.pushOp3(.less, leftv.local, rightv.local, dst);
                return self.initGenValue(dst, sema.BoolType, false);
            } else return GenValue.initNoValue();
        },
        .less_equal => {
            return genPushBinOp(self, .lessEqual, left, right, sema.BoolType, dst, !dstIsUsed);
        },
        .greater => {
            return genPushBinOp(self, .greater, left, right, sema.BoolType, dst, !dstIsUsed);
        },
        .greater_equal => {
            return genPushBinOp(self, .greaterEqual, left, right, sema.BoolType, dst, !dstIsUsed);
        },
        .bitwiseAnd => {
            return genPushBinOp(self, .bitwiseAnd, left, right, sema.NumberType, dst, !dstIsUsed);
        },
        .bitwiseOr => {
            return genPushBinOp(self, .bitwiseOr, left, right, sema.NumberType, dst, !dstIsUsed);
        },
        .bitwiseXor => {
            return genPushBinOp(self, .bitwiseXor, left, right, sema.NumberType, dst, !dstIsUsed);
        },
        .bitwiseLeftShift => {
            return genPushBinOp(self, .bitwiseLeftShift, left, right, sema.NumberType, dst, !dstIsUsed);
        },
        .bitwiseRightShift => {
            return genPushBinOp(self, .bitwiseRightShift, left, right, sema.NumberType, dst, !dstIsUsed);
        },
        .and_op => {
            const startTempLocal = self.curBlock.firstFreeTempLocal;
            defer self.computeNextTempLocalFrom(startTempLocal);

            const leftv = try self.genExprTo(left, dst, retain, !dstIsUsed);
            const jumpPc = try self.pushEmptyJumpNotCond(leftv.local);

            // ARC cleanup. First operand is not needed anymore.
            if (!retain) {
                try genReleaseIfRetainedTemp(self, leftv);
            }

            const rightv = try self.genExprTo(right, dst, retain, !dstIsUsed);
            self.patchJumpToCurrent(jumpPc);

            if (leftv.vtype.typeT == rightv.vtype.typeT) {
                return self.initGenValue(dst, leftv.vtype, retain and leftv.vtype.rcCandidate);
            } else {
                return self.initGenValue(dst, sema.AnyType, retain);
            }
        },
        .or_op => {
            const startTempLocal = self.curBlock.firstFreeTempLocal;
            defer self.computeNextTempLocalFrom(startTempLocal);

            const leftv = try self.genExprTo(left, dst, retain, !dstIsUsed);
            const jumpPc = try self.pushEmptyJumpCond(leftv.local);

            // ARC cleanup. First operand is not needed anymore.
            if (!retain) {
                try genReleaseIfRetainedTemp(self, leftv);
            }

            const rightv = try self.genExprTo(right, dst, retain, !dstIsUsed);
            self.patchJumpToCurrent(jumpPc);

            if (leftv.vtype.typeT == rightv.vtype.typeT) {
                return self.initGenValue(dst, leftv.vtype, retain and leftv.vtype.rcCandidate);
            } else return self.initGenValue(dst, sema.AnyType, retain);
        },
        else => return self.reportErrorAt("Unsupported binary op: {}", &.{fmt.v(op)}, nodeId),
    }
}

fn genLambdaMulti(self: *CompileChunk, nodeId: cy.NodeId, dst: LocalId, comptime dstIsUsed: bool) !GenValue {
    const node = self.nodes[nodeId];
    if (dstIsUsed) {
        const jumpPc = try self.pushEmptyJump();

        const func = self.semaFuncDecls.items[node.head.func.semaDeclId];

        try self.pushSemaBlock(func.semaBlockId);
        self.curBlock.frameLoc = nodeId;

        const jumpStackStart = self.blockJumpStack.items.len;
        const opStart = @intCast(u32, self.buf.ops.items.len);

        // Generate function body.
        try self.reserveFuncParams(func.numParams);
        try genInitLocals(self);

        try genStatements(self, node.head.func.bodyHead, false);

        try self.genBlockEnding();
        self.patchJumpToCurrent(jumpPc);

        const sblock = sema.curBlock(self);
        const numLocals = @intCast(u8, self.curBlock.numLocals + self.curBlock.numTempLocals);
        const numCaptured = @intCast(u8, sblock.params.items.len - func.numParams);

        self.patchBlockJumps(jumpStackStart);
        self.blockJumpStack.items.len = jumpStackStart;

        self.popSemaBlock();

        if (numCaptured == 0) {
            const funcPcOffset = @intCast(u8, self.buf.ops.items.len - opStart);
            try self.pushOptionalDebugSym(nodeId);

            const start = self.buf.ops.items.len;
            try self.buf.pushOpSlice(.lambda, &.{ funcPcOffset, func.numParams, numLocals, 0, 0, dst });
            self.buf.setOpArgU16(start + 4, @intCast(u16, func.inner.lambda.rFuncSigId));
            return self.initGenValue(dst, sema.AnyType, true);
        } else {
            const operandStart = self.operandStack.items.len;
            defer self.operandStack.items.len = operandStart;

            // Retain captured vars.
            for (sblock.params.items) |varId| {
                const svar = self.vars.items[varId];
                if (svar.isCaptured) {
                    const pId = self.capVarDescs.get(varId).?.user;
                    const pvar = &self.vars.items[pId];

                    if (svar.isBoxed) {
                        try self.buf.pushOp1(.retain, pvar.local);
                        try self.pushTempOperand(pvar.local);
                    }
                }
            }

            const funcPcOffset = @intCast(u8, self.buf.ops.items.len - opStart);
            const start = self.buf.ops.items.len;
            try self.buf.pushOpSlice(.closure, &.{ funcPcOffset, func.numParams, numCaptured, numLocals, 0, 0, dst });
            self.buf.setOpArgU16(start + 5, @intCast(u16, func.inner.lambda.rFuncSigId));
            try self.buf.pushOperands(self.operandStack.items[operandStart..]);
            return self.initGenValue(dst, sema.AnyType, true);
        }
    } else {
        return GenValue.initNoValue();
    }
}

fn genLambdaExpr(self: *CompileChunk, nodeId: cy.NodeId, dst: LocalId, comptime dstIsUsed: bool) !GenValue {
    const node = self.nodes[nodeId];
    if (dstIsUsed) {
        const jumpPc = try self.pushEmptyJump();

        const func = self.semaFuncDecls.items[node.head.func.semaDeclId];
        try self.pushSemaBlock(func.semaBlockId);
        const opStart = @intCast(u32, self.buf.ops.items.len);

        // Generate function body.
        try self.reserveFuncParams(func.numParams);
        try genInitLocals(self);
        _ = try self.genRetainedExprTo(node.head.func.bodyHead, 0, false);
        try self.endLocals();
        try self.buf.pushOp(.ret1);
        self.patchJumpToCurrent(jumpPc);

        const sblock = sema.curBlock(self);
        const numLocals = @intCast(u8, self.curBlock.numLocals + self.curBlock.numTempLocals);
        const numCaptured = @intCast(u8, sblock.params.items.len - func.numParams);

        self.popSemaBlock();

        if (numCaptured == 0) {
            const funcPcOffset = @intCast(u8, self.buf.ops.items.len - opStart);
            try self.pushOptionalDebugSym(nodeId);
            const start = self.buf.ops.items.len;
            try self.buf.pushOpSlice(.lambda, &.{ funcPcOffset, func.numParams, numLocals, 0, 0, dst });
            self.buf.setOpArgU16(start + 4, @intCast(u16, func.inner.lambda.rFuncSigId));
            return self.initGenValue(dst, sema.AnyType, true);
        } else {
            const operandStart = self.operandStack.items.len;
            defer self.operandStack.items.len = operandStart;

            // Retain captured vars.
            for (sblock.params.items) |varId| {
                const svar = self.vars.items[varId];
                if (svar.isCaptured) {
                    const pId = self.capVarDescs.get(varId).?.user;
                    const pvar = &self.vars.items[pId];

                    if (svar.isBoxed) {
                        try self.buf.pushOp1(.retain, pvar.local);
                        try self.pushTempOperand(pvar.local);
                    }
                }
            }

            const funcPcOffset = @intCast(u8, self.buf.ops.items.len - opStart);
            const start = self.buf.ops.items.len;
            try self.buf.pushOpSlice(.closure, &.{ funcPcOffset, func.numParams, numCaptured, numLocals, 0, 0, dst });
            self.buf.setOpArgU16(start + 5, @intCast(u16, func.inner.lambda.rFuncSigId));
            try self.buf.pushOperands(self.operandStack.items[operandStart..]);
            return self.initGenValue(dst, sema.AnyType, true);
        }
    } else {
        return GenValue.initNoValue();
    }
}

fn genListInit(self: *CompileChunk, nodeId: cy.NodeId, dst: LocalId, comptime dstIsUsed: bool) !GenValue {
    const node = self.nodes[nodeId];
    const startLocal = self.curBlock.firstFreeTempLocal;
    defer self.computeNextTempLocalFrom(startLocal);

    const argStartLocal = self.advanceNextTempLocalPastReservedTemps();

    var exprId = node.head.child_head;
    var i: u32 = 0;
    while (exprId != cy.NullId) : (i += 1) {
        const expr = self.nodes[exprId];
        _ = try self.genRetainedTempExpr(exprId, !dstIsUsed);
        exprId = expr.next;
    }

    if (dstIsUsed) {
        try self.pushDebugSym(nodeId);
        try self.buf.pushOp3(.list, argStartLocal, @intCast(u8, i), dst);
        return self.initGenValue(dst, sema.ListType, true);
    } else {
        return GenValue.initNoValue();
    }
}

fn genRangeAccessExpr(self: *CompileChunk, nodeId: cy.NodeId, dst: LocalId, retain: bool, comptime dstIsUsed: bool) !GenValue {
    _ = retain;
    const node = self.nodes[nodeId];
    const startTempLocal = self.curBlock.firstFreeTempLocal;
    defer self.computeNextTempLocalFrom(startTempLocal);

    var usedDstAsTemp = !self.canUseLocalAsTemp(dst);

    // Parent value.
    const parentv = try self.genExprToDestOrTempLocal(node.head.arr_range_expr.arr, dst, &usedDstAsTemp, !dstIsUsed);

    // Range left value.
    var leftv: GenValue = undefined;
    if (node.head.arr_range_expr.left == cy.NullId) {
        if (dstIsUsed) {
            if (usedDstAsTemp) {
                const leftDst = try self.nextFreeTempLocal();
                leftv = try genConstNumber(self, 0, leftDst);
            } else {
                leftv = try genConstNumber(self, 0, dst);
                usedDstAsTemp = true;
            }
        }
    } else {
        leftv = try self.genExprToDestOrTempLocal(node.head.arr_range_expr.left, dst, &usedDstAsTemp, !dstIsUsed);
    }

    // Range right value.
    var rightv: GenValue = undefined;
    if (node.head.arr_range_expr.right == cy.NullId) {
        if (dstIsUsed) {
            if (usedDstAsTemp) {
                const rightDst = try self.nextFreeTempLocal();
                rightv = try genNone(self, rightDst);
            } else {
                rightv = try genNone(self, dst);
                usedDstAsTemp = true;
            }
        }
    } else {
        rightv = try self.genExprToDestOrTempLocal(node.head.arr_range_expr.right, dst, &usedDstAsTemp, !dstIsUsed);
    }

    if (dstIsUsed) {
        try self.buf.pushOpSlice(.slice, &.{ parentv.local, leftv.local, rightv.local, dst });

        // ARC cleanup.
        try genReleaseIfRetainedTemp(self, parentv);

        return self.initGenValue(dst, sema.ListType, true);
    } else {
        return GenValue.initNoValue();
    }
}

fn genIndexAccessExpr(self: *CompileChunk, nodeId: cy.NodeId, dst: LocalId, comptime dstIsUsed: bool) !GenValue {
    const node = self.nodes[nodeId];
    const startTempLocal = self.curBlock.firstFreeTempLocal;
    defer self.computeNextTempLocalFrom(startTempLocal);

    var usedDstAsTemp = !self.canUseLocalAsTemp(dst);

    // Gen left.
    const leftv = try self.genExprPreferLocalOrReplaceableDest(node.head.left_right.left, dst, &usedDstAsTemp, !dstIsUsed);

    // Gen index.
    const index = self.nodes[node.head.left_right.right];
    const isReverseIndex = index.node_t == .unary_expr and index.head.unary.op == .minus;
    const indexv = try self.genExprPreferLocalOrReplaceableDest(node.head.left_right.right, dst, &usedDstAsTemp, !dstIsUsed);

    if (dstIsUsed) {
        try self.pushDebugSym(nodeId);
        if (isReverseIndex) {
            try self.buf.pushOp3(.reverseIndex, leftv.local, indexv.local, dst);
        } else {
            try self.buf.pushOp3(.index, leftv.local, indexv.local, dst);
        }

        // ARC cleanup.
        if (leftv.local != dst) {
            try genReleaseIfRetainedTemp(self, leftv);
        }
        if (indexv.local != dst) {
            try genReleaseIfRetainedTemp(self, indexv);
        }

        return self.initGenValue(dst, sema.AnyType, true);
    } else {
        return GenValue.initNoValue();
    }
}

fn genAccessExpr(self: *CompileChunk, nodeId: cy.NodeId, dst: LocalId, retain: bool, comptime dstIsUsed: bool) !GenValue {
    const node = self.nodes[nodeId];
    const crSymId = node.head.accessExpr.sema_crSymId;
    if (crSymId.isPresent()) {
        if (crSymId.isFuncSymId) {
            const rFuncSym = self.compiler.sema.getResolvedFuncSym(crSymId.id);
            const rSym = self.compiler.sema.getResolvedSym(rFuncSym.getResolvedSymId());
            const key = rSym.key.absResolvedSymKey;
            const rtSymId = try self.compiler.vm.ensureFuncSym(key.rParentSymId, key.nameId, rFuncSym.getResolvedFuncSigId());
            try self.buf.pushOp2(.staticFunc, @intCast(u8, rtSymId), dst);
            return self.initGenValue(dst, sema.AnyType, true);
        } else {
            const rSym = self.compiler.sema.getResolvedSym(crSymId.id);
            const key = rSym.key.absResolvedSymKey;
            if (rSym.symT == .variable) {
                const rtSymId = self.compiler.vm.getVarSym(key.rParentSymId, key.nameId).?;
                if (dstIsUsed) {
                    // Static variable.
                    try self.pushOptionalDebugSym(nodeId);       
                    try self.buf.pushOp2(.staticVar, @intCast(u8, rtSymId), dst);
                    return self.initGenValue(dst, sema.AnyType, true);
                } else {
                    return GenValue.initNoValue();
                }
            }
        }
    }
    return genField(self, node.head.accessExpr.left, node.head.accessExpr.right, dst, retain, dstIsUsed, nodeId);
}

fn genReleaseIfRetainedTempAt(self: *cy.CompileChunk, val: GenValue, nodeId: cy.NodeId) !void {
    if (val.retained and val.isTempLocal) {
        try self.pushOptionalDebugSym(nodeId);
        try self.buf.pushOp1(.release, val.local);
    }
}

fn genReleaseIfRetainedTemp(self: *cy.CompileChunk, val: GenValue) !void {
    return genReleaseIfRetainedTempAt(self, val, self.curNodeId);
}

fn genField(self: *cy.CompileChunk, leftId: cy.NodeId, rightId: cy.NodeId, dst: LocalId, retain: bool, comptime dstIsUsed: bool, debugNodeId: cy.NodeId) !GenValue {
    const startTempLocal = self.curBlock.firstFreeTempLocal;
    defer self.computeNextTempLocalFrom(startTempLocal);

    var usedDstAsTemp = !self.canUseLocalAsTemp(dst);

    // Left side.
    const leftv = try self.genExprPreferLocalOrReplaceableDest(leftId, dst, &usedDstAsTemp, !dstIsUsed);

    // Right should be an ident.
    const right = self.nodes[rightId];
    const name = self.getNodeTokenString(right);
    const fieldId = try self.compiler.vm.ensureFieldSym(name);

    if (dstIsUsed) {
        try self.pushDebugSym(debugNodeId);
        const leftIsTempRetained = leftv.retained and leftv.isTempLocal;
        if (retain or leftIsTempRetained) {
            try self.buf.pushOpSlice(.fieldRetain, &.{ leftv.local, dst, @intCast(u8, fieldId), 0, 0, 0 });

            // ARC cleanup.
            try genReleaseIfRetainedTemp(self, leftv);

            return self.initGenValue(dst, sema.AnyType, true);
        } else {
            try self.buf.pushOpSlice(.field, &.{ leftv.local, dst, @intCast(u8, fieldId), 0, 0, 0 });

            // ARC cleanup.
            try genReleaseIfRetainedTemp(self, leftv);

            return self.initGenValue(dst, sema.AnyType, false);
        }
    } else {
        return GenValue.initNoValue();
    }
}

/// Ensures that the resulting expression is copied to `dst`.
/// This does not care about what is already at `dst`, so assign statements need to consider that (for ARC correctness).
/// Most common usage is to copy to a temp or reserved temp `dst`.
/// Since assignments is a special case, dealing with the type of `dst` won't be included here.
///
/// `dst` indicates the local of the resulting value.
/// `retain` indicates the resulting val should be retained by 1 refcount. (eg. call args)
///    If the expr already does a +1 retain (eg. map literal) upon evaluation, nothing more needs to happen.
///    If the expr does not retain itself (eg. access expr), the generated code depends on additional criteria.
///    If `retain` is false, `dst` is a temp local, and the expr does a +1 retain (eg. call expr with any return);
///        it is added as an arcTempLocal to be released at the end of the arcTemp segment.
/// `dstIsUsed` provides a hint that the parent caller of `genExprTo2` intends to use `dst`. (eg. `false` for an expr statement)
///    If `dstIsUsed` is false and the current expr does not contain side-effects, it can omit generating its code.
pub fn genExprTo2(self: *CompileChunk, nodeId: cy.NodeId, dst: LocalId, requestedType: sema.Type, retain: bool, comptime dstIsUsed: bool) anyerror!GenValue {
    const node = self.nodes[nodeId];
    self.curNodeId = nodeId;
    // log.debug("gen reg expr {}", .{node.node_t});
    switch (node.node_t) {
        .ident => {
            return try genIdent(self, nodeId, dst, retain);
        },
        .true_literal => {
            if (dstIsUsed) {
                try self.buf.pushOp1(.true, dst);
                return self.initGenValue(dst, sema.BoolType, false);
            } else return GenValue.initNoValue();
        },
        .false_literal => {
            if (dstIsUsed) {
                try self.buf.pushOp1(.false, dst);
                return self.initGenValue(dst, sema.BoolType, false);
            } else return GenValue.initNoValue();
        },
        .number => {
            if (dstIsUsed) {
                const literal = self.getNodeTokenString(node);
                const val = try std.fmt.parseFloat(f64, literal);
                if (requestedType.typeT == .int) {
                    return try genConstInt(self, val, dst);
                } else {
                    return try genConstNumber(self, val, dst);
                }
            } else {
                return GenValue.initNoValue();
            }
        },
        .nonDecInt => {
            if (dstIsUsed) {
                const fval = node.head.nonDecInt.semaNumberVal;
                if (requestedType.typeT == .int) {
                    return try genConstInt(self, fval, dst);
                } else {
                    return try genConstNumber(self, fval, dst);
                }
            } else {
                return GenValue.initNoValue();
            }
        },
        .tagLiteral => {
            const name = self.getNodeTokenString(node);
            const symId = try self.compiler.vm.ensureTagLitSym(name);
            try self.buf.pushOp2(.tagLiteral, @intCast(u8, symId), dst);
            return self.initGenValue(dst, sema.TagLiteralType, false);
        },
        .string => {
            if (dstIsUsed) {
                const literal = self.getNodeTokenString(node);
                const str = try self.unescapeString(literal);
                return genString(self, str, dst);
            } else {
                return GenValue.initNoValue();
            }
        },
        .stringTemplate => {
            return genStringTemplate(self, nodeId, dst, dstIsUsed);
        },
        .none => {
            if (dstIsUsed) {
                return genNone(self, dst);
            } else return GenValue.initNoValue();
        },
        .tagInit => {
            const name = self.nodes[node.head.left_right.left];
            const tname = self.getNodeTokenString(name);
            const tid = self.compiler.vm.tagTypeSignatures.get(tname) orelse {
                return self.reportErrorAt("Missing tag type: `{}`", &.{v(tname)}, nodeId);
            };

            const tagLit = self.nodes[node.head.left_right.right];
            const lname = self.getNodeTokenString(tagLit);
            const symId = self.compiler.vm.tagLitSymSignatures.get(lname) orelse {
                return self.reportErrorAt("Missing tag literal: `{}`", &.{v(lname)}, nodeId);
            };
            const sym = self.compiler.vm.tagLitSyms.buf[symId];
            if (sym.symT == .one) {
                if (sym.inner.one.id == tid) {
                    try self.buf.pushOp3(.tag, @intCast(u8, tid), @intCast(u8, sym.inner.one.val), dst);
                    const vtype = sema.initTagType(tid);
                    return self.initGenValue(dst, vtype, false);
                }
            }
            return self.reportErrorAt("Tag `{}` does not have member `{}`", &.{ v(tname), v(lname) }, nodeId);
        },
        .matchBlock => {
            return genMatchBlock(self, nodeId, dst, retain);
        },
        .objectInit => {
            return genObjectInit(self, nodeId, dst, retain, dstIsUsed);
        },
        .if_expr => {
            return genIfExpr(self, nodeId, dst, retain, !dstIsUsed);
        },
        .map_literal => {
            return genMapInit(self, nodeId, dst, dstIsUsed);
        },
        .arr_literal => {
            return genListInit(self, nodeId, dst, dstIsUsed);
        },
        .arr_range_expr => {
            return genRangeAccessExpr(self, nodeId, dst, retain, dstIsUsed);
        },
        .arr_access_expr => {
            return genIndexAccessExpr(self, nodeId, dst, dstIsUsed);
        },
        .accessExpr => {
            return genAccessExpr(self, nodeId, dst, retain, dstIsUsed);
        },
        .comptExpr => {
            const child = self.nodes[node.head.child_head];
            // try self.buf.pushOp1(.none, dst);
            // return self.initGenValue(dst, sema.AnyType, false);
            return self.reportError("Unsupported compt expr {}", &.{fmt.v(child.node_t)});
        },
        .tryExpr => {
            var child: GenValue = undefined;
            if (retain) {
                child = try self.genRetainedTempExpr(node.head.child_head, false);
            } else {
                child = try self.genExpr(node.head.child_head, false);
            }
            const pc = self.buf.ops.items.len;
            try self.pushDebugSym(nodeId);
            try self.buf.pushOpSlice(.tryValue, &.{ child.local, dst, 0, 0 });
            try self.blockJumpStack.append(self.alloc, .{ .jumpT = .jumpToEndLocals, .pc = @intCast(u32, pc), .pcOffset = 3 });
            self.curBlock.requiresEndingRet1 = true;

            // ARC cleanup.
            if (!retain) {
                try genReleaseIfRetainedTemp(self, child);
            }

            return self.initGenValue(dst, sema.AnyType, retain);
        },
        .unary_expr => {
            const op = node.head.unary.op;
            switch (op) {
                .minus => {
                    const child = try self.genExpr(node.head.unary.child, !dstIsUsed);
                    if (dstIsUsed) {
                        try self.buf.pushOp2(.neg, child.local, dst);
                        return self.initGenValue(dst, sema.NumberType, false);
                    } else return GenValue.initNoValue();
                },
                .not => {
                    const child = try self.genExpr(node.head.unary.child, !dstIsUsed);
                    if (dstIsUsed) {
                        try self.buf.pushOp2(.not, child.local, dst);
                        return self.initGenValue(dst, sema.BoolType, false);
                    } else return GenValue.initNoValue();
                },
                .bitwiseNot => {
                    const child = try self.genExpr(node.head.unary.child, !dstIsUsed);
                    if (dstIsUsed) {
                        try self.buf.pushOp2(.bitwiseNot, child.local, dst);
                        return self.initGenValue(dst, sema.NumberType, false);
                    } else return GenValue.initNoValue();
                },
                // else => return self.reportErrorAt("Unsupported unary op: {}", .{op}, node),
            }
        },
        .group => {
            return genExprTo2(self, node.head.child_head, dst, requestedType, retain, dstIsUsed);
        },
        .binExpr => {
            return genBinExpr(self, nodeId, dst, requestedType, retain, dstIsUsed);
        },
        .callExpr => {
            const val = try genCallExpr(self, nodeId, dst, !dstIsUsed, false);
            if (dst != val.local) {
                try self.buf.pushOp2(.copy, val.local, dst);
            }
            return self.initGenValue(dst, val.vtype, val.retained);
        },
        .lambda_multi => {
            return genLambdaMulti(self, nodeId, dst, dstIsUsed);
        },
        .lambda_expr => {
            return genLambdaExpr(self, nodeId, dst, dstIsUsed);
        },
        .coresume => {
            const fiber = try self.genRetainedTempExpr(node.head.child_head, false);
            if (dstIsUsed) {
                try self.buf.pushOp2(.coresume, fiber.local, dst);
            } else {
                try self.buf.pushOp2(.coresume, fiber.local, cy.NullU8);
            }
            return self.initGenValue(dst, sema.AnyType, true);
        },
        .coyield => {
            const pc = self.buf.ops.items.len;
            try self.buf.pushOp2(.coyield, 0, 0);
            try self.blockJumpStack.append(self.alloc, .{ .jumpT = .jumpToEndLocals, .pc = @intCast(u32, pc), .pcOffset = 1 });

            // TODO: return coyield expression.
            if (dstIsUsed) {
                return try genNone(self, dst);
            } else {
                return GenValue.initNoValue();
            }
        },
        .coinit => {
            _ = try genCallExpr(self, node.head.child_head, dst, !dstIsUsed, true);
            return self.initGenValue(dst, sema.FiberType, true);
        },
        else => {
            return self.reportError("Unsupported {}", &.{fmt.v(node.node_t)});
        },
    }
}

/// Only generates the top declaration statements.
/// For imported modules only.
pub fn genTopDeclStatements(self: *CompileChunk, head: cy.NodeId) !void {
    var nodeId = head;
    while (nodeId != cy.NullId) {
        const node = self.nodes[nodeId];
        switch (node.node_t) {
            .exportStmt,
            .funcDecl => {
                try genStatement(self, nodeId, true);
            },
            else => {},
        }
        nodeId = node.next;
    }
}

fn shouldGenMainScopeReleaseOps(self: *cy.VMcompiler) bool {
    return !self.vm.config.singleRun;
}

pub fn genStatements(self: *CompileChunk, head: cy.NodeId, comptime attachEnd: bool) anyerror!void {
    var cur_id = head;
    var node = self.nodes[cur_id];

    while (node.next != cy.NullId) {
        try genStatement(self, cur_id, true);
        cur_id = node.next;
        node = self.nodes[cur_id];
    }

    // Check for last expression statement.
    if (node.node_t == .expr_stmt) {
        if (attachEnd) {
            const local = try genExprStmt(self, cur_id, true, false);
            if (shouldGenMainScopeReleaseOps(self.compiler)) {
                self.curBlock.endLocalsPc = @intCast(u32, self.buf.ops.items.len);
                self.nodes[0].head.root.genEndLocalsPc = self.curBlock.endLocalsPc;
                try self.endLocals();
            }
            try self.buf.pushOp1(.end, local);
        } else {
            _ = try genStatement(self, cur_id, true);
        }
    } else {
        if (attachEnd) {
            try genStatement(self, cur_id, false);
            if (shouldGenMainScopeReleaseOps(self.compiler)) {
                self.curBlock.endLocalsPc = @intCast(u32, self.buf.ops.items.len);
                self.nodes[0].head.root.genEndLocalsPc = self.curBlock.endLocalsPc;
                try self.endLocals();
            }
            try self.buf.pushOp1(.end, 255);
        } else {
            try genStatement(self, cur_id, true);
        }
    }
}

/// discardTopExprReg is usually true since statements aren't expressions and evaluating child expressions
/// would just grow the register stack unnecessarily. However, the last main statement requires the
/// resulting expr to persist to return from `eval`.
fn genStatement(self: *CompileChunk, nodeId: cy.NodeId, comptime discardTopExprReg: bool) !void {
    // log.debug("gen stmt {}", .{node.node_t});
    self.curNodeId = nodeId;

    self.resetNextFreeTemp();

    const node = self.nodes[nodeId];
    switch (node.node_t) {
        .pass_stmt => {
            return;
        },
        .expr_stmt => {
            _ = try genExprStmt(self, nodeId, false, discardTopExprReg);
        },
        .breakStmt => {
            const pc = try self.pushEmptyJump();
            try self.subBlockJumpStack.append(self.alloc, .{ .jumpT = .brk, .pc = pc });
        },
        .continueStmt => {
            const pc = try self.pushEmptyJump();
            try self.subBlockJumpStack.append(self.alloc, .{ .jumpT = .cont, .pc = pc });
        },
        .opAssignStmt => {
            const left = self.nodes[node.head.opAssignStmt.left];
            const genOp: cy.OpCode = switch (node.head.opAssignStmt.op) {
                .plus => .add,
                .minus => .sub,
                .star => .mul,
                .slash => .div,
                else => fmt.panic("Unexpected operator assignment.", &.{}),
            };
            if (left.node_t == .ident) {
                if (left.head.ident.semaVarId != cy.NullId) {
                    const svar = self.genGetVarPtr(left.head.ident.semaVarId).?;
                    const right = try self.genExpr(node.head.opAssignStmt.right, false);
                    if (svar.isBoxed) {
                        const tempLocal = try self.nextFreeTempLocal();
                        try self.buf.pushOp2(.boxValue, svar.local, tempLocal);

                        try self.pushDebugSym(nodeId);
                        try self.buf.pushOp3(genOp, tempLocal, right.local, tempLocal);

                        try self.buf.pushOp2(.setBoxValue, svar.local, tempLocal);
                        return;
                    } else {
                        try self.pushDebugSym(nodeId);
                        try self.buf.pushOp3(genOp, svar.local, right.local, svar.local);
                    }
                } else {
                    const crSymId = left.head.ident.sema_crSymId;
                    if (!crSymId.isFuncSymId) {
                        const rsym = self.compiler.sema.getResolvedSym(crSymId.id);
                        const rightv = try self.genExpr(node.head.opAssignStmt.right, false);
                        const rtSymId = try self.compiler.vm.ensureVarSym(rsym.key.absResolvedSymKey.rParentSymId, rsym.key.absResolvedSymKey.nameId);

                        const tempLocal = try self.nextFreeTempLocal();
                        try self.pushOptionalDebugSym(nodeId);       
                        try self.buf.pushOp2(.staticVar, @intCast(u8, rtSymId), tempLocal);

                        try self.pushDebugSym(nodeId);
                        try self.buf.pushOp3(genOp, tempLocal, rightv.local, tempLocal);
                        try self.buf.pushOp2(.setStaticVar, @intCast(u8, rtSymId), tempLocal);
                    } else {
                        stdx.fatal();
                    }
                }
            } else if (left.node_t == .accessExpr) {
                try genBinOpAssignToField(self, genOp, node.head.opAssignStmt.left, node.head.opAssignStmt.right);
            } else {
                unexpectedFmt("unsupported assignment to left {}", &.{fmt.v(left.node_t)});
            }
        },
        .assign_stmt => {
            const left = self.nodes[node.head.left_right.left];
            if (left.node_t == .ident) {
                if (left.head.ident.semaVarId != cy.NullId) {
                    try genSetVarToExpr(self, node.head.left_right.left, node.head.left_right.right, false);
                } else {
                    const crSymId = left.head.ident.sema_crSymId;
                    if (!crSymId.isFuncSymId) {
                        const rsym = self.compiler.sema.getResolvedSym(crSymId.id);
                        const rightv = try self.genRetainedTempExpr(node.head.left_right.right, false);
                        const rtSymId = try self.compiler.vm.ensureVarSym(rsym.key.absResolvedSymKey.rParentSymId, rsym.key.absResolvedSymKey.nameId);
                        try self.buf.pushOp2(.setStaticVar, @intCast(u8, rtSymId), rightv.local);
                    } else {
                        stdx.fatal();
                    }
                }
            } else if (left.node_t == .arr_access_expr) {
                const startTempLocal = self.curBlock.firstFreeTempLocal;
                defer self.setFirstFreeTempLocal(startTempLocal);

                const leftv = try self.genExpr(left.head.left_right.left, false);
                const indexv = try self.genExpr(left.head.left_right.right, false);
                const rightv = try self.genRetainedTempExpr(node.head.left_right.right, false);

                try self.pushDebugSym(nodeId);
                try self.buf.pushOp3(.setIndexRelease, leftv.local, indexv.local, rightv.local);

                // ARC cleanup. Right is not released since it's being assigned to the index.
                try genReleaseIfRetainedTempAt(self, leftv, left.head.left_right.left);
                try genReleaseIfRetainedTempAt(self, indexv, left.head.left_right.right);
            } else if (left.node_t == .accessExpr) {
                const startTempLocal = self.curBlock.firstFreeTempLocal;
                defer self.setFirstFreeTempLocal(startTempLocal);

                const leftv = try self.genExpr(left.head.accessExpr.left, false);

                const accessRight = self.nodes[left.head.accessExpr.right];
                if (accessRight.node_t != .ident) {
                    return self.reportErrorAt("Expected ident. Got {}.", &.{v(accessRight.node_t)}, left.head.accessExpr.right);
                }

                const rightv = try self.genRetainedTempExpr(node.head.left_right.right, false);

                const fieldName = self.getNodeTokenString(accessRight);
                const fieldId = try self.compiler.vm.ensureFieldSym(fieldName);
                try self.pushDebugSym(nodeId);
                try self.buf.pushOpSlice(.setFieldRelease, &.{ leftv.local, rightv.local, @intCast(u8, fieldId), 0, 0, 0 });

                // ARC cleanup. Right is not released since it's being assigned to the field.
                try genReleaseIfRetainedTempAt(self, leftv, left.head.accessExpr.left);
            } else {
                unexpectedFmt("unsupported assignment to left {}", &.{fmt.v(left.node_t)});
            }
        },
        .exportStmt => {
            const stmt = node.head.child_head;
            if (self.nodes[stmt].node_t == .funcDecl) {
                try genStatement(self, stmt, discardTopExprReg);
            } else if (self.nodes[stmt].node_t == .objectDecl) {
                try genStatement(self, stmt, discardTopExprReg);
            } else {
                // Nop. Static variables are hoisted and initialized at the start of the program.
            }
        },
        .varDecl => {
            // Nop. Static variables are hoisted and initialized at the start of the program.
        },
        .captureDecl => {
            if (node.head.left_right.right != cy.NullId) {
                // Can assume left is .ident from sema.
                try genSetVarToExpr(self, node.head.left_right.left, node.head.left_right.right, false);
            }
        },
        .staticDecl => {
            if (node.head.left_right.right != cy.NullId) {
                const left = self.nodes[node.head.left_right.left];
                std.debug.assert(left.node_t == .ident);

                const crSymId = left.head.ident.sema_crSymId;
                if (!crSymId.isFuncSymId) {
                    const rsym = self.compiler.sema.getResolvedSym(crSymId.id);
                    const rightv = try self.genRetainedTempExpr(node.head.left_right.right, false);
                    const rtSymId = try self.compiler.vm.ensureVarSym(rsym.key.absResolvedSymKey.rParentSymId, rsym.key.absResolvedSymKey.nameId);
                    try self.buf.pushOp2(.setStaticVar, @intCast(u8, rtSymId), rightv.local);
                } else {
                    stdx.fatal();
                }
            }
        },
        .tagDecl => {
            // Nop.
        },
        .typeAliasDecl => {
            // Nop.
        },
        .objectDecl => {
            const nameN = self.nodes[node.head.objectDecl.name];
            const name = self.getNodeTokenString(nameN);
            const nameId = try sema.ensureNameSym(self.compiler, name);
            const crObjSymId = nameN.head.ident.sema_crSymId;
            const robjSymId = crObjSymId.id;
            const sid = try self.compiler.vm.ensureObjectType(self.semaResolvedRootSymId, nameId);

            var funcId = node.head.objectDecl.funcsHead;
            var func: cy.Node = undefined;
            while (funcId != cy.NullId) : (funcId = func.next) {
                func = self.nodes[funcId];
                const decl = self.semaFuncDecls.items[func.head.func.semaDeclId];

                const funcName = decl.getName(self);
                if (decl.numParams > 0) {
                    const param = self.nodes[decl.paramHead];
                    const paramName = self.getNodeTokenString(self.nodes[param.head.funcParam.name]);
                    if (std.mem.eql(u8, paramName, "self")) {
                        // Struct method.
                        try genMethodDecl(self, sid, func, decl, funcName);
                        continue;
                    }
                }

                // const detail = cy.FuncSymDetail{
                //     .name = try self.alloc.dupe(u8, funcName),
                // };
                // try self.compiler.vm.funcSymDetails.append(self.alloc, detail);
                try genFuncDecl(self, robjSymId, funcId);
            }
        },
        .funcDeclInit => {
            // Nop. Func declaration initializer are hoisted and initialized at the start of the program.
        },
        .funcDecl => {
            try genFuncDecl(self, self.semaResolvedRootSymId, nodeId);
        },
        .forOptStmt => {
            self.nextSemaSubBlock();

            const topPc = @intCast(u32, self.buf.ops.items.len);
            const jumpStackSave = @intCast(u32, self.subBlockJumpStack.items.len);
            defer self.subBlockJumpStack.items.len = jumpStackSave;

            var optLocal: LocalId = undefined;
            if (node.head.forOptStmt.as != cy.NullId) {
                const as = self.nodes[node.head.forOptStmt.as];
                optLocal = self.genGetVar(as.head.ident.semaVarId).?.local;
                // Since this variable is used in the loop, it is considered defined before codegen.
                self.vars.items[as.head.ident.semaVarId].genIsDefined = true;
                try genSetVarToExpr(self, node.head.forOptStmt.as, node.head.forOptStmt.opt, false);
            } else {
                const optv = try self.genExpr(node.head.forOptStmt.opt, false);
                optLocal = optv.local;
            }

            const skipSkipJump = try self.pushEmptyJumpNotNone(optLocal);
            const skipBodyJump = try self.pushEmptyJump();
            self.patchJumpToCurrent(skipSkipJump);

            try genStatements(self, node.head.forOptStmt.bodyHead, false);
            try self.pushJumpBackTo(topPc);

            self.patchJumpToCurrent(skipBodyJump);

            self.patchForBlockJumps(jumpStackSave, self.buf.ops.items.len, topPc);
            self.prevSemaSubBlock();
        },
        .whileCondStmt => {
            self.nextSemaSubBlock();

            const topPc = @intCast(u32, self.buf.ops.items.len);
            const jumpStackSave = @intCast(u32, self.subBlockJumpStack.items.len);
            defer self.subBlockJumpStack.items.len = jumpStackSave;

            const condv = try self.genExpr(node.head.whileCondStmt.cond, false);
            const condLocal = condv.local;

            var jumpPc = try self.pushEmptyJumpNotCond(condLocal);

            try genStatements(self, node.head.whileCondStmt.bodyHead, false);
            try self.pushJumpBackTo(topPc);

            self.patchJumpToCurrent(jumpPc);

            self.patchForBlockJumps(jumpStackSave, self.buf.ops.items.len, topPc);
            self.prevSemaSubBlock();
        },
        .whileInfStmt => {
            self.nextSemaSubBlock();

            const pcSave = @intCast(u32, self.buf.ops.items.len);
            const jumpStackSave = @intCast(u32, self.subBlockJumpStack.items.len);
            defer self.subBlockJumpStack.items.len = jumpStackSave;

            // TODO: generate gas meter checks.
            // if (self.opts.gas_meter != .none) {
            //     try self.indent();
            //     if (self.opts.gas_meter == .error_interrupt) {
            //         _ = try self.writer.write("__interrupt_count += 1; if (__interrupt_count > __interrupt_max) throw globalThis._internal.interruptSym;\n");
            //     } else if (self.opts.gas_meter == .yield_interrupt) {
            //         _ = try self.writer.write("__interrupt_count += 1; if (__interrupt_count > __interrupt_max) yield globalThis._internal.interruptSym;\n");
            //     }
            // }

            try genStatements(self, node.head.child_head, false);
            try self.pushJumpBackTo(pcSave);

            self.patchForBlockJumps(jumpStackSave, self.buf.ops.items.len, pcSave);
            self.prevSemaSubBlock();
        },
        .for_iter_stmt => {
            self.nextSemaSubBlock();
            defer self.prevSemaSubBlock();

            const eachClause = self.nodes[node.head.for_iter_stmt.eachClause];

            var keyVar: sema.LocalVar = undefined;
            var keyIdent: cy.Node = undefined;
            var pairIter = false;
            if (eachClause.head.eachClause.key != cy.NullId) {
                keyIdent = self.nodes[eachClause.head.eachClause.key];
                keyVar = self.genGetVar(keyIdent.head.ident.semaVarId).?;
                pairIter = true;
            }
            const valIdent = self.nodes[eachClause.head.eachClause.value];
            const valVar = self.genGetVar(valIdent.head.ident.semaVarId).?;

            // At this point the temp var is loosely defined.
            self.vars.items[valIdent.head.ident.semaVarId].genIsDefined = true;
            if (pairIter) {
                self.vars.items[keyIdent.head.ident.semaVarId].genIsDefined = true;
            }

            // Loop needs to reserve temp locals.
            const reservedStart = self.reservedTempLocalStack.items.len;
            defer self.reservedTempLocalStack.items.len = reservedStart;

            // Reserve temp local for iterator.
            const iterLocal = try self.nextFreeTempLocal();
            try self.setReservedTempLocal(iterLocal);

            _ = try self.genRetainedExprTo(node.head.for_iter_stmt.iterable, iterLocal + 4, false);
            if (pairIter) {
                try pushCallObjSym(self, iterLocal, 1, 1, @intCast(u8, self.compiler.vm.pairIteratorObjSym), node.head.for_iter_stmt.iterable);
            } else {
                try pushCallObjSym(self, iterLocal, 1, 1, @intCast(u8, self.compiler.vm.iteratorObjSym), node.head.for_iter_stmt.iterable);
            }

            try self.buf.pushOp2(.copyRetainSrc, iterLocal, iterLocal + 5);
            if (pairIter) {
                try pushCallObjSym(self, iterLocal + 1, 1, 2, @intCast(u8, self.compiler.vm.nextPairObjSym), node.head.for_iter_stmt.iterable);
                try self.buf.pushOp2(.copyReleaseDst, iterLocal + 1, keyVar.local);
                try self.buf.pushOp2(.copyReleaseDst, iterLocal + 2, valVar.local);
            } else {
                try pushCallObjSym(self, iterLocal + 1, 1, 1, @intCast(u8, self.compiler.vm.nextObjSym), node.head.for_iter_stmt.iterable);
                try self.buf.pushOp2(.copyReleaseDst, iterLocal + 1, valVar.local);
            }

            const skipSkipJump = try self.pushEmptyJumpNotNone(if (pairIter) keyVar.local else valVar.local);
            const skipBodyJump = try self.pushEmptyJump();
            self.patchJumpToCurrent(skipSkipJump);

            const bodyPc = self.buf.ops.items.len;
            const jumpStackSave = @intCast(u32, self.subBlockJumpStack.items.len);
            defer self.subBlockJumpStack.items.len = jumpStackSave;
            try genStatements(self, node.head.for_iter_stmt.body_head, false);

            const contPc = self.buf.ops.items.len;
            try self.buf.pushOp2(.copyRetainSrc, iterLocal, iterLocal + 5);
            if (pairIter) {
                try pushCallObjSym(self, iterLocal + 1, 1, 2, @intCast(u8, self.compiler.vm.nextPairObjSym), node.head.for_iter_stmt.iterable);
                try self.buf.pushOp2(.copyReleaseDst, iterLocal + 1, keyVar.local);
                try self.buf.pushOp2(.copyReleaseDst, iterLocal + 2, valVar.local);
            } else {
                try pushCallObjSym(self, iterLocal + 1, 1, 1, @intCast(u8, self.compiler.vm.nextObjSym), node.head.for_iter_stmt.iterable);
                try self.buf.pushOp2(.copyReleaseDst, iterLocal + 1, valVar.local);
            }

            try self.pushJumpBackNotNone(bodyPc, if (pairIter) keyVar.local else valVar.local);

            self.patchForBlockJumps(jumpStackSave, self.buf.ops.items.len, contPc);

            self.patchJumpToCurrent(skipBodyJump);

            // TODO: Iter local should be a reserved hidden local (instead of temp) so it can be cleaned up by endLocals when aborting the current fiber.
            try self.pushOptionalDebugSym(nodeId);
            try self.buf.pushOp1(.release, iterLocal);
        },
        .for_range_stmt => {
            self.nextSemaSubBlock();
            defer self.prevSemaSubBlock();

            var local: u8 = cy.NullU8;
            if (node.head.for_range_stmt.eachClause != cy.NullId) {
                const eachClause = self.nodes[node.head.for_range_stmt.eachClause];
                const ident = self.nodes[eachClause.head.eachClause.value];
                local = self.genGetVar(ident.head.ident.semaVarId).?.local;

                // inc = as_clause.head.as_range_clause.inc;
                // if (as_clause.head.as_range_clause.step != NullId) {
                //     step = self.nodes[as_clause.head.as_range_clause.step];
                // }
            }

            // Loop needs to reserve temp locals.
            const reservedStart = self.reservedTempLocalStack.items.len;
            defer self.reservedTempLocalStack.items.len = reservedStart;

            // Set range start/end.
            const range_clause = self.nodes[node.head.for_range_stmt.range_clause];
            const rangeStartN = self.nodes[range_clause.head.left_right.left];
            const rangeEndN = self.nodes[range_clause.head.left_right.right];
            var lessThanCond = true;
            if (rangeStartN.node_t == .number and rangeEndN.node_t == .number) {
                const startLit = self.getNodeTokenString(rangeStartN);
                const endLit = self.getNodeTokenString(rangeEndN);
                const start = try std.fmt.parseFloat(f64, startLit);
                const end = try std.fmt.parseFloat(f64, endLit);
                if (start > end) {
                    lessThanCond = false;
                }
            }

            // Keep counter hidden from user. (User can't change it's value.)
            const counter = try self.nextFreeTempLocal();
            try self.setReservedTempLocal(counter);

            const rangeStart = try self.genExpr(range_clause.head.left_right.left, false);

            const rangeEnd = try self.nextFreeTempLocal();
            _ = try self.genExprTo(range_clause.head.left_right.right, rangeEnd, false, false);
            try self.setReservedTempLocal(rangeEnd);

            // Set custom step.
            const rangeStep = try self.nextFreeTempLocal();
            try self.setReservedTempLocal(rangeStep);
            _ = try genConstNumber(self, 1, rangeStep);

            const initPc = self.buf.ops.items.len;
            try self.buf.pushOpSlice(.forRangeInit, &.{ rangeStart.local, rangeEnd, rangeStep, counter, local, 0, 0 });

            const bodyPc = self.buf.ops.items.len;
            const jumpStackSave = @intCast(u32, self.subBlockJumpStack.items.len);
            defer self.subBlockJumpStack.items.len = jumpStackSave;
            try genStatements(self, node.head.for_range_stmt.body_head, false);

            // Perform counter update and perform check against end range.
            const jumpBackOffset = @intCast(u16, self.buf.ops.items.len - bodyPc);
            const forRangeOp = self.buf.ops.items.len;
            // The forRange op is patched by forRangeInit at runtime.
            self.buf.setOpArgU16(initPc + 6, @intCast(u16, self.buf.ops.items.len - initPc));
            try self.buf.pushOpSlice(.forRange, &.{ counter, rangeStep, rangeEnd, local, 0, 0 });
            self.buf.setOpArgU16(forRangeOp + 5, jumpBackOffset);

            self.patchForBlockJumps(jumpStackSave, self.buf.ops.items.len, forRangeOp);
        },
        .matchBlock => {
            _ = try genMatchBlock(self, nodeId, cy.NullU8, undefined);
        },
        .if_stmt => {
            const startTempLocal = self.curBlock.firstFreeTempLocal;

            const condv = try self.genExpr(node.head.left_right.left, false);

            var lastCondJump = try self.pushEmptyJumpNotCond(condv.local);
            self.setFirstFreeTempLocal(startTempLocal);

            self.nextSemaSubBlock();
            try genStatements(self, node.head.left_right.right, false);
            self.prevSemaSubBlock();

            var elseClauseId = node.head.left_right.extra;
            if (elseClauseId != cy.NullId) {
                var jumpsStart = self.subBlockJumpStack.items.len;
                defer self.subBlockJumpStack.items.len = jumpsStart;

                var endsWithElse = false;
                while (elseClauseId != cy.NullId) {
                    const pc = try self.pushEmptyJump();
                    try self.subBlockJumpStack.append(self.alloc, .{ .jumpT = .subBlockBreak, .pc = pc });

                    self.patchJumpToCurrent(lastCondJump);

                    const elseClause = self.nodes[elseClauseId];
                    if (elseClause.head.else_clause.cond == cy.NullId) {
                        self.nextSemaSubBlock();
                        try genStatements(self, elseClause.head.else_clause.body_head, false);
                        self.prevSemaSubBlock();
                        endsWithElse = true;
                        self.setFirstFreeTempLocal(startTempLocal);
                        break;
                    } else {
                        const elifCondv = try self.genExpr(elseClause.head.else_clause.cond, false);
                        lastCondJump = try self.pushEmptyJumpNotCond(elifCondv.local);

                        self.nextSemaSubBlock();
                        try genStatements(self, elseClause.head.else_clause.body_head, false);
                        self.prevSemaSubBlock();
                        elseClauseId = elseClause.head.else_clause.else_clause;

                        self.setFirstFreeTempLocal(startTempLocal);
                    }
                }

                if (!endsWithElse) {
                    self.patchJumpToCurrent(lastCondJump);
                }
                jumpsStart = self.patchSubBlockBreakJumps(jumpsStart, self.buf.ops.items.len);
            } else {
                self.patchJumpToCurrent(lastCondJump);
            }
        },
        .importStmt => {
            const ident = self.nodes[node.head.left_right.left];
            const name = self.getNodeTokenString(ident);

            const spec = self.nodes[node.head.left_right.right];
            const specPath = self.getNodeTokenString(spec);

            _ = name;
            _ = specPath;

            // const modId = try self.getOrLoadModule(specPath);
            // const leadSym = try self.ensureSemaSym(name);
            // try self.semaSymToMod.put(self.alloc, leadSym, modId);
        },
        .return_stmt => {
            if (self.blocks.items.len == 1) {
                try self.endLocals();
                try self.buf.pushOp1(.end, 255);
            } else {
                try self.endLocals();
                try self.buf.pushOp(.ret0);
            }
        },
        .return_expr_stmt => {
            if (self.blocks.items.len == 1) {
                const val = try self.genRetainedTempExpr(node.head.child_head, false);
                try self.endLocals();
                try self.buf.pushOp1(.end, @intCast(u8, val.local));
            } else {
                if (self.curBlock.rFuncSymId != cy.NullId) {
                    const retType = self.compiler.sema.resolvedFuncSyms.items[self.curBlock.rFuncSymId].retType;
                    _ = try genExprTo2(self, node.head.child_head, 0, retType, true, true);
                } else {
                    _ = try self.genRetainedExprTo(node.head.child_head, 0, false);
                }
                try self.endLocals();
                try self.buf.pushOp(.ret1);
            }
        },
        .atStmt => {
            const atExpr = self.nodes[node.head.atStmt.expr];
            const expr = self.nodes[atExpr.head.atExpr.child];
            if (expr.node_t == .callExpr) {
                const callee = self.nodes[expr.head.callExpr.callee];
                const name = self.getNodeTokenString(callee);

                if (std.mem.eql(u8, "genLabel", name)) {
                    if (expr.head.callExpr.getNumArgs(self.nodes) != 1) {
                        return self.reportErrorAt("genLabel expected 1 arg", &.{}, nodeId);
                    }

                    const arg = self.nodes[expr.head.callExpr.arg_head];
                    if (arg.node_t != .string) {
                        return self.reportErrorAt("genLabel expected string arg", &.{}, nodeId);
                    }

                    const label = self.getNodeTokenString(arg);
                    try self.buf.pushDebugLabel(self.buf.ops.items.len, label);
                } else if (std.mem.eql(u8, "dumpLocals", name)) {
                    const sblock = sema.curBlock(self);
                    try self.dumpLocals(sblock);
                } else if (std.mem.eql(u8, "dumpBytecode", name)) {
                    try cy.debug.dumpBytecode(self.compiler.vm, null);
                } else {
                    return self.reportErrorAt("Unsupported annotation: {}", &.{v(name)}, nodeId);
                }
            } else {
                return self.reportErrorAt("Unsupported atExpr: {}", &.{v(expr.node_t)}, nodeId);
            }
        },
        else => {
            return self.reportErrorAt("Unsupported statement: {}", &.{v(node.node_t)}, nodeId);
        },
    }
}

/// If `dst` is NullId, this will generate as a statement and NoValue is returned.
fn genMatchBlock(self: *CompileChunk, nodeId: cy.NodeId, dst: LocalId, retain: bool) !GenValue {
    const isStmt = dst == cy.NullId;
    const node = self.nodes[nodeId];
    const expr = try self.genExpr(node.head.matchBlock.expr, false);

    const operandStart = self.operandStack.items.len;

    var jumpsStart = self.subBlockJumpStack.items.len;
    defer self.subBlockJumpStack.items.len = jumpsStart;

    var hasElse = false;
    var numConds: u32 = 0;
    var curCase = node.head.matchBlock.firstCase;
    while (curCase != cy.NullId) {
        const case = self.nodes[curCase];
        var curCond = case.head.caseBlock.firstCond;
        while (curCond != cy.NullId) {
            const cond = self.nodes[curCond];
            if (cond.node_t == .elseCase) {
                // Skip else cond.
                curCond = cond.next;
                hasElse = true;
                continue;
            }
            const condv = try self.genExpr(curCond, false);
            try self.pushTempOperand(condv.local);
            // Reserve another two for jump.
            try self.pushTempOperand(0);
            try self.pushTempOperand(0);
            curCond = cond.next;
            numConds += 1;
        }
        curCase = case.next;
    }
    const condJumpStart = self.buf.ops.items.len + 4;
    const matchPc = self.buf.ops.items.len;
    try self.buf.pushOp2(.match, expr.local, @intCast(u8, numConds));
    try self.buf.pushOperands(self.operandStack.items[operandStart..]);
    // Reserve last jump for `else` case.
    try self.buf.pushOperand(0);
    try self.buf.pushOperand(0);
    self.operandStack.items.len = operandStart;

    // Generate case blocks.
    curCase = node.head.matchBlock.firstCase;
    var condOffset: u32 = 0;
    while (curCase != cy.NullId) {
        const case = self.nodes[curCase];
        self.nextSemaSubBlock();
        defer self.prevSemaSubBlock();

        const blockPc = self.buf.ops.items.len;
        if (isStmt) {
            try genStatements(self, case.head.caseBlock.firstChild, false);
        } else {
            // Check for single expression stmt.
            const first = self.nodes[case.head.caseBlock.firstChild];
            if (first.next == cy.NullId and first.node_t == .expr_stmt) {
                const firstExpr = first.head.child_head;
                _  = try genExprTo2(self, firstExpr, dst, sema.AnyType, retain, true);
            } else {
                try genStatements(self, case.head.caseBlock.firstChild, false);
            }
        }

        if (isStmt) {
            if (case.next != cy.NullId) {
                // Not the last block.
                // Reserve jump to end.
                const pc = try self.pushEmptyJump();
                try self.subBlockJumpStack.append(self.alloc, .{ .jumpT = .subBlockBreak, .pc = pc });
            }
        } else {
            const pc = try self.pushEmptyJump();
            try self.subBlockJumpStack.append(self.alloc, .{ .jumpT = .subBlockBreak, .pc = pc });
        }

        // For each cond for this case, patch the jump offset.
        var curCond = case.head.caseBlock.firstCond;
        while (curCond != cy.NullId) {
            const cond = self.nodes[curCond];
            // Patch jump.
            if (cond.node_t == .elseCase) {
                self.buf.setOpArgU16(condJumpStart + numConds * 3 - 1, @intCast(u16, blockPc - matchPc));
            } else {
                self.buf.setOpArgU16(condJumpStart + condOffset * 3, @intCast(u16, blockPc - matchPc));
            }
            condOffset += 1;
            curCond = cond.next;
        }

        curCase = case.next;
    }
    if (!hasElse) {
        // When missing an else case, patch else jump to end of the block.
        self.buf.setOpArgU16(condJumpStart + numConds * 3 - 1, @intCast(u16, self.buf.ops.items.len - matchPc));
    }

    if (!isStmt) {
        // Defaults to `none`.
        _ = try genNone(self, dst);
    }

    // Patch all block end jumps.
    jumpsStart = self.patchSubBlockBreakJumps(jumpsStart, self.buf.ops.items.len);

    if (isStmt) {
        return GenValue.initNoValue();
    } else {
        return self.initGenValue(dst, sema.AnyType, retain);
    }
}

fn genString(self: *CompileChunk, str: []const u8, dst: LocalId) !GenValue {
    const idx = try self.buf.getOrPushStringConst(str);
    try self.buf.pushOp2(.constOp, @intCast(u8, idx), dst);
    return self.initGenValue(dst, sema.StaticStringType, false);
}

fn genConstInt(self: *CompileChunk, val: f64, dst: LocalId) !GenValue {
    if (cy.Value.floatCanBeInteger(val)) {
        const i = @floatToInt(i64, val);
        if (i >= std.math.minInt(i8) and i <= std.math.maxInt(i8)) {
            try self.buf.pushOp2(.constI8Int, @bitCast(u8, @intCast(i8, i)), dst);
            return self.initGenValue(dst, sema.IntegerType, false);
        }
    } else {
        return self.reportError("TODO: coerce", &.{});
    }
    const int = @floatToInt(i32, val);
    const idx = try self.buf.pushConst(cy.Const.init(cy.Value.initI32(int).val));
    try self.buf.pushOp2(.constOp, @intCast(u8, idx), dst);
    return self.initGenValue(dst, sema.IntegerType, false);
}

fn genConstNumber(self: *CompileChunk, val: f64, dst: LocalId) !GenValue {
    if (cy.Value.floatCanBeInteger(val)) {
        const i = @floatToInt(i64, val);
        if (i >= std.math.minInt(i8) and i <= std.math.maxInt(i8)) {
            try self.buf.pushOp2(.constI8, @bitCast(u8, @intCast(i8, i)), dst);
            return self.initGenValue(dst, sema.NumberType, false);
        }
    }
    const idx = try self.buf.pushConst(cy.Const.init(@bitCast(u64, val)));
    try self.buf.pushOp2(.constOp, @intCast(u8, idx), dst);
    return self.initGenValue(dst, sema.NumberType, false);
}

fn genIfExpr(self: *CompileChunk, nodeId: cy.NodeId, dst: LocalId, retain: bool, comptime discardTopExprReg: bool) !GenValue {
    const node = self.nodes[nodeId];
    const startTempLocal = self.curBlock.firstFreeTempLocal;
    defer self.computeNextTempLocalFrom(startTempLocal);

    const condv = try self.genExpr(node.head.if_expr.cond, false);
    var jumpNotPc = try self.pushEmptyJumpNotCond(condv.local);

    self.computeNextTempLocalFrom(startTempLocal);

    var truev = try self.genExprTo(node.head.if_expr.body_expr, dst, retain, false);
    const jumpPc = try self.pushEmptyJump();
    self.patchJumpToCurrent(jumpNotPc);

    self.computeNextTempLocalFrom(startTempLocal);

    var falsev: GenValue = undefined;
    if (node.head.if_expr.else_clause != cy.NullId) {
        const else_clause = self.nodes[node.head.if_expr.else_clause];
        falsev = try self.genExprTo(else_clause.head.child_head, dst, retain, discardTopExprReg);
    } else {
        if (!discardTopExprReg) {
            falsev = try genNone(self, dst);
        }
    }

    self.patchJumpToCurrent(jumpPc);

    if (truev.vtype.typeT != falsev.vtype.typeT) {
        return self.initGenValue(dst, sema.AnyType, retain);
    } else {
        return self.initGenValue(dst, truev.vtype, retain and truev.vtype.rcCandidate);
    }
}

fn genCallExpr(self: *CompileChunk, nodeId: cy.NodeId, dst: LocalId, comptime discardTopExprReg: bool, comptime startFiber: bool) !GenValue {
    const startTempLocal = self.curBlock.firstFreeTempLocal;
    // Can assume temps after startTempLocal are not arc temps.
    defer self.setFirstFreeTempLocal(startTempLocal);

    var callStartLocal = self.advanceNextTempLocalPastReservedTemps();
    var considerUsingDst = true;
    if (self.blocks.items.len == 1) {
        // Main block.
        // Ensure call start register is at least 1 so the runtime can easily check
        // if framePtr is at main or a function.
        if (callStartLocal == 0) {
            callStartLocal = 1;
        }
        if (callStartLocal == 1) {
            considerUsingDst = false;
        }
    }

    // Reserve registers for return value and return info.
    if (considerUsingDst and dst + 1 == callStartLocal) {
        callStartLocal -= 1;
    } else {
        _ = try self.nextFreeTempLocal();
    }
    _ = try self.nextFreeTempLocal();
    _ = try self.nextFreeTempLocal();
    _ = try self.nextFreeTempLocal();

    const genCallStartLocal = if (startFiber) 1 else callStartLocal;

    const node = self.nodes[nodeId];
    const callee = self.nodes[node.head.callExpr.callee];
    if (!node.head.callExpr.has_named_arg) {
        if (callee.node_t == .accessExpr) {
            if (callee.head.accessExpr.sema_crSymId.isPresent()) {
                const crSymId = callee.head.accessExpr.sema_crSymId;
                if (crSymId.isFuncSymId) {
                    const funcSym = self.compiler.sema.getResolvedFuncSym(crSymId.id);
                    const rsym = self.compiler.sema.getResolvedSym(funcSym.getResolvedSymId());

                    const rFuncSigId = funcSym.getResolvedFuncSigId();

                    // Func sym.
                    var numArgs: u32 = undefined;
                    if (funcSym.declId != cy.NullId) {
                        const rFuncSig = self.compiler.sema.resolvedFuncSigs.items[rFuncSigId];
                        numArgs = try genCallArgs2(self, rFuncSig, node.head.callExpr.arg_head);
                    } else {
                        numArgs = try genCallArgs(self, node.head.callExpr.arg_head);
                    }

                    // var isStdCall = false;
                    const key = rsym.key.absResolvedSymKey;

                    const symId = try self.compiler.vm.ensureFuncSym(key.rParentSymId, key.nameId, rFuncSigId);
                    if (discardTopExprReg) {
                        try self.pushDebugSym(nodeId);
                        try self.buf.pushOpSlice(.callSym, &.{ callStartLocal, @intCast(u8, numArgs), 0, @intCast(u8, symId), 0, 0, 0, 0, 0, 0 });
                        return GenValue.initTempValue(callStartLocal, funcSym.retType, false);
                    } else {
                        try self.pushDebugSym(nodeId);
                        try self.buf.pushOpSlice(.callSym, &.{ callStartLocal, @intCast(u8, numArgs), 1, @intCast(u8, symId), 0, 0, 0, 0, 0, 0 });
                        return GenValue.initTempValue(callStartLocal, funcSym.retType, true);
                    }

                    // if (try self.readScopedVar(leftName)) |info| {
                    //     if (info.vtype.typeT == ListType.typeT) {
                    //         if (self.compiler.vm.hasMethodSym(cy.ListS, methodId)) {
                    //             isStdCall = true;
                    //         }
                    //     }
                    // }

                    // if (isStdCall) {
                    //     // Avoid retain/release for std call.
                    //     _ = try self.genExpr(left, false);
                    // } else {
                    //     _ = try self.genMaybeRetainExpr(left, false);
                    // }
                } else {
                    return genFuncValueCallExpr(self, node, callStartLocal, discardTopExprReg);
                }
            } else {
                // Assume left child is a valid reference from sema. Generate callObjSym.
                const right = self.nodes[callee.head.accessExpr.right];
                if (right.node_t == .ident) {
                    return genCallObjSym(self, callStartLocal, callee.head.accessExpr.left, callee.head.accessExpr.right, node.head.callExpr.arg_head, discardTopExprReg, nodeId);
                } else return self.reportErrorAt("Unsupported callee", &.{}, nodeId);
            }
        } else if (callee.node_t == .ident) {
            if (self.genGetVar(callee.head.ident.semaVarId)) |_| {
                return genFuncValueCallExpr(self, node, callStartLocal, discardTopExprReg);
            } else {
                var genArgs = false;
                var numArgs: u32 = undefined;
                const crSymId = callee.head.ident.sema_crSymId;
                var rFuncSym: ?sema.ResolvedFuncSym = null;
                if (crSymId.isPresent()) {
                    if (crSymId.isFuncSymId) {
                        const funcSym = self.compiler.sema.getResolvedFuncSym(crSymId.id);
                        const rFuncSigId = funcSym.getResolvedFuncSigId();
                        rFuncSym = funcSym;
                        if (funcSym.declId != cy.NullId) {
                            const rFuncSig = self.compiler.sema.resolvedFuncSigs.items[rFuncSigId];
                            numArgs = try genCallArgs2(self, rFuncSig, node.head.callExpr.arg_head);
                            genArgs = true;
                        }
                    } else {
                        const rsym = self.compiler.sema.getResolvedSym(crSymId.id);
                        if (rsym.symT == .variable) {
                            return genFuncValueCallExpr(self, node, callStartLocal, discardTopExprReg);
                        } else {
                            return self.reportErrorAt("Unsupported callee", &.{}, nodeId);
                        }
                    }
                }

                if (!genArgs) {
                    numArgs = try genCallArgs(self, node.head.callExpr.arg_head);
                }

                const coinitPc = self.buf.ops.items.len;
                if (startFiber) {
                    // Precompute first arg local since coinit doesn't need the startLocal.
                    // numArgs + 4 (ret slots) + 1 (min call start local for main block)
                    var initialStackSize = numArgs + 4 + 1;
                    if (initialStackSize < 16) {
                        initialStackSize = 16;
                    }
                    try self.buf.pushOpSlice(.coinit, &.{ callStartLocal + 4, @intCast(u8, numArgs), 0, @intCast(u8, initialStackSize), dst });
                }

                if (crSymId.isFuncSymId) {
                    const rtSymId = try self.genEnsureRtFuncSym(crSymId.id);
                    if (discardTopExprReg) {
                        try self.pushDebugSym(nodeId);
                        try self.buf.pushOpSlice(.callSym, &.{ genCallStartLocal, @intCast(u8, numArgs), 0, @intCast(u8, rtSymId), 0, 0, 0, 0, 0, 0 });
                    } else {
                        try self.pushDebugSym(nodeId);
                        try self.buf.pushOpSlice(.callSym, &.{ genCallStartLocal, @intCast(u8, numArgs), 1, @intCast(u8, rtSymId), 0, 0, 0, 0, 0, 0 });
                    }
                }

                if (startFiber) {
                    try self.buf.pushOp(.coreturn);
                    self.buf.setOpArgs1(coinitPc + 3, @intCast(u8, self.buf.ops.items.len - coinitPc));
                }

                if (rFuncSym) |funcSym| {
                    return GenValue.initTempValue(callStartLocal, funcSym.retType, !discardTopExprReg);
                } else {
                    return GenValue.initTempValue(callStartLocal, sema.AnyType, !discardTopExprReg);
                }
            }
        } else {
            // All other callees are treated as function value calls.
            return genFuncValueCallExpr(self, node, callStartLocal, discardTopExprReg);
        }
    } else return self.reportError("Unsupported named args", &.{});
}

fn genCallObjSym(self: *CompileChunk, callStartLocal: u8, leftId: cy.NodeId, identId: cy.NodeId, firstArgId: cy.NodeId, dstIsUsed: bool, debugNodeId: cy.NodeId) !GenValue {
    // One more arg for receiver.
    const numArgs = 1 + try genCallArgs(self, firstArgId);
        
    const ident = self.nodes[identId];
    const name = self.getNodeTokenString(ident);
    const methodId = try self.compiler.vm.ensureMethodSymKey(name, numArgs - 1);

    _ = try self.genRetainedTempExpr(leftId, false);

    if (dstIsUsed) {
        try pushCallObjSym(self, callStartLocal, @intCast(u8, numArgs), 0, @intCast(u8, methodId), debugNodeId);
        return GenValue.initTempValue(callStartLocal, sema.AnyType, false);
    } else {
        try pushCallObjSym(self, callStartLocal, @intCast(u8, numArgs), 1, @intCast(u8, methodId), debugNodeId);
        return GenValue.initTempValue(callStartLocal, sema.AnyType, true);
    }
}

fn genFuncValueCallExpr(self: *CompileChunk, node: cy.Node, callStartLocal: u8, comptime discardTopExprReg: bool) !GenValue {
    var numArgs: u32 = 0;
    var argId = node.head.callExpr.arg_head;
    while (argId != cy.NullId) : (numArgs += 1) {
        const arg = self.nodes[argId];
        _ = try self.genRetainedTempExpr(argId, false);
        argId = arg.next;
    }

    // TODO: Doesn't have to retain to a temp if it's a local (would just need retain op).
    // If copied to temp, it should be after the last param so it can persist until the function returns.
    const calleev = try self.genRetainedTempExpr(node.head.callExpr.callee, false);

    if (discardTopExprReg) {
        try self.buf.pushOp2(.call0, callStartLocal, @intCast(u8, numArgs));

        // ARC cleanup.
        try genReleaseIfRetainedTemp(self, calleev);

        return GenValue.initNoValue();
    } else {
        try self.buf.pushOp2(.call1, callStartLocal, @intCast(u8, numArgs));

        // ARC cleanup.
        try genReleaseIfRetainedTemp(self, calleev);

        return GenValue.initTempValue(callStartLocal, sema.AnyType, true);
    }
}

fn genFuncDecl(self: *CompileChunk, rParentSymId: sema.ResolvedSymId, nodeId: cy.NodeId) !void {
    const node = self.nodes[nodeId];
    const func = &self.semaFuncDecls.items[node.head.func.semaDeclId];
    const rsym = self.compiler.sema.resolvedSyms.items[func.inner.staticFunc.semaResolvedSymId];
    const rFuncSym = self.compiler.sema.resolvedFuncSyms.items[func.inner.staticFunc.semaResolvedFuncSymId];
    const key = rsym.key.absResolvedSymKey;

    const rFuncSigId = rFuncSym.getResolvedFuncSigId();
    const symId = try self.compiler.vm.ensureFuncSym(rParentSymId, key.nameId, rFuncSigId);

    const jumpPc = try self.pushEmptyJump();

    try self.pushSemaBlock(func.semaBlockId);
    self.curBlock.frameLoc = nodeId;
    self.curBlock.rFuncSymId = func.inner.staticFunc.semaResolvedFuncSymId;

    const jumpStackStart = self.blockJumpStack.items.len;

    const opStart = @intCast(u32, self.buf.ops.items.len);
    try self.reserveFuncParams(func.numParams);
    try genInitLocals(self);
    try genStatements(self, node.head.func.bodyHead, false);
    // TODO: Check last statement to skip adding ret.
    try self.genBlockEnding();
    func.genEndLocalsPc = self.curBlock.endLocalsPc;

    // Reserve another local for the call return info.
    const sblock = sema.curBlock(self);
    const numLocals = @intCast(u32, self.curBlock.numLocals + self.curBlock.numTempLocals);
    const numCaptured = @intCast(u8, sblock.params.items.len - func.numParams);
    std.debug.assert(numCaptured == 0);

    self.patchJumpToCurrent(jumpPc);

    self.patchBlockJumps(jumpStackStart);
    self.blockJumpStack.items.len = jumpStackStart;

    self.popSemaBlock();
    
    const rtSym = cy.FuncSymbolEntry.initFunc(opStart, @intCast(u16, numLocals), func.numParams, rFuncSigId);
    self.compiler.vm.setFuncSym(symId, rtSym);
}

fn genCallArgs2(self: *CompileChunk, rFuncSig: sema.ResolvedFuncSig, first: cy.NodeId) !u32 {
    const sig = rFuncSig.slice();
    var numArgs: u32 = 0;
    var argId = first;
    while (argId != cy.NullId) : (numArgs += 1) {
        const arg = self.nodes[argId];
        const rParamTypeSymId = sig[numArgs];
        const reqType = try sema.getTypeForResolvedSym(self, rParamTypeSymId);
        _ = try self.genRetainedTempExpr2(argId, reqType, false);
        argId = arg.next;
    }
    return numArgs;
}

fn genCallArgs(self: *CompileChunk, first: cy.NodeId) !u32 {
    var numArgs: u32 = 0;
    var argId = first;
    while (argId != cy.NullId) : (numArgs += 1) {
        const arg = self.nodes[argId];
        _ = try self.genRetainedTempExpr(argId, false);
        argId = arg.next;
    }
    return numArgs;
}

fn genStaticInitializerDeps(c: *CompileChunk, crSymId: sema.CompactResolvedSymId) !void {
    if (c.semaInitializerSyms.get(crSymId)) |ref| {
        // Contains dependencies. Generate initializers for them first.
        const deps = c.bufU32.items[ref.depsStart..ref.depsEnd];
        for (deps) |dep| {
            const crDepSym = @bitCast(sema.CompactResolvedSymId, dep);
            // Only if it has a static initializer and hasn't been visited.
            if (sema.symHasStaticInitializer(c, crDepSym)) {
                try genStaticInitializerDFS(c, crDepSym);
            }
        }
    }
}

/// Generates var decl initializer.
/// If the declaration contains dependencies those are generated first in DFS order.
pub fn genStaticInitializerDFS(self: *CompileChunk, crSymId: sema.CompactResolvedSymId) anyerror!void {
    if (crSymId.isFuncSymId) {
        // Check that the resolved sym hasn't already been visited for generation.
        const rFuncSym = self.compiler.sema.getResolvedFuncSymPtr(crSymId.id);
        if (rFuncSym.genStaticInitVisited) {
            return;
        }
        rFuncSym.genStaticInitVisited = true;

        const rFuncSigId = rFuncSym.getResolvedFuncSigId();
        const chunk = &self.compiler.chunks.items[rFuncSym.chunkId];
        const func = chunk.semaFuncDecls.items[rFuncSym.declId];
        const name = func.getName(chunk);
        const nameId = try sema.ensureNameSym(chunk.compiler, name);
        const node = chunk.nodes[func.nodeId];
        // Generate deps first.
        try genStaticInitializerDeps(chunk, crSymId);

        log.debug("gen static func init: {s}", .{name});

        // Clear register state.
        chunk.resetNextFreeTemp();
        const exprv = try chunk.genRetainedTempExpr(node.head.func.bodyHead, false);

        const rSym = self.compiler.sema.getResolvedSym(rFuncSym.getResolvedSymId());
        const rtSymId = try self.compiler.vm.ensureFuncSym(rSym.key.absResolvedSymKey.rParentSymId, nameId, rFuncSigId);
        try chunk.pushDebugSym(node.head.func.bodyHead);
        try self.buf.pushOp2(.setStaticFunc, @intCast(u8, rtSymId), exprv.local);
    } else {
        const rSym = self.compiler.sema.getResolvedSymPtr(crSymId.id);
        if (rSym.genStaticInitVisited) {
            return;
        }
        rSym.genStaticInitVisited = true;

        if (rSym.symT == .variable) {
            const declId = rSym.inner.variable.declId;
            const chunk = &self.compiler.chunks.items[rSym.inner.variable.chunkId];
            const decl = chunk.nodes[declId];

            // Generate deps first.
            try genStaticInitializerDeps(chunk, crSymId);

            log.debug("gen static var init: {s}", .{sema.getName(self.compiler, rSym.key.absResolvedSymKey.nameId)});

            // Clear register state.
            chunk.resetNextFreeTemp();
            const exprv = try chunk.genRetainedTempExpr(decl.head.varDecl.right, false);

            const rtSymId = try self.compiler.vm.ensureVarSym(rSym.key.absResolvedSymKey.rParentSymId, rSym.key.absResolvedSymKey.nameId);
            try self.buf.pushOp2(.setStaticVar, @intCast(u8, rtSymId), exprv.local);
        } else {
            stdx.panicFmt("Unsupported sym {}", .{rSym.symT});
        }
    }
}

fn genNone(self: *CompileChunk, dst: LocalId) !GenValue {
    try self.buf.pushOp1(.none, dst);
    return self.initGenValue(dst, sema.AnyType, false);
}

fn genExprStmt(self: *CompileChunk, stmtId: cy.NodeId, retainEscapeTop: bool, comptime discardTopExprReg: bool) !LocalId {
    const stmt = self.nodes[stmtId];
    var val: GenValue = undefined;
    if (retainEscapeTop) {
        const node = self.nodes[stmt.head.child_head];
        var retLocal = false;
        if (node.node_t == .ident) {
            if (self.genGetVar(node.head.ident.semaVarId)) |svar| {
                if (!svar.isBoxed) {
                    if (svar.vtype.rcCandidate) {
                        try self.buf.pushOp1(.retain, svar.local);
                        val = GenValue.initLocalValue(svar.local, svar.vtype, true);
                    } else {
                        val = GenValue.initLocalValue(svar.local, svar.vtype, false);
                    }
                    retLocal = true;
                }
            }
        }
        if (!retLocal) {
            val = try self.genRetainedTempExpr(stmt.head.child_head, discardTopExprReg);
        }
    } else {
        val = try self.genExpr(stmt.head.child_head, discardTopExprReg);
    }
    return val.local;
}

fn genBinOpAssignToField(self: *CompileChunk, code: cy.OpCode, leftId: cy.NodeId, rightId: cy.NodeId) !void {
    const left = self.nodes[leftId];

    const startTempLocal = self.curBlock.firstFreeTempLocal;
    defer self.computeNextTempLocalFrom(startTempLocal);

    const accessRight = self.nodes[left.head.accessExpr.right];
    if (accessRight.node_t != .ident) {
        return self.reportErrorAt("Expected ident. Got {}.", &.{v(accessRight.node_t)}, left.head.accessExpr.right);
    }
    const fieldName = self.getNodeTokenString(accessRight);
    const fieldId = try self.compiler.vm.ensureFieldSym(fieldName);

    const accessLeftv = try self.genExpr(left.head.accessExpr.left, false);
    const accessLocal = try self.nextFreeTempLocal();
    try self.pushDebugSym(leftId);
    try self.buf.pushOpSlice(.field, &.{ accessLeftv.local, accessLocal, @intCast(u8, fieldId), 0, 0, 0 });

    const rightv = try self.genExpr(rightId, false);
    try self.buf.pushOp3(code, accessLocal, rightv.local, accessLocal);

    try self.pushDebugSym(leftId);
    try self.buf.pushOp3(.setField, @intCast(u8, fieldId), accessLeftv.local, accessLocal);

    // ARC cleanup. Right is not released since it's being assigned to the index.
    try genReleaseIfRetainedTempAt(self, accessLeftv, left.head.accessExpr.left);
}

fn genMethodDecl(self: *CompileChunk, structId: cy.TypeId, node: cy.Node, func: sema.FuncDecl, name: []const u8) !void {
    // log.debug("gen method {s}", .{name});
    const methodId = try self.compiler.vm.ensureMethodSymKey(name, func.numParams - 1);

    const jumpPc = try self.pushEmptyJump();

    try self.pushSemaBlock(func.semaBlockId);

    const opStart = @intCast(u32, self.buf.ops.items.len);
    try self.reserveFuncParams(func.numParams);
    try genInitLocals(self);
    try genStatements(self, node.head.func.bodyHead, false);
    // TODO: Check last statement to skip adding ret.
    try self.genBlockEnding();

    // Reserve another local for the call return info.
    const numLocals = @intCast(u32, self.blockNumLocals() + 1 - func.numParams);

    self.popSemaBlock();

    self.patchJumpToCurrent(jumpPc);

    const sym = cy.MethodSym.initFuncOffset(opStart, numLocals);
    try self.compiler.vm.addMethodSym(structId, methodId, sym);
}

fn genSetVarToExpr(self: *CompileChunk, leftId: cy.NodeId, exprId: cy.NodeId, comptime discardTopExprReg: bool) !void {
    _ = discardTopExprReg;
    const varId = self.nodes[leftId].head.ident.semaVarId;
    const expr = self.nodes[exprId];
    if (self.genGetVarPtr(varId)) |svar| {
        if (svar.isBoxed) {
            if (!svar.genIsDefined) {
                const exprv = try self.genExpr(exprId, false);
                try self.buf.pushOp2(.box, @intCast(u8, exprv.local), svar.local);

                // ARC cleanup.
                try genReleaseIfRetainedTemp(self, exprv);

                svar.vtype = exprv.vtype;

                svar.genIsDefined = true;
                return;
            } else {
                try genSetBoxedVarToExpr(self, svar, exprId);
                return;
            }
        }

        if (expr.node_t == .ident) {
            const crSymId = expr.head.ident.sema_crSymId;
            if (crSymId.isPresent()) {
                // Copying a symbol.
                if (!svar.genIsDefined or !svar.vtype.rcCandidate) {
                    const exprv = try self.genRetainedExprTo(exprId, svar.local, false);
                    svar.vtype = exprv.vtype;
                    svar.genIsDefined = true;
                } else {
                    const exprv = try self.genRetainedTempExpr(exprId, false);
                    svar.vtype = exprv.vtype;
                    try self.buf.pushOp2(.copyReleaseDst, exprv.local, svar.local);
                }
                return;
            }

            const exprv = try self.genExpr(exprId, false);

            if (svar.genIsDefined) {
                if (svar.vtype.rcCandidate) {
                    // log.debug("releaseSet {} {}", .{varId, svar.vtype.typeT});
                    if (exprv.vtype.rcCandidate) {
                        try self.buf.pushOp2(.copyRetainRelease, exprv.local, svar.local);
                    } else {
                        try self.buf.pushOp2(.copyReleaseDst, exprv.local, svar.local);
                    }
                } else {
                    // log.debug("set {} {}", .{varId, svar.vtype.typeT});
                    if (exprv.vtype.rcCandidate) {
                        try self.buf.pushOp2(.copyRetainSrc, exprv.local, svar.local);
                    } else {
                        try self.buf.pushOp2(.copy, exprv.local, svar.local);
                    }
                }
                if (svar.vtype.typeT != exprv.vtype.typeT) {
                    svar.vtype = exprv.vtype;
                }
            } else {
                if (exprv.vtype.rcCandidate) {
                    if (exprv.vtype.typeT == .box) {
                        // TODO: Becomes boxValue if child is known to not be an rcCandidate.
                        try self.buf.pushOp2(.boxValueRetain, exprv.local, svar.local);
                    } else {
                        try self.buf.pushOp2(.copyRetainSrc, exprv.local, svar.local);
                    }
                } else {
                    try self.buf.pushOp2(.copy, exprv.local, svar.local);
                }
                svar.genIsDefined = true;
                svar.vtype = exprv.vtype;
            }
            return;
        } else if (expr.node_t == .tagLiteral) {
            if (svar.vtype.typeT == .tag) {
                const name = self.getNodeTokenString(expr);
                const symId = try self.compiler.vm.ensureTagLitSym(name);
                const sym = self.compiler.vm.tagLitSyms.buf[symId];
                if (sym.symT == .one and sym.inner.one.id == svar.vtype.inner.tag.tagId) {
                    try self.buf.pushOp3(.tag, svar.vtype.inner.tag.tagId, @intCast(u8, sym.inner.one.val), svar.local);
                    return;
                }
            }
        }

        // Retain rval.
        if (!svar.genIsDefined or !svar.vtype.rcCandidate) {
            const exprv = try self.genRetainedExprTo(exprId, svar.local, false);
            svar.vtype = exprv.vtype;
            if (!svar.genIsDefined) {
                svar.genIsDefined = true;
            }
        } else {
            const exprv = try self.genRetainedTempExpr(exprId, false);
            try self.buf.pushOp2(.copyReleaseDst, exprv.local, svar.local);
            svar.vtype = exprv.vtype;
        }
    } else {
        const left = self.nodes[leftId];
        return self.reportErrorAt("Undefined var: `{}`", &.{v(self.getNodeTokenString(left))}, leftId);
    }
}

fn genSetBoxedVarToExpr(self: *CompileChunk, svar: *sema.LocalVar, exprId: cy.NodeId) !void {
    // Retain rval.
    const exprv = try self.genRetainedTempExpr(exprId, false);
    svar.vtype = exprv.vtype;
    svar.genIsDefined = true;
    if (!svar.vtype.rcCandidate) {
        try self.buf.pushOp2(.setBoxValue, svar.local, exprv.local);
    } else {
        try self.buf.pushOp2(.setBoxValueRelease, svar.local, exprv.local);
    }
}

/// Reserve locals upfront and gen var initializer if necessary.
pub fn genInitLocals(self: *CompileChunk) !void {
    const sblock = sema.curBlock(self);

    // Reserve the locals.
    var numInitializers: u32 = 0;
    for (sblock.locals.items) |varId| {
        const svar = self.genGetVarPtr(varId).?;
        _ = try self.reserveLocalVar(varId);
        if (svar.genInitializer) {
            numInitializers += 1;
        }
        // log.debug("reserve {} {s}", .{local, self.getVarName(varId)});
    }

    if (numInitializers > 0) {
        try self.buf.pushOp1(.setInitN, @intCast(u8, numInitializers));
        for (sblock.locals.items) |varId| {
            const svar = self.genGetVar(varId).?;
            if (svar.genInitializer) {
                // log.debug("init {} {s}", .{svar.local, self.getVarName(varId)});
                try self.buf.pushOperand(svar.local);
            }
        }
    }

    self.resetNextFreeTemp();
}

fn unexpectedFmt(format: []const u8, vals: []const fmt.FmtValue) noreturn {
    if (builtin.mode == .Debug) {
        fmt.printStderr(format, vals);
    }
    stdx.fatal();
}

fn pushCallObjSym(chunk: *cy.CompileChunk, startLocal: u8, numArgs: u8, numRet: u8, symId: u8, nodeId: cy.NodeId) !void {
    try chunk.pushDebugSym(nodeId);
    try chunk.buf.pushOpSlice(.callObjSym, &.{ startLocal, numArgs, numRet, symId, 0, 0, 0, 0, 0, 0, 0, 0, 0 });
}