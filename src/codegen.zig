const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const cy = @import("cyber.zig");
const RegisterCstr = cy.register.RegisterCstr;
const RegisterId = cy.register.RegisterId;
const rt = cy.rt;
const sema = cy.sema;
const types = cy.types;
const bt = types.BuiltinTypeSymIds;
const Chunk = cy.Chunk;
const fmt = @import("fmt.zig");
const v = fmt.v;
const log = stdx.log.scoped(.codegen);

const LocalId = u8;

pub const GenValue = struct {
    vtype: types.TypeId,

    /// TODO: Rename to reg.
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

    pub fn initLocalValue(local: LocalId, vtype: types.TypeId, retained: bool) GenValue {
        return .{
            .vtype = vtype,
            .local = local,
            .isTempLocal = false,
            .retained = retained,
        };
    }

    pub fn initTempValue(local: LocalId, vtype: types.TypeId, retained: bool) GenValue {
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

fn identifier(c: *Chunk, nodeId: cy.NodeId, cstr: RegisterCstr) !GenValue {
    const node = c.nodes[nodeId];
    if (c.genGetVar(node.head.ident.semaVarId)) |svar| {
        // crSymId would be active instead.
        stdx.debug.dassert(!svar.isStaticAlias);

        var dst: RegisterId = undefined;
        if (!svar.isCaptured()) {
            dst = try c.rega.selectFromLocalVar(cstr, svar.local);
        } else {
            dst = try c.rega.selectFromNonLocalVar(cstr, cstr.mustRetain);
        }
        if (cstr.mustRetain and types.isRcCandidateType(c.compiler, svar.vtype)) {
            // Ensure retain +1.
            if (svar.isBoxed) {
                if (svar.isCaptured()) {
                    try c.buf.pushOp3(.captured, c.curBlock.closureLocal, svar.capturedIdx, dst);
                    try c.buf.pushOp2(.boxValueRetain, dst, dst);
                } else {
                    try c.buf.pushOp2(.boxValueRetain, svar.local, dst);
                }
            } else {
                if (dst == svar.local) {
                    try c.buf.pushOp1(.retain, svar.local);
                } else {
                    try c.buf.pushOp2(.copyRetainSrc, svar.local, dst);
                }
            }
            return c.initGenValue(dst, svar.vtype, true);
        } else {
            if (svar.isBoxed) {
                if (svar.isCaptured()) {
                    try c.buf.pushOp3(.captured, c.curBlock.closureLocal, svar.capturedIdx, dst);
                    try c.buf.pushOp2(.boxValue, dst, dst);
                } else {
                    try c.buf.pushOp2(.boxValue, svar.local, dst);
                }
            } else {
                if (dst == svar.local) {
                    // Nop.
                } else {
                    try c.buf.pushOp2(.copy, svar.local, dst);
                }
            }
            return c.initGenValue(dst, svar.vtype, false);
        }
    } else {
        return symbolTo(c, node.head.ident.sema_crSymId, cstr);
    }
}

fn symbolTo(self: *Chunk, crSymId: sema.CompactResolvedSymId, req: RegisterCstr) !GenValue {
    if (crSymId.isFuncSymId) {
        const rFuncSym = self.compiler.sema.getResolvedFuncSym(crSymId.id);
        const rSymId = rFuncSym.getResolvedSymId();
        const rSym = self.compiler.sema.getResolvedSym(rSymId);

        const rtSymId = try self.compiler.vm.ensureFuncSym(rSym.key.absResolvedSymKey.rParentSymId, rSym.key.absResolvedSymKey.nameId, rFuncSym.getResolvedFuncSigId());
        const pc = self.buf.len();

        const dst = try self.rega.selectFromNonLocalVar(req, true);

        try self.pushOptionalDebugSym(self.curNodeId);
        try self.buf.pushOp3(.staticFunc, 0, 0, dst);
        self.buf.setOpArgU16(pc + 1, @intCast(rtSymId));
        return self.initGenValue(dst, bt.Any, true);
    } else {
        const rSym = self.compiler.sema.getResolvedSym(crSymId.id);
        switch (rSym.symT) {
            .variable => {
                const key = rSym.key.absResolvedSymKey;
                const varId = try self.compiler.vm.ensureVarSym(key.rParentSymId, key.nameId);

                const dst = try self.rega.selectFromNonLocalVar(req, true);
                try self.pushOptionalDebugSym(self.curNodeId);       
                const pc = self.buf.len();
                try self.buf.pushOp3(.staticVar, 0, 0, dst);
                self.buf.setOpArgU16(pc + 1, @intCast(varId));

                const stype = rSym.inner.variable.rTypeSymId;
                return self.initGenValue(dst, stype, true);
            },
            .builtinType => {
                const typeId = rSym.inner.builtinType.typeId;
                const dst = try self.rega.selectFromNonLocalVar(req, true);
                try self.buf.pushOp1(.sym, @intFromEnum(cy.heap.MetaTypeKind.object));
                try self.buf.pushOperandsRaw(std.mem.asBytes(&typeId));
                try self.buf.pushOperand(dst);
                return self.initGenValue(dst, bt.MetaType, true);
            },
            .object => {
                const typeId = rSym.inner.object.typeId;
                const dst = try self.rega.selectFromNonLocalVar(req, true);
                try self.buf.pushOp1(.sym, @intFromEnum(cy.heap.MetaTypeKind.object));
                try self.buf.pushOperandsRaw(std.mem.asBytes(&typeId));
                try self.buf.pushOperand(dst);
                return self.initGenValue(dst, bt.MetaType, true);
            },
            else => {
                const name = sema.getName(self.compiler, rSym.key.absResolvedSymKey.nameId);
                return self.reportError("Can't use symbol `{}` as a value.", &.{v(name)});
            }
        }
    }
}

fn stringTemplate(self: *Chunk, nodeId: cy.NodeId, req: RegisterCstr) !GenValue {
    const node = self.nodes[nodeId];
    const operandStart = self.operandStack.items.len;
    defer self.operandStack.items.len = operandStart;

    const dst = try self.rega.selectFromNonLocalVar(req, true); 
    const tempStart = self.rega.getNextTemp();
    defer self.rega.setNextTemp(tempStart);

    var expStringPart = true;
    var curId = node.head.stringTemplate.partsHead;
    var numExprs: u32 = 0;
    while (curId != cy.NullId) {
        const cur = self.nodes[curId];
        if (expStringPart) {
            const raw = self.getNodeTokenString(cur);
            const str = try self.unescapeString(raw);
            const idx = try self.buf.getOrPushStringConst(str);
            try self.operandStack.append(self.alloc, cy.InstDatum.initArg(@intCast(idx)));
        } else {
            _ = try expression(self, curId, RegisterCstr.tempMustRetain);
            numExprs += 1;
        }
        curId = cur.next;
        expStringPart = !expStringPart;
    }

    try self.pushOptionalDebugSym(nodeId);
    try self.buf.pushOp3(.stringTemplate, tempStart, @intCast(numExprs), dst);
    try self.buf.pushOperands(self.operandStack.items[operandStart..]);
    return self.initGenValue(dst, bt.String, true);
}

fn objectInit(self: *Chunk, nodeId: cy.NodeId, req: RegisterCstr) !GenValue {
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

            const numFields = self.compiler.vm.types.buf[typeId].numFields;
            // Repurpose stack for sorting fields.
            const sortedFieldsStart = self.assignedVarStack.items.len;
            try self.assignedVarStack.resize(self.alloc, self.assignedVarStack.items.len + numFields);
            defer self.assignedVarStack.items.len = sortedFieldsStart;

            // Initially set to NullId so leftovers are defaulted to `none`.
            const initFields = self.assignedVarStack.items[sortedFieldsStart..];
            @memset(initFields, cy.NullId);

            const dst = try self.rega.selectFromNonLocalVar(req, true);
            const tempStart = self.rega.getNextTemp();
            defer self.rega.setNextTemp(tempStart);

            var i: u32 = 0;
            var entryId = initializer.head.child_head;
            // First iteration to sort the initializer fields.
            while (entryId != cy.NullId) : (i += 1) {
                const entry = self.nodes[entryId];
                const prop = self.nodes[entry.head.mapEntry.left];
                const fieldName = self.getNodeTokenString(prop);
                const fieldIdx = self.compiler.vm.getStructFieldIdx(typeId, fieldName) orelse {
                    const objectName = self.compiler.vm.types.buf[typeId].name;
                    return self.reportErrorAt("Missing field `{}` in `{}`.", &.{v(fieldName), v(objectName)}, entry.head.mapEntry.left);
                };
                initFields[fieldIdx] = entryId;
                entryId = entry.next;
            }

            i = 0;
            while (i < numFields) : (i += 1) {
                entryId = self.assignedVarStack.items[sortedFieldsStart + i];
                if (entryId == cy.NullId) {
                    // Push none.
                    const local = try self.rega.consumeNextTemp();
                    try self.buf.pushOp1(.none, local);
                } else {
                    const entry = self.nodes[entryId];
                    _ = try expression(self, entry.head.mapEntry.right, RegisterCstr.tempMustRetain);
                }
            }

            if (self.compiler.vm.types.buf[typeId].numFields <= 4) {
                try self.pushOptionalDebugSym(nodeId);
                try self.buf.pushOpSlice(.objectSmall, &[_]u8{ @intCast(typeId), tempStart, @intCast(numFields), dst });
            } else {
                try self.buf.pushOpSlice(.object, &[_]u8{ @intCast(typeId), tempStart, @intCast(numFields), dst });
            }
            const type_ = node.head.objectInit.sema_rSymId;
            return GenValue.initTempValue(dst, type_, true);
        }
    }
    const oname = self.getNodeTokenString(stype);
    return self.reportErrorAt("Expected object type: `{}`", &.{v(oname)}, nodeId);
}

fn mapInit(self: *Chunk, nodeId: cy.NodeId, req: RegisterCstr) !GenValue {
    const node = self.nodes[nodeId];
    const operandStart = self.operandStack.items.len;
    defer self.operandStack.items.len = operandStart;

    const dst = try self.rega.selectFromNonLocalVar(req, true);

    const tempStart = self.rega.getNextTemp();
    defer self.rega.setNextTemp(tempStart);

    var i: u32 = 0;
    var entry_id = node.head.child_head;
    while (entry_id != cy.NullId) : (i += 1) {
        var entry = self.nodes[entry_id];
        const key = self.nodes[entry.head.mapEntry.left];

        switch (key.node_t) {
            .ident => {
                const name = self.getNodeTokenString(key);
                const idx = try self.buf.getOrPushStringConst(name);

                try self.operandStack.ensureUnusedCapacity(self.alloc, 2);
                @as(*align (1) u16, @ptrCast(self.operandStack.items.ptr + self.operandStack.items.len)).* = @intCast(idx);
                self.operandStack.items.len += 2;
            },
            .string => {
                const name = self.getNodeTokenString(key);
                const idx = try self.buf.getOrPushStringConst(name);
                try self.operandStack.ensureUnusedCapacity(self.alloc, 2);
                @as(*align (1) u16, @ptrCast(self.operandStack.items.ptr + self.operandStack.items.len)).* = @intCast(idx);
                self.operandStack.items.len += 2;
            },
            else => stdx.panicFmt("unsupported key {}", .{key.node_t}),
        }

        _ = try expression(self, entry.head.mapEntry.right, RegisterCstr.tempMustRetain);
        entry_id = entry.next;
    }

    if (i == 0) {
        try self.buf.pushOp1(.mapEmpty, dst);
    } else {
        try self.buf.pushOp3(.map, tempStart, @intCast(i), dst);
        try self.buf.pushOperands(self.operandStack.items[operandStart..]);
    }
    return self.initGenValue(dst, bt.Map, true);
}

fn binExprGeneric(c: *Chunk, code: cy.OpCode, nodeId: cy.NodeId, vtype: types.TypeId, req: RegisterCstr) !GenValue {
    const node = c.nodes[nodeId];
    const left = node.head.binExpr.left;
    const right = node.head.binExpr.right;

    const dst = try c.rega.selectFromNonLocalVar(req, false);
    const tempStart = c.rega.getNextTemp();
    defer c.rega.setNextTemp(tempStart);

    var canUseDst = c.canUseDstAsTempForBinOp(dst);
    const leftv = try expression(c, left, RegisterCstr.preferIf(dst, canUseDst));
    if (leftv.local == dst) {
        canUseDst = false;
    }
    const rightv = try expression(c, right, RegisterCstr.preferIf(dst, canUseDst));

    switch (code) {
        .mod,
        .pow,
        .div,
        .mul => {
            try c.pushDebugSym(nodeId);
        },
        else => {
            try c.pushOptionalDebugSym(nodeId);
        },
    }
    try c.buf.pushOp3(code, leftv.local, rightv.local, dst);

    // ARC cleanup.
    try releaseIfRetainedTemp(c, leftv);
    try releaseIfRetainedTemp(c, rightv);

    return c.initGenValue(dst, vtype, false);
}

fn binExpr(c: *Chunk, nodeId: cy.NodeId, cstr: RegisterCstr) !GenValue {
    const node = c.nodes[nodeId];
    const left = node.head.binExpr.left;
    const right = node.head.binExpr.right;

    const op = node.head.binExpr.op;
    switch (op) {
        .percent => {
            return binExprGeneric(c, .mod, nodeId, bt.Number, cstr);
        },
        .slash => {
            return binExprGeneric(c, .div, nodeId, bt.Number, cstr);
        },
        .star => {
            return binExprGeneric(c, .mul, nodeId, bt.Number, cstr);
        },
        .caret => {
            return binExprGeneric(c, .pow, nodeId, bt.Number, cstr);
        },
        .equal_equal => {
            return binExprGeneric(c, .compare, nodeId, bt.Boolean, cstr);
        },
        .bang_equal => {
            return binExprGeneric(c, .compareNot, nodeId, bt.Boolean, cstr);
        },
        .plus => {
            const dst = try c.rega.selectFromNonLocalVar(cstr, false);
            const tempStart = c.rega.getNextTemp();
            defer c.rega.setNextTemp(tempStart);

            var canUseDst = c.canUseDstAsTempForBinOp(dst);
            const leftv = try expression(c, left, RegisterCstr.preferIf(dst, canUseDst));
            if (leftv.local == dst) {
                canUseDst = false;
            }
            const rightv = try expression(c, right, RegisterCstr.preferIf(dst, canUseDst));

            if (leftv.vtype == bt.Integer and rightv.vtype == bt.Integer) {
                try c.buf.pushOp3(.addInt, leftv.local, rightv.local, dst);
                return c.initGenValue(dst, bt.Integer, false);
            }
            try c.pushDebugSym(nodeId);
            try c.buf.pushOp3(.add, leftv.local, rightv.local, dst);

            // ARC cleanup.
            try releaseIfRetainedTemp(c, leftv);
            try releaseIfRetainedTemp(c, rightv);

            return c.initGenValue(dst, bt.Number, false);
        },
        .minus => {
            const dst = try c.rega.selectFromNonLocalVar(cstr, false);
            const tempStart = c.rega.getNextTemp();
            defer c.rega.setNextTemp(tempStart);

            var canUseDst = c.canUseDstAsTempForBinOp(dst);
            const leftv = try expression(c, left, RegisterCstr.preferIf(dst, canUseDst));
            if (leftv.local == dst) {
                canUseDst = false;
            }
            const rightv = try expression(c, right, RegisterCstr.preferIf(dst, canUseDst));

            if (leftv.vtype == bt.Integer and rightv.vtype == bt.Integer) {
                try c.buf.pushOp3(.subInt, leftv.local, rightv.local, dst);
                return c.initGenValue(dst, bt.Integer, false);
            }
            try c.pushDebugSym(nodeId);
            try c.buf.pushOp3(.sub, leftv.local, rightv.local, dst);

            // ARC cleanup.
            try releaseIfRetainedTemp(c, leftv);
            try releaseIfRetainedTemp(c, rightv);

            return c.initGenValue(dst, bt.Number, false);
        },
        .less => {
            const dst = try c.rega.selectFromNonLocalVar(cstr, false);
            const tempStart = c.rega.getNextTemp();
            defer c.rega.setNextTemp(tempStart);

            var canUseDst = c.canUseDstAsTempForBinOp(dst);
            const leftv = try expression(c, left, RegisterCstr.preferIf(dst, canUseDst));
            if (leftv.local == dst) {
                canUseDst = false;
            }
            const rightv = try expression(c, right, RegisterCstr.preferIf(dst, canUseDst));

            if (c.nodeTypes[left] == bt.Integer and c.nodeTypes[right] == bt.Integer) {
                try c.buf.pushOp3(.lessInt, leftv.local, rightv.local, dst);
                return c.initGenValue(dst, bt.Boolean, false);
            } else {
                try c.pushDebugSym(nodeId);
                try c.buf.pushOp3(.less, leftv.local, rightv.local, dst);
                return c.initGenValue(dst, bt.Boolean, false);
            }
        },
        .less_equal => {
            return binExprGeneric(c, .lessEqual, nodeId, bt.Boolean, cstr);
        },
        .greater => {
            return binExprGeneric(c, .greater, nodeId, bt.Boolean, cstr);
        },
        .greater_equal => {
            return binExprGeneric(c, .greaterEqual, nodeId, bt.Boolean, cstr);
        },
        .bitwiseAnd => {
            return binExprGeneric(c, .bitwiseAnd, nodeId, bt.Number, cstr);
        },
        .bitwiseOr => {
            return binExprGeneric(c, .bitwiseOr, nodeId, bt.Number, cstr);
        },
        .bitwiseXor => {
            return binExprGeneric(c, .bitwiseXor, nodeId, bt.Number, cstr);
        },
        .bitwiseLeftShift => {
            return binExprGeneric(c, .bitwiseLeftShift, nodeId, bt.Number, cstr);
        },
        .bitwiseRightShift => {
            return binExprGeneric(c, .bitwiseRightShift, nodeId, bt.Number, cstr);
        },
        .and_op => {
            const dst = try c.rega.selectFromNonLocalVar(cstr, true);

            const leftv = try expression(c, left, RegisterCstr.initExact(dst, cstr.mustRetain));
            const jumpPc = try c.pushEmptyJumpNotCond(leftv.local);

            // ARC cleanup. First operand is not needed anymore.
            try releaseIfRetainedTemp(c, leftv);

            const rightv = try expression(c, right, RegisterCstr.initExact(dst, cstr.mustRetain));
            c.patchJumpNotCondToCurPc(jumpPc);

            if (leftv.vtype  == rightv.vtype) {
                return c.initGenValue(dst, leftv.vtype, leftv.retained or rightv.retained);
            } else {
                return c.initGenValue(dst, bt.Any, leftv.retained or rightv.retained);
            }
        },
        .or_op => {
            const dst = try c.rega.selectFromNonLocalVar(cstr, true);

            const leftv = try expression(c, left, RegisterCstr.initExact(dst, cstr.mustRetain));
            const jumpPc = try c.pushEmptyJumpCond(leftv.local);

            // ARC cleanup. First operand is not needed anymore.
            try releaseIfRetainedTemp(c, leftv);

            const rightv = try expression(c, right, RegisterCstr.initExact(dst, cstr.mustRetain));
            c.patchJumpCondToCurPc(jumpPc);

            if (leftv.vtype == rightv.vtype) {
                return c.initGenValue(dst, leftv.vtype, leftv.retained or rightv.retained);
            } else {
                return c.initGenValue(dst, bt.Any, leftv.retained or rightv.retained);
            }
        },
        else => return c.reportErrorAt("Unsupported binary op: {}", &.{v(op)}, nodeId),
    }
}

fn lambdaMulti(self: *Chunk, nodeId: cy.NodeId, req: RegisterCstr) !GenValue {
    const node = self.nodes[nodeId];

    const jumpPc = try self.pushEmptyJump();

    const func = self.semaFuncDecls.items[node.head.func.semaDeclId];

    try self.pushSemaBlock(func.semaBlockId);
    self.curBlock.frameLoc = nodeId;

    const jumpStackStart = self.blockJumpStack.items.len;
    const opStart: u32 = @intCast(self.buf.ops.items.len);

    // Generate function body.
    try self.reserveFuncParams(func.numParams);
    try initVarLocals(self);

    try genStatements(self, node.head.func.bodyHead, false);

    try self.genBlockEnding();
    self.patchJumpToCurPc(jumpPc);

    const sblock = sema.curBlock(self);
    const numCaptured: u8 = @intCast(sblock.captures.items.len);
    const closureLocal = self.curBlock.closureLocal;

    self.patchBlockJumps(jumpStackStart);
    self.blockJumpStack.items.len = jumpStackStart;

    const stackSize = self.getMaxUsedRegisters();
    self.popSemaBlock();

    const dst = try self.rega.selectFromNonLocalVar(req, true);

    if (numCaptured == 0) {
        const funcPcOffset: u8 = @intCast(self.buf.ops.items.len - opStart);
        try self.pushOptionalDebugSym(nodeId);

        const start = self.buf.ops.items.len;
        try self.buf.pushOpSlice(.lambda, &.{ funcPcOffset, func.numParams, stackSize, 0, 0, dst });
        self.buf.setOpArgU16(start + 4, @intCast(func.inner.lambda.rFuncSigId));
        return self.initGenValue(dst, bt.Any, true);
    } else {
        const operandStart = self.operandStack.items.len;
        defer self.operandStack.items.len = operandStart;

        for (sblock.captures.items) |varId| {
            const pId = self.capVarDescs.get(varId).?.user;
            const pvar = &self.vars.items[pId];
            if (!pvar.isBoxed) {
                // Ensure captured vars are boxed.
                try self.buf.pushOp2(.box, pvar.local, pvar.local);
                pvar.isBoxed = true;
            }
            try self.pushTempOperand(pvar.local);
        }

        const funcPcOffset: u8 = @intCast(self.buf.ops.items.len - opStart);
        const start = self.buf.ops.items.len;
        try self.buf.pushOpSlice(.closure, &.{ funcPcOffset, func.numParams, numCaptured, stackSize, 0, 0, closureLocal, dst });
        self.buf.setOpArgU16(start + 5, @intCast(func.inner.lambda.rFuncSigId));
        try self.buf.pushOperands(self.operandStack.items[operandStart..]);
        return self.initGenValue(dst, bt.Any, true);
    }
}

fn lambdaExpr(self: *Chunk, nodeId: cy.NodeId, req: RegisterCstr) !GenValue {
    const node = self.nodes[nodeId];

    const jumpPc = try self.pushEmptyJump();

    const func = self.semaFuncDecls.items[node.head.func.semaDeclId];
    try self.pushSemaBlock(func.semaBlockId);
    const opStart: u32 = @intCast(self.buf.ops.items.len);

    // Generate function body.
    try self.reserveFuncParams(func.numParams);
    try initVarLocals(self);

    _ = try expression(self, node.head.func.bodyHead, RegisterCstr.exactMustRetain(0));
    try self.endLocals();
    try self.buf.pushOp(.ret1);
    self.patchJumpToCurPc(jumpPc);

    const sblock = sema.curBlock(self);
    const numCaptured: u8 = @intCast(sblock.captures.items.len);
    const closureLocal = self.curBlock.closureLocal;
    const stackSize = self.getMaxUsedRegisters();
    self.popSemaBlock();

    const dst = try self.rega.selectFromNonLocalVar(req, true);

    if (numCaptured == 0) {
        const funcPcOffset: u8 = @intCast(self.buf.ops.items.len - opStart);
        try self.pushOptionalDebugSym(nodeId);
        const start = self.buf.ops.items.len;
        try self.buf.pushOpSlice(.lambda, &.{ funcPcOffset, func.numParams, stackSize, 0, 0, dst });
        self.buf.setOpArgU16(start + 4, @intCast(func.inner.lambda.rFuncSigId));
        return self.initGenValue(dst, bt.Any, true);
    } else {
        const operandStart = self.operandStack.items.len;
        defer self.operandStack.items.len = operandStart;

        for (sblock.captures.items) |varId| {
            const pId = self.capVarDescs.get(varId).?.user;
            const pvar = &self.vars.items[pId];
            if (!pvar.isBoxed) {
                // Ensure captured vars are boxed.
                try self.buf.pushOp2(.box, pvar.local, pvar.local);
                pvar.isBoxed = true;
            }
            try self.pushTempOperand(pvar.local);
        }

        const funcPcOffset: u8 = @intCast(self.buf.ops.items.len - opStart);
        const start = self.buf.ops.items.len;
        try self.buf.pushOpSlice(.closure, &.{ funcPcOffset, func.numParams, numCaptured, stackSize, 0, 0, closureLocal, dst });
        self.buf.setOpArgU16(start + 5, @intCast(func.inner.lambda.rFuncSigId));
        try self.buf.pushOperands(self.operandStack.items[operandStart..]);
        return self.initGenValue(dst, bt.Any, true);
    }
}

fn listInit(self: *Chunk, nodeId: cy.NodeId, req: RegisterCstr) !GenValue {
    const node = self.nodes[nodeId];

    const dst = try self.rega.selectFromNonLocalVar(req, true);
    const tempStart = self.rega.getNextTemp();
    defer self.rega.setNextTemp(tempStart);

    var exprId = node.head.child_head;
    var i: u32 = 0;
    while (exprId != cy.NullId) : (i += 1) {
        const expr = self.nodes[exprId];
        _ = try expression(self, exprId, RegisterCstr.tempMustRetain);
        exprId = expr.next;
    }

    try self.pushDebugSym(nodeId);
    try self.buf.pushOp3(.list, tempStart, @intCast(i), dst);
    return self.initGenValue(dst, bt.List, true);
}

fn sliceExpr(self: *Chunk, nodeId: cy.NodeId, cstr: RegisterCstr) !GenValue {
    const node = self.nodes[nodeId];

    const dst = try self.rega.selectFromNonLocalVar(cstr, true);
    const tempStart = self.rega.getNextTemp();
    defer self.rega.setNextTemp(tempStart);

    var canUseDst = self.canUseDstAsTempForBinOp(dst);

    // Parent value.
    const parentv = try expression(self, node.head.sliceExpr.arr, RegisterCstr.preferIf(dst, canUseDst));
    if (parentv.local == dst) {
        canUseDst = false;
    }

    // Range left value.
    var leftv: GenValue = undefined;
    if (node.head.sliceExpr.left == cy.NullId) {
        if (canUseDst) {
            leftv = try constNumber(self, 0, dst);
            canUseDst = false;
        } else {
            const leftDst = try self.rega.consumeNextTemp();
            leftv = try constNumber(self, 0, leftDst);
        }
    } else {
        leftv = try expression(self, node.head.sliceExpr.left, RegisterCstr.preferIf(dst, canUseDst));
        if (leftv.local == dst) {
            canUseDst = false;
        }
    }

    // Range right value.
    var rightv: GenValue = undefined;
    if (node.head.sliceExpr.right == cy.NullId) {
        if (canUseDst) {
            rightv = try genNone(self, dst);
            canUseDst = false;
        } else {
            const rightDst = try self.rega.consumeNextTemp();
            rightv = try genNone(self, rightDst);
        }
    } else {
        rightv = try expression(self, node.head.sliceExpr.right, RegisterCstr.preferIf(dst, canUseDst));
    }

    try self.pushDebugSym(nodeId);
    try self.buf.pushOpSlice(.slice, &.{ parentv.local, leftv.local, rightv.local, dst });

    // ARC cleanup.
    try releaseIfRetainedTemp(self, parentv);
    try releaseIfRetainedTemp(self, leftv);
    try releaseIfRetainedTemp(self, rightv);

    return self.initGenValue(dst, bt.List, true);
}

fn indexExpr(self: *Chunk, nodeId: cy.NodeId, cstr: RegisterCstr) !GenValue {
    const node = self.nodes[nodeId];

    const dst = try self.rega.selectFromNonLocalVar(cstr, true);
    const tempStart = self.rega.getNextTemp();
    defer self.rega.setNextTemp(tempStart);

    var canUseDst = self.canUseDstAsTempForBinOp(dst);

    // Gen left.
    const leftv = try expression(self, node.head.left_right.left, RegisterCstr.preferIf(dst, canUseDst));
    if (leftv.local == dst) {
        canUseDst = false;
    }

    // Gen index.
    const index = self.nodes[node.head.left_right.right];
    const isReverseIndex = index.node_t == .unary_expr and index.head.unary.op == .minus;
    const indexv = try expression(self, node.head.left_right.right, RegisterCstr.preferIf(dst, canUseDst));

    try self.pushDebugSym(nodeId);
    if (isReverseIndex) {
        try self.buf.pushOp3(.reverseIndex, leftv.local, indexv.local, dst);
    } else {
        try self.buf.pushOp3(.index, leftv.local, indexv.local, dst);
    }

    // ARC cleanup.
    try releaseIfRetainedTemp(self, leftv);
    try releaseIfRetainedTemp(self, indexv);

    return self.initGenValue(dst, bt.Any, true);
}

fn accessExpr(self: *Chunk, nodeId: cy.NodeId, req: RegisterCstr) !GenValue {
    const node = self.nodes[nodeId];
    const crSymId = node.head.accessExpr.sema_crSymId;
    if (crSymId.isPresent()) {
        if (crSymId.isFuncSymId) {
            const rFuncSym = self.compiler.sema.getResolvedFuncSym(crSymId.id);
            const rSym = self.compiler.sema.getResolvedSym(rFuncSym.getResolvedSymId());
            const key = rSym.key.absResolvedSymKey;
            const rtSymId = try self.compiler.vm.ensureFuncSym(key.rParentSymId, key.nameId, rFuncSym.getResolvedFuncSigId());

            try self.pushOptionalDebugSym(nodeId);
            const pc = self.buf.len();
            const dst = try self.rega.selectFromNonLocalVar(req, true);
            try self.buf.pushOp3(.staticFunc, 0, 0, dst);
            self.buf.setOpArgU16(pc + 1, @intCast(rtSymId));
            return self.initGenValue(dst, bt.Any, true);
        } else {
            const rSym = self.compiler.sema.getResolvedSym(crSymId.id);
            const key = rSym.key.absResolvedSymKey;
            switch (rSym.symT) {
                .variable => {
                    const rtSymId = self.compiler.vm.getVarSym(key.rParentSymId, key.nameId).?;
                    // Static variable.
                    try self.pushOptionalDebugSym(nodeId);       
                    const pc = self.buf.len();

                    const dst = try self.rega.selectFromNonLocalVar(req, true);
                    try self.buf.pushOp3(.staticVar, 0, 0, dst);
                    self.buf.setOpArgU16(pc + 1, @intCast(rtSymId));

                    const vtype = rSym.inner.variable.rTypeSymId;
                    return self.initGenValue(dst, vtype, true);
                },
                .enumMember => {
                    const enumId = rSym.inner.enumMember.enumId;
                    const val = cy.Value.initEnum(@intCast(enumId), @intCast(rSym.inner.enumMember.memberId));
                    const idx = try self.buf.pushConst(cy.Const.init(val.val));
                    const dst = try self.rega.selectFromNonLocalVar(req, false);
                    try constOp(self, idx, dst);

                    const vtype = rSym.key.absResolvedSymKey.rParentSymId;
                    return self.initGenValue(dst, vtype, false);
                },
                .object => {
                    const typeId = rSym.getObjectTypeId(self.compiler.vm).?;
                    const dst = try self.rega.selectFromNonLocalVar(req, true);
                    try self.buf.pushOp1(.sym, @intFromEnum(cy.heap.MetaTypeKind.object));
                    try self.buf.pushOperandsRaw(std.mem.asBytes(&typeId));
                    try self.buf.pushOperand(dst);
                    return self.initGenValue(dst, bt.MetaType, true);
                },
                else => {
                    return self.reportError("Unsupported accessExpr: {}", &.{v(rSym.symT)});
                }
            }
        }
    } else {
        return field(self, nodeId, req);
    }
}

fn constOp(c: *cy.Chunk, idx: usize, dst: u8) !void {
    const pc = c.buf.len();
    try c.buf.pushOp3(.constOp, 0, 0, dst);
    c.buf.setOpArgU16(pc + 1, @intCast(idx));
}

fn releaseIfRetainedTempAt(c: *cy.Chunk, val: GenValue, nodeId: cy.NodeId) !void {
    if (val.retained and val.isTempLocal) {
        try c.pushOptionalDebugSym(nodeId);
        try c.buf.pushOp1(.release, val.local);
    }
}

fn releaseIfRetainedTemp(c: *cy.Chunk, val: GenValue) !void {
    return releaseIfRetainedTempAt(c, val, c.curNodeId);
}

fn field(self: *cy.Chunk, nodeId: cy.NodeId, req: RegisterCstr) !GenValue {
    const node = self.nodes[nodeId];
    const leftId = node.head.accessExpr.left;
    const rightId = node.head.accessExpr.right;

    const dst = try self.rega.selectFromNonLocalVar(req, true);
    const tempStart = self.rega.getNextTemp();
    defer self.rega.setNextTemp(tempStart);

    // Left side.
    const leftv = try expression(self, leftId, RegisterCstr.prefer(dst));

    // Right should be an ident.
    const right = self.nodes[rightId];
    const name = self.getNodeTokenString(right);
    const fieldId = try self.compiler.vm.ensureFieldSym(name);

    const newtype = (try sema.getAccessExprResult(self, leftv.vtype, name)).exprT;

    try self.pushDebugSym(nodeId);
    const leftIsTempRetained = leftv.retained and leftv.isTempLocal;
    if (req.mustRetain or leftIsTempRetained) {
        const pc = self.buf.len();
        try self.buf.pushOpSlice(.fieldRetain, &.{ leftv.local, dst, 0, 0, 0, 0, 0 });
        self.buf.setOpArgU16(pc + 3, @intCast(fieldId));

        // ARC cleanup.
        try releaseIfRetainedTemp(self, leftv);

        return self.initGenValue(dst, newtype, true);
    } else {
        const pc = self.buf.len();
        try self.buf.pushOpSlice(.field, &.{ leftv.local, dst, 0, 0, 0, 0, 0 });
        self.buf.setOpArgU16(pc + 3, @intCast(fieldId));

        // ARC cleanup.
        try releaseIfRetainedTemp(self, leftv);

        return self.initGenValue(dst, newtype, false);
    }
}

/// Only generates the top declaration statements.
/// For imported modules only.
pub fn topDeclStatements(c: *Chunk, head: cy.NodeId) !void {
    var nodeId = head;
    while (nodeId != cy.NullId) {
        const node = c.nodes[nodeId];
        switch (node.node_t) {
            .objectDecl => {
                try statement(c, nodeId);
            },
            .funcDecl => {
                try statement(c, nodeId);
            },
            else => {},
        }
        nodeId = node.next;
    }
}

fn shouldGenMainScopeReleaseOps(self: *cy.VMcompiler) bool {
    return !self.vm.config.singleRun;
}

pub fn genStatements(self: *Chunk, head: cy.NodeId, comptime attachEnd: bool) anyerror!void {
    var cur_id = head;
    var node = self.nodes[cur_id];

    while (node.next != cy.NullId) {
        try statement(self, cur_id);
        cur_id = node.next;
        node = self.nodes[cur_id];
    }

    // Check for last expression statement.
    if (node.node_t == .expr_stmt) {
        if (attachEnd) {
            const local = try exprStmt(self, cur_id, true);
            if (shouldGenMainScopeReleaseOps(self.compiler)) {
                self.curBlock.endLocalsPc = @intCast(self.buf.ops.items.len);
                self.nodes[0].head.root.genEndLocalsPc = self.curBlock.endLocalsPc;
                try self.endLocals();
            }
            try self.buf.pushOp1(.end, local);
        } else {
            _ = try statement(self, cur_id);
        }
    } else {
        if (attachEnd) {
            try statement(self, cur_id);
            if (shouldGenMainScopeReleaseOps(self.compiler)) {
                self.curBlock.endLocalsPc = @intCast(self.buf.ops.items.len);
                self.nodes[0].head.root.genEndLocalsPc = self.curBlock.endLocalsPc;
                try self.endLocals();
            }
            try self.buf.pushOp1(.end, 255);
        } else {
            try statement(self, cur_id);
        }
    }
}

fn statement(c: *Chunk, nodeId: cy.NodeId) !void {
    const node = c.nodes[nodeId];
    // log.debug("gen stmt {}", .{node.node_t});
    c.curNodeId = nodeId;

    const tempStart = c.rega.getNextTemp();
    var hasError = false;
    defer {
        if (!hasError) {
            c.rega.setNextTemp(tempStart);
        }
    }
    errdefer {
        hasError = true;
    }

    switch (node.node_t) {
        .pass_stmt => {
            return;
        },
        .breakStmt => {
            const pc = try c.pushEmptyJump();
            try c.subBlockJumpStack.append(c.alloc, .{ .jumpT = .brk, .pc = pc });
        },
        .continueStmt => {
            const pc = try c.pushEmptyJump();
            try c.subBlockJumpStack.append(c.alloc, .{ .jumpT = .cont, .pc = pc });
        },
        .expr_stmt => {
            _ = try exprStmt(c, nodeId, false);
        },
        .localDecl => {
            if (node.head.localDecl.right != cy.NullId) {
                // Can assume left is .ident from sema.
                const varSpec = c.nodes[node.head.localDecl.varSpec];
                try assignExprToLocalVar(c, varSpec.head.varSpec.name, node.head.localDecl.right);
            }
        },
        .assign_stmt => {
            try assignStmt(c, nodeId);
        },
        .opAssignStmt => {
            try opAssignStmt(c, nodeId);
        },
        .importStmt => {
            const ident = c.nodes[node.head.left_right.left];
            const name = c.getNodeTokenString(ident);

            const spec = c.nodes[node.head.left_right.right];
            const specPath = c.getNodeTokenString(spec);

            _ = name;
            _ = specPath;

            // const modId = try self.getOrLoadModule(specPath);
            // const leadSym = try self.ensureSemaSym(name);
            // try self.semaSymToMod.put(self.alloc, leadSym, modId);
        },
        .funcDeclInit => {
            // Nop. Func declaration initializer are hoisted and initialized at the start of the program.
        },
        .funcDecl => {
            try funcDecl(c, c.semaResolvedRootSymId, nodeId);
        },
        .enumDecl => {
            // Nop.
        },
        .typeAliasDecl => {
            // Nop.
        },
        .objectDecl => {
            const nameN = c.nodes[node.head.objectDecl.name];
            const name = c.getNodeTokenString(nameN);
            const nameId = try sema.ensureNameSym(c.compiler, name);
            const crObjSymId = nameN.head.ident.sema_crSymId;
            const robjSymId = crObjSymId.id;
            const sid = try c.compiler.vm.ensureObjectType(c.semaResolvedRootSymId, nameId, robjSymId);

            var funcId = node.head.objectDecl.funcsHead;
            var func: cy.Node = undefined;
            while (funcId != cy.NullId) : (funcId = func.next) {
                func = c.nodes[funcId];
                const decl = c.semaFuncDecls.items[func.head.func.semaDeclId];

                const funcName = decl.getName(c);
                if (decl.numParams > 0) {
                    const param = c.nodes[decl.paramHead];
                    const paramName = c.getNodeTokenString(c.nodes[param.head.funcParam.name]);
                    if (std.mem.eql(u8, paramName, "self")) {
                        // Struct method.
                        try genMethodDecl(c, sid, func, decl, funcName);
                        continue;
                    }
                }

                // const detail = cy.FuncSymDetail{
                //     .name = try c.alloc.dupe(u8, funcName),
                // };
                // try c.compiler.vm.funcSymDetails.append(c.alloc, detail);
                try funcDecl(c, robjSymId, funcId);
            }
        },
        .if_stmt => {
            try ifStmt(c, nodeId);
        },
        .matchBlock => {
            _ = try matchBlock(c, nodeId, null);
        },
        .tryStmt => {
            // push try block.
            var errorVarLocal: u8 = cy.NullU8;
            if (node.head.tryStmt.errorVar != cy.NullId) {
                const errorVarN = c.nodes[node.head.tryStmt.errorVar];
                errorVarLocal = c.genGetVar(errorVarN.head.ident.semaVarId).?.local;
            }
            const pushTryPc = c.buf.ops.items.len;
            try c.buf.pushOp3(.pushTry, errorVarLocal, 0, 0);

            // try body.
            c.nextSemaSubBlock();
            try genStatements(c, node.head.tryStmt.tryFirstStmt, false);
            c.prevSemaSubBlock();

            // pop try block.
            const popTryPc = c.buf.ops.items.len;
            try c.buf.pushOp2(.popTry, 0, 0);
            c.buf.setOpArgU16(pushTryPc + 2, @intCast(c.buf.ops.items.len - pushTryPc));

            // catch body.
            c.nextSemaSubBlock();
            try genStatements(c, node.head.tryStmt.catchFirstStmt, false);
            c.prevSemaSubBlock();

            c.buf.setOpArgU16(popTryPc + 1, @intCast(c.buf.ops.items.len - popTryPc));
        },
        .staticDecl => {
            // Nop. Static variables are hoisted and initialized at the start of the program.
        },
        .whileOptStmt => {
            try whileOptStmt(c, nodeId);
        },
        .whileInfStmt => {
            c.nextSemaSubBlock();

            const pcSave: u32 = @intCast(c.buf.ops.items.len);
            const jumpStackSave: u32 = @intCast(c.subBlockJumpStack.items.len);
            defer c.subBlockJumpStack.items.len = jumpStackSave;

            // TODO: generate gas meter checks.
            // if (c.opts.gas_meter != .none) {
            //     try c.indent();
            //     if (c.opts.gas_meter == .error_interrupt) {
            //         _ = try c.writer.write("__interrupt_count += 1; if (__interrupt_count > __interrupt_max) throw globalThis._internal.interruptSym;\n");
            //     } else if (c.opts.gas_meter == .yield_interrupt) {
            //         _ = try c.writer.write("__interrupt_count += 1; if (__interrupt_count > __interrupt_max) yield globalThis._internal.interruptSym;\n");
            //     }
            // }

            try genStatements(c, node.head.child_head, false);
            try c.pushJumpBackTo(pcSave);

            c.patchForBlockJumps(jumpStackSave, c.buf.ops.items.len, pcSave);
            c.prevSemaSubBlock();
        },
        .whileCondStmt => {
            c.nextSemaSubBlock();

            const topPc: u32 = @intCast(c.buf.ops.items.len);
            const jumpStackSave: u32 = @intCast(c.subBlockJumpStack.items.len);
            defer c.subBlockJumpStack.items.len = jumpStackSave;

            const condv = try expression(c, node.head.whileCondStmt.cond, RegisterCstr.simple);
            const condLocal = condv.local;

            var jumpPc = try c.pushEmptyJumpNotCond(condLocal);

            // Enter while body.

            // ARC cleanup.
            try releaseIfRetainedTemp(c, condv);

            c.rega.setNextTemp(tempStart);

            try genStatements(c, node.head.whileCondStmt.bodyHead, false);
            try c.pushJumpBackTo(topPc);

            c.patchJumpNotCondToCurPc(jumpPc);

            c.patchForBlockJumps(jumpStackSave, c.buf.ops.items.len, topPc);
            c.prevSemaSubBlock();
        },
        .for_range_stmt => {
            try forRangeStmt(c, nodeId);
        },
        .for_iter_stmt => {
            try forIterStmt(c, nodeId);
        },
        .return_stmt => {
            if (c.blocks.items.len == 1) {
                try c.endLocals();
                try c.buf.pushOp1(.end, 255);
            } else {
                try c.endLocals();
                try c.buf.pushOp(.ret0);
            }
        },
        .return_expr_stmt => {
            if (c.blocks.items.len == 1) {
                // Main block.
                const val = try expression(c, node.head.child_head, RegisterCstr.simpleMustRetain);
                try c.endLocals();
                try c.buf.pushOp1(.end, @intCast(val.local));
            } else {
                // if (c.curBlock.rFuncSymId != cy.NullId) {
                //     const retType = c.compiler.sema.resolvedFuncSyms.items[c.curBlock.rFuncSymId].retType;
                //     _ = try genExprTo2(c, node.head.child_head, 0, retType, true, true);
                    // _ = try expression(c, node.head.child_head, RegisterCstr.exactMustRetain(0));
                // } else {
                    _ = try expression(c, node.head.child_head, RegisterCstr.exactMustRetain(0));
                // }
                try c.endLocals();
                try c.buf.pushOp(.ret1);
            }
        },
        .atStmt => {
            try atStmt(c, nodeId);
        },
        else => {
            return c.reportErrorAt("Unsupported statement: {}", &.{v(node.node_t)}, nodeId);
        }
    }
}

fn whileOptStmt(c: *cy.Chunk, nodeId: cy.NodeId) !void {
    const node = c.nodes[nodeId];
    c.nextSemaSubBlock();

    const topPc: u32 = @intCast(c.buf.ops.items.len);
    const jumpStackSave: u32 = @intCast(c.subBlockJumpStack.items.len);
    defer c.subBlockJumpStack.items.len = jumpStackSave;

    var optLocal: LocalId = undefined;
    if (node.head.whileOptStmt.some != cy.NullId) {
        const some = c.nodes[node.head.whileOptStmt.some];
        optLocal = c.genGetVar(some.head.ident.semaVarId).?.local;
        // Since this variable is used in the loop, it is considered defined before codegen.
        c.vars.items[some.head.ident.semaVarId].isDefinedOnce = true;
        try assignExprToLocalVar(c, node.head.whileOptStmt.some, node.head.whileOptStmt.opt);
    } else {
        const optv = try expression(c, node.head.whileOptStmt.opt, RegisterCstr.simple);
        optLocal = optv.local;
    }

    const skipSkipJump = try c.pushEmptyJumpNotNone(optLocal);
    const skipBodyJump = try c.pushEmptyJump();
    c.patchJumpNotNoneToCurPc(skipSkipJump);

    try genStatements(c, node.head.whileOptStmt.bodyHead, false);
    try c.pushJumpBackTo(topPc);

    c.patchJumpToCurPc(skipBodyJump);

    c.patchForBlockJumps(jumpStackSave, c.buf.ops.items.len, topPc);
    c.prevSemaSubBlock();
}

fn opAssignStmt(c: *cy.Chunk, nodeId: cy.NodeId) !void {
    const node = c.nodes[nodeId];
    const left = c.nodes[node.head.opAssignStmt.left];
    const genOp: cy.OpCode = switch (node.head.opAssignStmt.op) {
        .plus => .add,
        .minus => .sub,
        .star => .mul,
        .slash => .div,
        else => fmt.panic("Unexpected operator assignment.", &.{}),
    };
    if (left.node_t == .ident) {
        if (left.head.ident.semaVarId != cy.NullId) {
            const svar = c.genGetVarPtr(left.head.ident.semaVarId).?;
            const right = try expression(c, node.head.opAssignStmt.right, RegisterCstr.simple);
            if (svar.isBoxed) {
                const temp = try c.rega.consumeNextTemp();
                if (svar.isCaptured()) {
                    try c.buf.pushOp3(.captured, c.curBlock.closureLocal, svar.capturedIdx, temp);
                    try c.buf.pushOp2(.boxValue, temp, temp);
                } else {
                    try c.buf.pushOp2(.boxValue, svar.local, temp);
                }

                try c.pushDebugSym(nodeId);
                try c.buf.pushOp3(genOp, temp, right.local, temp);

                if (svar.isCaptured()) {
                    const temp2 = try c.rega.consumeNextTemp();
                    try c.buf.pushOp3(.captured, c.curBlock.closureLocal, svar.capturedIdx, temp2);
                    try c.buf.pushOp2(.setBoxValue, temp2, temp);
                } else {
                    try c.buf.pushOp2(.setBoxValue, svar.local, temp);
                }
                return;
            } else {
                try c.pushDebugSym(nodeId);
                try c.buf.pushOp3(genOp, svar.local, right.local, svar.local);
            }
        } else {
            const crSymId = left.head.ident.sema_crSymId;
            if (!crSymId.isFuncSymId) {
                const rsym = c.compiler.sema.getResolvedSym(crSymId.id);
                const rightv = try expression(c, node.head.opAssignStmt.right, RegisterCstr.simple);
                const rtSymId = try c.compiler.vm.ensureVarSym(rsym.key.absResolvedSymKey.rParentSymId, rsym.key.absResolvedSymKey.nameId);

                const temp = try c.rega.consumeNextTemp();
                try c.pushOptionalDebugSym(nodeId);       
                var pc = c.buf.len();
                try c.buf.pushOp3(.staticVar, 0, 0, temp);
                c.buf.setOpArgU16(pc + 1, @intCast(rtSymId));

                try c.pushDebugSym(nodeId);
                try c.buf.pushOp3(genOp, temp, rightv.local, temp);

                pc = c.buf.ops.items.len;
                try c.buf.pushOp3(.setStaticVar, 0, 0, temp);
                c.buf.setOpArgU16(pc + 1, @intCast(rtSymId));
            } else {
                stdx.fatal();
            }
        }
    } else if (left.node_t == .accessExpr) {
        try binOpAssignToField(c, genOp, node.head.opAssignStmt.left, node.head.opAssignStmt.right);
    } else if (left.node_t == .indexExpr) {
        try binOpAssignToIndex(c, genOp, node.head.opAssignStmt.left, node.head.opAssignStmt.right);
    } else {
        return c.reportErrorAt("Unsupported assignment to left: {}", &.{v(left.node_t)}, nodeId);
    }
}

fn assignStmt(c: *cy.Chunk, nodeId: cy.NodeId) !void {
    const node = c.nodes[nodeId];
    const left = c.nodes[node.head.left_right.left];
    if (left.node_t == .ident) {
        if (left.head.ident.semaVarId != cy.NullId) {
            try assignExprToLocalVar(c, node.head.left_right.left, node.head.left_right.right);
        } else {
            const crSymId = left.head.ident.sema_crSymId;
            if (!crSymId.isFuncSymId) {
                const rsym = c.compiler.sema.getResolvedSym(crSymId.id);
                const rightv = try expression(c, node.head.left_right.right, RegisterCstr.tempMustRetain);
                const rtSymId = try c.compiler.vm.ensureVarSym(rsym.key.absResolvedSymKey.rParentSymId, rsym.key.absResolvedSymKey.nameId);

                const pc = c.buf.len();
                try c.buf.pushOp3(.setStaticVar, 0, 0, rightv.local);
                c.buf.setOpArgU16(pc + 1, @intCast(rtSymId));
            } else {
                return c.reportError("Unsupported", &.{});
            }
        }
    } else if (left.node_t == .indexExpr) {
        const leftv = try expression(c, left.head.left_right.left, RegisterCstr.simple);
        const indexv = try expression(c, left.head.left_right.right, RegisterCstr.simple);
        const rightv = try expression(c, node.head.left_right.right, RegisterCstr.simpleMustRetain);

        try c.pushDebugSym(nodeId);
        try c.buf.pushOp3(.setIndexRelease, leftv.local, indexv.local, rightv.local);

        // ARC cleanup. Right is not released since it's being assigned to the index.
        try releaseIfRetainedTempAt(c, leftv, left.head.left_right.left);
        try releaseIfRetainedTempAt(c, indexv, left.head.left_right.right);
    } else if (left.node_t == .accessExpr) {
        const leftv = try expression(c, left.head.accessExpr.left, RegisterCstr.simple);

        const accessRight = c.nodes[left.head.accessExpr.right];
        if (accessRight.node_t != .ident) {
            return c.reportErrorAt("Expected ident. Got {}.", &.{v(accessRight.node_t)}, left.head.accessExpr.right);
        }

        const rightv = try expression(c, node.head.left_right.right, RegisterCstr.simpleMustRetain);

        const fieldName = c.getNodeTokenString(accessRight);
        const fieldId = try c.compiler.vm.ensureFieldSym(fieldName);

        const recvT = c.nodeTypes[left.head.accessExpr.left];
        if (rightv.vtype == bt.Dynamic or types.isAnyOrDynamic(recvT)) {
            // Runtime type check on dynamic right or any/dynamic receiver.
            try c.pushDebugSym(nodeId);
            try c.buf.pushOpSlice(.setCheckFieldRelease, &[_]u8{ leftv.local, rightv.local, @intCast(fieldId), 0, 0, 0 });
        } else {
            try c.pushDebugSym(nodeId);
            try c.buf.pushOpSlice(.setFieldRelease, &[_]u8{ leftv.local, rightv.local, @intCast(fieldId), 0, 0, 0 });
        }

        // ARC cleanup. Right is not released since it's being assigned to the field.
        try releaseIfRetainedTempAt(c, leftv, left.head.accessExpr.left);
    } else {
        return c.reportErrorAt("Unsupported assignment to left: {}", &.{v(left.node_t)}, nodeId);
    }
}

fn ifStmt(c: *cy.Chunk, nodeId: cy.NodeId) !void {
    const tempStart = c.rega.getNextTemp();

    const node = c.nodes[nodeId];
    const condv = try expression(c, node.head.left_right.left, RegisterCstr.simple);

    var lastCondJump = try c.pushEmptyJumpNotCond(condv.local);

    // Enter if body.

    // ARC cleanup.
    try releaseIfRetainedTemp(c, condv);

    c.rega.setNextTemp(tempStart);

    c.nextSemaSubBlock();
    try genStatements(c, node.head.left_right.right, false);
    c.prevSemaSubBlock();

    var elseClauseId = node.head.left_right.extra;
    if (elseClauseId != cy.NullId) {
        var jumpsStart = c.subBlockJumpStack.items.len;
        defer c.subBlockJumpStack.items.len = jumpsStart;

        var endsWithElse = false;
        while (elseClauseId != cy.NullId) {
            const pc = try c.pushEmptyJump();
            try c.subBlockJumpStack.append(c.alloc, .{ .jumpT = .subBlockBreak, .pc = pc });

            c.patchJumpNotCondToCurPc(lastCondJump);

            const elseClause = c.nodes[elseClauseId];
            if (elseClause.head.else_clause.cond == cy.NullId) {
                c.nextSemaSubBlock();
                try genStatements(c, elseClause.head.else_clause.body_head, false);
                c.prevSemaSubBlock();
                endsWithElse = true;
                break;
            } else {
                const tempStart2 = c.rega.getNextTemp();
                const elseCondv = try expression(c, elseClause.head.else_clause.cond, RegisterCstr.simple);
                lastCondJump = try c.pushEmptyJumpNotCond(elseCondv.local);

                // Enter else-if body.
    
                // ARC cleanup.
                try releaseIfRetainedTemp(c, elseCondv);

                c.rega.setNextTemp(tempStart2);

                c.nextSemaSubBlock();
                try genStatements(c, elseClause.head.else_clause.body_head, false);
                c.prevSemaSubBlock();
                elseClauseId = elseClause.head.else_clause.else_clause;
            }
        }

        if (!endsWithElse) {
            c.patchJumpNotCondToCurPc(lastCondJump);
        }
        jumpsStart = c.patchSubBlockBreakJumps(jumpsStart, c.buf.ops.items.len);
    } else {
        c.patchJumpNotCondToCurPc(lastCondJump);
    }
}

fn atStmt(c: *Chunk, nodeId: cy.NodeId) !void {
    const node = c.nodes[nodeId];
    const atExpr = c.nodes[node.head.atStmt.expr];
    const expr = c.nodes[atExpr.head.atExpr.child];
    if (expr.node_t == .callExpr) {
        const callee = c.nodes[expr.head.callExpr.callee];
        const name = c.getNodeTokenString(callee);

        if (std.mem.eql(u8, "genLabel", name)) {
            if (expr.head.callExpr.numArgs != 1) {
                return c.reportErrorAt("genLabel expected 1 arg", &.{}, nodeId);
            }

            const arg = c.nodes[expr.head.callExpr.arg_head];
            if (arg.node_t != .string) {
                return c.reportErrorAt("genLabel expected string arg", &.{}, nodeId);
            }

            const label = c.getNodeTokenString(arg);
            try c.buf.pushDebugLabel(c.buf.ops.items.len, label);
        } else if (std.mem.eql(u8, "dumpLocals", name)) {
            const sblock = sema.curBlock(c);
            try c.dumpLocals(sblock);
        } else if (std.mem.eql(u8, "dumpBytecode", name)) {
            try cy.debug.dumpBytecode(c.compiler.vm, null);
        } else {
            return c.reportErrorAt("Unsupported annotation: {}", &.{v(name)}, nodeId);
        }
    } else {
        return c.reportErrorAt("Unsupported atExpr: {}", &.{v(expr.node_t)}, nodeId);
    }
}

fn forIterStmt(c: *Chunk, nodeId: cy.NodeId) !void {
    c.nextSemaSubBlock();
    defer c.prevSemaSubBlock();

    const node = c.nodes[nodeId];

    const hasEach = node.head.for_iter_stmt.eachClause != cy.NullId;

    var valIdent: cy.Node = undefined;
    var valVar: sema.LocalVar = undefined;
    var keyVar: sema.LocalVar = undefined;
    var keyIdent: cy.Node = undefined;
    var pairIter = false;

    if (hasEach) {
        const eachClause = c.nodes[node.head.for_iter_stmt.eachClause];
        if (eachClause.head.eachClause.key != cy.NullId) {
            keyIdent = c.nodes[eachClause.head.eachClause.key];
            keyVar = c.genGetVar(keyIdent.head.ident.semaVarId).?;
            pairIter = true;
        }
        valIdent = c.nodes[eachClause.head.eachClause.value];
        valVar = c.genGetVar(valIdent.head.ident.semaVarId).?;

        // At this point the temp var is loosely defined.
        c.vars.items[valIdent.head.ident.semaVarId].isDefinedOnce = true;
        if (pairIter) {
            c.vars.items[keyIdent.head.ident.semaVarId].isDefinedOnce = true;
        }
    }

    // Reserve temp local for iterator.
    const iterLocal = try c.rega.consumeNextTemp();

    const rFuncSigId = try sema.ensureResolvedFuncSig(c.compiler, &.{ bt.Any }, bt.Any);

    _ = try expression(c, node.head.for_iter_stmt.iterable, RegisterCstr.exactMustRetain(iterLocal + 4));
    if (pairIter) {
        try pushCallObjSym(c, iterLocal, 1, 1,
            @intCast(c.compiler.vm.pairIteratorObjSym), @intCast(rFuncSigId),
            node.head.for_iter_stmt.iterable);
    } else {
        try pushCallObjSym(c, iterLocal, 1, 1,
            @intCast(c.compiler.vm.iteratorObjSym), @intCast(rFuncSigId),
            node.head.for_iter_stmt.iterable);
    }

    try c.buf.pushOp2(.copyRetainSrc, iterLocal, iterLocal + 5);
    if (pairIter) {
        try pushCallObjSym(c, iterLocal + 1, 1, 2,
            @intCast(c.compiler.vm.nextPairObjSym), @intCast(rFuncSigId),
            node.head.for_iter_stmt.iterable);
        if (hasEach) {
            try c.pushOptionalDebugSym(nodeId);
            try c.buf.pushOp2(.copyReleaseDst, iterLocal + 1, keyVar.local);
            try c.pushOptionalDebugSym(nodeId);
            try c.buf.pushOp2(.copyReleaseDst, iterLocal + 2, valVar.local);
        }
    } else {
        try pushCallObjSym(c, iterLocal + 1, 1, 1,
            @intCast(c.compiler.vm.nextObjSym), @intCast(rFuncSigId),
            node.head.for_iter_stmt.iterable);
        if (hasEach) {
            try c.pushOptionalDebugSym(nodeId);
            try c.buf.pushOp2(.copyReleaseDst, iterLocal + 1, valVar.local);
        }
    }

    const skipSkipJump = try c.pushEmptyJumpNotNone(if (pairIter) keyVar.local else valVar.local);
    const skipBodyJump = try c.pushEmptyJump();
    c.patchJumpNotNoneToCurPc(skipSkipJump);

    const bodyPc = c.buf.ops.items.len;
    const jumpStackSave: u32 = @intCast(c.subBlockJumpStack.items.len);
    defer c.subBlockJumpStack.items.len = jumpStackSave;
    try genStatements(c, node.head.for_iter_stmt.body_head, false);

    const contPc = c.buf.ops.items.len;

    try c.buf.pushOp2(.copyRetainSrc, iterLocal, iterLocal + 5);
    if (pairIter) {
        try pushCallObjSym(c, iterLocal + 1, 1, 2,
            @intCast(c.compiler.vm.nextPairObjSym), @intCast(rFuncSigId),
            node.head.for_iter_stmt.iterable);
        if (hasEach) {
            try c.pushOptionalDebugSym(nodeId);
            try c.buf.pushOp2(.copyReleaseDst, iterLocal + 1, keyVar.local);
            try c.pushOptionalDebugSym(nodeId);
            try c.buf.pushOp2(.copyReleaseDst, iterLocal + 2, valVar.local);
        }
    } else {
        try pushCallObjSym(c, iterLocal + 1, 1, 1,
            @intCast(c.compiler.vm.nextObjSym), @intCast(rFuncSigId),
            node.head.for_iter_stmt.iterable);
        if (hasEach) {
            try c.pushOptionalDebugSym(nodeId);
            try c.buf.pushOp2(.copyReleaseDst, iterLocal + 1, valVar.local);
        }
    }

    try c.pushJumpBackNotNone(bodyPc, if (pairIter) keyVar.local else valVar.local);

    c.patchForBlockJumps(jumpStackSave, c.buf.ops.items.len, contPc);

    c.patchJumpToCurPc(skipBodyJump);

    // TODO: Iter local should be a reserved hidden local (instead of temp) so it can be cleaned up by endLocals when aborting the current fiber.
    try c.pushOptionalDebugSym(nodeId);
    try c.buf.pushOp1(.release, iterLocal);
}

fn forRangeStmt(c: *Chunk, nodeId: cy.NodeId) !void {
    c.nextSemaSubBlock();
    defer c.prevSemaSubBlock();

    const node = c.nodes[nodeId];

    var local: u8 = cy.NullU8;
    if (node.head.for_range_stmt.eachClause != cy.NullId) {
        const eachClause = c.nodes[node.head.for_range_stmt.eachClause];
        const ident = c.nodes[eachClause.head.eachClause.value];
        local = c.genGetVar(ident.head.ident.semaVarId).?.local;

        // inc = as_clause.head.as_range_clause.inc;
        // if (as_clause.head.as_range_clause.step != NullId) {
        //     step = self.nodes[as_clause.head.as_range_clause.step];
        // }
    }

    // Set range start/end.
    const range_clause = c.nodes[node.head.for_range_stmt.range_clause];
    const rangeStartN = c.nodes[range_clause.head.left_right.left];
    const rangeEndN = c.nodes[range_clause.head.left_right.right];
    var lessThanCond = true;
    if (rangeStartN.node_t == .number and rangeEndN.node_t == .number) {
        const startLit = c.getNodeTokenString(rangeStartN);
        const endLit = c.getNodeTokenString(rangeEndN);
        const start = try std.fmt.parseFloat(f64, startLit);
        const end = try std.fmt.parseFloat(f64, endLit);
        if (start > end) {
            lessThanCond = false;
        }
    }

    // Reserve temp locals until end of block.

    // Keep counter hidden from user. (User can't change it's value.)
    const counter = try c.rega.consumeNextTemp();
    const rangeEnd = try c.rega.consumeNextTemp();
    const rangeStep = try c.rega.consumeNextTemp();

    const rangeStart = try expression(c, range_clause.head.left_right.left, RegisterCstr.simple);

    _ = try expression(c, range_clause.head.left_right.right, RegisterCstr.exact(rangeEnd));

    // Set custom step.
    _ = try constNumber(c, 1, rangeStep);

    const initPc = c.buf.ops.items.len;
    try c.buf.pushOpSlice(.forRangeInit, &.{ rangeStart.local, rangeEnd, rangeStep, counter, local, 0, 0 });

    const bodyPc = c.buf.ops.items.len;
    const jumpStackSave: u32 = @intCast(c.subBlockJumpStack.items.len);
    defer c.subBlockJumpStack.items.len = jumpStackSave;
    try genStatements(c, node.head.for_range_stmt.body_head, false);

    // Perform counter update and perform check against end range.
    const jumpBackOffset: u16 = @intCast(c.buf.ops.items.len - bodyPc);
    const forRangeOp = c.buf.ops.items.len;
    // The forRange op is patched by forRangeInit at runtime.
    c.buf.setOpArgU16(initPc + 6, @intCast(c.buf.ops.items.len - initPc));
    try c.buf.pushOpSlice(.forRange, &.{ counter, rangeStep, rangeEnd, local, 0, 0 });
    c.buf.setOpArgU16(forRangeOp + 5, jumpBackOffset);

    c.patchForBlockJumps(jumpStackSave, c.buf.ops.items.len, forRangeOp);
}

/// If `cstr` is null, this will generate as a statement and NoValue is returned.
fn matchBlock(self: *Chunk, nodeId: cy.NodeId, cstr: ?RegisterCstr) !GenValue {
    const isStmt = cstr == null;
    var dst: RegisterId = undefined;
    if (!isStmt) {
        dst = try self.rega.selectFromNonLocalVar(cstr.?, true);
    }
    const node = self.nodes[nodeId];
    const expr = try expression(self, node.head.matchBlock.expr, RegisterCstr.simple);

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
            const condv = try expression(self, curCond, RegisterCstr.simple);
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
    try self.buf.pushOp2(.match, expr.local, @intCast(numConds));
    try self.buf.pushOperands(self.operandStack.items[operandStart..]);
    // Reserve last jump for `else` case.
    try self.buf.pushOperand(0);
    try self.buf.pushOperand(0);
    self.operandStack.items.len = operandStart;

    var retained = false;

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
                const exprv = try expression(self, firstExpr, RegisterCstr.initExact(dst, cstr.?.mustRetain));
                retained = retained or exprv.retained;
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
                self.buf.setOpArgU16(condJumpStart + numConds * 3 - 1, @intCast(blockPc - matchPc));
            } else {
                self.buf.setOpArgU16(condJumpStart + condOffset * 3, @intCast(blockPc - matchPc));
            }
            condOffset += 1;
            curCond = cond.next;
        }

        curCase = case.next;
    }
    if (!hasElse) {
        // When missing an else case, patch else jump to end of the block.
        self.buf.setOpArgU16(condJumpStart + numConds * 3 - 1, @intCast(self.buf.ops.items.len - matchPc));
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
        return self.initGenValue(dst, bt.Any, retained);
    }
}

fn string(c: *Chunk, str: []const u8, dst: LocalId) !GenValue {
    const idx = try c.buf.getOrPushStringConst(str);
    try constOp(c, idx, dst);
    return c.initGenValue(dst, bt.StaticString, false);
}

fn constInt(self: *Chunk, val: f64, dst: LocalId) !GenValue {
    if (cy.Value.floatCanBeInteger(val)) {
        const i: i64 = @intFromFloat(val);
        if (i >= std.math.minInt(i8) and i <= std.math.maxInt(i8)) {
            try self.buf.pushOp2(.constI8Int, @bitCast(@as(i8, @intCast(i))), dst);
            return self.initGenValue(dst, bt.Integer, false);
        }
    } else {
        return self.reportError("TODO: coerce", &.{});
    }
    const int: i32 = @intFromFloat(val);
    const idx = try self.buf.pushConst(cy.Const.init(cy.Value.initI32(int).val));
    try constOp(self, idx, dst);
    return self.initGenValue(dst, bt.Integer, false);
}

fn constNumber(self: *Chunk, val: f64, dst: LocalId) !GenValue {
    if (cy.Value.floatCanBeInteger(val)) {
        const i: i64 = @intFromFloat(val);
        if (i >= std.math.minInt(i8) and i <= std.math.maxInt(i8)) {
            try self.buf.pushOp2(.constI8, @bitCast(@as(i8, @intCast(i))), dst);
            return self.initGenValue(dst, bt.Number, false);
        }
    }
    const idx = try self.buf.pushConst(cy.Const.init(@bitCast(val)));
    try constOp(self, idx, dst);
    return self.initGenValue(dst, bt.Number, false);
}

fn ifExpr(self: *Chunk, nodeId: cy.NodeId, req: RegisterCstr) !GenValue {
    const node = self.nodes[nodeId];

    const dst = try self.rega.selectFromNonLocalVar(req, true);
    const tempStart = self.rega.getNextTemp();

    const condv = try expression(self, node.head.if_expr.cond, RegisterCstr.simple);
    var jumpNotPc = try self.pushEmptyJumpNotCond(condv.local);

    // Enter if body.

    // ARC cleanup.
    try releaseIfRetainedTemp(self, condv);

    self.rega.setNextTemp(tempStart);

    const truev = try expression(self, node.head.if_expr.body_expr, RegisterCstr.initExact(dst, req.mustRetain));
    const jumpPc = try self.pushEmptyJump();
    self.patchJumpNotCondToCurPc(jumpNotPc);

    var falsev: GenValue = undefined;
    if (node.head.if_expr.else_clause != cy.NullId) {
        const else_clause = self.nodes[node.head.if_expr.else_clause];
        falsev = try expression(self, else_clause.head.child_head, RegisterCstr.initExact(dst, req.mustRetain));
    } else {
        falsev = try genNone(self, dst);
    }

    self.patchJumpToCurPc(jumpPc);

    if (truev.vtype != falsev.vtype) {
        return self.initGenValue(dst, bt.Any, truev.retained or falsev.retained);
    } else {
        // Even though types match, rcCandidate can differ.
        const effType = if (types.isRcCandidateType(self.compiler, truev.vtype)) truev.vtype else falsev.vtype;
        return self.initGenValue(dst, effType, truev.retained or falsev.retained);
    }
}

fn callExpr(c: *Chunk, nodeId: cy.NodeId, req: RegisterCstr, comptime startFiber: bool) !GenValue {
    const val = try callExpr2(c, nodeId, req, startFiber);
    if (!startFiber) {
        if (req.type == .exact) {
            if (req.reg != val.local) {
                try c.buf.pushOp2(.copy, val.local, req.reg);
            }
        }
    }
    return c.initGenValue(val.local, val.vtype, val.retained);
}

fn callExpr2(self: *Chunk, nodeId: cy.NodeId, req: RegisterCstr, comptime startFiber: bool) !GenValue {
    var fiberStartLocal: u8 = undefined;
    var fiberDst: u8 = undefined;
    if (startFiber) {
        fiberDst = try self.rega.selectFromNonLocalVar(req, true);
    }
    var callStartLocal: u8 = undefined;

    var tempStart = self.rega.getNextTemp();
    defer {
        if (startFiber) {
            self.rega.setNextTempAfterOr(fiberDst, tempStart);
        } else {
            self.rega.setNextTemp(tempStart + 1);
        }
    }

    if (startFiber) {
        fiberStartLocal = tempStart;
        callStartLocal = 1;
    } else {
        callStartLocal = tempStart;
        if (self.blocks.items.len == 1) {
            // Main block.
            // Ensure call start register is at least 1 so the runtime can easily check
            // if framePtr is at main or a function.
            if (callStartLocal == 0) {
                _ = try self.rega.consumeNextTemp();
                callStartLocal = 1;
                tempStart += 1;
            }
        }
        var reserveReturnLocal = true;
        if (callStartLocal > 1) {
            if (req.type == .exact and req.reg + 1 == callStartLocal) {
                // Optimization: Shifts start local to the left if the dst is only one register away.
                callStartLocal -= 1;
                tempStart -= 1;
                reserveReturnLocal = false;
            }
        }
        // Reserve registers for return value and return info.
        if (reserveReturnLocal) {
            _ = try self.rega.consumeNextTemp();
        }
        _ = try self.rega.consumeNextTemp();
        _ = try self.rega.consumeNextTemp();
        _ = try self.rega.consumeNextTemp();
    }

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
                    var callArgsRes: CallArgsResult = undefined;
                    if (funcSym.declId != cy.NullId) {
                        const rFuncSig = self.compiler.sema.resolvedFuncSigs.items[rFuncSigId];
                        callArgsRes = try callArgs2(self, rFuncSig, node.head.callExpr.arg_head);
                    } else {
                        callArgsRes = try callArgs(self, node.head.callExpr.arg_head);
                    }

                    if (callArgsRes.hasDynamicArgs) {
                        try genCallTypeCheck(self, callStartLocal, node.head.callExpr.numArgs, rFuncSigId, nodeId);
                    }

                    const key = rsym.key.absResolvedSymKey;
                    const symId = try self.compiler.vm.ensureFuncSym(key.rParentSymId, key.nameId, rFuncSigId);
                    try pushCallSym(self, callStartLocal, node.head.callExpr.numArgs, 1, symId, nodeId);
                    return GenValue.initTempValue(callStartLocal, funcSym.retType, true);
                } else {
                    return genFuncValueCallExpr(self, nodeId, fiberDst,
                        if (startFiber) fiberStartLocal else callStartLocal, startFiber);
                }
            } else {
                // Dynamic method call.
                // Assume left child is a valid reference from sema. Generate callObjSym.
                const right = self.nodes[callee.head.accessExpr.right];
                std.debug.assert(right.node_t == .ident);
                return callObjSym(self, callStartLocal, nodeId);
            }
        } else if (callee.node_t == .ident) {
            if (self.genGetVar(callee.head.ident.semaVarId)) |_| {
                return genFuncValueCallExpr(self, nodeId, fiberDst,
                    if (startFiber) fiberStartLocal else callStartLocal, startFiber);
            } else {
                const crSymId = callee.head.ident.sema_crSymId;
                stdx.debug.dassert(crSymId.isPresent());
                var rFuncSym: ?sema.ResolvedFuncSym = null;
                if (crSymId.isFuncSymId) {
                    const funcSym = self.compiler.sema.getResolvedFuncSym(crSymId.id);
                    const rFuncSigId = funcSym.getResolvedFuncSigId();
                    rFuncSym = funcSym;

                    var callArgsRes: CallArgsResult = undefined;
                    if (funcSym.declId != cy.NullId) {
                        const rFuncSig = self.compiler.sema.resolvedFuncSigs.items[rFuncSigId];
                        callArgsRes = try callArgs2(self, rFuncSig, node.head.callExpr.arg_head);
                    } else {
                        callArgsRes = try callArgs(self, node.head.callExpr.arg_head);
                    }
            
                    if (callArgsRes.hasDynamicArgs) {
                        try genCallTypeCheck(self, callStartLocal, node.head.callExpr.numArgs, rFuncSigId, nodeId);
                    }
                } else {
                    const rsym = self.compiler.sema.getResolvedSym(crSymId.id);
                    if (rsym.symT == .variable) {
                        return genFuncValueCallExpr(self, nodeId, fiberDst,
                            if (startFiber) fiberStartLocal else callStartLocal, startFiber);
                    } else {
                        return self.reportErrorAt("Unsupported callee", &.{}, nodeId);
                    }
                }

                const coinitPc = self.buf.ops.items.len;
                if (startFiber) {
                    // Precompute first arg local since coinit doesn't need the startLocal.
                    // numArgs + 4 (ret slots) + 1 (min call start local for main block)
                    var initialStackSize = node.head.callExpr.numArgs + 4 + 1;
                    if (initialStackSize < 16) {
                        initialStackSize = 16;
                    }
                    try self.pushOptionalDebugSym(nodeId);
                    try self.buf.pushOpSlice(.coinit, &[_]u8{ fiberStartLocal, node.head.callExpr.numArgs, 0, @intCast(initialStackSize), fiberDst });
                }

                if (crSymId.isFuncSymId) {
                    const rtSymId = try self.genEnsureRtFuncSym(crSymId.id);
                    try pushCallSym(self, callStartLocal, node.head.callExpr.numArgs, 1, rtSymId, nodeId);
                } else {
                    return self.reportError("Unsupported coinit func call.", &.{});
                }

                if (startFiber) {
                    try self.buf.pushOp(.coreturn);
                    self.buf.setOpArgs1(coinitPc + 3, @intCast(self.buf.ops.items.len - coinitPc));
                    return GenValue.initTempValue(fiberDst, bt.Fiber, true);
                } else {
                    if (crSymId.isPresent()) {
                        if (rFuncSym) |funcSym| {
                            return GenValue.initTempValue(callStartLocal, funcSym.retType, true);
                        } else {
                            return GenValue.initTempValue(callStartLocal, bt.Any, true);
                        }
                    } else {
                        return GenValue.initTempValue(callStartLocal, bt.Any, true);
                    }
                }
            }
        } else {
            // All other callees are treated as function value calls.
            return genFuncValueCallExpr(self, nodeId, fiberDst,
                if (startFiber) fiberStartLocal else callStartLocal, startFiber);
        }
    } else return self.reportError("Unsupported named args", &.{});
}

fn callObjSym(self: *Chunk, callStartLocal: u8, callExprId: cy.NodeId) !GenValue {
    const node = self.nodes[callExprId];
    const callee = self.nodes[node.head.callExpr.callee];
    const firstArgId = node.head.callExpr.arg_head;
    const ident = self.nodes[callee.head.accessExpr.right];

    // One more arg for receiver.
    _ = try callArgs(self, firstArgId);
    const numArgs = 1 + node.head.callExpr.numArgs;
        
    const name = self.getNodeTokenString(ident);

    const methodSymId = try self.compiler.vm.ensureMethodSym(name, numArgs - 1);

    _ = try expression(self, callee.head.accessExpr.left, RegisterCstr.tempMustRetain);

    const rFuncSigId: u16 = @intCast(ident.head.ident.semaMethodSigId);
    try pushCallObjSym(self, callStartLocal, @intCast(numArgs), 1,
        @intCast(methodSymId), rFuncSigId, callExprId);
    return GenValue.initTempValue(callStartLocal, bt.Any, true);
}

fn release(self: *Chunk, local: u8, nodeId: cy.NodeId) !void {
    try self.pushOptionalDebugSym(nodeId);
    try self.buf.pushOp1(.release, local);
}

fn genFuncValueCallExpr(self: *Chunk, nodeId: cy.NodeId, fiberDst: u8, startLocal: u8, comptime startFiber: bool) !GenValue {
    const node = self.nodes[nodeId];

    var fiberStartLocal: u8 = undefined;
    var callStartLocal: u8 = undefined;
    if (startFiber) {
        fiberStartLocal = startLocal;
        callStartLocal = 1;
    } else {
        callStartLocal = startLocal;
    }

    var numArgs: u32 = 0;
    var argId = node.head.callExpr.arg_head;
    while (argId != cy.NullId) : (numArgs += 1) {
        const arg = self.nodes[argId];
        _ = try expression(self, argId, RegisterCstr.tempMustRetain);
        argId = arg.next;
    }

    // TODO: Doesn't have to retain to a temp if it's a local (would just need retain op).
    // If copied to temp, it should be after the last param so it can persist until the function returns.
    const calleev = try expression(self, node.head.callExpr.callee, RegisterCstr.tempMustRetain);

    const coinitPc = self.buf.ops.items.len;
    if (startFiber) {
        // Precompute first arg local since coinit doesn't need the startLocal.
        // numArgs + 4 (ret slots) + 1 (min call start local for main block)
        var initialStackSize = numArgs + 4 + 1;
        if (initialStackSize < 16) {
            initialStackSize = 16;
        }
        try self.buf.pushOpSlice(.coinit, &[_]u8{ fiberStartLocal, @as(u8, @intCast(numArgs)) + 1, 0, @intCast(initialStackSize), fiberDst });
    }

    try self.pushDebugSym(nodeId);
    try self.buf.pushOp2(.call1, callStartLocal, @intCast(numArgs));

    // ARC cleanup.
    if (startFiber) {
        try release(self, @intCast(5 + numArgs), nodeId);
    } else {
        try release(self, calleev.local, nodeId);
    }

    if (startFiber) {
        try self.buf.pushOp(.coreturn);
        self.buf.setOpArgs1(coinitPc + 3, @intCast(self.buf.ops.items.len - coinitPc));
        return GenValue.initTempValue(fiberDst, bt.Fiber, true);
    } else {
        return GenValue.initTempValue(callStartLocal, bt.Any, true);
    }
}

fn funcDecl(self: *Chunk, rParentSymId: sema.ResolvedSymId, nodeId: cy.NodeId) !void {
    const node = self.nodes[nodeId];
    const func = &self.semaFuncDecls.items[node.head.func.semaDeclId];
    const name = func.getName(self);
    const nameId = try sema.ensureNameSym(self.compiler, name);
    const symId = try self.compiler.vm.ensureFuncSym(rParentSymId, nameId, func.rFuncSigId);

    const jumpPc = try self.pushEmptyJump();

    try self.pushSemaBlock(func.semaBlockId);
    self.curBlock.frameLoc = nodeId;

    const jumpStackStart = self.blockJumpStack.items.len;

    const opStart: u32 = @intCast(self.buf.ops.items.len);
    try self.reserveFuncParams(func.numParams);
    try initVarLocals(self);
    try genStatements(self, node.head.func.bodyHead, false);
    // TODO: Check last statement to skip adding ret.
    try self.genBlockEnding();
    func.genEndLocalsPc = self.curBlock.endLocalsPc;

    // Reserve another local for the call return info.
    const sblock = sema.curBlock(self);
    const numCaptured: u8 = @intCast(sblock.captures.items.len);
    std.debug.assert(numCaptured == 0);

    self.patchJumpToCurPc(jumpPc);

    self.patchBlockJumps(jumpStackStart);
    self.blockJumpStack.items.len = jumpStackStart;

    const stackSize = self.getMaxUsedRegisters();
    self.popSemaBlock();
    
    const rtSym = rt.FuncSymbolEntry.initFunc(opStart, @intCast(stackSize), func.numParams, func.rFuncSigId);
    self.compiler.vm.setFuncSym(symId, rtSym);
}

const CallArgsResult = struct {
    hasDynamicArgs: bool,
};

fn callArgs2(c: *Chunk, rFuncSig: sema.ResolvedFuncSig, first: cy.NodeId) !CallArgsResult {
    const params = rFuncSig.params();
    var i: u32 = 0;
    var argId = first;
    var hasDynamicArgs = false;
    while (argId != cy.NullId) : (i += 1) {
        const arg = c.nodes[argId];
        const cstrType = params[i];
        _ = cstrType;
        const argv = try expression(c, argId, RegisterCstr.tempMustRetain);
        hasDynamicArgs = hasDynamicArgs or argv.vtype == bt.Dynamic;
        argId = arg.next;
    }
    return CallArgsResult{
        .hasDynamicArgs = hasDynamicArgs,
    };
}

fn callArgs(c: *Chunk, first: cy.NodeId) !CallArgsResult {
    var argId = first;
    var hasDynamicArgs = false;
    while (argId != cy.NullId) {
        const arg = c.nodes[argId];
        const argv = try expression(c, argId, RegisterCstr.tempMustRetain);
        hasDynamicArgs = hasDynamicArgs or argv.vtype == bt.Dynamic;
        argId = arg.next;
    }
    return CallArgsResult{
        .hasDynamicArgs = hasDynamicArgs,
    };
}

fn genStaticInitializerDeps(c: *Chunk, crSymId: sema.CompactResolvedSymId) !void {
    if (c.semaInitializerSyms.get(crSymId)) |ref| {
        // Contains dependencies. Generate initializers for them first.
        const deps = c.bufU32.items[ref.depsStart..ref.depsEnd];
        for (deps) |dep| {
            const crDepSym: sema.CompactResolvedSymId = @bitCast(dep);
            // Only if it has a static initializer and hasn't been visited.
            if (sema.symHasStaticInitializer(c, crDepSym)) {
                try genStaticInitializerDFS(c, crDepSym);
            }
        }
    }
}

/// Generates var decl initializer.
/// If the declaration contains dependencies those are generated first in DFS order.
pub fn genStaticInitializerDFS(self: *Chunk, crSymId: sema.CompactResolvedSymId) anyerror!void {
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
        chunk.rega.resetNextTemp();
        const exprv = try expression(chunk, node.head.func.bodyHead, RegisterCstr.tempMustRetain);

        const rSym = self.compiler.sema.getResolvedSym(rFuncSym.getResolvedSymId());
        const rtSymId = try self.compiler.vm.ensureFuncSym(rSym.key.absResolvedSymKey.rParentSymId, nameId, rFuncSigId);
        try chunk.pushDebugSym(node.head.func.bodyHead);
        const pc = self.buf.len();
        try self.buf.pushOp3(.setStaticFunc, 0, 0, exprv.local);
        self.buf.setOpArgU16(pc + 1, @intCast(rtSymId));
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
            chunk.rega.resetNextTemp();
            const exprv = try expression(chunk, decl.head.staticDecl.right, RegisterCstr.tempMustRetain);

            const rtSymId = try self.compiler.vm.ensureVarSym(rSym.key.absResolvedSymKey.rParentSymId, rSym.key.absResolvedSymKey.nameId);

            const pc = self.buf.len();
            try self.buf.pushOp3(.setStaticVar, 0, 0, exprv.local);
            self.buf.setOpArgU16(pc + 1, @intCast(rtSymId));
        } else {
            stdx.panicFmt("Unsupported sym {}", .{rSym.symT});
        }
    }
}

fn genNone(self: *Chunk, dst: LocalId) !GenValue {
    try self.buf.pushOp1(.none, dst);
    return self.initGenValue(dst, bt.None, false);
}

/// When `cstr` contains an exact dst, this ensures that the resulting value is copied to the dst.
/// This does not care about what is already at the dst, so assign statements need
/// to consider that (for ARC correctness).
fn expression(c: *Chunk, nodeId: cy.NodeId, cstr: RegisterCstr) anyerror!GenValue {
    const node = c.nodes[nodeId];
    // log.debug("gen expr: {}", .{node.node_t});
    c.curNodeId = nodeId;
    switch (node.node_t) {
        .ident => {
            return identifier(c, nodeId, cstr);
        },
        .true_literal => {
            const dst = try c.rega.selectFromNonLocalVar(cstr, false);
            try c.buf.pushOp1(.true, dst);
            return c.initGenValue(dst, bt.Boolean, false);
        },
        .false_literal => {
            const dst = try c.rega.selectFromNonLocalVar(cstr, false);
            try c.buf.pushOp1(.false, dst);
            return c.initGenValue(dst, bt.Boolean, false);
        },
        .number => {
            const literal = c.getNodeTokenString(node);
            const val = try std.fmt.parseFloat(f64, literal);

            const dst = try c.rega.selectFromNonLocalVar(cstr, false);
            if (c.nodeTypes[nodeId] == bt.Integer) {
                return constInt(c, val, dst);
            } else {
                return constNumber(c, val, dst);
            }
        },
        .nonDecInt => {
            const fval = node.head.nonDecInt.semaNumberVal;
            const dst = try c.rega.selectFromNonLocalVar(cstr, false);
            if (c.nodeTypes[nodeId] == bt.Integer) {
                return try constInt(c, fval, dst);
            } else {
                return try constNumber(c, fval, dst);
            }
        },
        .symbolLit => {
            const name = c.getNodeTokenString(node);
            const symId = try c.compiler.vm.ensureSymbol(name);
            const dst = try c.rega.selectFromNonLocalVar(cstr, false);
            try c.buf.pushOp2(.tagLiteral, @intCast(symId), dst);
            return c.initGenValue(dst, bt.Symbol, false);
        },
        .errorSymLit => {
            const symN = c.nodes[node.head.errorSymLit.symbol];
            const name = c.getNodeTokenString(symN);
            const symId = try c.compiler.vm.ensureSymbol(name);
            const val = cy.Value.initErrorSymbol(@intCast(symId));
            const idx = try c.buf.pushConst(cy.Const.init(val.val));
            const dst = try c.rega.selectFromNonLocalVar(cstr, false);
            try constOp(c, idx, dst);
            return c.initGenValue(dst, bt.Error, false);
        },
        .string => {
            const literal = c.getNodeTokenString(node);
            const str = try c.unescapeString(literal);
            const dst = try c.rega.selectFromNonLocalVar(cstr, false);
            return string(c, str, dst);
        },
        .stringTemplate => {
            return stringTemplate(c, nodeId, cstr);
        },
        .none => {
            const dst = try c.rega.selectFromNonLocalVar(cstr, false);
            return genNone(c, dst);
        },
        .group => {
            return expression(c, node.head.child_head, cstr);
        },
        .map_literal => {
            return mapInit(c, nodeId, cstr);
        },
        .arr_literal => {
            return listInit(c, nodeId, cstr);
        },
        .if_expr => {
            return ifExpr(c, nodeId, cstr);
        },
        .unary_expr => {
            const op = node.head.unary.op;
            switch (op) {
                .minus => {
                    const dst = try c.rega.selectFromNonLocalVar(cstr, false);
                    const child = try expression(c, node.head.unary.child, RegisterCstr.exact(dst));
                    try c.buf.pushOp1(.neg, dst);
                    try releaseIfRetainedTemp(c, child);
                    return c.initGenValue(dst, bt.Number, false);
                },
                .not => {
                    const dst = try c.rega.selectFromNonLocalVar(cstr, false);
                    const child = try expression(c, node.head.unary.child, RegisterCstr.exact(dst));
                    try c.buf.pushOp1(.not, dst);
                    try releaseIfRetainedTemp(c, child);
                    return c.initGenValue(dst, bt.Boolean, false);
                },
                .bitwiseNot => {
                    const dst = try c.rega.selectFromNonLocalVar(cstr, false);
                    const child = try expression(c, node.head.unary.child, RegisterCstr.exact(dst));
                    try c.buf.pushOp1(.bitwiseNot, dst);
                    try releaseIfRetainedTemp(c, child);
                    return c.initGenValue(dst, bt.Number, false);
                },
            }
        },
        .binExpr => {
            return binExpr(c, nodeId, cstr);
        },
        .lambda_multi => {
            return lambdaMulti(c, nodeId, cstr);
        },
        .lambda_expr => {
            return lambdaExpr(c, nodeId, cstr);
        },
        .callExpr => {
            return callExpr(c, nodeId, cstr, false);
        },
        .matchBlock => {
            return matchBlock(c, nodeId, cstr);
        },
        .coresume => {
            const dst = try c.rega.selectFromNonLocalVar(cstr, true);
            const tempStart = c.rega.getNextTemp();
            defer c.rega.setNextTemp(tempStart);

            const fiber = try expression(c, node.head.child_head, RegisterCstr.tempMustRetain);
            try c.buf.pushOp2(.coresume, fiber.local, dst);
            return c.initGenValue(dst, bt.Any, true);
        },
        .coyield => {
            const pc = c.buf.ops.items.len;
            try c.buf.pushOp2(.coyield, 0, 0);
            try c.blockJumpStack.append(c.alloc, .{ .jumpT = .jumpToEndLocals, .pc = @intCast(pc), .pcOffset = 1 });

            // TODO: return coyield expression.
            const dst = try c.rega.selectFromNonLocalVar(cstr, false);
            return genNone(c, dst);
        },
        .coinit => {
            return callExpr(c, node.head.child_head, cstr, true);
        },
        .sliceExpr => {
            return sliceExpr(c, nodeId, cstr);
        },
        .indexExpr => {
            return indexExpr(c, nodeId, cstr);
        },
        .accessExpr => {
            return accessExpr(c, nodeId, cstr);
        },
        .objectInit => {
            return objectInit(c, nodeId, cstr);
        },
        .castExpr => {
            const child = try expression(c, node.head.castExpr.expr, cstr);
            const tSymId = node.head.castExpr.semaTypeSymId;

            const tSym = c.compiler.sema.getResolvedSym(tSymId);
            if (tSym.symT == .object) {
                const typeId = tSym.getObjectTypeId(c.compiler.vm).?;
                try c.pushDebugSym(nodeId);
                const pc = c.buf.ops.items.len;
                try c.buf.pushOpSlice(.cast, &.{ child.local, 0, 0 });
                c.buf.setOpArgU16(pc + 2, @intCast(typeId));
            } else if (tSym.symT == .builtinType) {
                if (types.toRtConcreteType(tSymId)) |typeId| {
                    try c.pushDebugSym(nodeId);
                    const pc = c.buf.ops.items.len;
                    try c.buf.pushOpSlice(.cast, &.{ child.local, 0, 0 });
                    c.buf.setOpArgU16(pc + 2, @intCast(typeId));
                } else {
                    // Cast to abstract type.
                    try c.pushDebugSym(nodeId);
                    const pc = c.buf.ops.items.len;
                    try c.buf.pushOpSlice(.castAbstract, &.{ child.local, 0, 0 });
                    c.buf.setOpArgU16(pc + 2, @intCast(tSymId));
                }
            }

            // ARC cleanup.
            if (!cstr.mustRetain) {
                try releaseIfRetainedTemp(c, child);
            }

            return c.initGenValue(child.local, tSymId, child.retained);
        },
        .throwExpr => {
            const child = try expression(c, node.head.child_head, cstr);

            try c.pushDebugSym(nodeId);
            try c.buf.pushOp1(.throw, child.local);

            // ARC cleanup.
            if (!cstr.mustRetain) {
                try releaseIfRetainedTemp(c, child);
            }

            return child;
        },
        .tryExpr => {
            const dst = try c.rega.selectFromNonLocalVar(cstr, true);

            const pushTryPc = c.buf.ops.items.len;
            try c.buf.pushOp3(.pushTry, 0, 0, 0);

            const child = try expression(c, node.head.tryExpr.expr, RegisterCstr.initExact(dst, cstr.mustRetain));

            const popTryPc = c.buf.ops.items.len;
            try c.buf.pushOp2(.popTry, 0, 0);
            c.buf.setOpArgU16(pushTryPc + 2, @intCast(c.buf.ops.items.len - pushTryPc));

            // Generate else clause.
            var elsev: GenValue = undefined;
            if (node.head.tryExpr.elseExpr != cy.NullId) {
                // Error is not copied anywhere.
                c.buf.setOpArgs1(pushTryPc + 1, cy.NullU8);
                elsev = try expression(c, node.head.tryExpr.elseExpr, RegisterCstr.initExact(dst, cstr.mustRetain));
            } else {
                // Error goes directly to dst.
                c.buf.setOpArgs1(pushTryPc + 1, dst);
                elsev = c.initGenValue(dst, bt.Error, false);
            }

            c.buf.setOpArgU16(popTryPc + 1, @intCast(c.buf.ops.items.len - popTryPc));

            return c.initGenValue(dst, bt.Any, child.retained or elsev.retained);
        },
        else => {
            return c.reportErrorAt("Unsupported expression: {}", &.{v(node.node_t)}, nodeId);
        }
    }
}

fn exprStmt(c: *Chunk, stmtId: cy.NodeId, retain: bool) !LocalId {
    const stmt = c.nodes[stmtId];

    const val = try expression(c, stmt.head.child_head, RegisterCstr.initSimple(retain));

    // ARC cleanup.
    if (!retain) {
        try releaseIfRetainedTemp(c, val);
    }

    return val.local;
}

fn binOpAssignToIndex(c: *Chunk, code: cy.OpCode, leftId: cy.NodeId, rightId: cy.NodeId) !void {
    const left = c.nodes[leftId];

    const leftv = try expression(c, left.head.left_right.left, RegisterCstr.simple);
    const indexv = try expression(c, left.head.left_right.right, RegisterCstr.simple);
    const temp = try c.rega.consumeNextTemp();

    try c.pushDebugSym(leftId);
    try c.buf.pushOp3(.index, leftv.local, indexv.local, temp);

    const rightv = try expression(c, rightId, RegisterCstr.simpleMustRetain);
    try c.buf.pushOp3(code, temp, rightv.local, temp);

    try c.pushDebugSym(leftId);
    try c.buf.pushOp3(.setIndexRelease, leftv.local, indexv.local, temp);

    // ARC cleanup. Right is not released since it's being assigned to the index.
    try releaseIfRetainedTempAt(c, leftv, left.head.left_right.left);
    try releaseIfRetainedTempAt(c, indexv, left.head.left_right.right);
}

fn binOpAssignToField(self: *Chunk, code: cy.OpCode, leftId: cy.NodeId, rightId: cy.NodeId) !void {
    const left = self.nodes[leftId];

    const accessRight = self.nodes[left.head.accessExpr.right];
    if (accessRight.node_t != .ident) {
        return self.reportErrorAt("Expected ident. Got {}.", &.{v(accessRight.node_t)}, left.head.accessExpr.right);
    }
    const fieldName = self.getNodeTokenString(accessRight);
    const fieldId = try self.compiler.vm.ensureFieldSym(fieldName);

    const accessLeftv = try expression(self, left.head.accessExpr.left, RegisterCstr.simple);
    const accessLocal = try self.rega.consumeNextTemp();
    try self.pushDebugSym(leftId);

    const pc = self.buf.len();
    try self.buf.pushOpSlice(.field, &.{ accessLeftv.local, accessLocal, 0, 0, 0, 0, 0 });
    self.buf.setOpArgU16(pc + 3, @intCast(fieldId));

    const rightv = try expression(self, rightId, RegisterCstr.simple);
    try self.buf.pushOp3(code, accessLocal, rightv.local, accessLocal);

    try self.pushDebugSym(leftId);
    try self.buf.pushOp3(.setField, @intCast(fieldId), accessLeftv.local, accessLocal);

    // ARC cleanup. Right is not released since it's being assigned to the index.
    try releaseIfRetainedTempAt(self, accessLeftv, left.head.accessExpr.left);
}

fn genMethodDecl(self: *Chunk, typeId: rt.TypeId, node: cy.Node, func: sema.FuncDecl, name: []const u8) !void {
    // log.debug("gen method {s}", .{name});
    const methodId = try self.compiler.vm.ensureMethodSym(name, func.numParams - 1);

    const jumpPc = try self.pushEmptyJump();

    try self.pushSemaBlock(func.semaBlockId);

    const opStart: u32 = @intCast(self.buf.ops.items.len);
    try self.reserveFuncParams(func.numParams);
    try initVarLocals(self);
    try genStatements(self, node.head.func.bodyHead, false);
    // TODO: Check last statement to skip adding ret.
    try self.genBlockEnding();

    const stackSize = self.getMaxUsedRegisters();
    self.popSemaBlock();

    self.patchJumpToCurPc(jumpPc);

    const funcSig = self.compiler.sema.getResolvedFuncSig(func.rFuncSigId);
    if (funcSig.isTyped) {
        const sym = rt.MethodSym.initSingleTypedFunc(func.rFuncSigId, opStart, stackSize);
        try self.compiler.vm.addMethodSym(typeId, methodId, sym);
    } else {
        const sym = rt.MethodSym.initSingleUntypedFunc(func.rFuncSigId, opStart, stackSize);
        try self.compiler.vm.addMethodSym(typeId, methodId, sym);
    }
}

fn assignExprToLocalVar(c: *Chunk, leftId: cy.NodeId, exprId: cy.NodeId) !void {
    const varId = c.nodes[leftId].head.ident.semaVarId;
    const expr = c.nodes[exprId];

    const svar = c.genGetVarPtr(varId).?;
    if (svar.isBoxed) {
        stdx.debug.dassert(svar.isDefinedOnce);
        try assignExprToBoxedVar(c, svar, exprId);
        return;
    }

    if (expr.node_t == .ident) {
        const crSymId = expr.head.ident.sema_crSymId;
        if (crSymId.isPresent()) {
            // Copying a symbol.
            if (!svar.isDefinedOnce or !types.isRcCandidateType(c.compiler, svar.vtype)) {
                const exprv = try expression(c, exprId, RegisterCstr.exactMustRetain(svar.local));
                svar.vtype = exprv.vtype;
                svar.isDefinedOnce = true;
            } else {
                const exprv = try expression(c, exprId, RegisterCstr.tempMustRetain);
                svar.vtype = exprv.vtype;
                try c.pushOptionalDebugSym(leftId);
                try c.buf.pushOp2(.copyReleaseDst, exprv.local, svar.local);
            }
            return;
        }

        const exprv = try expression(c, exprId, RegisterCstr.simple);
        if (svar.isDefinedOnce) {
            if (types.isRcCandidateType(c.compiler, svar.vtype)) {
                // log.debug("releaseSet {} {}", .{varId, svar.vtype.typeT});
                if (types.isRcCandidateType(c.compiler, exprv.vtype)) {
                    try c.buf.pushOp2(.copyRetainRelease, exprv.local, svar.local);
                } else {
                    try c.pushOptionalDebugSym(leftId);
                    try c.buf.pushOp2(.copyReleaseDst, exprv.local, svar.local);
                }
            } else {
                // log.debug("set {} {}", .{varId, svar.vtype.typeT});
                if (types.isRcCandidateType(c.compiler, exprv.vtype)) {
                    try c.buf.pushOp2(.copyRetainSrc, exprv.local, svar.local);
                } else {
                    try c.buf.pushOp2(.copy, exprv.local, svar.local);
                }
            }
            svar.vtype = exprv.vtype;
        } else {
            if (types.isRcCandidateType(c.compiler, exprv.vtype)) {
                try c.buf.pushOp2(.copyRetainSrc, exprv.local, svar.local);
            } else {
                try c.buf.pushOp2(.copy, exprv.local, svar.local);
            }
            svar.isDefinedOnce = true;
            svar.vtype = exprv.vtype;
        }
        return;
    } else if (expr.node_t == .symbolLit) {
        if (types.isEnumType(c.compiler, svar.vtype)) {
            const name = c.getNodeTokenString(expr);
            const nameId = try sema.ensureNameSym(c.compiler, name);
            const sym = c.compiler.sema.getResolvedSym(svar.vtype);
            const enumSym = c.compiler.vm.enums.buf[sym.inner.enumType.enumId];

            for (enumSym.members, 0..) |mNameId, i| {
                if (nameId == mNameId) {
                    const val = cy.Value.initEnum(@intCast(sym.inner.enumType.enumId), @intCast(i));
                    const idx = try c.buf.pushConst(cy.Const.init(val.val));
                    try constOp(c, idx, svar.local);
                    return;
                }
            }

            // const symId = try c.compiler.vm.ensureSymbol(name);
            // const sym = c.compiler.vm.syms.buf[symId];
            // if (sym.symT == .one and sym.inner.one.id == svar.vtype.inner.enumT.tagId) {
            //     try c.buf.pushOp3(.tag, svar.vtype.inner.enumT.tagId, @intCast(u8, sym.inner.one.val), svar.local);
            //     return;
            // }
        }
    }

    // Retain rval.
    if (!svar.isDefinedOnce or !types.isRcCandidateType(c.compiler, svar.vtype)) {
        const exprv = try expression(c, exprId, RegisterCstr.exactMustRetain(svar.local));
        svar.vtype = exprv.vtype;
        svar.isDefinedOnce = true;
    } else {
        const exprv = try expression(c, exprId, RegisterCstr.simpleMustRetain);
        stdx.debug.dassert(exprv.local != svar.local);
        try c.pushOptionalDebugSym(leftId);
        try c.buf.pushOp2(.copyReleaseDst, exprv.local, svar.local);
        svar.vtype = exprv.vtype;
    }
}

fn assignExprToBoxedVar(self: *Chunk, svar: *sema.LocalVar, exprId: cy.NodeId) !void {
    // Retain rval.
    const exprv = try expression(self, exprId, RegisterCstr.tempMustRetain);
    svar.vtype = exprv.vtype;
    svar.isDefinedOnce = true;
    if (!types.isRcCandidateType(self.compiler, svar.vtype)) {
        if (svar.isCaptured()) {
            const temp = try self.rega.consumeNextTemp();
            defer self.rega.setNextTemp(temp);
            try self.buf.pushOp3(.captured, self.curBlock.closureLocal, svar.capturedIdx, temp);
            try self.buf.pushOp2(.setBoxValue, temp, exprv.local);
        } else {
            try self.buf.pushOp2(.setBoxValue, svar.local, exprv.local);
        }
    } else {
        if (svar.isCaptured()) {
            const temp = try self.rega.consumeNextTemp();
            defer self.rega.setNextTemp(temp);
            try self.buf.pushOp3(.captured, self.curBlock.closureLocal, svar.capturedIdx, temp);
            try self.buf.pushOp2(.setBoxValueRelease, temp, exprv.local);
        } else {
            try self.buf.pushOp2(.setBoxValueRelease, svar.local, exprv.local);
        }
    }
}

/// Reserve and initialize all var locals to `none`. 
/// There are a few reasons why this is needed:
/// 1. Since explicit initializers can fail (errors are thrown by default)
///    the shared endLocals inst can still rely on the var locals having defined values.
/// 2. By always generating this fixed sized `init` inst, it allows single-pass
///    compilation and doesn't require copying a temp inst buffer after a dynamically sized prelude.
/// 3. It allows variables first assigned in a loop construct to rely on a defined value for
///    a release op before use. This also allows the lifetime of all var locals to the block end.
pub fn initVarLocals(self: *Chunk) !void {
    const sblock = sema.curBlock(self);

    // Use the callee local for closure.
    if (sblock.captures.items.len > 0) {
        self.curBlock.closureLocal = @intCast(4 + sblock.params.items.len);

        // Captured vars are already defined.
        for (sblock.captures.items) |varId| {
            self.vars.items[varId].isDefinedOnce = true;
        }
    }

    // Reserve the locals.
    for (sblock.locals.items) |varId| {
        _ = try self.reserveLocalVar(varId);

        // Reset boxed.
        const svar = &self.vars.items[varId];
        if (!svar.isCaptured()) {
            svar.isBoxed = false;
        }
        // log.debug("reserve {} {s}", .{local, self.getVarName(varId)});
    }

    // Main block var locals start at 0 otherwise after the call return info and params.
    const startLocal: u8 = if (self.semaBlockDepth() == 1) 0 else @intCast(4 + sblock.params.items.len + 1);
    try self.buf.pushOp2(.init, startLocal, @intCast(sblock.locals.items.len));
}

fn unexpectedFmt(format: []const u8, vals: []const fmt.FmtValue) noreturn {
    if (builtin.mode == .Debug) {
        fmt.printStderr(format, vals);
    }
    stdx.fatal();
}

fn pushCallObjSym(chunk: *cy.Chunk, startLocal: u8, numArgs: u8, numRet: u8, symId: u8, rFuncSigId: u16, nodeId: cy.NodeId) !void {
    try chunk.pushDebugSym(nodeId);
    const start = chunk.buf.ops.items.len;
    try chunk.buf.pushOpSlice(.callObjSym, &.{ startLocal, numArgs, numRet, symId, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 });
    chunk.buf.setOpArgU16(start + 5, rFuncSigId);
}

fn pushCallSym(c: *cy.Chunk, startLocal: u8, numArgs: u32, numRet: u8, symId: u32, nodeId: cy.NodeId) !void {
    try c.pushDebugSym(nodeId);
    const start = c.buf.ops.items.len;
    try c.buf.pushOpSlice(.callSym, &[_]u8{ startLocal, @intCast(numArgs), numRet, 0, 0, 0, 0, 0, 0, 0, 0 });
    c.buf.setOpArgU16(start + 4, @intCast(symId));
}

fn genCallTypeCheck(c: *cy.Chunk, startLocal: u8, numArgs: u32, funcSigId: sema.ResolvedFuncSigId, nodeId: cy.NodeId) !void {
    try c.pushDebugSym(nodeId);
    const start = c.buf.ops.items.len;
    try c.buf.pushOpSlice(.callTypeCheck, &[_]u8{ startLocal, @intCast(numArgs), 0, 0, });
    c.buf.setOpArgU16(start + 3, @intCast(funcSigId));
}