const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const cy = @import("cyber.zig");
const vmc = cy.vmc;
const RegisterCstr = cy.register.RegisterCstr;
const RegisterId = cy.register.RegisterId;
const rt = cy.rt;
const sema = cy.sema;
const types = cy.types;
const bt = types.BuiltinTypeSymIds;
const Chunk = cy.Chunk;
const fmt = @import("fmt.zig");
const v = fmt.v;
const log = cy.log.scoped(.codegen);

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
            .vtype = bt.Undefined,
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
    const semaType = c.nodeTypes[nodeId];
    if (c.genGetVar(node.head.ident.semaVarId)) |svar| {
        // csymId would be active instead.
        cy.dassert(svar.type != .staticAlias);

        var dst: RegisterId = undefined;
        if (svar.type == .objectMemberAlias) {
            dst = try c.rega.selectFromNonLocalVar(cstr, cstr.mustRetain);
            const sblock = sema.curBlock(c);
            const selfLocal: u8 = @intCast(4 + sblock.params.items.len - 1);
            const name = c.getNodeTokenString(node);
            const fieldId = try c.compiler.vm.ensureFieldSym(name);
            try pushFieldRetain(c, selfLocal, dst, @intCast(fieldId), nodeId);
            return c.initGenValue(dst, bt.Any, true);
        } else if (svar.type == .parentObjectMemberAlias) {
            dst = try c.rega.selectFromNonLocalVar(cstr, cstr.mustRetain);
            try c.buf.pushOp3(.captured, c.curBlock.closureLocal, svar.capturedIdx, dst);
            try c.buf.pushOp2(.boxValue, dst, dst);
            const name = c.getNodeTokenString(node);
            const fieldId = try c.compiler.vm.ensureFieldSym(name);
            try pushFieldRetain(c, dst, dst, @intCast(fieldId), nodeId);
            return c.initGenValue(dst, bt.Any, true);
        } else if (!svar.isParentLocalAlias()) {
            dst = try c.rega.selectFromLocalVar(cstr, svar.local);
        } else {
            dst = try c.rega.selectFromNonLocalVar(cstr, cstr.mustRetain);
        }
        if (cstr.mustRetain and types.isRcCandidateType(c.compiler, semaType)) {
            // Ensure retain +1.
            if (svar.isBoxed) {
                if (svar.isParentLocalAlias()) {
                    try c.buf.pushOp3(.captured, c.curBlock.closureLocal, svar.capturedIdx, dst);
                    try c.buf.pushOp2(.boxValueRetain, dst, dst);
                } else {
                    try c.buf.pushOp2(.boxValueRetain, svar.local, dst);
                }
            } else {
                if (dst == svar.local) {
                    try c.buf.pushOp1(.retain, svar.local);
                } else {
                    try c.buf.pushOp2Ext(.copyRetainSrc, svar.local, dst, c.desc(nodeId));
                }
            }
            return c.initGenValue(dst, semaType, true);
        } else {
            if (svar.isBoxed) {
                if (svar.isParentLocalAlias()) {
                    try c.buf.pushOp3(.captured, c.curBlock.closureLocal, svar.capturedIdx, dst);
                    try c.buf.pushOp2(.boxValue, dst, dst);
                } else {
                    try c.buf.pushOp2(.boxValue, svar.local, dst);
                }
            } else {
                if (dst == svar.local) {
                    // Nop.
                } else {
                    try c.buf.pushOp2Ext(.copy, svar.local, dst, c.desc(nodeId));
                }
            }
            return c.initGenValue(dst, semaType, false);
        }
    } else {
        return symbolTo(c, node.head.ident.sema_csymId, cstr);
    }
}

fn symbolTo(self: *Chunk, csymId: sema.CompactSymbolId, req: RegisterCstr) !GenValue {
    if (csymId.isFuncSymId) {
        const rFuncSym = self.compiler.sema.getFuncSym(csymId.id);
        const symId = rFuncSym.getSymbolId();
        const rSym = self.compiler.sema.getSymbol(symId);

        const rtSymId = try self.compiler.vm.ensureFuncSym(rSym.key.resolvedSymKey.parentSymId, rSym.key.resolvedSymKey.nameId, rFuncSym.getFuncSigId());
        const pc = self.buf.len();

        const dst = try self.rega.selectFromNonLocalVar(req, true);

        try self.pushOptionalDebugSym(self.curNodeId);
        try self.buf.pushOp3(.staticFunc, 0, 0, dst);
        self.buf.setOpArgU16(pc + 1, @intCast(rtSymId));
        return self.initGenValue(dst, bt.Any, true);
    } else {
        const rSym = self.compiler.sema.getSymbol(csymId.id);
        switch (rSym.symT) {
            .variable => {
                const key = rSym.key.resolvedSymKey;
                const varId = try self.compiler.vm.ensureVarSym(key.parentSymId, key.nameId);

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
                const name = sema.getName(self.compiler, rSym.key.resolvedSymKey.nameId);
                return self.reportError("Can't use symbol `{}` as a value.", &.{v(name)});
            }
        }
    }
}

fn stringTemplate(self: *Chunk, nodeId: cy.NodeId, req: RegisterCstr) !GenValue {
    const node = self.nodes[nodeId];
    const operandStart = self.operandStack.items.len;
    defer self.operandStack.items.len = operandStart;

    const regStart = self.regStack.items.len;
    defer self.regStack.items.len = regStart;

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
            try self.operandStack.append(self.alloc, @intCast(idx));
        } else {
            const argv = try expression(self, curId, RegisterCstr.temp);
            const argRetained = try self.pushUnwindIndexIfRetainedTemp(argv);
            if (argRetained) {
                try self.regStack.append(self.alloc, argv.local);
            }
            numExprs += 1;
        }
        curId = cur.next;
        expStringPart = !expStringPart;
    }

    const retainedTemps = self.regStack.items[regStart..];
    defer self.popUnwindTempIndexN(@intCast(retainedTemps.len));

    try self.pushOptionalDebugSym(nodeId);
    try self.buf.pushOp3(.stringTemplate, tempStart, @intCast(numExprs), dst);
    try self.buf.pushOperands(self.operandStack.items[operandStart..]);

    try pushReleases(self, retainedTemps, nodeId);

    // string or rawstring
    return self.initGenValue(dst, bt.Any, true);
}

fn objectInit(self: *Chunk, nodeId: cy.NodeId, req: RegisterCstr) !GenValue {
    const node = self.nodes[nodeId];
    const stype = self.nodes[node.head.objectInit.name];

    if (node.head.objectInit.sema_symId != cy.NullId) {
        const rSym = self.compiler.sema.getSymbol(node.head.objectInit.sema_symId);
        if (rSym.symT == .object) {
            const initializer = self.nodes[node.head.objectInit.initializer];

            // TODO: Would it be faster/efficient to copy the fields into contiguous registers
            //       and copy all at once to heap or pass locals into the operands and iterate each and copy to heap?
            //       The current implementation is the former.

            const mod = self.compiler.sema.getModule(rSym.inner.object.modId);

            // TODO: This is one case where having an IR would be helpful
            //       to avoid initializing this buffer again.
            const fieldsDataStart = self.stackData.items.len;
            try self.stackData.resize(self.alloc, self.stackData.items.len + mod.fields.len);
            defer self.stackData.items.len = fieldsDataStart;

            // Initially set to NullId so leftovers are defaulted to `none`.
            const fieldsData = self.stackData.items[fieldsDataStart..];
            @memset(fieldsData, .{ .nodeId = cy.NullId });

            const dst = try self.rega.selectFromNonLocalVar(req, true);
            const tempStart = self.rega.getNextTemp();
            defer self.rega.setNextTemp(tempStart);

            var i: u32 = 0;
            var entryId = initializer.head.child_head;

            while (entryId != cy.NullId) : (i += 1) {
                const entry = self.nodes[entryId];
                fieldsData[entry.head.mapEntry.semaFieldIdx] = .{ .nodeId = entryId };
                entryId = entry.next;
            }

            var reqTypeCheck = false;
            const typeCheckIdxStart = self.stackData.items.len;
            for (fieldsData, 0..) |item, fidx| {
                if (item.nodeId == cy.NullId) {
                    if (mod.fields[fidx].typeId == bt.Dynamic) {
                        const local = try self.rega.consumeNextTemp();
                        try self.buf.pushOp1(.none, local);
                    } else {
                        const local = try self.rega.consumeNextTemp();
                        try genZeroInit(self, mod.fields[fidx].typeId, local, nodeId);
                    }
                } else {
                    const entry = self.nodes[item.nodeId];
                    const exprv = try expression(self, entry.head.mapEntry.right, RegisterCstr.tempMustRetain);
                    if (mod.fields[fidx].typeId != bt.Dynamic and exprv.vtype == bt.Dynamic) {
                        reqTypeCheck = true;
                        try self.stackData.append(self.alloc, .{ .idx = @intCast(fidx) });
                    }
                }
            }

            if (reqTypeCheck) {
                try self.pushFailableDebugSym(nodeId);
                const typeCheckIdxes = self.stackData.items[typeCheckIdxStart..];
                try self.buf.pushOp2(.objectTypeCheck, tempStart, @intCast(typeCheckIdxes.len));
                for (typeCheckIdxes) |fidx| {
                    const start = self.buf.ops.items.len;
                    try self.buf.pushOperands(&.{ @as(u8, @intCast(fidx.idx)), 0, 0, 0, 0 });
                    self.buf.setOpArgU32(start + 1, mod.fields[fidx.idx].typeId);
                }
            }

            const typeId = rSym.getObjectTypeId(self.compiler.vm).?;
            try pushObjectInit(self, typeId, tempStart, @intCast(mod.fields.len), dst, nodeId);
            const type_ = node.head.objectInit.sema_symId;
            return GenValue.initTempValue(dst, type_, true);
        }
    }
    const oname = self.getNodeTokenString(stype);
    return self.reportErrorAt("Expected object type: `{}`", &.{v(oname)}, nodeId);
}

fn genZeroInit(c: *cy.Chunk, typeId: types.TypeId, dst: RegisterId, debugNodeId: cy.NodeId) !void {
    switch (typeId) {
        bt.Any,
        bt.Dynamic => try c.buf.pushOp1(.none, dst),
        bt.Boolean => try c.buf.pushOp1(.false, dst),
        bt.Integer => _ = try constInt(c, 0, dst),
        bt.Float => _ = try constFloat(c, 0, dst),
        bt.List => {
            try c.pushOptionalDebugSym(debugNodeId);
            try c.buf.pushOp3(.list, dst, 0, dst);
        },
        bt.Map => {
            try c.buf.pushOp1(.mapEmpty, dst);
        },
        // bt.Rawstring => {
        // },
        bt.String => {
            _ = try string(c, "", dst);
        },
        else => {
            const sym = c.compiler.sema.getSymbol(typeId);
            if (sym.symT == .object) {
                // Unwind stack registers.
                const tempStart = c.rega.getNextTemp();
                defer c.rega.setNextTemp(tempStart);

                const modId = sym.inner.object.modId;
                const mod = c.compiler.sema.getModule(modId);

                for (mod.fields) |field| {
                    const local = try c.rega.consumeNextTemp();
                    try genZeroInit(c, field.typeId, local, debugNodeId);
                }

                const rtTypeId = sym.getObjectTypeId(c.compiler.vm).?;
                try pushObjectInit(c, rtTypeId, tempStart, @intCast(mod.fields.len), dst, debugNodeId);
            } else {
                return error.Unsupported;
            }
        },
    }
}

fn pushObjectInit(c: *cy.Chunk, typeId: rt.TypeId, startLocal: u8, numFields: u8, dst: RegisterId, debugNodeId: cy.NodeId) !void {
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
            else => cy.panicFmt("unsupported key {}", .{key.node_t}),
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

fn binExprGeneric(c: *Chunk, code: cy.OpCode, vtype: types.TypeId, opts: GenBinExprOptions) !GenValue {
    const dst = try c.rega.selectFromNonLocalVar(opts.cstr, false);
    const tempStart = c.rega.getNextTemp();
    defer c.rega.setNextTemp(tempStart);

    var canUseDst = c.canUseDstAsTempForBinOp(dst);
    var leftv: GenValue = undefined;
    if (opts.leftv) |_leftv| {
        leftv = _leftv;
    } else {
        leftv = try expression(c, opts.leftId, RegisterCstr.preferIf(dst, canUseDst));
        if (leftv.local == dst) {
            canUseDst = false;
        }
    }
    const rightv = try expression(c, opts.rightId, RegisterCstr.preferIf(dst, canUseDst));

    try c.pushOptionalDebugSym(opts.debugNodeId);
    try c.buf.pushOp3(code, leftv.local, rightv.local, dst);

    // ARC cleanup.
    if (opts.leftv == null) {
        try releaseIfRetainedTemp(c, leftv);
    }
    try releaseIfRetainedTemp(c, rightv);

    return c.initGenValue(dst, vtype, false);
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

fn getInfixMGID(c: *Chunk, op: cy.BinaryExprOp) vmc.MethodGroupId {
    return switch (op) {
        .less => c.compiler.vm.@"infix<MGID",
        .greater => c.compiler.vm.@"infix>MGID",
        .less_equal => c.compiler.vm.@"infix<=MGID",
        .greater_equal => c.compiler.vm.@"infix>=MGID",
        .minus => c.compiler.vm.@"infix-MGID",
        .plus => c.compiler.vm.@"infix+MGID",
        .star => c.compiler.vm.@"infix*MGID",
        .slash => c.compiler.vm.@"infix/MGID",
        .percent => c.compiler.vm.@"infix%MGID",
        .caret => c.compiler.vm.@"infix^MGID",
        .bitwiseAnd => c.compiler.vm.@"infix&MGID",
        .bitwiseOr => c.compiler.vm.@"infix|MGID",
        .bitwiseXor => c.compiler.vm.@"infix||MGID",
        .bitwiseLeftShift => c.compiler.vm.@"infix<<MGID",
        .bitwiseRightShift => c.compiler.vm.@"infix>>MGID",
        else => cy.fatal(),
    };
}

const GenBinExprOptions = struct {
    leftId: cy.NodeId,
    rightId: cy.NodeId,
    debugNodeId: cy.NodeId,
    op: cy.BinaryExprOp,
    genStrat: cy.GenBinExprStrategy,
    cstr: RegisterCstr,

    /// Whether to use an existing leftv managed by the caller.
    leftv: ?GenValue = null,
    rightv: ?GenValue = null,
};

/// Abstracted to allow assign op to share codegen with binExpr.
fn binExpr2(c: *Chunk, opts: GenBinExprOptions) !GenValue {
    switch (opts.op) {
        .equal_equal => {
            return binExprGeneric(c, .compare, bt.Boolean, opts);
        },
        .bang_equal => {
            return binExprGeneric(c, .compareNot, bt.Boolean, opts);
        },
        .bitwiseAnd,
        .bitwiseOr,
        .bitwiseXor,
        .bitwiseLeftShift,
        .bitwiseRightShift,
        .percent,
        .slash,
        .star,
        .caret,
        .minus,
        .plus => {
            const leftT = c.nodeTypes[opts.leftId];
            switch (opts.genStrat) {
                .none => cy.fatal(),
                .specialized => {
                    const dst = try c.rega.selectFromNonLocalVar(opts.cstr, false);
                    const tempStart = c.rega.getNextTemp();
                    defer c.rega.setNextTemp(tempStart);

                    var canUseDst = c.canUseDstAsTempForBinOp(dst);
                    var leftv: GenValue = undefined;
                    if (opts.leftv) |_leftv| {
                        leftv = _leftv;
                    } else {
                        leftv = try expression(c, opts.leftId, RegisterCstr.preferIf(dst, canUseDst));
                        if (leftv.local == dst) {
                            canUseDst = false;
                        }
                    }
                    const rightv = try expression(c, opts.rightId, RegisterCstr.preferIf(dst, canUseDst));

                    if (leftT == bt.Float) {
                        try pushInlineBinExpr(c, getFloatOpCode(opts.op), leftv.local, rightv.local, dst, opts.debugNodeId);
                    } else if (leftT == bt.Integer) {
                        try pushInlineBinExpr(c, getIntOpCode(opts.op), leftv.local, rightv.local, dst, opts.debugNodeId);
                    } else {
                        cy.fatal();
                    }

                    // ARC cleanup.
                    try releaseIfRetainedTemp(c, rightv);

                    return c.initGenValue(dst, leftT, false);
                },
                .generic => {
                    return callObjSymBinExpr(c, getInfixMGID(c, opts.op), opts);
                }
            }
        },
        .less_equal,
        .greater,
        .greater_equal,
        .less => {
            const leftT = c.nodeTypes[opts.leftId];
            switch (opts.genStrat) {
                .none => cy.fatal(),
                .specialized => {
                    const dst = try c.rega.selectFromNonLocalVar(opts.cstr, false);
                    const tempStart = c.rega.getNextTemp();
                    defer c.rega.setNextTemp(tempStart);

                    var canUseDst = c.canUseDstAsTempForBinOp(dst);
                    var leftv: GenValue = undefined;
                    if (opts.leftv) |_leftv| {
                        leftv = _leftv;
                    } else {
                        leftv = try expression(c, opts.leftId, RegisterCstr.preferIf(dst, canUseDst));
                        if (leftv.local == dst) {
                            canUseDst = false;
                        }
                    }
                    const rightv = try expression(c, opts.rightId, RegisterCstr.preferIf(dst, canUseDst));

                    if (leftT == bt.Float) {
                        try pushInlineBinExpr(c, getFloatOpCode(opts.op), leftv.local, rightv.local, dst, opts.debugNodeId);
                    } else if (leftT == bt.Integer) {
                        try pushInlineBinExpr(c, getIntOpCode(opts.op), leftv.local, rightv.local, dst, opts.debugNodeId);
                    } else {
                        cy.fatal();
                    }

                    // ARC cleanup.
                    try releaseIfRetainedTemp(c, rightv);

                    return c.initGenValue(dst, bt.Boolean, false);
                },
                .generic => {
                    return callObjSymBinExpr(c, getInfixMGID(c, opts.op), opts);
                }
            }
        },
        .and_op => {
            const dst = try c.rega.selectFromNonLocalVar(opts.cstr, true);

            var leftv: GenValue = undefined;
            if (opts.leftv) |_leftv| {
                leftv = _leftv;
            } else {
                leftv = try expression(c, opts.leftId, RegisterCstr.initExact(dst, opts.cstr.mustRetain));
            }

            const jumpPc = try c.pushEmptyJumpNotCond(leftv.local);

            // ARC cleanup. First operand is not needed anymore.
            try releaseIfRetainedTemp(c, leftv);

            const rightv = try expression(c, opts.rightId, RegisterCstr.initExact(dst, opts.cstr.mustRetain));
            c.patchJumpNotCondToCurPc(jumpPc);

            if (leftv.vtype  == rightv.vtype) {
                return c.initGenValue(dst, leftv.vtype, leftv.retained or rightv.retained);
            } else {
                return c.initGenValue(dst, bt.Any, leftv.retained or rightv.retained);
            }
        },
        .or_op => {
            const dst = try c.rega.selectFromNonLocalVar(opts.cstr, true);

            var leftv: GenValue = undefined;
            if (opts.leftv) |_leftv| {
                leftv = _leftv;
            } else {
                leftv = try expression(c, opts.leftId, RegisterCstr.initExact(dst, opts.cstr.mustRetain));
            }
            const jumpPc = try c.pushEmptyJumpCond(leftv.local);

            // ARC cleanup. First operand is not needed anymore.
            try releaseIfRetainedTemp(c, leftv);

            const rightv = try expression(c, opts.rightId, RegisterCstr.initExact(dst, opts.cstr.mustRetain));
            c.patchJumpCondToCurPc(jumpPc);

            if (leftv.vtype == rightv.vtype) {
                return c.initGenValue(dst, leftv.vtype, leftv.retained or rightv.retained);
            } else {
                return c.initGenValue(dst, bt.Any, leftv.retained or rightv.retained);
            }
        },
        else => return c.reportErrorAt("Unsupported binary op: {}", &.{v(opts.op)}, opts.debugNodeId),
    }
}

fn binExpr(c: *Chunk, nodeId: cy.NodeId, cstr: RegisterCstr) !GenValue {
    const node = c.nodes[nodeId];
    return binExpr2(c, .{
        .leftId = node.head.binExpr.left,
        .rightId = node.head.binExpr.right,
        .debugNodeId = nodeId,
        .op = node.head.binExpr.op,
        .genStrat = node.head.binExpr.semaGenStrat,
        .cstr = cstr,
    });
}

fn lambdaMulti(self: *Chunk, nodeId: cy.NodeId, req: RegisterCstr) !GenValue {
    const node = self.nodes[nodeId];

    const jumpPc = try self.pushEmptyJump();

    const func = self.semaFuncDecls.items[node.head.func.semaDeclId];

    try pushSemaBlock(self, func.semaBlockId);
    self.curBlock.frameLoc = nodeId;

    const jumpStackStart = self.blockJumpStack.items.len;
    const opStart: u32 = @intCast(self.buf.ops.items.len);

    // Generate function body.
    try reserveFuncRegs(self, func.numParams);

    try genStatements(self, node.head.func.bodyHead, false);

    try genBlockEnd(self);
    self.patchJumpToCurPc(jumpPc);

    const sblock = sema.curBlock(self);
    const numCaptured: u8 = @intCast(sblock.captures.items.len);
    const closureLocal = self.curBlock.closureLocal;

    self.patchBlockJumps(jumpStackStart);
    self.blockJumpStack.items.len = jumpStackStart;

    const stackSize = self.getMaxUsedRegisters();
    popSemaBlock(self);

    const dst = try self.rega.selectFromNonLocalVar(req, true);

    if (numCaptured == 0) {
        const funcPcOffset: u8 = @intCast(self.buf.ops.items.len - opStart);
        try self.pushOptionalDebugSym(nodeId);

        const start = self.buf.ops.items.len;
        try self.buf.pushOpSlice(.lambda, &.{ funcPcOffset, func.numParams, stackSize, 0, 0, dst });
        self.buf.setOpArgU16(start + 4, @intCast(func.inner.lambda.funcSigId));
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
        self.buf.setOpArgU16(start + 5, @intCast(func.inner.lambda.funcSigId));
        try self.buf.pushOperands(self.operandStack.items[operandStart..]);
        return self.initGenValue(dst, bt.Any, true);
    }
}

fn lambdaExpr(self: *Chunk, nodeId: cy.NodeId, req: RegisterCstr) !GenValue {
    const node = self.nodes[nodeId];

    const jumpPc = try self.pushEmptyJump();

    const func = self.semaFuncDecls.items[node.head.func.semaDeclId];
    try pushSemaBlock(self, func.semaBlockId);
    const opStart: u32 = @intCast(self.buf.ops.items.len);

    // Generate function body.
    try reserveFuncRegs(self, func.numParams);

    _ = try expression(self, node.head.func.bodyHead, RegisterCstr.exactMustRetain(0));
    try genBlockReleaseLocals(self);
    try self.buf.pushOp(.ret1);
    self.patchJumpToCurPc(jumpPc);

    const sblock = sema.curBlock(self);
    const numCaptured: u8 = @intCast(sblock.captures.items.len);
    const closureLocal = self.curBlock.closureLocal;
    const stackSize = self.getMaxUsedRegisters();
    popSemaBlock(self);

    const dst = try self.rega.selectFromNonLocalVar(req, true);

    if (numCaptured == 0) {
        const funcPcOffset: u8 = @intCast(self.buf.ops.items.len - opStart);
        try self.pushOptionalDebugSym(nodeId);
        const start = self.buf.ops.items.len;
        try self.buf.pushOpSlice(.lambda, &.{ funcPcOffset, func.numParams, stackSize, 0, 0, dst });
        self.buf.setOpArgU16(start + 4, @intCast(func.inner.lambda.funcSigId));
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
        self.buf.setOpArgU16(start + 5, @intCast(func.inner.lambda.funcSigId));
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

    try self.pushFailableDebugSym(nodeId);
    try self.buf.pushOp3(.list, tempStart, @intCast(i), dst);
    return self.initGenValue(dst, bt.List, true);
}

fn sliceExpr(self: *Chunk, nodeId: cy.NodeId, cstr: RegisterCstr) !GenValue {
    const node = self.nodes[nodeId];

    const selfT = self.nodeTypes[node.head.sliceExpr.arr];
    if (selfT == bt.List) {
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
                leftv = try constInt(self, 0, dst);
                canUseDst = false;
            } else {
                const leftDst = try self.rega.consumeNextTemp();
                leftv = try constInt(self, 0, leftDst);
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

        try pushInlineTernExpr(self, .sliceList, parentv.local, leftv.local, rightv.local, dst, nodeId);

        // ARC cleanup.
        try releaseIfRetainedTemp(self, parentv);
        try releaseIfRetainedTemp(self, leftv);
        try releaseIfRetainedTemp(self, rightv);

        return self.initGenValue(dst, bt.List, true);
    } else {
        const callStartLocal = try beginCall(self, cstr);
        defer endCall(self, callStartLocal);

        const typeStart = self.compiler.typeStack.items.len;
        defer self.compiler.typeStack.items.len = typeStart;

        const start = self.regStack.items.len;
        defer self.regStack.items.len = start;

        const selfv = try expression(self, node.head.sliceExpr.arr, RegisterCstr.temp);
        const selfRetained = try self.pushUnwindIndexIfRetainedTemp(selfv);
        if (selfRetained) {
            try self.pushReg(selfv.local);
        }
        try self.compiler.typeStack.append(self.alloc, bt.Any);

        // Range left value.
        var leftv: GenValue = undefined;
        if (node.head.sliceExpr.left == cy.NullId) {
            const leftDst = try self.rega.consumeNextTemp();
            leftv = try constInt(self, 0, leftDst);
        } else {
            leftv = try expression(self, node.head.sliceExpr.left, RegisterCstr.temp);
            const leftRetained = try self.pushUnwindIndexIfRetainedTemp(leftv);
            if (leftRetained) {
                try self.pushReg(leftv.local);
            }
        }
        try self.compiler.typeStack.append(self.alloc, leftv.vtype);

        // Range right value.
        var rightv: GenValue = undefined;
        if (node.head.sliceExpr.right == cy.NullId) {
            const rightDst = try self.rega.consumeNextTemp();
            rightv = try genNone(self, rightDst);
        } else {
            rightv = try expression(self, node.head.sliceExpr.right, RegisterCstr.temp);
            const rightRetained = try self.pushUnwindIndexIfRetainedTemp(rightv);
            if (rightRetained) {
                try self.pushReg(rightv.local);
            }
        }
        try self.compiler.typeStack.append(self.alloc, rightv.vtype);

        const retainedTemps = self.regStack.items[start..];
        defer self.popUnwindTempIndexN(@intCast(retainedTemps.len));

        const funcSigId = try sema.ensureFuncSig(self.compiler, self.compiler.typeStack.items[typeStart..], bt.Any);
        return try callObjSymRelease(self, callStartLocal, self.compiler.vm.sliceMGID, funcSigId, 3, retainedTemps, cstr, nodeId);
    }
}

fn indexExpr(c: *Chunk, nodeId: cy.NodeId, optLeft: ?GenValue, optIndex: ?GenValue, cstr: RegisterCstr) !GenValue {
    const node = c.nodes[nodeId];
    const leftT = c.nodeTypes[node.head.indexExpr.left];
    switch (node.head.indexExpr.semaGenStrat) {
        .none => cy.fatal(),
        .specialized => {
            const dst = try c.rega.selectFromNonLocalVar(cstr, true);
            const tempStart = c.rega.getNextTemp();
            defer c.rega.setNextTemp(tempStart);

            var canUseDst = c.canUseDstAsTempForBinOp(dst);

            // Gen left.
            var leftv: GenValue = undefined;
            if (optLeft) |_leftv| {
                leftv = _leftv;
            } else {
                leftv = try expression(c, node.head.indexExpr.left, RegisterCstr.preferIf(dst, canUseDst));
                if (leftv.local == dst) {
                    canUseDst = false;
                }
            }

            // Gen index.
            var indexv: GenValue = undefined;
            if (optIndex) |_indexv| {
                indexv = _indexv;
            } else {
                indexv = try expression(c, node.head.indexExpr.right, RegisterCstr.preferIf(dst, canUseDst));
            }

            if (leftT == bt.List) {
                try pushInlineBinExpr(c, .indexList, leftv.local, indexv.local, dst, nodeId);
            } else if (leftT == bt.Map) {
                try pushInlineBinExpr(c, .indexMap, leftv.local, indexv.local, dst, nodeId);
            } else {
                cy.fatal();
            }

            // ARC cleanup.
            if (optLeft == null) {
                try releaseIfRetainedTemp(c, leftv);
            }
            if (optIndex == null) {
                try releaseIfRetainedTemp(c, indexv);
            }

            return c.initGenValue(dst, bt.Dynamic, true);
        },
        .generic => {
            return callObjSymBinExpr(c, c.compiler.vm.indexMGID, .{
                .leftId = node.head.indexExpr.left,
                .rightId = node.head.indexExpr.right,
                .op = undefined,
                .genStrat = node.head.indexExpr.semaGenStrat,
                .cstr = cstr,
                .debugNodeId = nodeId,
                .leftv = optLeft,
                .rightv = optIndex,
            });
        }
    }
}

fn accessExpr(self: *Chunk, nodeId: cy.NodeId, req: RegisterCstr) !GenValue {
    const node = self.nodes[nodeId];
    const csymId = node.head.accessExpr.sema_csymId;
    if (csymId.isPresent()) {
        if (csymId.isFuncSymId) {
            const rFuncSym = self.compiler.sema.getFuncSym(csymId.id);
            const rSym = self.compiler.sema.getSymbol(rFuncSym.getSymbolId());
            const key = rSym.key.resolvedSymKey;
            const rtSymId = try self.compiler.vm.ensureFuncSym(key.parentSymId, key.nameId, rFuncSym.getFuncSigId());

            try self.pushOptionalDebugSym(nodeId);
            const pc = self.buf.len();
            const dst = try self.rega.selectFromNonLocalVar(req, true);
            try self.buf.pushOp3(.staticFunc, 0, 0, dst);
            self.buf.setOpArgU16(pc + 1, @intCast(rtSymId));
            return self.initGenValue(dst, bt.Any, true);
        } else {
            const rSym = self.compiler.sema.getSymbol(csymId.id);
            const key = rSym.key.resolvedSymKey;
            switch (rSym.symT) {
                .variable => {
                    const rtSymId = self.compiler.vm.getVarSym(key.parentSymId, key.nameId).?;
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

                    const vtype = rSym.key.resolvedSymKey.parentSymId;
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
        return genField(self, nodeId, req);
    }
}

fn constOp(c: *cy.Chunk, idx: usize, dst: u8) !void {
    const pc = c.buf.len();
    try c.buf.pushOp3(.constOp, 0, 0, dst);
    c.buf.setOpArgU16(pc + 1, @intCast(idx));
}

fn releaseIfRetainedTempAt(c: *cy.Chunk, val: GenValue, nodeId: cy.NodeId) !void {
    if (val.retained and val.isTempLocal) {
        try pushRelease(c, val.local, nodeId);
    }
}

fn releaseIfRetainedTemp(c: *cy.Chunk, val: GenValue) !void {
    return releaseIfRetainedTempAt(c, val, c.curNodeId);
}

fn genField(self: *cy.Chunk, nodeId: cy.NodeId, req: RegisterCstr) !GenValue {
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

    try pushFieldRetain(self, leftv.local, dst, @intCast(fieldId), nodeId);

    // ARC cleanup.
    try releaseIfRetainedTemp(self, leftv);

    return self.initGenValue(dst, newtype, true);
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
    if (head == cy.NullId) {
        try self.buf.pushOp1(.end, 255);
        return;
    }
    
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
                try genBlockReleaseLocals(self);
            }
            try self.buf.pushOp1(.end, local);
        } else {
            _ = try statement(self, cur_id);
        }
    } else {
        if (attachEnd) {
            try statement(self, cur_id);
            if (shouldGenMainScopeReleaseOps(self.compiler)) {
                try genBlockReleaseLocals(self);
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
    defer c.rega.setNextTemp(tempStart);

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
                const varId = c.nodes[varSpec.head.varSpec.name].head.ident.semaVarId;

                _ = try reserveLocalReg(c, varId, false);
                try assignExprToLocalVar(c, varSpec.head.varSpec.name, node.head.localDecl.right, true);
                c.curBlock.nextLocalReg += 1;
            }
        },
        .assign_stmt => {
            try assignStmt(c, nodeId);
        },
        .opAssignStmt => {
            try opAssignStmt(c, nodeId);
        },
        .importStmt => {
            // Nop.
        },
        .funcDeclInit => {
            // Nop. Func declaration initializer are hoisted and initialized at the start of the program.
        },
        .funcDecl => {
            try funcDecl(c, c.semaRootSymId, nodeId);
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
            const crObjSymId = nameN.head.ident.sema_csymId;
            const robjSymId = crObjSymId.id;
            const sid = try c.compiler.vm.ensureObjectType(c.semaRootSymId, nameId, robjSymId);

            const body = c.nodes[node.head.objectDecl.body];
            var funcId = body.head.objectDeclBody.funcsHead;
            var func: cy.Node = undefined;
            while (funcId != cy.NullId) : (funcId = func.next) {
                func = c.nodes[funcId];
                const decl = c.semaFuncDecls.items[func.head.func.semaDeclId];

                const funcName = decl.getName(c);
                if (decl.numParams > 0) {
                    const param = c.nodes[decl.paramHead];
                    const paramName = c.getNodeTokenString(c.nodes[param.head.funcParam.name]);
                    if (std.mem.eql(u8, paramName, "self")) {
                        // Object method.
                        if (func.node_t == .funcDecl) {
                            try genMethodDecl(c, sid, func, decl, funcName);
                        }
                        continue;
                    }
                }

                // const detail = cy.FuncSymDetail{
                //     .name = try c.alloc.dupe(u8, funcName),
                // };
                // try c.compiler.vm.funcSymDetails.append(c.alloc, detail);
                if (func.node_t == .funcDecl) {
                    try funcDecl(c, robjSymId, funcId);
                }
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
                errorVarLocal = try reserveLocalReg(c, errorVarN.head.ident.semaVarId, true);
            }
            const pushTryPc = c.buf.ops.items.len;
            try c.buf.pushOp3(.pushTry, errorVarLocal, 0, 0);

            // try body.
            nextSemaSubBlock(c);
            try genStatements(c, node.head.tryStmt.tryFirstStmt, false);
            try genSubBlockReleaseLocals(c);
            prevSemaSubBlock(c);

            // pop try block.
            const popTryPc = c.buf.ops.items.len;
            try c.buf.pushOp2(.popTry, 0, 0);
            c.buf.setOpArgU16(pushTryPc + 2, @intCast(c.buf.ops.items.len - pushTryPc));

            // catch body.
            nextSemaSubBlock(c);
            try genStatements(c, node.head.tryStmt.catchFirstStmt, false);
            try genSubBlockReleaseLocals(c);
            prevSemaSubBlock(c);

            c.buf.setOpArgU16(popTryPc + 1, @intCast(c.buf.ops.items.len - popTryPc));
        },
        .staticDecl => {
            // Nop. Static variables are hoisted and initialized at the start of the program.
        },
        .whileOptStmt => {
            try whileOptStmt(c, nodeId);
        },
        .whileInfStmt => {
            nextSemaSubBlock(c);

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
            try genSubBlockReleaseLocals(c);
            try c.pushJumpBackTo(pcSave);

            c.patchForBlockJumps(jumpStackSave, c.buf.ops.items.len, pcSave);
            prevSemaSubBlock(c);
        },
        .whileCondStmt => {
            nextSemaSubBlock(c);

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
            try genSubBlockReleaseLocals(c);
            try c.pushJumpBackTo(topPc);

            c.patchJumpNotCondToCurPc(jumpPc);

            c.patchForBlockJumps(jumpStackSave, c.buf.ops.items.len, topPc);
            prevSemaSubBlock(c);
        },
        .for_range_stmt => {
            try forRangeStmt(c, nodeId);
        },
        .for_iter_stmt => {
            try forIterStmt(c, nodeId);
        },
        .return_stmt => {
            if (c.blocks.items.len == 1) {
                try genBlockReleaseLocals(c);
                try c.buf.pushOp1(.end, 255);
            } else {
                try genBlockReleaseLocals(c);
                try c.buf.pushOp(.ret0);
            }
        },
        .return_expr_stmt => {
            // TODO: If the returned expr is a local, consume the local after copying to reg 0.
            if (c.blocks.items.len == 1) {
                // Main block.
                const val = try expression(c, node.head.child_head, RegisterCstr.simpleMustRetain);
                try genBlockReleaseLocals(c);
                try c.buf.pushOp1(.end, @intCast(val.local));
            } else {
                // if (c.curBlock.funcSymId != cy.NullId) {
                //     const retType = c.compiler.sema.resolvedFuncSyms.items[c.curBlock.funcSymId].retType;
                //     _ = try genExprTo2(c, node.head.child_head, 0, retType, true, true);
                    // _ = try expression(c, node.head.child_head, RegisterCstr.exactMustRetain(0));
                // } else {
                    _ = try expression(c, node.head.child_head, RegisterCstr.exactMustRetain(0));
                // }
                try genBlockReleaseLocals(c);
                try c.buf.pushOp(.ret1);
            }
        },
        .comptimeStmt => {
            try comptimeStmt(c, nodeId);
        },
        else => {
            return c.reportErrorAt("Unsupported statement: {}", &.{v(node.node_t)}, nodeId);
        }
    }
}

fn whileOptStmt(c: *cy.Chunk, nodeId: cy.NodeId) !void {
    const node = c.nodes[nodeId];
    nextSemaSubBlock(c);

    const topPc: u32 = @intCast(c.buf.ops.items.len);
    const jumpStackSave: u32 = @intCast(c.subBlockJumpStack.items.len);
    defer c.subBlockJumpStack.items.len = jumpStackSave;

    var optLocal: LocalId = undefined;
    if (node.head.whileOptStmt.some != cy.NullId) {
        const some = c.nodes[node.head.whileOptStmt.some];
        optLocal = try reserveLocalReg(c, some.head.ident.semaVarId, false);
        try assignExprToLocalVar(c, node.head.whileOptStmt.some, node.head.whileOptStmt.opt, true);
        c.curBlock.nextLocalReg += 1;
    } else {
        const optv = try expression(c, node.head.whileOptStmt.opt, RegisterCstr.simple);
        optLocal = optv.local;
    }

    const skipSkipJump = try c.pushEmptyJumpNotNone(optLocal);
    const skipBodyJump = try c.pushEmptyJump();
    c.patchJumpNotNoneToCurPc(skipSkipJump);

    try genStatements(c, node.head.whileOptStmt.bodyHead, false);
    try genSubBlockReleaseLocals(c);
    try c.pushJumpBackTo(topPc);

    c.patchJumpToCurPc(skipBodyJump);

    c.patchForBlockJumps(jumpStackSave, c.buf.ops.items.len, topPc);
    prevSemaSubBlock(c);
}

fn opAssignStmt(c: *cy.Chunk, nodeId: cy.NodeId) !void {
    const node = c.nodes[nodeId];
    const left = c.nodes[node.head.opAssignStmt.left];
    switch (node.head.opAssignStmt.op) {
        .percent,
        .caret,
        .plus,
        .minus,
        .star,
        .slash => {},
        else => fmt.panic("Unexpected operator assignment.", &.{}),
    }
    if (left.node_t == .ident) {
        if (left.head.ident.semaVarId != cy.NullId) {
            const svar = c.genGetVarPtr(left.head.ident.semaVarId).?;
            if (svar.isBoxed) {
                const temp = try c.rega.consumeNextTemp();
                if (svar.isParentLocalAlias()) {
                    try c.buf.pushOp3(.captured, c.curBlock.closureLocal, svar.capturedIdx, temp);
                    try c.buf.pushOp2(.boxValue, temp, temp);
                } else {
                    try c.buf.pushOp2(.boxValue, svar.local, temp);
                }

                _ = try binExpr2(c, .{
                    .leftId = node.head.opAssignStmt.left,
                    .rightId = node.head.opAssignStmt.right,
                    .debugNodeId = nodeId,
                    .op = node.head.opAssignStmt.op,
                    .genStrat = node.head.opAssignStmt.semaGenStrat,
                    .leftv = GenValue.initTempValue(temp, svar.vtype, false),
                    .cstr = RegisterCstr.exact(temp),
                });

                if (svar.isParentLocalAlias()) {
                    const temp2 = try c.rega.consumeNextTemp();
                    try c.buf.pushOp3(.captured, c.curBlock.closureLocal, svar.capturedIdx, temp2);
                    try c.buf.pushOp2(.setBoxValue, temp2, temp);
                } else {
                    try c.buf.pushOp2(.setBoxValue, svar.local, temp);
                }
                return;
            } else {
                _ = try binExpr2(c, .{
                    .leftId = node.head.opAssignStmt.left,
                    .rightId = node.head.opAssignStmt.right,
                    .debugNodeId = nodeId,
                    .op = node.head.opAssignStmt.op,
                    .genStrat = node.head.opAssignStmt.semaGenStrat,
                    .leftv = GenValue.initLocalValue(svar.local, svar.vtype, false),
                    .cstr = RegisterCstr.exact(svar.local),
                });
            }
        } else {
            const csymId = left.head.ident.sema_csymId;
            if (!csymId.isFuncSymId) {
                const rsym = c.compiler.sema.getSymbol(csymId.id);
                const rtSymId = try c.compiler.vm.ensureVarSym(rsym.key.resolvedSymKey.parentSymId, rsym.key.resolvedSymKey.nameId);

                const temp = try c.rega.consumeNextTemp();
                try c.pushOptionalDebugSym(nodeId);       
                var pc = c.buf.len();
                try c.buf.pushOp3(.staticVar, 0, 0, temp);
                c.buf.setOpArgU16(pc + 1, @intCast(rtSymId));

                _ = try binExpr2(c, .{
                    .leftId = node.head.opAssignStmt.left,
                    .rightId = node.head.opAssignStmt.right,
                    .debugNodeId = nodeId,
                    .op = node.head.opAssignStmt.op,
                    .genStrat = node.head.opAssignStmt.semaGenStrat,
                    .leftv = GenValue.initTempValue(temp, bt.Any, false), // TODO: Get sym type.
                    .cstr = RegisterCstr.exact(temp),
                });

                pc = c.buf.ops.items.len;
                try c.buf.pushOp3(.setStaticVar, 0, 0, temp);
                c.buf.setOpArgU16(pc + 1, @intCast(rtSymId));
            } else {
                cy.fatal();
            }
        }
    } else if (left.node_t == .accessExpr) {
        try binOpAssignToField(c, node.head.opAssignStmt.left, node.head.opAssignStmt.right, node.head.opAssignStmt.op, node.head.opAssignStmt.semaGenStrat, nodeId);
    } else if (left.node_t == .indexExpr) {
        try binOpAssignToIndex(c, node.head.opAssignStmt.left, node.head.opAssignStmt.right, node.head.opAssignStmt.op, node.head.opAssignStmt.semaGenStrat, nodeId);
    } else {
        return c.reportErrorAt("Unsupported assignment to left: {}", &.{v(left.node_t)}, nodeId);
    }
}

fn assignStmt(c: *cy.Chunk, nodeId: cy.NodeId) !void {
    const node = c.nodes[nodeId];
    const left = c.nodes[node.head.left_right.left];
    if (left.node_t == .ident) {
        if (left.head.ident.semaVarId != cy.NullId) {
            try assignExprToLocalVar(c, node.head.left_right.left, node.head.left_right.right, false);
        } else {
            const csymId = left.head.ident.sema_csymId;
            if (!csymId.isFuncSymId) {
                const rsym = c.compiler.sema.getSymbol(csymId.id);
                const rightv = try expression(c, node.head.left_right.right, RegisterCstr.tempMustRetain);
                const rtSymId = try c.compiler.vm.ensureVarSym(rsym.key.resolvedSymKey.parentSymId, rsym.key.resolvedSymKey.nameId);

                const pc = c.buf.len();
                try c.buf.pushOp3(.setStaticVar, 0, 0, rightv.local);
                c.buf.setOpArgU16(pc + 1, @intCast(rtSymId));
            } else {
                return c.reportError("Unsupported", &.{});
            }
        }
    } else if (left.node_t == .indexExpr) {
        switch (left.head.indexExpr.semaGenStrat) {
            .none => cy.fatal(),
            .specialized => {
                const selfT = c.nodeTypes[left.head.indexExpr.left];

                const regStart = c.regStack.items.len;
                defer c.regStack.items.len = regStart;

                const leftv = try expression(c, left.head.indexExpr.left, RegisterCstr.simple);
                const leftRetained = try c.pushUnwindIndexIfRetainedTemp(leftv);
                if (leftRetained) {
                    try c.pushReg(leftv.local);
                }

                const indexv = try expression(c, left.head.indexExpr.right, RegisterCstr.simple);
                const indexRetained = try c.pushUnwindIndexIfRetainedTemp(indexv);
                if (indexRetained) {
                    try c.pushReg(indexv.local);
                }

                const rightv = try expression(c, node.head.left_right.right, RegisterCstr.simple);
                const rightRetained = try c.pushUnwindIndexIfRetainedTemp(rightv);
                if (rightRetained) {
                    try c.pushReg(rightv.local);
                }

                const retainedTemps = c.regStack.items[regStart..];
                defer c.popUnwindTempIndexN(@intCast(retainedTemps.len));

                if (selfT == bt.List) {
                    // Reuse binExpr layout since there is no dst.
                    try pushInlineBinExpr(c, .setIndexList, leftv.local, indexv.local, rightv.local, nodeId);
                } else if (selfT == bt.Map) {
                    try pushInlineBinExpr(c, .setIndexMap, leftv.local, indexv.local, rightv.local, nodeId);
                } else {
                    cy.fatal();
                }

                // ARC cleanup.
                try pushReleases(c, retainedTemps, nodeId);
            },
            .generic => {
                _ = try callObjSymTernNoRet(c, c.compiler.vm.setIndexMGID, 
                    left.head.indexExpr.left, 
                    left.head.indexExpr.right,
                    node.head.left_right.right,
                    nodeId,
                );
            },
        }
    } else if (left.node_t == .accessExpr) {
        const leftv = try expression(c, left.head.accessExpr.left, RegisterCstr.simple);
        const leftRetained = try c.pushUnwindIndexIfRetainedTemp(leftv);

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
            try c.pushFailableDebugSym(nodeId);
            try c.buf.pushOpSlice(.setCheckFieldRelease, &[_]u8{ leftv.local, rightv.local, @intCast(fieldId), 0, 0, 0 });
        } else {
            try c.pushFailableDebugSym(nodeId);
            try c.buf.pushOpSlice(.setFieldRelease, &[_]u8{ leftv.local, rightv.local, @intCast(fieldId), 0, 0, 0 });
        }


        // ARC cleanup. Right is not released since it's being assigned to the field.
        if (leftRetained) {
            try pushRelease(c, leftv.local, left.head.accessExpr.left);
            c.popUnwindTempIndex();
        }
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

    nextSemaSubBlock(c);
    try genStatements(c, node.head.left_right.right, false);
    try genSubBlockReleaseLocals(c);
    prevSemaSubBlock(c);

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
                nextSemaSubBlock(c);
                try genStatements(c, elseClause.head.else_clause.body_head, false);
                try genSubBlockReleaseLocals(c);
                prevSemaSubBlock(c);
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

                nextSemaSubBlock(c);
                try genStatements(c, elseClause.head.else_clause.body_head, false);
                try genSubBlockReleaseLocals(c);
                prevSemaSubBlock(c);
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

fn comptimeStmt(c: *Chunk, nodeId: cy.NodeId) !void {
    const node = c.nodes[nodeId];
    const expr = c.nodes[node.head.comptimeStmt.expr];
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
            try c.buf.pushDebugLabel(label);
        } else if (std.mem.eql(u8, "dumpLocals", name)) {
            const sblock = sema.curBlock(c);
            try c.dumpLocals(sblock);
        } else if (std.mem.eql(u8, "dumpBytecode", name)) {
            try cy.debug.dumpBytecode(c.compiler.vm, null);
        } else {
            return c.reportErrorAt("Unsupported annotation: {}", &.{v(name)}, nodeId);
        }
    } else {
        return c.reportErrorAt("Unsupported expr: {}", &.{v(expr.node_t)}, nodeId);
    }
}

fn forIterStmt(c: *Chunk, nodeId: cy.NodeId) !void {
    nextSemaSubBlock(c);
    defer prevSemaSubBlock(c);

    const node = c.nodes[nodeId];
    const iterNodeId = node.head.for_iter_stmt.iterable;
    const eachNodeId = node.head.for_iter_stmt.eachClause;

    const hasEach = node.head.for_iter_stmt.eachClause != cy.NullId;

    var valReg: RegisterId = undefined;
    var seqRegs: [10]RegisterId = undefined;
    var seqLen: u32 = 0;
    var seqIter = false;

    if (hasEach) {
        const eachClause = c.nodes[eachNodeId];
        if (eachClause.node_t == .ident) {
            valReg = try reserveLocalReg(c, eachClause.head.ident.semaVarId, false);
        } else if (eachClause.node_t == .seqDestructure) {
            var curId = eachClause.head.seqDestructure.head;
            var nextLocalReg = c.curBlock.nextLocalReg;
            while (curId != cy.NullId) {
                const ident = c.nodes[curId];
                try reserveLocalRegAt(c, ident.head.ident.semaVarId, nextLocalReg);
                seqRegs[seqLen] = nextLocalReg;
                nextLocalReg += 1;
                seqLen += 1;
                if (seqLen == 10) {
                    return c.reportErrorAt("Too many destructure identifiers: 10", &.{}, node.head.for_iter_stmt.eachClause);
                }
                curId = ident.next;
            }
            seqIter = true;
        } else {
            cy.unexpected();
        }
    }

    // Reserve temp local for iterator.
    const iterLocal = try c.rega.consumeNextTemp();

    const funcSigId = try sema.ensureFuncSig(c.compiler, &.{ bt.Any }, bt.Any);

    const iterv = try expression(c, iterNodeId, RegisterCstr.exactMustRetain(iterLocal + 4));
    if (seqIter) {
        try pushCallObjSym(c, iterLocal, 1, true,
            @intCast(c.compiler.vm.seqIteratorMGID), @intCast(funcSigId),
            iterNodeId);
    } else {
        const extraIdx = try c.fmtExtraDesc("iterator()", .{});
        try pushCallObjSymExt(c, iterLocal, 1, true,
            @intCast(c.compiler.vm.iteratorMGID), @intCast(funcSigId),
            iterNodeId, extraIdx);
    }
    if (iterv.retained) {
        try pushRelease(c, iterv.local, iterNodeId);
    }

    var skipFromTop: u32 = undefined;

    var extraIdx = try c.fmtExtraDesc("push iterator arg", .{});
    try c.buf.pushOp2Ext(.copyRetainSrc, iterLocal, iterLocal + 5, c.descExtra(iterNodeId, extraIdx));

    if (seqIter) {
        try pushCallObjSym(c, iterLocal + 1, 1, true,
            @intCast(c.compiler.vm.nextSeqMGID), @intCast(funcSigId),
            iterNodeId);
        try pushRelease(c, iterLocal + 5, iterNodeId);
        if (hasEach) {
            skipFromTop = try c.pushEmptyJumpNone(iterLocal + 1);

            try c.pushFailableDebugSym(nodeId);
            try c.buf.pushOp2(.seqDestructure, iterLocal + 1, @intCast(seqLen));
            try c.buf.pushOperands(seqRegs[0..seqLen]);
            c.curBlock.nextLocalReg += @intCast(seqLen);

            try pushRelease(c, iterLocal + 1, eachNodeId);
        } else {
            skipFromTop = try c.pushEmptyJumpNone(iterLocal + 1);
        }
    } else {
        extraIdx = try c.fmtExtraDesc("next()", .{});
        try pushCallObjSymExt(c, iterLocal + 1, 1, true,
            @intCast(c.compiler.vm.nextMGID), @intCast(funcSigId),
            iterNodeId, extraIdx);

        extraIdx = try c.fmtExtraDesc("release iter", .{});
        try pushReleaseExt(c, iterLocal + 5, iterNodeId, extraIdx);

        if (hasEach) {
            extraIdx = try c.fmtExtraDesc("copy next() to local", .{});
            try c.pushOptionalDebugSym(nodeId);
            try c.buf.pushOp2Ext(.copy, iterLocal + 1, valReg, c.descExtra(eachNodeId, extraIdx));
            c.curBlock.nextLocalReg += 1;
        }
        skipFromTop = try c.pushEmptyJumpNone(iterLocal + 1);
    }

    const bodyPc = c.buf.ops.items.len;
    const jumpStackSave: u32 = @intCast(c.subBlockJumpStack.items.len);
    defer c.subBlockJumpStack.items.len = jumpStackSave;
    try genStatements(c, node.head.for_iter_stmt.body_head, false);
    try genSubBlockReleaseLocals(c);

    const contPc = c.buf.ops.items.len;

    var skipFromInner: u32 = undefined;

    extraIdx = try c.fmtExtraDesc("push iterator arg", .{});
    try c.buf.pushOp2Ext(.copyRetainSrc, iterLocal, iterLocal + 5, c.descExtra(iterNodeId, extraIdx));

    if (seqIter) {
        try pushCallObjSym(c, iterLocal + 1, 1, true,
            @intCast(c.compiler.vm.nextSeqMGID), @intCast(funcSigId),
            iterNodeId);
        try pushRelease(c, iterLocal + 5, iterNodeId);
        if (hasEach) {
            // Must check for `none` before destructuring.
            skipFromInner = try c.pushEmptyJumpNone(iterLocal + 1);

            try c.pushFailableDebugSym(nodeId);
            try c.buf.pushOp2(.seqDestructure, iterLocal + 1, @intCast(seqLen));
            try c.buf.pushOperands(seqRegs[0..seqLen]);
        }
        try pushRelease(c, iterLocal + 1, eachNodeId);
    } else {
        extraIdx = try c.fmtExtraDesc("next()", .{});
        try pushCallObjSymExt(c, iterLocal + 1, 1, true,
            @intCast(c.compiler.vm.nextMGID), @intCast(funcSigId),
            iterNodeId, extraIdx);

        extraIdx = try c.fmtExtraDesc("release iter", .{});
        try pushReleaseExt(c, iterLocal + 5, iterNodeId, extraIdx);

        if (hasEach) {
            extraIdx = try c.fmtExtraDesc("copy next() to local", .{});
            try c.pushOptionalDebugSym(eachNodeId);
            try c.buf.pushOp2Ext(.copy, iterLocal + 1, valReg, c.descExtra(eachNodeId, extraIdx));
        }
    }

    if (seqIter and hasEach) {
        // Already performed none check.
        try c.pushJumpBackTo(bodyPc);
        c.patchJumpNoneToCurPc(skipFromInner);
    } else {
        try c.pushJumpBackNotNone(bodyPc, iterLocal + 1);
    }
    c.patchJumpNoneToCurPc(skipFromTop);

    c.patchForBlockJumps(jumpStackSave, c.buf.ops.items.len, contPc);

    // TODO: Iter local should be a reserved hidden local (instead of temp) so it can be cleaned up by endLocals when aborting the current fiber.
    try c.pushOptionalDebugSym(nodeId);
    try c.buf.pushOp1(.release, iterLocal);
}

fn forRangeStmt(c: *Chunk, nodeId: cy.NodeId) !void {
    nextSemaSubBlock(c);
    defer prevSemaSubBlock(c);

    const node = c.nodes[nodeId];

    var local: u8 = cy.NullU8;
    if (node.head.for_range_stmt.eachClause != cy.NullId) {
        const ident = c.nodes[node.head.for_range_stmt.eachClause];
        local = try reserveLocalReg(c, ident.head.ident.semaVarId, true);

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
    _ = try constInt(c, 1, rangeStep);

    const initPc = c.buf.ops.items.len;
    try c.pushFailableDebugSym(nodeId);
    try c.buf.pushOpSlice(.forRangeInit, &.{ rangeStart.local, rangeEnd, rangeStep, counter, local, 0, 0 });

    const bodyPc = c.buf.ops.items.len;
    const jumpStackSave: u32 = @intCast(c.subBlockJumpStack.items.len);
    defer c.subBlockJumpStack.items.len = jumpStackSave;
    try genStatements(c, node.head.for_range_stmt.body_head, false);
    try genSubBlockReleaseLocals(c);

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
        nextSemaSubBlock(self);
        defer prevSemaSubBlock(self);

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
        try genSubBlockReleaseLocals(self);

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

fn constInt(self: *Chunk, val: u64, dst: LocalId) !GenValue {
    // TODO: Can be constU8.
    if (val <= std.math.maxInt(i8)) {
        try self.buf.pushOp2(.constI8, @bitCast(@as(i8, @intCast(val))), dst);
        return self.initGenValue(dst, bt.Integer, false);
    }
    const idx = try self.buf.pushConst(cy.Const.init(cy.Value.initInt(@intCast(val)).val));
    try constOp(self, idx, dst);
    return self.initGenValue(dst, bt.Integer, false);
}

fn constFloat(self: *Chunk, val: f64, dst: LocalId) !GenValue {
    const idx = try self.buf.pushConst(cy.Const.init(@bitCast(val)));
    try constOp(self, idx, dst);
    return self.initGenValue(dst, bt.Float, false);
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

fn callExpr(c: *Chunk, nodeId: cy.NodeId, req: RegisterCstr, comptime startFiber: bool, coinitNodeId: if (startFiber) cy.NodeId else void) !GenValue {
    const val = try callExpr2(c, nodeId, req, startFiber, coinitNodeId);
    if (!startFiber) {
        if (req.type == .exact) {
            if (req.reg != val.local) {
                try c.buf.pushOp2(.copy, val.local, req.reg);
            }
        }
    }
    return c.initGenValue(val.local, val.vtype, val.retained);
}

/// Returns callStartLocal and advances the temp local.
/// Assumes the call does not begin a coroutine.
fn beginCall(self: *Chunk, dstCstr: RegisterCstr) !u8 {
    var callStartLocal = self.rega.getNextTemp();
    if (self.blocks.items.len == 1) {
        // Main block.
        // Ensure call start register is at least 1 so the runtime can easily check
        // if framePtr is at main or a function.
        if (callStartLocal == 0) {
            _ = try self.rega.consumeNextTemp();
            callStartLocal = 1;
        }
    }
    var reserveReturnLocal = true;
    if (callStartLocal > 1) {
        if (dstCstr.type == .exact and dstCstr.reg + 1 == callStartLocal) {
            // Optimization: Shifts start local to the left if the dst is only one register away.
            callStartLocal -= 1;
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
    return callStartLocal;
}

fn endCall(self: *Chunk, callLocalStart: u8) void {
    self.rega.setNextTemp(callLocalStart + 1);
}

fn callExpr2(self: *Chunk, nodeId: cy.NodeId, req: RegisterCstr, comptime startFiber: bool, coinitNodeId: if (startFiber) cy.NodeId else void) !GenValue {
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
            endCall(self, callStartLocal);
        }
    }

    if (startFiber) {
        fiberStartLocal = tempStart;
        callStartLocal = 1;
    } else {
        callStartLocal = try beginCall(self, req);
    }

    const node = self.nodes[nodeId];
    const callee = self.nodes[node.head.callExpr.callee];
    if (!node.head.callExpr.has_named_arg) {
        if (callee.node_t == .accessExpr) {
            if (callee.head.accessExpr.sema_csymId.isPresent()) {
                const csymId = callee.head.accessExpr.sema_csymId;
                if (csymId.isFuncSymId) {
                    const funcSym = self.compiler.sema.getFuncSym(csymId.id);
                    const rsym = self.compiler.sema.getSymbol(funcSym.getSymbolId());

                    const funcSigId = funcSym.getFuncSigId();

                    const start = self.operandStack.items.len;
                    defer self.operandStack.items.len = start;

                    // Func sym.
                    var callArgsRes: CallArgsResult = undefined;
                    if (funcSym.declId != cy.NullId) {
                        const rFuncSig = self.compiler.sema.resolvedFuncSigs.items[funcSigId];
                        callArgsRes = try callArgs2(self, rFuncSig, node.head.callExpr.arg_head);
                    } else {
                        callArgsRes = try callArgs(self, node.head.callExpr.arg_head);
                    }

                    if (callArgsRes.hasDynamicArgs) {
                        try genCallTypeCheck(self, callStartLocal + 4, node.head.callExpr.numArgs, funcSigId, nodeId);
                    }

                    const key = rsym.key.resolvedSymKey;
                    const symId = try self.compiler.vm.ensureFuncSym(key.parentSymId, key.nameId, funcSigId);

                    const retainedTemps = self.operandStack.items[start..];
                    defer self.popUnwindTempIndexN(@intCast(retainedTemps.len));

                    try pushCallSym(self, callStartLocal, node.head.callExpr.numArgs, 1, symId, nodeId);
                    try pushReleases(self, retainedTemps, nodeId);

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
                const csymId = callee.head.ident.sema_csymId;
                cy.dassert(csymId.isPresent());
                var rFuncSym: ?sema.FuncSym = null;

                const start = self.operandStack.items.len;
                defer self.operandStack.items.len = start;

                if (csymId.isFuncSymId) {
                    const funcSym = self.compiler.sema.getFuncSym(csymId.id);
                    const funcSigId = funcSym.getFuncSigId();
                    rFuncSym = funcSym;

                    var callArgsRes: CallArgsResult = undefined;
                    if (funcSym.declId != cy.NullId) {
                        const rFuncSig = self.compiler.sema.resolvedFuncSigs.items[funcSigId];
                        callArgsRes = try callArgs2(self, rFuncSig, node.head.callExpr.arg_head);
                    } else {
                        callArgsRes = try callArgs(self, node.head.callExpr.arg_head);
                    }
            
                    if (callArgsRes.hasDynamicArgs) {
                        try genCallTypeCheck(self, callStartLocal + 4, node.head.callExpr.numArgs, funcSigId, nodeId);
                    }
                } else {
                    const rsym = self.compiler.sema.getSymbol(csymId.id);
                    if (rsym.symT == .variable) {
                        return genFuncValueCallExpr(self, nodeId, fiberDst,
                            if (startFiber) fiberStartLocal else callStartLocal, startFiber);
                    } else {
                        return self.reportErrorAt("Unsupported callee", &.{}, nodeId);
                    }
                }

                const coinitPc = self.buf.ops.items.len;
                var frameLocSave: cy.NodeId = undefined;
                if (startFiber) {
                    // Precompute first arg local since coinit doesn't need the startLocal.
                    // numArgs + 4 (ret slots) + 1 (min call start local for main block)
                    var initialStackSize = node.head.callExpr.numArgs + 4 + 1;
                    if (initialStackSize < 16) {
                        initialStackSize = 16;
                    }
                    try self.pushOptionalDebugSym(nodeId);
                    try self.buf.pushOpSlice(.coinit, &[_]u8{ fiberStartLocal, node.head.callExpr.numArgs, 0, @intCast(initialStackSize), fiberDst });

                    // TODO: Have an actual block for fiber.
                    frameLocSave = self.curBlock.frameLoc;
                    self.curBlock.frameLoc = coinitNodeId;
                }

                if (csymId.isFuncSymId) {
                    const rtSymId = try self.genEnsureRtFuncSym(csymId.id);
                    const retainedTemps = self.operandStack.items[start..];
                    defer self.popUnwindTempIndexN(@intCast(retainedTemps.len));

                    if (startFiber) {
                        // Separate stack for fiber block.
                        try self.unwindTempIndexStack.append(self.alloc, @bitCast(@as(u32, cy.NullId)));
                        for (retainedTemps) |*operand| {
                            operand.* = operand.* - fiberStartLocal + callStartLocal + 4;
                            try self.pushUnwindIndex(operand.*);
                        }
                    }
                    defer if (startFiber) self.popUnwindTempIndexN(@intCast(retainedTemps.len + 1));

                    try pushCallSym(self, callStartLocal, node.head.callExpr.numArgs, 1, rtSymId, nodeId);
                    try pushReleases(self, retainedTemps, nodeId);
                } else {
                    return self.reportError("Unsupported coinit func call.", &.{});
                }

                if (startFiber) {
                    try self.buf.pushOp(.coreturn);
                    self.buf.setOpArgs1(coinitPc + 3, @intCast(self.buf.ops.items.len - coinitPc));

                    self.curBlock.frameLoc = frameLocSave;
                    return GenValue.initTempValue(fiberDst, bt.Fiber, true);
                } else {
                    if (csymId.isPresent()) {
                        if (rFuncSym) |funcSym| {
                            const retained = types.isRcCandidateType(self.compiler, funcSym.retType);
                            return GenValue.initTempValue(callStartLocal, funcSym.retType, retained);
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

fn callObjSymUnaryExpr(
    self: *Chunk, mgId: vmc.MethodGroupId, childId: cy.NodeId, cstr: RegisterCstr, debugNodeId: cy.NodeId,
) !GenValue {
    const callStartLocal = try beginCall(self, cstr);
    defer endCall(self, callStartLocal);

    // Gen self arg.
    const selfv = try expression(self, childId, RegisterCstr.temp);

    const funcSigId = try sema.ensureFuncSig(self.compiler, &.{ bt.Any }, bt.Any);
    try pushCallObjSym(self, callStartLocal, 1, true, @intCast(mgId), @intCast(funcSigId), debugNodeId);
    if (selfv.retained) {
        try pushRelease(self, selfv.local, debugNodeId);
    }

    if (cstr.type == .exact) {
        if (cstr.reg != callStartLocal) {
            try self.buf.pushOp2(.copy, callStartLocal, cstr.reg);
            return GenValue.initTempValue(cstr.reg, bt.Any, true);
        }
    }

    return GenValue.initTempValue(callStartLocal, bt.Any, true);
}

fn callObjSymRelease(self: *Chunk, callStartLocal: u8, mgId: vmc.MethodGroupId, funcSigId: sema.FuncSigId, numArgs: u8, 
    releaseIds: []const u8, dst: RegisterCstr, debugNodeId: cy.NodeId) !GenValue {

    try pushCallObjSym(self, callStartLocal, numArgs, true, @intCast(mgId), @intCast(funcSigId), debugNodeId);

    try pushReleases(self, releaseIds, debugNodeId);

    if (dst.type == .exact) {
        if (dst.reg != callStartLocal) {
            try self.buf.pushOp2(.copy, callStartLocal, dst.reg);
            return GenValue.initTempValue(dst.reg, bt.Any, true);
        }
    }

    return GenValue.initTempValue(callStartLocal, bt.Any, true);
}

fn callObjSymArgs(
    self: *Chunk, selfId: cy.NodeId, mgId: vmc.MethodGroupId, argIds: []const cy.NodeId, dst: RegisterCstr,
    debugNodeId: cy.NodeId,
) !GenValue {
    const callStartLocal = try beginCall(self, dst);
    defer endCall(self, callStartLocal);

    const typeStart = self.compiler.typeStack.items.len;
    defer self.compiler.typeStack.items.len = typeStart;

    const start = self.operandStack.items.len;
    defer self.operandStack.items.len = start;

    // Gen self arg.
    const selfv = try expression(self, selfId, RegisterCstr.tempMustRetain);
    if (selfv.retained) {
        try self.pushTempOperand(selfv.local);
    }
    try self.compiler.typeStack.append(self.alloc, bt.Any);

    // Gen arg.
    for (argIds) |argId| {
        const argv = try expression(self, argId, RegisterCstr.temp);
        if (argv.retained) {
            try self.pushTempOperand(argv.local);
        }
        try self.compiler.typeStack.append(self.alloc, argv.vtype);
    }

    const funcSigId = try sema.ensureFuncSig(self.compiler, self.compiler.typeStack.items[typeStart..], bt.Any);
    try pushCallObjSym(self, callStartLocal, 2, 1, @intCast(mgId), @intCast(funcSigId), debugNodeId);


    try pushReleases(self, self.operandStack.items[start..], debugNodeId);

    if (dst.type == .exact) {
        if (dst.reg != callStartLocal) {
            try self.buf.pushOp2(.copy, callStartLocal, dst.reg);
            return GenValue.initTempValue(dst.reg, bt.Any, true);
        }
    }

    return GenValue.initTempValue(callStartLocal, bt.Any, true);
}

fn callObjSymTernNoRet(self: *Chunk, mgId: vmc.MethodGroupId, a: cy.NodeId, b: cy.NodeId, c: cy.NodeId, debugNodeId: cy.NodeId) !GenValue {
    const callStartLocal = try beginCall(self, RegisterCstr.none);
    defer endCall(self, callStartLocal);

    const regStart = self.regStack.items.len;
    defer self.regStack.items.len = regStart;

    const av = try expression(self, a, RegisterCstr.temp);
    const aRetained = try self.pushUnwindIndexIfRetainedTemp(av);
    if (aRetained) {
        try self.pushReg(av.local);
    }

    const bv = try expression(self, b, RegisterCstr.temp);
    const bRetained = try self.pushUnwindIndexIfRetainedTemp(bv);
    if (bRetained) {
        try self.pushReg(bv.local);
    }

    const cv = try expression(self, c, RegisterCstr.temp);
    const cRetained = try self.pushUnwindIndexIfRetainedTemp(cv);
    if (cRetained) {
        try self.pushReg(cv.local);
    }

    const retainedTemps = self.regStack.items[regStart..];
    defer self.popUnwindTempIndexN(@intCast(retainedTemps.len));

    const bType = self.nodeTypes[b];
    const cType = self.nodeTypes[c];
    const funcSigId = try sema.ensureFuncSig(self.compiler, &.{ bt.Any, bType, cType }, bt.Any);
    try pushCallObjSym(self, callStartLocal, 3, false, @intCast(mgId), @intCast(funcSigId), debugNodeId);

    try pushReleases(self, retainedTemps, debugNodeId);

    return GenValue.initTempValue(callStartLocal, bt.None, false);
}

fn callObjSymBinExpr(self: *Chunk, mgId: vmc.MethodGroupId, opts: GenBinExprOptions) !GenValue {
    const callStartLocal = try beginCall(self, opts.cstr);
    defer endCall(self, callStartLocal);

    // Gen self arg.
    var selfv: GenValue = undefined;
    if (opts.leftv) |leftv| {
        const selfArg = try self.rega.consumeNextTemp();
        if (types.isRcCandidateType(self.compiler, leftv.vtype)) {
            try self.buf.pushOp2(.copyRetainSrc, leftv.local, selfArg);
            selfv = GenValue.initTempValue(selfArg, leftv.vtype, true);
        } else {
            try self.buf.pushOp2(.copy, leftv.local, selfArg);
            selfv = GenValue.initTempValue(selfArg, leftv.vtype, false);
        }
    } else {
        selfv = try expression(self, opts.leftId, RegisterCstr.temp);
    }
    const selfRetained = try self.pushUnwindIndexIfRetainedTemp(selfv);

    // Gen arg.
    var argv: GenValue = undefined;
    if (opts.rightv) |rightv| {
        const argReg = try self.rega.consumeNextTemp();
        if (types.isRcCandidateType(self.compiler, rightv.vtype)) {
            try self.buf.pushOp2(.copyRetainSrc, rightv.local, argReg);
            argv = GenValue.initTempValue(argReg, rightv.vtype, true);
        } else {
            try self.buf.pushOp2(.copy, rightv.local, argReg);
            argv = GenValue.initTempValue(argReg, rightv.vtype, false);
        }
    } else {
        argv = try expression(self, opts.rightId, RegisterCstr.temp);
    }
    const argRetained = try self.pushUnwindIndexIfRetainedTemp(argv);

    defer {
        if (selfRetained) {
            self.popUnwindTempIndex();
        }
        if (argRetained) {
            self.popUnwindTempIndex();
        }
    }

    const argT = self.nodeTypes[opts.rightId];
    const funcSigId = try sema.ensureFuncSig(self.compiler, &.{ bt.Any, argT }, bt.Any);
    try pushCallObjSym(self, callStartLocal, 2, true, @intCast(mgId), @intCast(funcSigId), opts.debugNodeId);

    try pushReleaseOpt2(self, selfv.retained, selfv.local, argv.retained, argv.local, opts.debugNodeId);

    if (opts.cstr.type == .exact) {
        if (opts.cstr.reg != callStartLocal) {
            try self.buf.pushOp2(.copy, callStartLocal, opts.cstr.reg);
            return GenValue.initTempValue(opts.cstr.reg, bt.Any, true);
        }
    }

    return GenValue.initTempValue(callStartLocal, bt.Any, true);
}

fn callObjSym(self: *Chunk, callStartLocal: u8, callExprId: cy.NodeId) !GenValue {
    const node = self.nodes[callExprId];
    const callee = self.nodes[node.head.callExpr.callee];
    const firstArgId = node.head.callExpr.arg_head;
    const ident = self.nodes[callee.head.accessExpr.right];

    const start = self.operandStack.items.len;
    defer self.operandStack.items.len = start;

    // Gen self arg.
    const selfv = try expression(self, callee.head.accessExpr.left, RegisterCstr.temp);
    const selfRetained = try self.pushUnwindIndexIfRetainedTemp(selfv);
    if (selfRetained) {
        try self.operandStack.append(self.alloc, selfv.local);
    }

    // Gen args in parens.
    _ = try callArgs(self, firstArgId);

    const retainedTemps = self.operandStack.items[start..];
    defer self.popUnwindTempIndexN(@intCast(retainedTemps.len));

    // One more arg for receiver.
    const numArgs = 1 + node.head.callExpr.numArgs;
        
    const name = self.getNodeTokenString(ident);

    const methodSymId = try self.compiler.vm.ensureMethodGroup(name);

    const funcSigId: u16 = @intCast(ident.head.ident.semaMethodSigId);
    try pushCallObjSym(self, callStartLocal, @intCast(numArgs), true,
        @intCast(methodSymId), funcSigId, callExprId);
    try pushReleases(self, retainedTemps, callExprId);
    return GenValue.initTempValue(callStartLocal, bt.Any, true);
}

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

fn pushReleases(self: *Chunk, regs: []const u8, debugNodeId: cy.NodeId) !void {
    if (regs.len > 1) {
        try self.pushOptionalDebugSym(debugNodeId);
        try self.buf.pushOp1(.releaseN, @intCast(regs.len));
        try self.buf.pushOperands(regs);
    } else if (regs.len == 1) {
        try pushRelease(self, regs[0], debugNodeId);
    }
}

fn pushReleaseExt(c: *Chunk, local: u8, nodeId: cy.NodeId, extraIdx: u32) !void {
    try c.pushOptionalDebugSym(nodeId);
    try c.buf.pushOp1Ext(.release, local, c.descExtra(nodeId, extraIdx));
}

fn pushRelease(c: *Chunk, local: u8, nodeId: cy.NodeId) !void {
    try c.pushOptionalDebugSym(nodeId);
    try c.buf.pushOp1Ext(.release, local, c.desc(nodeId));
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

    const start = self.operandStack.items.len;
    defer self.operandStack.items.len = start;

    var numArgs: u32 = 0;
    var argId = node.head.callExpr.arg_head;
    while (argId != cy.NullId) : (numArgs += 1) {
        const arg = self.nodes[argId];
        const argv = try expression(self, argId, RegisterCstr.temp);
        const argRetained = try self.pushUnwindIndexIfRetainedTemp(argv);
        if (argRetained) {
            try self.operandStack.append(self.alloc, argv.local);
        }
        argId = arg.next;
    }

    // TODO: Doesn't have to retain to a temp if it's a local (would just need retain op).
    // If copied to temp, it should be after the last param so it can persist until the function returns.
    const calleev = try expression(self, node.head.callExpr.callee, RegisterCstr.tempMustRetain);
    const calleeRetained = try self.pushUnwindIndexIfRetainedTemp(calleev);
    if (calleeRetained) {
        if (startFiber) {
            try self.operandStack.append(self.alloc, @intCast(5 + numArgs));
        } else {
            try self.operandStack.append(self.alloc, calleev.local);
        }
    }

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

    const retainedTemps = self.operandStack.items[start..];
    defer self.popUnwindTempIndexN(@intCast(retainedTemps.len));

    try self.pushFailableDebugSym(nodeId);
    try self.buf.pushOp3(.call, callStartLocal, @intCast(numArgs), 1);

    // ARC cleanup.
    try pushReleases(self, retainedTemps, nodeId);

    if (startFiber) {
        try self.buf.pushOp(.coreturn);
        self.buf.setOpArgs1(coinitPc + 3, @intCast(self.buf.ops.items.len - coinitPc));
        return GenValue.initTempValue(fiberDst, bt.Fiber, true);
    } else {
        return GenValue.initTempValue(callStartLocal, bt.Any, true);
    }
}

fn funcDecl(self: *Chunk, parentSymId: sema.SymbolId, nodeId: cy.NodeId) !void {
    const node = self.nodes[nodeId];
    const func = &self.semaFuncDecls.items[node.head.func.semaDeclId];
    const name = func.getName(self);
    const nameId = try sema.ensureNameSym(self.compiler, name);
    const symId = try self.compiler.vm.ensureFuncSym(parentSymId, nameId, func.funcSigId);

    const jumpPc = try self.pushEmptyJump();

    try pushSemaBlock(self, func.semaBlockId);
    if (self.compiler.config.genDebugFuncMarkers) {
        try self.compiler.buf.pushDebugFuncStart(node.head.func.semaDeclId, self.id);
    }
    self.curBlock.frameLoc = nodeId;

    try self.unwindTempIndexStack.append(self.alloc, @bitCast(@as(u32, cy.NullId)));
    defer self.popUnwindTempIndex();

    const jumpStackStart = self.blockJumpStack.items.len;

    const opStart: u32 = @intCast(self.buf.ops.items.len);
    try reserveFuncRegs(self, func.numParams);
    try genStatements(self, node.head.func.bodyHead, false);
    // TODO: Check last statement to skip adding ret.
    try genBlockEnd(self);

    // Reserve another local for the call return info.
    const sblock = sema.curBlock(self);
    const numCaptured: u8 = @intCast(sblock.captures.items.len);
    std.debug.assert(numCaptured == 0);

    self.patchJumpToCurPc(jumpPc);

    self.patchBlockJumps(jumpStackStart);
    self.blockJumpStack.items.len = jumpStackStart;

    const stackSize = self.getMaxUsedRegisters();
    popSemaBlock(self);

    if (self.compiler.config.genDebugFuncMarkers) {
        try self.compiler.buf.pushDebugFuncEnd(node.head.func.semaDeclId, self.id);
    }
    
    const rtSym = rt.FuncSymbol.initFunc(opStart, @intCast(stackSize), func.numParams, func.funcSigId);
    self.compiler.vm.setFuncSym(symId, rtSym);
}

const CallArgsResult = struct {
    hasDynamicArgs: bool,
};

fn callArgs2(c: *Chunk, rFuncSig: sema.FuncSig, first: cy.NodeId) !CallArgsResult {
    const params = rFuncSig.params();
    var i: u32 = 0;
    var argId = first;
    var hasDynamicArgs = false;
    while (argId != cy.NullId) : (i += 1) {
        const arg = c.nodes[argId];
        const cstrType = params[i];
        _ = cstrType;
        const argv = try expression(c, argId, RegisterCstr.tempMustRetain);
        const retainedTemp = try c.pushUnwindIndexIfRetainedTemp(argv);
        if (retainedTemp) {
            try c.operandStack.append(c.alloc, argv.local);
        }
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
        const retainedTemp = try c.pushUnwindIndexIfRetainedTemp(argv);
        if (retainedTemp) {
            try c.operandStack.append(c.alloc, argv.local);
        }
        hasDynamicArgs = hasDynamicArgs or argv.vtype == bt.Dynamic;
        argId = arg.next;
    }
    return CallArgsResult{
        .hasDynamicArgs = hasDynamicArgs,
    };
}

fn genStaticInitializerDeps(c: *Chunk, csymId: sema.CompactSymbolId) !void {
    if (c.semaInitializerSyms.get(csymId)) |ref| {
        // Contains dependencies. Generate initializers for them first.
        const deps = c.bufU32.items[ref.depsStart..ref.depsEnd];
        for (deps) |dep| {
            const crDepSym: sema.CompactSymbolId = @bitCast(dep);
            // Only if it has a static initializer and hasn't been visited.
            if (sema.symHasStaticInitializer(c, crDepSym)) {
                try genStaticInitializerDFS(c, crDepSym);
            }
        }
    }
}

/// Generates var decl initializer.
/// If the declaration contains dependencies those are generated first in DFS order.
pub fn genStaticInitializerDFS(self: *Chunk, csymId: sema.CompactSymbolId) anyerror!void {
    if (csymId.isFuncSymId) {
        // Check that the resolved sym hasn't already been visited for generation.
        const rFuncSym = self.compiler.sema.getFuncSymPtr(csymId.id);
        if (rFuncSym.genStaticInitVisited) {
            return;
        }
        rFuncSym.genStaticInitVisited = true;

        const funcSigId = rFuncSym.getFuncSigId();
        const chunk = &self.compiler.chunks.items[rFuncSym.chunkId];
        const func = chunk.semaFuncDecls.items[rFuncSym.declId];
        const name = func.getName(chunk);
        const nameId = try sema.ensureNameSym(chunk.compiler, name);
        const node = chunk.nodes[func.nodeId];
        // Generate deps first.
        try genStaticInitializerDeps(chunk, csymId);

        log.debug("gen static func init: {s}", .{name});

        // Clear register state.
        chunk.rega.resetNextTemp();

        try self.unwindTempIndexStack.append(self.alloc, @bitCast(@as(u32, cy.NullId)));
        defer self.popUnwindTempIndex();

        const exprv = try expression(chunk, node.head.func.bodyHead, RegisterCstr.tempMustRetain);
        _ = try chunk.pushUnwindIndexIfRetainedTemp(exprv);
        defer chunk.popUnwindTempIndex();

        const rSym = self.compiler.sema.getSymbol(rFuncSym.getSymbolId());
        const rtSymId = try self.compiler.vm.ensureFuncSym(rSym.key.resolvedSymKey.parentSymId, nameId, funcSigId);
        try chunk.pushFailableDebugSym(node.head.func.bodyHead);
        const pc = self.buf.len();
        try self.buf.pushOp3(.setStaticFunc, 0, 0, exprv.local);
        self.buf.setOpArgU16(pc + 1, @intCast(rtSymId));
    } else {
        const rSym = self.compiler.sema.getSymbolPtr(csymId.id);
        if (rSym.genStaticInitVisited) {
            return;
        }
        rSym.genStaticInitVisited = true;

        if (rSym.symT == .variable) {
            const declId = rSym.inner.variable.declId;
            const chunk = &self.compiler.chunks.items[rSym.inner.variable.chunkId];
            const decl = chunk.nodes[declId];

            // Only generate for user vars.
            if (decl.node_t == .staticDecl) {
                // Generate deps first.
                try genStaticInitializerDeps(chunk, csymId);

                log.debug("gen static var init: {s}", .{sema.getName(self.compiler, rSym.key.resolvedSymKey.nameId)});

                // Clear register state.
                chunk.rega.resetNextTemp();

                try self.unwindTempIndexStack.append(self.alloc, @bitCast(@as(u32, cy.NullId)));
                defer self.popUnwindTempIndex();

                const exprv = try expression(chunk, decl.head.staticDecl.right, RegisterCstr.tempMustRetain);

                const rtSymId = try self.compiler.vm.ensureVarSym(rSym.key.resolvedSymKey.parentSymId, rSym.key.resolvedSymKey.nameId);

                const pc = self.buf.len();
                try self.buf.pushOp3(.setStaticVar, 0, 0, exprv.local);
                self.buf.setOpArgU16(pc + 1, @intCast(rtSymId));
            }
        } else {
            cy.panicFmt("Unsupported sym {}", .{rSym.symT});
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
            const dst = try c.rega.selectFromNonLocalVar(cstr, false);
            if (c.nodeTypes[nodeId] == bt.Integer) {
                const val = try std.fmt.parseInt(u64, literal, 10);
                return constInt(c, val, dst);
            } else {
                const val = try std.fmt.parseFloat(f64, literal);
                return constFloat(c, val, dst);
            }
        },
        .float => {
            const literal = c.getNodeTokenString(node);
            const val = try std.fmt.parseFloat(f64, literal);

            const dst = try c.rega.selectFromNonLocalVar(cstr, false);
            return constFloat(c, val, dst);
        },
        .nonDecInt => {
            const dst = try c.rega.selectFromNonLocalVar(cstr, false);
            return try constInt(c, node.head.nonDecInt.semaVal, dst);
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
                .not => {
                    const dst = try c.rega.selectFromNonLocalVar(cstr, false);
                    const child = try expression(c, node.head.unary.child, RegisterCstr.exact(dst));
                    try c.buf.pushOp1(.not, dst);
                    try releaseIfRetainedTemp(c, child);
                    return c.initGenValue(dst, bt.Boolean, false);
                },
                .minus,
                .bitwiseNot => {
                    const childT = c.nodeTypes[node.head.unary.child];
                    switch (node.head.unary.semaGenStrat) {
                        .none => cy.fatal(),
                        .specialized => {
                            const dst = try c.rega.selectFromNonLocalVar(cstr, false);

                            const tempStart = c.rega.getNextTemp();
                            defer c.rega.setNextTemp(tempStart);

                            const canUseDst = !c.isParamOrLocalVar(dst);
                            const childv = try expression(c, node.head.unary.child, RegisterCstr.preferIf(dst, canUseDst));

                            if (childT == bt.Integer) {
                                try pushInlineUnaryExpr(c, getIntUnaryOpCode(op), childv.local, dst, nodeId);
                            } else if (childT == bt.Float) {
                                try pushInlineUnaryExpr(c, getFloatUnaryOpCode(op), childv.local, dst, nodeId);
                            } else {
                                cy.fatal();
                            }

                            // ARC cleanup.
                            try releaseIfRetainedTemp(c, childv);

                            return c.initGenValue(dst, childT, false);
                        },
                        .generic => {
                            if (op == .minus) {
                                return callObjSymUnaryExpr(c, c.compiler.vm.@"prefix-MGID", node.head.unary.child, cstr, nodeId);
                            } else {
                                return callObjSymUnaryExpr(c, c.compiler.vm.@"prefix~MGID", node.head.unary.child, cstr, nodeId);
                            }
                        }
                    }
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
            return callExpr(c, nodeId, cstr, false, {});
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
            try c.buf.pushOp2(.coyield, c.curBlock.startLocalReg, c.curBlock.nextLocalReg);

            // TODO: return coyield expression.
            const dst = try c.rega.selectFromNonLocalVar(cstr, false);
            return genNone(c, dst);
        },
        .coinit => {
            return callExpr(c, node.head.child_head, cstr, true, nodeId);
        },
        .sliceExpr => {
            return sliceExpr(c, nodeId, cstr);
        },
        .indexExpr => {
            return indexExpr(c, nodeId, null, null, cstr);
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

            const tSym = c.compiler.sema.getSymbol(tSymId);
            if (tSym.symT == .object) {
                const typeId = tSym.getObjectTypeId(c.compiler.vm).?;
                try c.pushFailableDebugSym(nodeId);
                const pc = c.buf.ops.items.len;
                try c.buf.pushOpSlice(.cast, &.{ child.local, 0, 0 });
                c.buf.setOpArgU16(pc + 2, @intCast(typeId));
            } else if (tSym.symT == .builtinType) {
                if (types.toRtConcreteType(tSymId)) |typeId| {
                    try c.pushFailableDebugSym(nodeId);
                    const pc = c.buf.ops.items.len;
                    try c.buf.pushOpSlice(.cast, &.{ child.local, 0, 0 });
                    c.buf.setOpArgU16(pc + 2, @intCast(typeId));
                } else {
                    // Cast to abstract type.
                    try c.pushFailableDebugSym(nodeId);
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

            try c.pushFailableDebugSym(nodeId);
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
        .comptimeExpr => {
            const child = c.nodes[node.head.comptimeExpr.child];
            if (child.node_t == .ident) {
                const name = c.getNodeTokenString(child);
                if (std.mem.eql(u8, name, "ModUri")) {
                    const dst = try c.rega.selectFromNonLocalVar(cstr, false);
                    return string(c, c.srcUri, dst);
                } else {
                    cy.unexpected();
                }
            } else {
                cy.unexpected();
            }
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

fn binOpAssignToIndex(
    c: *Chunk, leftId: cy.NodeId, rightId: cy.NodeId, op: cy.BinaryExprOp,
    genStrat: cy.GenBinExprStrategy, debugNodeId: cy.NodeId,
) !void {
    const left = c.nodes[leftId];

    const selfT = c.nodeTypes[left.head.indexExpr.left];
    switch (left.head.indexExpr.semaGenStrat) {
        .none => cy.fatal(),
        .specialized => {
            const regStart = c.regStack.items.len;
            defer c.regStack.items.len = regStart;

            const selfv = try expression(c, left.head.indexExpr.left, RegisterCstr.simple);
            const selfRetained = try c.pushUnwindIndexIfRetainedTemp(selfv);
            if (selfRetained) {
                try c.pushReg(selfv.local);
            }

            const indexv = try expression(c, left.head.indexExpr.right, RegisterCstr.simple);
            const indexRetained = try c.pushUnwindIndexIfRetainedTemp(indexv);
            if (indexRetained) {
                try c.pushReg(indexv.local);
            }

            const leftLocal = try c.rega.consumeNextTemp();

            const leftv = try indexExpr(c, leftId, selfv, indexv, RegisterCstr.exact(leftLocal));
            const leftRetained = try c.pushUnwindIndexIfRetainedTemp(leftv);
            if (leftRetained) {
                try c.pushReg(leftv.local);
            }

            const resv = try binExpr2(c, .{
                .leftId = leftId,
                .rightId = rightId,
                .debugNodeId = debugNodeId,
                .op = op,
                .genStrat = genStrat,
                .leftv = c.initGenValue(leftLocal, bt.Any, false),
                .cstr = RegisterCstr.simple,
            });
            const resRetained = try c.pushUnwindIndexIfRetainedTemp(resv);
            if (resRetained) {
                try c.pushReg(resv.local);
            }

            const retainedTemps = c.regStack.items[regStart..];
            defer c.popUnwindTempIndexN(@intCast(retainedTemps.len));

            if (selfT == bt.List) {
                // Reuse binExpr layout since there is no dst.
                try pushInlineBinExpr(c, .setIndexList, selfv.local, indexv.local, resv.local, debugNodeId);
            } else if (selfT == bt.Map) {
                try pushInlineBinExpr(c, .setIndexMap, selfv.local, indexv.local, resv.local, debugNodeId);
            } else {
                cy.fatal();
            }

            // ARC cleanup.
            try pushReleases(c, retainedTemps, debugNodeId);
        },
        .generic => {
            const callStartLocal = try beginCall(c, RegisterCstr.none);
            defer endCall(c, callStartLocal);

            const regStart = c.regStack.items.len;
            defer c.regStack.items.len = regStart;

            const selfv = try expression(c, left.head.indexExpr.left, RegisterCstr.temp);
            const selfRetained = try c.pushUnwindIndexIfRetainedTemp(selfv);
            if (selfRetained) {
                try c.pushReg(selfv.local);
            }

            const indexv = try expression(c, left.head.indexExpr.right, RegisterCstr.temp);
            const indexRetained = try c.pushUnwindIndexIfRetainedTemp(indexv);
            if (indexRetained) {
                try c.pushReg(indexv.local);
            }

            const resLocal = try c.rega.consumeNextTemp();
            const leftLocal = try c.rega.consumeNextTemp();

            const leftv = try indexExpr(c, leftId, selfv, indexv, RegisterCstr.exact(leftLocal));
            const leftRetained = try c.pushUnwindIndexIfRetainedTemp(leftv);
            if (leftRetained) {
                try c.pushReg(leftv.local);
            }

            const resv = try binExpr2(c, .{
                .leftId = leftId,
                .rightId = rightId,
                .debugNodeId = debugNodeId,
                .op = op,
                .genStrat = genStrat,
                .leftv = c.initGenValue(leftLocal, bt.Any, false),
                .cstr = RegisterCstr.exact(resLocal),
            });
            const resRetained = try c.pushUnwindIndexIfRetainedTemp(resv);
            if (resRetained) {
                try c.pushReg(resv.local);
            }

            const retainedTemps = c.regStack.items[regStart..];
            defer c.popUnwindTempIndexN(@intCast(retainedTemps.len));

            const indexType = c.nodeTypes[left.head.indexExpr.right];
            const funcSigId = try sema.ensureFuncSig(c.compiler, &.{ bt.Any, indexType, resv.vtype }, bt.Any);
            try pushCallObjSym(c, callStartLocal, 3, false, @intCast(c.compiler.vm.setIndexMGID), @intCast(funcSigId), debugNodeId);

            // ARC cleanup.
            try pushReleases(c, retainedTemps, debugNodeId);
        },
    }
}

fn binOpAssignToField(
    self: *Chunk, leftId: cy.NodeId, rightId: cy.NodeId, op: cy.BinaryExprOp,
    genStrat: cy.GenBinExprStrategy, debugNodeId: cy.NodeId,
) !void {
    const left = self.nodes[leftId];

    const accessRight = self.nodes[left.head.accessExpr.right];
    if (accessRight.node_t != .ident) {
        return self.reportErrorAt("Expected ident. Got {}.", &.{v(accessRight.node_t)}, left.head.accessExpr.right);
    }

    const fieldName = self.getNodeTokenString(accessRight);
    const fieldId = try self.compiler.vm.ensureFieldSym(fieldName);

    const recv = try expression(self, left.head.accessExpr.left, RegisterCstr.simple);
    const leftLocal = try self.rega.consumeNextTemp();

    try pushFieldRetain(self, recv.local, leftLocal, @intCast(fieldId), leftId);

    const resv = try binExpr2(self, .{
        .leftId = leftId,
        .rightId = rightId,
        .debugNodeId = debugNodeId,
        .op = op,
        .genStrat = genStrat,
        .leftv = self.initGenValue(leftLocal, bt.Any, false),
        .cstr = RegisterCstr.simple,
    });

    try self.pushFailableDebugSym(debugNodeId);
    try self.buf.pushOp3(.setField, @intCast(fieldId), recv.local, resv.local);

    // ARC cleanup. Right is not released since it's being assigned to the index.
    try releaseIfRetainedTempAt(self, recv, left.head.accessExpr.left);
}

fn genMethodDecl(self: *Chunk, typeId: rt.TypeId, node: cy.Node, func: sema.FuncDecl, name: []const u8) !void {
    // log.debug("gen method {s}", .{name});
    const mgId = try self.compiler.vm.ensureMethodGroup(name);

    try self.unwindTempIndexStack.append(self.alloc, @bitCast(@as(u32, cy.NullId)));
    defer self.popUnwindTempIndex();

    const jumpPc = try self.pushEmptyJump();

    try pushSemaBlock(self, func.semaBlockId);
    if (self.compiler.config.genDebugFuncMarkers) {
        try self.compiler.buf.pushDebugFuncStart(node.head.func.semaDeclId, self.id);
    }

    const opStart: u32 = @intCast(self.buf.ops.items.len);
    try reserveFuncRegs(self, func.numParams);
    try genStatements(self, node.head.func.bodyHead, false);
    // TODO: Check last statement to skip adding ret.
    try genBlockEnd(self);

    const stackSize = self.getMaxUsedRegisters();
    popSemaBlock(self);
    if (self.compiler.config.genDebugFuncMarkers) {
        try self.compiler.buf.pushDebugFuncEnd(node.head.func.semaDeclId, self.id);
    }

    self.patchJumpToCurPc(jumpPc);

    const funcSig = self.compiler.sema.getFuncSig(func.funcSigId);
    if (funcSig.reqCallTypeCheck) {
        const m = rt.MethodInit.initTyped(func.funcSigId, opStart, stackSize, func.numParams);
        try self.compiler.vm.addMethod(typeId, mgId, m);
    } else {
        const m = rt.MethodInit.initUntyped(func.funcSigId, opStart, stackSize, func.numParams);
        try self.compiler.vm.addMethod(typeId, mgId, m);
    }
}

fn assignExprToLocalVar(c: *Chunk, leftId: cy.NodeId, exprId: cy.NodeId, comptime init: bool) !void {
    const varId = c.nodes[leftId].head.ident.semaVarId;
    const varT = c.nodeTypes[leftId];
    const expr = c.nodes[exprId];

    const svar = c.genGetVarPtr(varId).?;
    if (svar.isBoxed) {
        try assignExprToBoxedVar(c, svar, exprId);
        return;
    }

    if (expr.node_t == .ident) {
        const csymId = expr.head.ident.sema_csymId;
        if (csymId.isPresent()) {
            // Copying a symbol.
            if (init or !types.isRcCandidateType(c.compiler, svar.vtype)) {
                const exprv = try expression(c, exprId, RegisterCstr.exactMustRetain(svar.local));
                svar.vtype = exprv.vtype;
            } else {
                const exprv = try expression(c, exprId, RegisterCstr.tempMustRetain);
                svar.vtype = exprv.vtype;
                try c.pushOptionalDebugSym(leftId);
                try c.buf.pushOp2(.copyReleaseDst, exprv.local, svar.local);
            }
            return;
        }

        const exprv = try expression(c, exprId, RegisterCstr.simple);
        if (!init) {
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
            svar.vtype = exprv.vtype;
        }
        return;
    } else if (expr.node_t == .symbolLit) {
        if (types.isEnumType(c.compiler, svar.vtype)) {
            const name = c.getNodeTokenString(expr);
            const nameId = try sema.ensureNameSym(c.compiler, name);
            const sym = c.compiler.sema.getSymbol(svar.vtype);
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
    if (init or !types.isRcCandidateType(c.compiler, varT)) {
        const exprv = try expression(c, exprId, RegisterCstr.exactMustRetain(svar.local));
        svar.vtype = exprv.vtype;
    } else {
        const exprv = try expression(c, exprId, RegisterCstr.simpleMustRetain);
        cy.dassert(exprv.local != svar.local);
        try c.pushOptionalDebugSym(leftId);
        try c.buf.pushOp2(.copyReleaseDst, exprv.local, svar.local);
        svar.vtype = exprv.vtype;
    }
}

fn assignExprToBoxedVar(self: *Chunk, svar: *sema.LocalVar, exprId: cy.NodeId) !void {
    // Retain rval.
    const exprv = try expression(self, exprId, RegisterCstr.tempMustRetain);
    svar.vtype = exprv.vtype;
    if (!types.isRcCandidateType(self.compiler, svar.vtype)) {
        if (svar.isParentLocalAlias()) {
            const temp = try self.rega.consumeNextTemp();
            defer self.rega.setNextTemp(temp);
            try self.buf.pushOp3(.captured, self.curBlock.closureLocal, svar.capturedIdx, temp);
            try self.buf.pushOp2(.setBoxValue, temp, exprv.local);
        } else {
            try self.buf.pushOp2(.setBoxValue, svar.local, exprv.local);
        }
    } else {
        if (svar.isParentLocalAlias()) {
            const temp = try self.rega.consumeNextTemp();
            defer self.rega.setNextTemp(temp);
            try self.buf.pushOp3(.captured, self.curBlock.closureLocal, svar.capturedIdx, temp);
            try self.buf.pushOp2(.setBoxValueRelease, temp, exprv.local);
        } else {
            try self.buf.pushOp2(.setBoxValueRelease, svar.local, exprv.local);
        }
    }
}

fn reserveLocalRegAt(c: *Chunk, varId: sema.LocalVarId, reg: u8) !void {
    const svar = &c.vars.items[varId];
    svar.local = reg;

    try c.varDeclStack.append(c.alloc, .{
        // Name was only needed for sema.
        .namePtr = undefined,
        .nameLen = undefined,
        .varId = varId,
    });

    // Reset boxed.
    if (!svar.isParentLocalAlias()) {
        svar.isBoxed = false;
    }
    log.tracev("reserve {s}: {}", .{sema.getVarName(c, varId), svar.local});
}

fn reserveLocalReg(c: *Chunk, varId: sema.LocalVarId, advanceNext: bool) !RegisterId {
    try reserveLocalRegAt(c, varId, c.curBlock.nextLocalReg);
    defer {
        if (advanceNext) {
            c.curBlock.nextLocalReg += 1;
        }
    }
    return c.curBlock.nextLocalReg;
}

fn unexpectedFmt(format: []const u8, vals: []const fmt.FmtValue) noreturn {
    if (builtin.mode == .Debug) {
        fmt.printStderr(format, vals);
    }
    cy.fatal();
}

fn pushFieldRetain(c: *cy.Chunk, recv: u8, dst: u8, fieldId: u16, debugNodeId: cy.NodeId) !void {
    try c.pushFailableDebugSym(debugNodeId);
    const start = c.buf.ops.items.len;
    try c.buf.pushOpSlice(.fieldRetain, &.{ recv, dst, 0, 0, 0, 0, 0 });
    c.buf.setOpArgU16(start + 3, fieldId);
}

fn pushInlineUnaryExpr(chunk: *cy.Chunk, code: cy.OpCode, child: u8, dst: u8, nodeId: cy.NodeId) !void {
    try chunk.pushFailableDebugSym(nodeId);
    try chunk.buf.pushOpSlice(code, &.{ child, dst, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 });
}

fn pushInlineBinExpr(chunk: *cy.Chunk, code: cy.OpCode, left: u8, right: u8, dst: u8, nodeId: cy.NodeId) !void {
    try chunk.pushFailableDebugSym(nodeId);
    try chunk.buf.pushOpSlice(code, &.{ left, right, dst, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 });
}

fn pushInlineTernExpr(chunk: *cy.Chunk, code: cy.OpCode, a: u8, b: u8, c: u8, dst: u8, nodeId: cy.NodeId) !void {
    try chunk.pushFailableDebugSym(nodeId);
    try chunk.buf.pushOpSlice(code, &.{ a, b, c, dst, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 });
}

fn pushCallObjSym(chunk: *cy.Chunk, startLocal: u8, numArgs: u8, expectReturn: bool, symId: u8, funcSigId: u16, nodeId: cy.NodeId) !void {
    try pushCallObjSymExt(chunk, startLocal, numArgs, expectReturn, symId, funcSigId, nodeId, cy.NullId);
}

fn pushCallObjSymExt(chunk: *cy.Chunk, startLocal: u8, numArgs: u8, expectReturn: bool, symId: u8, funcSigId: u16, nodeId: cy.NodeId, extraIdx: u32) !void {
    try chunk.pushFailableDebugSym(nodeId);
    const start = chunk.buf.ops.items.len;
    try chunk.buf.pushOpSliceExt(.callObjSym, &.{
        startLocal, numArgs, @as(u8, if (expectReturn) 1 else 0), symId, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    }, chunk.descExtra(nodeId, extraIdx));
    chunk.buf.setOpArgU16(start + 5, funcSigId);
}

fn pushCallSym(c: *cy.Chunk, startLocal: u8, numArgs: u32, numRet: u8, symId: u32, nodeId: cy.NodeId) !void {
    try c.pushFailableDebugSym(nodeId);
    const start = c.buf.ops.items.len;
    try c.buf.pushOpSliceExt(.callSym, &.{ startLocal, @as(u8, @intCast(numArgs)), numRet, 0, 0, 0, 0, 0, 0, 0, 0 }, c.desc(nodeId));
    c.buf.setOpArgU16(start + 4, @intCast(symId));
}

fn genCallTypeCheck(c: *cy.Chunk, startLocal: u8, numArgs: u32, funcSigId: sema.FuncSigId, nodeId: cy.NodeId) !void {
    try c.pushFailableDebugSym(nodeId);
    const start = c.buf.ops.items.len;
    try c.buf.pushOpSlice(.callTypeCheck, &[_]u8{ startLocal, @intCast(numArgs), 0, 0, });
    c.buf.setOpArgU16(start + 3, @intCast(funcSigId));
}

fn genSubBlockReleaseLocals(c: *Chunk) !void {
    const sblock = sema.curSubBlock(c);

    const start = c.operandStack.items.len;
    defer c.operandStack.items.len = start;

    const varDecls = c.varDeclStack.items[sblock.varDeclStart..];
    for (varDecls) |decl| {
        const svar = c.vars.items[decl.varId];
        if (svar.lifetimeRcCandidate) {
            try c.operandStack.append(c.alloc, svar.local);
        }
    }

    const locals = c.operandStack.items[start..];
    if (locals.len > 0) {
        try pushReleases(c, locals, sblock.nodeId);
    }
}

/// Only the locals that are alive at this moment are considered for released.
fn genBlockReleaseLocals(c: *Chunk) !void {
    const block = sema.curBlock(c);

    const start = c.operandStack.items.len;
    defer c.operandStack.items.len = start;

    const varDecls = c.varDeclStack.items[block.varDeclStart..];
    for (varDecls) |decl| {
        const svar = c.vars.items[decl.varId];
        if (svar.lifetimeRcCandidate) {
            try c.operandStack.append(c.alloc, svar.local);
        }
    }
    
    const locals = c.operandStack.items[start..];
    if (locals.len > 0) {
        const nodeId = sema.getBlockNodeId(c, block);
        try pushReleases(c, locals, nodeId);
    }
}

fn genBlockEnd(c: *Chunk) !void {
    try genBlockReleaseLocals(c);
    if (c.curBlock.requiresEndingRet1) {
        try c.buf.pushOp(.ret1);
    } else {
        try c.buf.pushOp(.ret0);
    }
}

pub fn reserveMainRegs(self: *Chunk) !void {
    var nextReg: u8 = 0;

    self.curBlock.startLocalReg = nextReg;
    self.curBlock.nextLocalReg = nextReg;

    // Reset temp register state.
    const sblock = sema.curBlock(self);
    const tempRegStart = nextReg + sblock.maxLocals;
    self.rega.resetState(tempRegStart);
}

/// Reserve params and captured vars.
/// Function stack layout:
/// [startLocal/retLocal] [retInfo] [retAddress] [prevFramePtr] [params...] [callee] [var locals...] [temp locals...]
/// `callee` is reserved so that function values can call static functions with the same call convention.
/// For this reason, `callee` isn't freed in the function body and a separate release inst is required for lambda calls.
/// A closure can also occupy the callee and is used to do captured var lookup.
pub fn reserveFuncRegs(self: *Chunk, numParams: u32) !void {
    // First local is reserved for a single return value.
    // Second local is reserved for the return info.
    // Third local is reserved for the return address.
    // Fourth local is reserved for the previous frame pointer.
    var nextReg: u8 = 4;

    const sblock = sema.curBlock(self);

    // Reserve func params.
    var numParamCopies: u8 = 0;
    const params = sblock.params.items[0..numParams];
    for (params) |varId| {
        const svar = &self.vars.items[varId];
        if (!svar.isParentLocalAlias()) {
            svar.isBoxed = false;
        }
        if (svar.inner.param.copied) {
            // Forward reserve the param copy.
            svar.local = @intCast(4 + params.len + 1 + numParamCopies);
            try self.varDeclStack.append(self.alloc, .{
                .namePtr = undefined,
                .nameLen = undefined,
                .varId = varId,
            });

            // Copy param to local.
            try self.buf.pushOp2(.copyRetainSrc, nextReg, svar.local);

            numParamCopies += 1;
        } else {
            svar.local = nextReg;
        }
        nextReg += 1;
    }

    // An extra callee slot is reserved so that function values
    // can call static functions with the same call convention.
    // It's also used to store the closure object.
    if (sblock.captures.items.len > 0) {
        self.curBlock.closureLocal = nextReg;
    }
    nextReg += 1;

    if (sblock.params.items.len > numParams) {
        {
            @panic("This path is actually used?");
        }
        for (sblock.params.items[numParams..]) |varId| {
            self.vars.items[varId].local = nextReg;
            nextReg += 1;
        }
    }

    self.curBlock.startLocalReg = nextReg;
    nextReg += numParamCopies;
    self.curBlock.nextLocalReg = nextReg;

    // Reset temp register state.
    const tempRegStart = nextReg + sblock.maxLocals;
    self.rega.resetState(tempRegStart);
}

fn nextSemaSubBlock(self: *Chunk) void {
    self.curSemaSubBlockId = self.nextSemaSubBlockId;
    self.nextSemaSubBlockId += 1;

    // Codegen varDeclStart can be different than sema due to param copies.
    const sblock = sema.curSubBlock(self);
    sblock.varDeclStart = @intCast(self.varDeclStack.items.len);
}

/// `genSubBlockReleaseLocals` is not called here since
/// loop sub blocks need to generate a jump statement after the release ops.
fn prevSemaSubBlock(c: *Chunk) void {
    const sblock = sema.curSubBlock(c);

    // Unwind var decls.
    c.varDeclStack.items.len = sblock.varDeclStart;

    c.curSemaSubBlockId = sblock.prevSubBlockId;

    // Recede nextLocalReg.
    c.curBlock.nextLocalReg -= sblock.numLocals;
}

pub fn pushSemaBlock(self: *Chunk, id: sema.BlockId) !void {
    // Codegen block should be pushed first so nextSemaSubBlock can use it.
    try self.pushBlock();

    try self.semaBlockStack.append(self.alloc, id);
    self.curSemaBlockId = id;

    // Codegen varDeclStart can be different than sema due to param copies.
    sema.curBlock(self).varDeclStart = @intCast(self.varDeclStack.items.len);

    self.nextSemaSubBlockId = self.semaBlocks.items[id].firstSubBlockId;
    nextSemaSubBlock(self);
}

fn popSemaBlock(self: *Chunk) void {
    prevSemaSubBlock(self);

    self.varDeclStack.items.len = sema.curBlock(self).varDeclStart;

    self.semaBlockStack.items.len -= 1;
    self.curSemaBlockId = self.semaBlockStack.items[self.semaBlockStack.items.len-1];

    self.popBlock();
}