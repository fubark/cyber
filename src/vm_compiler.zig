const std = @import("std");
const stdx = @import("stdx");
const cy = @import("cyber.zig");

const NullId = std.math.maxInt(u32);
const NullByteId = std.math.maxInt(u8);
const log = stdx.log.scoped(.vm_compiler);
const f64NegOne = cy.Value.initF64(-1);
const f64One = cy.Value.initF64(1);

pub const VMcompiler = struct {
    alloc: std.mem.Allocator,
    vm: *cy.VM,
    buf: cy.ByteCodeBuffer,
    lastErr: []const u8,

    /// Context vars.
    src: []const u8,
    nodes: []const cy.Node,
    tokens: []const cy.Token,
    funcDecls: []const cy.FunctionDeclaration,
    funcParams: []const cy.FunctionParam,
    blocks: std.ArrayListUnmanaged(Block),
    jumpStack: std.ArrayListUnmanaged(Jump),
    loadStack: std.ArrayListUnmanaged(Load),
    operandStack: std.ArrayListUnmanaged(cy.OpData),
    curBlock: *Block,

    pub fn init(self: *VMcompiler, vm: *cy.VM) void {
        self.* = .{
            .alloc = vm.alloc,
            .vm = vm,
            .buf = cy.ByteCodeBuffer.init(vm.alloc),
            .lastErr = "",
            .nodes = undefined,
            .tokens = undefined,
            .funcDecls = undefined,
            .funcParams = undefined,
            .blocks = .{},
            .jumpStack = .{},
            .loadStack = .{},
            .operandStack = .{},
            .curBlock = undefined,
            .src = undefined,
        };
    }

    pub fn deinit(self: *VMcompiler) void {
        self.alloc.free(self.lastErr);
        self.blocks.deinit(self.alloc);
        self.buf.deinit();
        self.jumpStack.deinit(self.alloc);
        self.loadStack.deinit(self.alloc);
        self.operandStack.deinit(self.alloc);
    }

    pub fn compile(self: *VMcompiler, ast: cy.ParseResultView) !ResultView {
        self.buf.clear();
        self.blocks.clearRetainingCapacity();
        self.nodes = ast.nodes.items;
        self.funcDecls = ast.func_decls.items;
        self.funcParams = ast.func_params;
        self.src = ast.src;
        self.tokens = ast.tokens;

        const root = self.nodes[ast.root_id];

        try self.pushBlock();
        defer self.popBlock();

        self.genStatements(root.head.child_head, true) catch {
            return ResultView{
                .buf = self.buf,
                .hasError = true,
            };
        };
        self.buf.mainLocalSize = self.curBlock.vars.size;

        return ResultView{
            .buf = self.buf,
            .hasError = false,
        };
    }

    fn endLocals(self: *VMcompiler) !void {
        var iter = self.curBlock.vars.valueIterator();
        while (iter.next()) |info| {
            if (info.rcCandidate and !info.isCapturedVar) {
                try self.buf.pushOp1(.release, info.localOffset);
            }
        }
    }

    fn pushBlock(self: *VMcompiler) !void {
        try self.blocks.append(self.alloc, Block.init());
        self.curBlock = &self.blocks.items[self.blocks.items.len-1];
    }

    fn popBlock(self: *VMcompiler) void {
        var last = self.blocks.pop();
        last.deinit(self.alloc);
        if (self.blocks.items.len > 0) {
            self.curBlock = &self.blocks.items[self.blocks.items.len-1];
        }
    }

    fn getScopedVarInfoPtr(self: *VMcompiler, varName: []const u8) ?*VarInfo {
        if (self.curBlock.vars.getPtr(varName)) |info| {
            return info;
        }
        return null;
    }

    fn getScopedVarInfo(self: *VMcompiler, varName: []const u8) ?VarInfo {
        if (self.curBlock.vars.get(varName)) |info| {
            return info;
        }
        return null;
    }

    /// Returns the VarInfo if the var was in the current scope or the immediate non-main parent scope.
    /// If found in the parent scope, the var is recorded as a captured var.
    /// Captured vars are given a tempId (order among other captured vars) and patched later by a local offset at the end of the block.
    fn readScopedVar(self: *VMcompiler, varName: []const u8) !?VarInfo {
        if (self.curBlock.vars.get(varName)) |info| {
            return info;
        }
        // Only check one block above that isn't the main scope.
        if (self.blocks.items.len > 2) {
            if (self.blocks.items[self.blocks.items.len-2].vars.get(varName)) |info| {
                if (!info.hasStaticType) {
                    const localInfo = try self.reserveCapturedVar(varName, AnyType, info.localOffset);
                    return localInfo.*;
                } else {
                    return error.UnsupportedStaticType;
                }
            }
        }
        return null;
    }

    fn genStatements(self: *VMcompiler, head: cy.NodeId, comptime attachEnd: bool) anyerror!void {
        var cur_id = head;
        while (cur_id != NullId) {
            const node = self.nodes[cur_id];
            if (attachEnd) {
                if (node.next == NullId) {
                    try self.genStatement(node, false);
                } else {
                    try self.genStatement(node, true);
                }
            } else {
                try self.genStatement(node, true);
            }
            cur_id = node.next;
        }
        if (attachEnd) {
            try self.endLocals();
            try self.buf.pushOp(.end);
        }
    }

    fn reserveCapturedVar(self: *VMcompiler, name: []const u8, vtype: Type, parentLocalOffset: u8) !*VarInfo {
        const tempOffset = @intCast(u8, self.curBlock.capturedVars.items.len);
        try self.curBlock.capturedVars.append(self.alloc, .{
            .name = name,
            .parentLocalOffset = parentLocalOffset,
            .localOffset = tempOffset,
        });
        const res = try self.curBlock.vars.getOrPut(self.alloc, name);
        if (res.found_existing) {
            return error.VarExists;
        } else {
            res.value_ptr.* = .{
                .vtype = vtype,
                .localOffset = tempOffset,
                .rcCandidate = vtype.rcCandidate,
                .hasStaticType = false,
                .isCapturedVar = true,
            };
            return res.value_ptr;
        }
    }

    fn reserveLocalVar(self: *VMcompiler, name: []const u8, vtype: Type) !*VarInfo {
        const offset = try self.curBlock.reserveLocal();
        const res = try self.curBlock.vars.getOrPut(self.alloc, name);
        if (res.found_existing) {
            return error.VarExists;
        } else {
            res.value_ptr.* = .{
                .vtype = vtype,
                .localOffset = offset,
                .rcCandidate = vtype.rcCandidate,
                .hasStaticType = false,
                .isCapturedVar = false,
            };
            return res.value_ptr;
        }
    }

    fn genSetVar(self: *VMcompiler, name: []const u8, vtype: Type) !u8 {
        if (self.getScopedVarInfoPtr(name)) |info| {
            if (info.vtype.rcCandidate) {
                try self.buf.pushOp1(.releaseSet, info.localOffset);
            } else {
                try self.buf.pushOp1(.set, info.localOffset);
            }
            if (info.vtype.typeT != vtype.typeT) {
                info.vtype = vtype;
                if (!info.rcCandidate and vtype.rcCandidate) {
                    info.rcCandidate = true;
                }
            }
            return info.localOffset;
        } else {
            const info = try self.reserveLocalVar(name, vtype);
            try self.buf.pushOp1(.setNew, info.localOffset);
            return info.localOffset;
        }
    }

    fn reserveFuncParams(self: *VMcompiler, func: cy.FunctionDeclaration) !void {
        if (func.params.end > func.params.start) {
            for (self.funcParams[func.params.start..func.params.end]) |param| {
                const paramName = self.src[param.name.start..param.name.end];
                const paramT = AnyType;
                _ = try self.reserveLocalVar(paramName, paramT);
            }
        }
    }

    /// discardTopExprReg is usually true since statements aren't expressions and evaluating child expressions
    /// would just grow the register stack unnecessarily. However, the last main statement requires the
    /// resulting expr to persist to return from `eval`.
    fn genStatement(self: *VMcompiler, node: cy.Node, comptime discardTopExprReg: bool) !void {
        // log.debug("gen stmt {}", .{node.node_t});
        switch (node.node_t) {
            .expr_stmt => {
                const expr = self.nodes[node.head.child_head];
                _ = try self.genExpr(expr, discardTopExprReg);
            },
            .break_stmt => {
                try self.jumpStack.append(self.alloc, .{
                    .pc = @intCast(u32, self.buf.ops.items.len),
                });
                try self.buf.pushOp1(.jump, 0);
            },
            .add_assign_stmt => {
                const left = self.nodes[node.head.left_right.left];
                if (left.node_t == .ident) {
                    const identToken = self.tokens[left.start_token];
                    const varName = self.src[identToken.start_pos .. identToken.data.end_pos];

                    if (self.getScopedVarInfo(varName)) |info| {
                        const right = self.nodes[node.head.left_right.right];
                        const rtype = try self.genExpr(right, false);
                        if (info.vtype.typeT != .number and info.vtype.typeT != .any and rtype.typeT != info.vtype.typeT) {
                            return self.reportError("Type mismatch: Expected {}", .{info.vtype.typeT}, node);
                        }
                        try self.buf.pushOp1(.addSet, info.localOffset);
                    } else stdx.panic("variable not declared");
                } else {
                    stdx.panicFmt("unsupported assignment to left {}", .{left.node_t});
                }
            },
            .assign_stmt => {
                const left = self.nodes[node.head.left_right.left];
                if (left.node_t == .ident) {
                    const identToken = self.tokens[left.start_token];
                    const varName = self.src[identToken.start_pos .. identToken.data.end_pos];

                    if (self.getScopedVarInfo(varName)) |info| {
                        if (info.isCapturedVar) {
                            return self.reportError("Can not reassign to a captured variable.", .{}, left);
                        }
                    }

                    const right = self.nodes[node.head.left_right.right];
                    const rtype = try self.genMaybeRetainExpr(right, false);
                    _ = try self.genSetVar(varName, rtype);
                } else if (left.node_t == .arr_access_expr) {
                    const accessLeft = self.nodes[left.head.left_right.left];
                    _ = try self.genExpr(accessLeft, false);
                    const accessRight = self.nodes[left.head.left_right.right];
                    _ = try self.genExpr(accessRight, false);

                    const right = self.nodes[node.head.left_right.right];
                    _ = try self.genExpr(right, false);
                    try self.buf.pushOp(.setIndex);
                } else {
                    stdx.panicFmt("unsupported assignment to left {}", .{left.node_t});
                }
            },
            .func_decl => {
                const func = self.funcDecls[node.head.func.decl_id];

                const name = self.src[func.name.start..func.name.end];
                const symId = try self.vm.ensureFuncSym(name);

                const jumpOpStart = self.buf.ops.items.len;
                try self.buf.pushOp1(.jump, 0);

                try self.pushBlock();
                const opStart = @intCast(u32, self.buf.ops.items.len);
                try self.reserveFuncParams(func);
                try self.genStatements(node.head.func.body_head, false);
                // TODO: Check last statement to skip adding ret.
                try self.endLocals();
                try self.buf.pushOp(.ret0);

                // Reserve another local for the call return info.
                const numLocals = self.curBlock.vars.size + 1;
                self.popBlock();

                self.buf.setOpArgs1(jumpOpStart + 1, @intCast(u8, self.buf.ops.items.len - jumpOpStart));

                const sym = cy.FuncSymbolEntry.initFunc(opStart, numLocals);
                try self.vm.setFuncSym(symId, sym);
            },
            .for_inf_stmt => {
                self.curBlock.pcSave = @intCast(u32, self.buf.ops.items.len);
                const jumpStackSave = @intCast(u32, self.jumpStack.items.len);

                // TODO: generate gas meter checks.
                // if (self.opts.gas_meter != .none) {
                //     try self.indent();
                //     if (self.opts.gas_meter == .error_interrupt) {
                //         _ = try self.writer.write("__interrupt_count += 1; if (__interrupt_count > __interrupt_max) throw globalThis._internal.interruptSym;\n");
                //     } else if (self.opts.gas_meter == .yield_interrupt) {
                //         _ = try self.writer.write("__interrupt_count += 1; if (__interrupt_count > __interrupt_max) yield globalThis._internal.interruptSym;\n");
                //     }
                // }

                try self.genStatements(node.head.child_head, false);
                try self.buf.pushOp1(.jumpBack, @intCast(u8, self.buf.ops.items.len - self.curBlock.pcSave));

                // Patch break jumps.
                for (self.jumpStack.items[jumpStackSave..]) |jump| {
                    self.buf.setOpArgs1(jump.pc + 1, @intCast(u8, self.buf.ops.items.len - jump.pc));
                }
                self.jumpStack.items.len = jumpStackSave;
            },
            .for_iter_stmt => {
                const iterable = self.nodes[node.head.for_iter_stmt.iterable];
                _ = try self.genExpr(iterable, false);

                var local: u8 = NullByteId;
                const as_clause = self.nodes[node.head.for_iter_stmt.as_clause];
                const ident = self.nodes[as_clause.head.as_iter_clause.value];
                const ident_token = self.tokens[ident.start_token];
                const asName = self.src[ident_token.start_pos..ident_token.data.end_pos];
                if (self.getScopedVarInfo(asName)) |info| {
                    local = info.localOffset;
                } else {
                    const info = try self.reserveLocalVar(asName, AnyType);
                    local = info.localOffset;
                }

                const forOpStart = self.buf.ops.items.len;
                try self.buf.pushOp2(.forIter, local, 0);

                try self.genStatements(node.head.for_iter_stmt.body_head, false);
                try self.buf.pushOp(.cont);
                self.buf.setOpArgs1(forOpStart+2, @intCast(u8, self.buf.ops.items.len - forOpStart));
            },
            .for_range_stmt => {
                var local: u8 = NullByteId;
                if (node.head.for_range_stmt.as_clause != NullId) {
                    const asClause = self.nodes[node.head.for_range_stmt.as_clause];
                    const ident = self.nodes[asClause.head.as_range_clause.ident];
                    const ident_token = self.tokens[ident.start_token];
                    const asName = self.src[ident_token.start_pos..ident_token.data.end_pos];
                    if (self.getScopedVarInfo(asName)) |info| {
                        local = info.localOffset;
                    } else {
                        const info = try self.reserveLocalVar(asName, NumberType);
                        local = info.localOffset;
                    }
                    // inc = as_clause.head.as_range_clause.inc;
                    // if (as_clause.head.as_range_clause.step != NullId) {
                    //     step = self.nodes[as_clause.head.as_range_clause.step];
                    // }
                }

                // Push range start/end.
                const range_clause = self.nodes[node.head.for_range_stmt.range_clause];
                const left_range = self.nodes[range_clause.head.left_right.left];
                _ = try self.genExpr(left_range, false);
                const right_range = self.nodes[range_clause.head.left_right.right];
                _ = try self.genExpr(right_range, false);

                // Push step.
                const stepConst = try self.buf.pushConst(.{ .val = f64One.val });
                try self.buf.pushOp1(.pushConst, @intCast(u8, stepConst));

                // Push for range op with asLocal.
                const forOpStart = self.buf.ops.items.len;
                try self.buf.pushOp2(.forRange, local, 0);

                try self.genStatements(node.head.for_range_stmt.body_head, false);
                try self.buf.pushOp(.cont);
                self.buf.setOpArgs1(forOpStart+2, @intCast(u8, self.buf.ops.items.len - forOpStart));
            },
            .if_stmt => {
                const cond = self.nodes[node.head.left_right.left];
                _ = try self.genExpr(cond, false);

                var opStart = self.buf.ops.items.len;
                try self.buf.pushOp1(.jumpNotCond, 0);

                try self.genStatements(node.head.left_right.right, false);
                self.buf.setOpArgs1(opStart + 1, @intCast(u8, self.buf.ops.items.len - opStart));

                var elseClauseId = node.head.left_right.extra;
                while (elseClauseId != NullId) {
                    const elseClause = self.nodes[elseClauseId];
                    if (elseClause.head.else_clause.cond == NullId) {
                        try self.genStatements(elseClause.head.else_clause.body_head, false);
                        break;
                    } else {
                        const elseCond = self.nodes[elseClause.head.else_clause.cond];
                        _ = try self.genExpr(elseCond, false);

                        opStart = self.buf.ops.items.len;
                        try self.buf.pushOp1(.jumpNotCond, 0);

                        try self.genStatements(elseClause.head.else_clause.body_head, false);
                        self.buf.setOpArgs1(opStart + 1, @intCast(u8, self.buf.ops.items.len - opStart));
                        elseClauseId = elseClause.head.else_clause.else_clause;
                    }
                }
            },
            .return_stmt => {
                if (self.blocks.items.len == 1) {
                    try self.endLocals();
                    try self.buf.pushOp(.end);
                } else {
                    try self.endLocals();
                    try self.buf.pushOp(.ret0);
                }
            },
            .return_expr_stmt => {
                const expr = self.nodes[node.head.child_head];
                _ = try self.genExpr(expr, false);

                if (self.blocks.items.len == 1) {
                    try self.endLocals();
                    try self.buf.pushOp(.end);
                } else {
                    try self.endLocals();
                    try self.buf.pushOp(.ret1);
                }
            },
            else => return self.reportError("Unsupported node", .{}, node),
        }
    }

    fn genLoadLocal(self: *VMcompiler, info: VarInfo) !void {
        try self.buf.pushOp1(.load, info.localOffset);
        if (info.isCapturedVar) {
            try self.loadStack.append(self.alloc, .{
                .pc = @intCast(u32, self.buf.ops.items.len) - 1,
                .tempOffset = info.localOffset,
            });
        }
    }

    fn genMaybeRetainExpr(self: *VMcompiler, node: cy.Node, comptime discardTopExprReg: bool) anyerror!Type {
        if (node.node_t == .ident) {
            const token = self.tokens[node.start_token];
            const name = self.src[token.start_pos..token.data.end_pos];
            if (try self.readScopedVar(name)) |info| {
                if (info.vtype.rcCandidate) {
                    try self.buf.pushOp1(.loadRetain, info.localOffset);
                    if (info.isCapturedVar) {
                        try self.loadStack.append(self.alloc, .{
                            .pc = @intCast(u32, self.buf.ops.items.len) - 1,
                            .tempOffset = info.localOffset,
                        });
                    }
                } else {
                    try self.genLoadLocal(info);
                }
                return info.vtype;
            } else {
                try self.buf.pushOp(.pushNone);
                return AnyType;
            }
        } else {
            return self.genExpr(node, discardTopExprReg);
        }
    }

    fn genExpr(self: *VMcompiler, node: cy.Node, comptime discardTopExprReg: bool) anyerror!Type {
        // log.debug("gen expr {}", .{node.node_t});
        switch (node.node_t) {
            .true_literal => {
                if (!discardTopExprReg) {
                    try self.buf.pushOp(.pushTrue);
                }
                return BoolType;
            },
            .false_literal => {
                if (!discardTopExprReg) {
                    try self.buf.pushOp(.pushFalse);
                }
                return BoolType;
            },
            .arr_literal => {
                var expr_id = node.head.child_head;
                var i: u32 = 0;
                while (expr_id != NullId) : (i += 1) {
                    var expr = self.nodes[expr_id];
                    _ = try self.genExpr(expr, discardTopExprReg);
                    expr_id = expr.next;
                }

                if (!discardTopExprReg) {
                    try self.buf.pushOp1(.pushList, @intCast(u8, i));
                }
                return ListType;
            },
            .map_literal => {
                const operandStart = self.operandStack.items.len;
                defer self.operandStack.items.len = operandStart;

                var i: u32 = 0;
                var entry_id = node.head.child_head;
                while (entry_id != NullId) : (i += 1) {
                    var entry = self.nodes[entry_id];
                    const key = self.nodes[entry.head.left_right.left];

                    if (!discardTopExprReg) {
                        switch (key.node_t) {
                            .ident => {
                                const token = self.tokens[key.start_token];
                                const name = self.src[token.start_pos..token.data.end_pos];
                                const idx = try self.buf.pushStringConst(name);
                                try self.operandStack.append(self.alloc, .{ .arg = @intCast(u8, idx) });
                            },
                            else => stdx.panicFmt("unsupported key {}", .{key.node_t}),
                        }
                    }

                    const val = self.nodes[entry.head.left_right.right];
                    _ = try self.genExpr(val, discardTopExprReg);
                    entry_id = entry.next;
                }

                if (!discardTopExprReg) {
                    if (i == 0) {
                        try self.buf.pushOp(.pushMapEmpty);
                    } else {
                        try self.buf.pushOp1(.pushMap, @intCast(u8, i));
                        try self.buf.pushOperands(self.operandStack.items[operandStart..]);
                    }
                }
                return MapType;
            },
            .number => {
                if (!discardTopExprReg) {
                    const token = self.tokens[node.start_token];
                    const literal = self.src[token.start_pos..token.data.end_pos];
                    const val = try std.fmt.parseFloat(f64, literal);
                    const idx = try self.buf.pushConst(.{ .val = @bitCast(u64, val) });
                    try self.buf.pushOp1(.pushConst, @intCast(u8, idx));
                }
                return NumberType;
            },
            .string => {
                if (!discardTopExprReg) {
                    const token = self.tokens[node.start_token];
                    const literal = self.src[token.start_pos+1..token.data.end_pos-1];
                    const idx = try self.buf.pushStringConst(literal);
                    try self.buf.pushOp1(.pushConst, @intCast(u8, idx));
                }
                return ConstStringType;
            },
            .ident => {
                const token = self.tokens[node.start_token];
                const name = self.src[token.start_pos..token.data.end_pos];
                if (try self.readScopedVar(name)) |info| {
                    try self.genLoadLocal(info);
                    return info.vtype;
                } else {
                    try self.buf.pushOp(.pushNone);
                    return AnyType;
                }
            },
            .arr_range_expr => {
                const arr = self.nodes[node.head.arr_range_expr.arr];
                _ = try self.genExpr(arr, discardTopExprReg);

                if (node.head.arr_range_expr.left == NullId) {
                    if (!discardTopExprReg) {
                        const idx = try self.buf.pushConst(.{ .val = 0 });
                        try self.buf.pushOp1(.pushConst, @intCast(u8, idx));
                    }
                } else {
                    const left = self.nodes[node.head.arr_range_expr.left];
                    _ = try self.genExpr(left, discardTopExprReg);
                }
                if (node.head.arr_range_expr.right == NullId) {
                    if (!discardTopExprReg) {
                        const idx = try self.buf.pushConst(.{ .val = f64NegOne.val });
                        try self.buf.pushOp1(.pushConst, @intCast(u8, idx));
                    }
                } else {
                    const right = self.nodes[node.head.arr_range_expr.right];
                    _ = try self.genExpr(right, discardTopExprReg);
                }

                if (!discardTopExprReg) {
                    try self.buf.pushOp(.pushSlice);
                }
                return ListType;
            },
            .access_expr => {
                const left = self.nodes[node.head.left_right.left];
                _ = try self.genExpr(left, discardTopExprReg);

                // right should be an ident.
                const right = self.nodes[node.head.left_right.right];
                const token = self.tokens[right.start_token];

                const name = self.src[token.start_pos .. token.data.end_pos];
                const symId = try self.vm.ensureFieldSym(name);

                if (!discardTopExprReg) {
                    try self.buf.pushOp1(.pushField, @intCast(u8, symId));
                }

                return AnyType;
            },
            .arr_access_expr => {
                const left = self.nodes[node.head.left_right.left];
                _ = try self.genExpr(left, discardTopExprReg);

                const index = self.nodes[node.head.left_right.right];
                _ = try self.genExpr(index, discardTopExprReg);

                if (!discardTopExprReg) {
                    try self.buf.pushOp(.pushIndex);
                }
                return AnyType;
            },
            .unary_expr => {
                const child = self.nodes[node.head.unary.child];
                const op = node.head.unary.op;
                switch (op) {
                    .not => {
                        _ = try self.genExpr(child, discardTopExprReg);
                        if (!discardTopExprReg) {
                            try self.buf.pushOp(.pushNot);
                        }
                        return BoolType;
                    },
                    else => return self.reportError("Unsupported unary op: {}", .{op}, node),
                }
            },
            .bin_expr => {
                const left = self.nodes[node.head.left_right.left];
                const right = self.nodes[node.head.left_right.right];

                var ltype: Type = undefined;
                var rtype: Type = undefined;

                const op = @intToEnum(cy.BinaryExprOp, node.head.left_right.extra);
                switch (op) {
                    .plus => {
                        _ = try self.genExpr(left, discardTopExprReg);
                        _ = try self.genExpr(right, discardTopExprReg);
                        if (!discardTopExprReg) {
                            try self.buf.pushOp(.pushAdd);
                        }
                        return NumberType;
                    },
                    .minus => {
                        // Generating pushMinus1 for fib.cy increases performance ~10-12%.
                        var leftVar: u8 = 255;
                        if (left.node_t == .ident) {
                            const token = self.tokens[left.start_token];
                            const name = self.src[token.start_pos .. token.data.end_pos];
                            if (try self.readScopedVar(name)) |info| {
                                leftVar = info.localOffset;
                            }
                        }
                        if (leftVar == 255) {
                            _ = try self.genExpr(left, discardTopExprReg);
                        }
                        var rightVar: u8 = 255;
                        if (right.node_t == .ident) {
                            const token = self.tokens[right.start_token];
                            const name = self.src[token.start_pos .. token.data.end_pos];
                            if (try self.readScopedVar(name)) |info| {
                                rightVar = info.localOffset;
                            }
                        }
                        if (rightVar == 255) {
                            _ = try self.genExpr(right, discardTopExprReg);
                        }

                        if (!discardTopExprReg) {
                            if (leftVar != rightVar) {
                                try self.buf.pushOp2(.pushMinus1, leftVar, rightVar);
                            } else {
                                if (leftVar == 255) {
                                    try self.buf.pushOp(.pushMinus);
                                } else {
                                    try self.buf.pushOp2(.pushMinus2, leftVar, leftVar);
                                }
                            }
                        }
                        return NumberType;
                    },
                    .and_op => {
                        ltype = try self.genExpr(left, discardTopExprReg);
                        rtype = try self.genExpr(right, discardTopExprReg);
                        if (!discardTopExprReg) {
                            try self.buf.pushOp(.pushAnd);
                        }
                        if (ltype.typeT == rtype.typeT) {
                            return ltype;
                        } else return AnyType;
                    },
                    .or_op => {
                        ltype = try self.genExpr(left, discardTopExprReg);
                        rtype = try self.genExpr(right, discardTopExprReg);
                        if (!discardTopExprReg) {
                            try self.buf.pushOp(.pushOr);
                        }
                        if (ltype.typeT == rtype.typeT) {
                            return ltype;
                        } else return AnyType;
                    },
                    .equal_equal => {
                        _ = try self.genExpr(left, discardTopExprReg);
                        _ = try self.genExpr(right, discardTopExprReg);
                        if (!discardTopExprReg) {
                            try self.buf.pushOp(.pushCompare);
                        }
                        return BoolType;
                    },
                    .less => {
                        _ = try self.genExpr(left, discardTopExprReg);
                        _ = try self.genExpr(right, discardTopExprReg);
                        if (!discardTopExprReg) {
                            try self.buf.pushOp(.pushLess);
                        }
                        return BoolType;
                    },
                    .less_equal => {
                        _ = try self.genExpr(left, discardTopExprReg);
                        _ = try self.genExpr(right, discardTopExprReg);
                        if (!discardTopExprReg) {
                            try self.buf.pushOp(.pushLessEqual);
                        }
                        return BoolType;
                    },
                    .greater => {
                        _ = try self.genExpr(left, discardTopExprReg);
                        _ = try self.genExpr(right, discardTopExprReg);
                        if (!discardTopExprReg) {
                            try self.buf.pushOp(.pushGreater);
                        }
                        return BoolType;
                    },
                    .greater_equal => {
                        _ = try self.genExpr(left, discardTopExprReg);
                        _ = try self.genExpr(right, discardTopExprReg);
                        if (!discardTopExprReg) {
                            try self.buf.pushOp(.pushGreaterEqual);
                        }
                        return BoolType;
                    },
                    else => return self.reportError("Unsupported binary op: {}", .{op}, node),
                }
            },
            .call_expr => {
                const callee = self.nodes[node.head.func_call.callee];
                if (!node.head.func_call.has_named_arg) {
                    if (callee.node_t == .access_expr) {
                        const right = self.nodes[callee.head.left_right.right];
                        if (right.node_t == .ident) {
                            const left = self.nodes[callee.head.left_right.left];
                            _ = try self.genExpr(left, false);

                            var numArgs: u32 = 1;
                            var arg_id = node.head.func_call.arg_head;
                            while (arg_id != NullId) : (numArgs += 1) {
                                const arg = self.nodes[arg_id];
                                _ = try self.genMaybeRetainExpr(arg, false);
                                arg_id = arg.next;
                            }

                            const identToken = self.tokens[right.start_token];
                            const str = self.src[identToken.start_pos .. identToken.data.end_pos];
                            // const slice = try self.buf.getStringConst(str);
                            // try self.buf.pushExtra(.{ .two = .{ slice.start, slice.end } });
                            const symId = try self.vm.ensureStructSym(str);

                            try self.buf.pushOp2(.callObjSym, @intCast(u8, symId), @intCast(u8, numArgs));
                            return AnyType;
                        } else return self.reportError("Unsupported callee", .{}, node);
                    } else if (callee.node_t == .ident) {
                        const token = self.tokens[callee.start_token];
                        const name = self.src[token.start_pos..token.data.end_pos];

                        if (try self.readScopedVar(name)) |info| {
                            var numArgs: u32 = 1;
                            var arg_id = node.head.func_call.arg_head;
                            while (arg_id != NullId) : (numArgs += 1) {
                                const arg = self.nodes[arg_id];
                                _ = try self.genMaybeRetainExpr(arg, false);
                                arg_id = arg.next;
                            }

                            // Load callee after args so it can be easily discarded.
                            try self.genLoadLocal(info);

                            if (discardTopExprReg) {
                                try self.buf.pushOp1(.pushCall0, @intCast(u8, numArgs));
                            } else {
                                try self.buf.pushOp1(.pushCall1, @intCast(u8, numArgs));
                            }
                            return AnyType;
                        } else {
                            var numArgs: u32 = 0;
                            var arg_id = node.head.func_call.arg_head;
                            while (arg_id != NullId) : (numArgs += 1) {
                                const arg = self.nodes[arg_id];
                                _ = try self.genMaybeRetainExpr(arg, false);
                                arg_id = arg.next;
                            }

                            const symId = try self.vm.ensureFuncSym(name);
                            if (discardTopExprReg) {
                                try self.buf.pushOp2(.pushCallSym0, @intCast(u8, symId), @intCast(u8, numArgs));
                            } else {
                                try self.buf.pushOp2(.pushCallSym1, @intCast(u8, symId), @intCast(u8, numArgs));
                            }
                            return AnyType;
                        }
                    } else return self.reportError("Unsupported callee", .{}, node);
                } else return self.reportError("Unsupported named args", .{}, node);
            },
            .lambda_expr => {
                if (!discardTopExprReg) {
                    const jumpOpStart = self.buf.ops.items.len;
                    try self.buf.pushOp1(.jump, 0);

                    try self.pushBlock();
                    const opStart = @intCast(u32, self.buf.ops.items.len);

                    // Generate function body.
                    const loadStackSave = @intCast(u32, self.loadStack.items.len);
                    const func = self.funcDecls[node.head.func.decl_id];
                    try self.reserveFuncParams(func);
                    const numParams = @intCast(u8, self.curBlock.stackLen);
                    const expr = self.nodes[node.head.func.body_head];
                    _ = try self.genMaybeRetainExpr(expr, false);
                    try self.endLocals();
                    try self.buf.pushOp(.ret1);
                    self.buf.setOpArgs1(jumpOpStart + 1, @intCast(u8, self.buf.ops.items.len - jumpOpStart));

                    // Reserve captured var locals together and push them onto execution stack.
                    for (self.curBlock.capturedVars.items) |*capVar| {
                        capVar.localOffset = try self.curBlock.reserveLocal();
                        const info = self.getScopedVarInfo(capVar.name).?;
                        if (info.vtype.rcCandidate) {
                            try self.buf.pushOp1(.loadRetain, capVar.parentLocalOffset);
                        } else {
                            try self.buf.pushOp1(.load, capVar.parentLocalOffset);
                        }
                    }

                    // Patch captured var load ops.
                    for (self.loadStack.items[loadStackSave..]) |load| {
                        const capVar = self.curBlock.capturedVars.items[load.tempOffset];
                        self.buf.setOpArgs1(load.pc, capVar.localOffset);
                    }
                    self.loadStack.items.len = loadStackSave;

                    const numLocals = @intCast(u8, self.curBlock.vars.size + 1);
                    const numCaptured = @intCast(u8, self.curBlock.capturedVars.items.len);
                    self.popBlock();

                    const funcPcOffset = @intCast(u8, self.buf.ops.items.len - opStart);
                    if (numCaptured == 0) {
                        try self.buf.pushOpSlice(.pushLambda, &.{ funcPcOffset, numParams, numLocals });
                    } else {
                        try self.buf.pushOpSlice(.pushClosure, &.{ funcPcOffset, numParams, numCaptured, numLocals });
                    }
                }
                return AnyType;
            },
            else => return self.reportError("Unsupported node", .{}, node),
        }
    }

    fn reportError(self: *VMcompiler, comptime fmt: []const u8, args: anytype, node: cy.Node) anyerror {
        const token = self.tokens[node.start_token];
        const customMsg = try std.fmt.allocPrint(self.alloc, fmt, args);
        defer self.alloc.free(customMsg);
        self.alloc.free(self.lastErr);
        self.lastErr = try std.fmt.allocPrint(self.alloc, "{s}: {} at {}", .{customMsg, node.node_t, token.start_pos});
        return error.CompileError;
    }
};

pub const ResultView = struct {
    buf: cy.ByteCodeBuffer,
    hasError: bool,
};

const CapturedVar = struct {
    name: []const u8,
    parentLocalOffset: u8,
    localOffset: u8,
};

const Block = struct {
    stackLen: u32,
    vars: std.StringHashMapUnmanaged(VarInfo),
    capturedVars: std.ArrayListUnmanaged(CapturedVar),
    pcSave: u32,

    fn init() Block {
        return .{
            .stackLen = 0,
            .vars = .{},
            .capturedVars = .{},
            .pcSave = 0,
        };
    }

    fn deinit(self: *Block, alloc: std.mem.Allocator) void {
        self.vars.deinit(alloc);
        self.capturedVars.deinit(alloc);
    }

    fn reserveLocal(self: *Block) !u8 {
        const idx = self.stackLen;
        self.stackLen += 1;
        if (idx <= std.math.maxInt(u8)) {
            return @intCast(u8, idx);
        } else {
            log.debug("Exceeded max local count.", .{});
            return error.CompileError;
        }
    }
};

const VarInfo = struct {
    /// The current type of the local in the traversed block.
    /// This is updated when there is a variable reassignment or a child block returns.
    vtype: Type,
    /// Stack offset from frame it was declared in.
    localOffset: u8,
    /// Maybe points to a ref counted object throughout the local's lifetime.
    /// Once this is true, it can not be reverted to false.
    /// The end of the block will emit release ops for any locals that are rc candidates.
    rcCandidate: bool,
    hasStaticType: bool,

    /// Whether this variable is captured.
    /// Captured variables can not be reassigned to another value. This restriction avoids
    /// an extra retain/release op for closure calls.
    isCapturedVar: bool,
};

const TypeTag = enum {
    any,
    boolean,
    number,
    list,
    string,
};

const Type = struct {
    typeT: TypeTag,
    rcCandidate: bool,
};

const AnyType = Type{
    .typeT = .any,
    .rcCandidate = true,
};

const BoolType = Type{
    .typeT = .boolean,
    .rcCandidate = false,
};

const NumberType = Type{
    .typeT = .number,
    .rcCandidate = false,
};

const ConstStringType = Type{
    .typeT = .string,
    .rcCandidate = false,
};

const ListType = Type{
    .typeT = .list,
    .rcCandidate = true,
};

const MapType = Type{
    .typeT = .list,
    .rcCandidate = true,
};

const ValueAddrType = enum {
    frameOffset,
};

const ValueAddr = struct {
    addrT: ValueAddrType,
    inner: union {
        frameOffset: u32,
    },
};

const Jump = struct {
    pc: u32,
};

const Load = struct {
    pc: u32,
    tempOffset: u8,
};