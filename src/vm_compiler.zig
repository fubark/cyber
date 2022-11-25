const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const cy = @import("cyber.zig");

const log = stdx.log.scoped(.vm_compiler);

const NullId = std.math.maxInt(u32);
const NullByteId = std.math.maxInt(u8);
const f64NegOne = cy.Value.initF64(-1);
const f64One = cy.Value.initF64(1);

const dumpCompileErrorStackTrace = builtin.mode == .Debug and true;

pub const VMcompiler = struct {
    alloc: std.mem.Allocator,
    vm: *cy.VM,
    buf: cy.ByteCodeBuffer,
    lastErr: []const u8,

    /// Context vars.
    src: []const u8,
    nodes: []cy.Node,
    tokens: []const cy.Token,
    funcDecls: []const cy.FuncDecl,
    funcParams: []const cy.FunctionParam,
    semaBlocks: std.ArrayListUnmanaged(SemaBlock),
    semaSubBlocks: std.ArrayListUnmanaged(SemaSubBlock),
    vars: std.ArrayListUnmanaged(SemaVar),
    capVarParents: std.AutoHashMapUnmanaged(SemaVarId, SemaVarId),
    blocks: std.ArrayListUnmanaged(Block),
    blockJumpStack: std.ArrayListUnmanaged(BlockJump),
    subBlockJumpStack: std.ArrayListUnmanaged(SubBlockJump),
    nodeStack: std.ArrayListUnmanaged(cy.NodeId),
    assignedVarStack: std.ArrayListUnmanaged(SemaVarId),
    operandStack: std.ArrayListUnmanaged(cy.OpData),
    curBlock: *Block,
    curSemaBlockId: SemaBlockId,
    semaBlockDepth: u32,
    curSemaSubBlockId: SemaSubBlockId,

    // Used during codegen to advance to the next saved sema block.
    nextSemaBlockId: u32,
    nextSemaSubBlockId: u32,

    u8Buf: std.ArrayListUnmanaged(u8),

    pub fn init(self: *VMcompiler, vm: *cy.VM) !void {
        self.* = .{
            .alloc = vm.alloc,
            .vm = vm,
            .buf = try cy.ByteCodeBuffer.init(vm.alloc),
            .lastErr = "",
            .nodes = undefined,
            .tokens = undefined,
            .funcDecls = undefined,
            .funcParams = undefined,
            .semaBlocks = .{},
            .semaSubBlocks = .{},
            .vars = .{},
            .capVarParents = .{},
            .blocks = .{},
            .blockJumpStack = .{},
            .subBlockJumpStack = .{},
            .nodeStack = .{},
            .assignedVarStack = .{},
            .operandStack = .{},
            .curBlock = undefined,
            .curSemaBlockId = undefined,
            .curSemaSubBlockId = undefined,
            .nextSemaBlockId = undefined,
            .nextSemaSubBlockId = undefined,
            .semaBlockDepth = undefined,
            .src = undefined,
            .u8Buf = .{},
        };
    }

    pub fn deinit(self: *VMcompiler) void {
        self.alloc.free(self.lastErr);

        for (self.semaSubBlocks.items) |*block| {
            block.deinit(self.alloc);
        }
        self.semaSubBlocks.deinit(self.alloc);

        for (self.semaBlocks.items) |*sblock| {
            sblock.deinit(self.alloc);
        }
        self.semaBlocks.deinit(self.alloc);

        self.blocks.deinit(self.alloc);
        self.buf.deinit();
        self.blockJumpStack.deinit(self.alloc);
        self.subBlockJumpStack.deinit(self.alloc);
        self.assignedVarStack.deinit(self.alloc);
        self.operandStack.deinit(self.alloc);
        self.u8Buf.deinit(self.alloc);
        self.nodeStack.deinit(self.alloc);
        self.vars.deinit(self.alloc);
        self.capVarParents.deinit(self.alloc);
    }

    fn semaExpr(self: *VMcompiler, nodeId: cy.NodeId, comptime discardTopExprReg: bool) anyerror!Type {
        const node = self.nodes[nodeId];
        // log.debug("sema expr {}", .{node.node_t});
        switch (node.node_t) {
            .true_literal => {
                return BoolType;
            },
            .false_literal => {
                return BoolType;
            },
            .none => {
                return AnyType;
            },
            .arr_literal => {
                var expr_id = node.head.child_head;
                var i: u32 = 0;
                while (expr_id != NullId) : (i += 1) {
                    var expr = self.nodes[expr_id];
                    _ = try self.semaExpr(expr_id, discardTopExprReg);
                    expr_id = expr.next;
                }

                return ListType;
            },
            .structInit => {
                const initializer = self.nodes[node.head.structInit.initializer];

                var i: u32 = 0;
                var entry_id = initializer.head.child_head;
                while (entry_id != NullId) : (i += 1) {
                    var entry = self.nodes[entry_id];
                    _ = try self.semaExpr(entry.head.left_right.right, discardTopExprReg);
                    entry_id = entry.next;
                }
                return AnyType;
            },
            .map_literal => {
                var i: u32 = 0;
                var entry_id = node.head.child_head;
                while (entry_id != NullId) : (i += 1) {
                    var entry = self.nodes[entry_id];

                    _ = try self.semaExpr(entry.head.left_right.right, discardTopExprReg);
                    entry_id = entry.next;
                }
                return MapType;
            },
            .number => {
                return NumberType;
            },
            .string => {
                return ConstStringType;
            },
            .stringTemplate => {
                const first = self.nodes[node.head.stringTemplate.partsHead];
                if (node.head.stringTemplate.firstIsString and first.next == NullId) {
                    // nop
                    return ConstStringType;
                }

                var nextIsExpr = !node.head.stringTemplate.firstIsString;
                var curId = node.head.stringTemplate.partsHead;
                while (true) {
                    const cur = self.nodes[curId];
                    if (nextIsExpr) {
                        _ = try self.semaExpr(curId, discardTopExprReg);
                    }
                    if (cur.next == NullId) {
                        break;
                    } else {
                        curId = cur.next;
                        nextIsExpr = !nextIsExpr;
                    }
                }
                return StringType;
            },
            .ident => {
                if (try self.semaReadVar(nodeId)) |svar| {
                    return svar.vtype;
                } else {
                    return AnyType;
                }
            },
            .if_expr => {
                _ = try self.semaExpr(node.head.if_expr.cond, false);

                _ = try self.semaExpr(node.head.if_expr.body_expr, discardTopExprReg);

                if (node.head.if_expr.else_clause != NullId) {
                    const else_clause = self.nodes[node.head.if_expr.else_clause];
                    _ = try self.semaExpr(else_clause.head.child_head, discardTopExprReg);
                }
                return AnyType;
            },
            .arr_range_expr => {
                _ = try self.semaExpr(node.head.arr_range_expr.arr, discardTopExprReg);

                if (node.head.arr_range_expr.left == NullId) {
                    // nop
                } else {
                    _ = try self.semaExpr(node.head.arr_range_expr.left, discardTopExprReg);
                }
                if (node.head.arr_range_expr.right == NullId) {
                    // nop
                } else {
                    _ = try self.semaExpr(node.head.arr_range_expr.right, discardTopExprReg);
                }

                return ListType;
            },
            .access_expr => {
                _ = try self.semaExpr(node.head.left_right.left, discardTopExprReg);
                return AnyType;
            },
            .arr_access_expr => {
                _ = try self.semaExpr(node.head.left_right.left, discardTopExprReg);

                const index = self.nodes[node.head.left_right.right];
                if (index.node_t == .unary_expr and index.head.unary.op == .minus) {
                    _ = try self.semaExpr(index.head.unary.child, discardTopExprReg);
                } else {
                    _ = try self.semaExpr(node.head.left_right.right, discardTopExprReg);
                }
                return AnyType;
            },
            .unary_expr => {
                const op = node.head.unary.op;
                switch (op) {
                    .minus => {
                        _ = try self.semaExpr(node.head.unary.child, discardTopExprReg);
                        return NumberType;
                    },
                    .not => {
                        _ = try self.semaExpr(node.head.unary.child, discardTopExprReg);
                        return BoolType;
                    },
                    // else => return self.reportError("Unsupported unary op: {}", .{op}, node),
                }
            },
            .bin_expr => {
                const left = node.head.left_right.left;
                const right = node.head.left_right.right;

                const op = @intToEnum(cy.BinaryExprOp, node.head.left_right.extra);
                switch (op) {
                    .plus => {
                        const ltype = try self.semaExpr(left, discardTopExprReg);
                        _ = try self.semaExpr(right, discardTopExprReg);
                        if (ltype.typeT == .string) {
                            return StringType;
                        } else {
                            return NumberType;
                        }
                    },
                    .star => {
                        _ = try self.semaExpr(left, discardTopExprReg);
                        _ = try self.semaExpr(right, discardTopExprReg);
                        return NumberType;
                    },
                    .slash => {
                        _ = try self.semaExpr(left, discardTopExprReg);
                        _ = try self.semaExpr(right, discardTopExprReg);
                        return NumberType;
                    },
                    .percent => {
                        _ = try self.semaExpr(left, discardTopExprReg);
                        _ = try self.semaExpr(right, discardTopExprReg);
                        return NumberType;
                    },
                    .caret => {
                        _ = try self.semaExpr(left, discardTopExprReg);
                        _ = try self.semaExpr(right, discardTopExprReg);
                        return NumberType;
                    },
                    .minus => {
                        _ = try self.semaExpr(left, discardTopExprReg);
                        _ = try self.semaExpr(right, discardTopExprReg);
                        return NumberType;
                    },
                    .bitwiseAnd => {
                        _ = try self.semaExpr(left, discardTopExprReg);
                        _ = try self.semaExpr(right, discardTopExprReg);
                        return NumberType;
                    },
                    .and_op => {
                        const ltype = try self.semaExpr(left, discardTopExprReg);
                        const rtype = try self.semaExpr(right, discardTopExprReg);
                        if (ltype.typeT == rtype.typeT) {
                            return ltype;
                        } else return AnyType;
                    },
                    .or_op => {
                        const ltype = try self.semaExpr(left, discardTopExprReg);
                        const rtype = try self.semaExpr(right, discardTopExprReg);
                        if (ltype.typeT == rtype.typeT) {
                            return ltype;
                        } else return AnyType;
                    },
                    .bang_equal => {
                        _ = try self.semaExpr(left, discardTopExprReg);
                        _ = try self.semaExpr(right, discardTopExprReg);
                        return BoolType;
                    },
                    .equal_equal => {
                        _ = try self.semaExpr(left, discardTopExprReg);
                        _ = try self.semaExpr(right, discardTopExprReg);
                        return BoolType;
                    },
                    .less => {
                        _ = try self.semaExpr(left, discardTopExprReg);
                        _ = try self.semaExpr(right, discardTopExprReg);
                        return BoolType;
                    },
                    .less_equal => {
                        _ = try self.semaExpr(left, discardTopExprReg);
                        _ = try self.semaExpr(right, discardTopExprReg);
                        return BoolType;
                    },
                    .greater => {
                        _ = try self.semaExpr(left, discardTopExprReg);
                        _ = try self.semaExpr(right, discardTopExprReg);
                        return BoolType;
                    },
                    .greater_equal => {
                        _ = try self.semaExpr(left, discardTopExprReg);
                        _ = try self.semaExpr(right, discardTopExprReg);
                        return BoolType;
                    },
                    else => return self.reportError("Unsupported binary op: {}", .{op}, node),
                }
            },
            .coyield => {
                return AnyType;
            },
            .costart => {
                _ = try self.semaExpr(node.head.child_head, discardTopExprReg);
                return FiberType;
            },
            .call_expr => {
                const callee = self.nodes[node.head.func_call.callee];
                if (!node.head.func_call.has_named_arg) {
                    if (callee.node_t == .access_expr) {
                        const right = self.nodes[callee.head.left_right.right];
                        if (right.node_t == .ident) {
                            var numArgs: u32 = 1;
                            var arg_id = node.head.func_call.arg_head;
                            while (arg_id != NullId) : (numArgs += 1) {
                                const arg = self.nodes[arg_id];
                                _ = try self.semaExpr(arg_id, false);
                                arg_id = arg.next;
                            }

                            _ = try self.semaExpr(callee.head.left_right.left, false);

                            return AnyType;
                        } else return self.reportError("Unsupported callee", .{}, node);
                    } else if (callee.node_t == .ident) {
                        if (try self.semaReadVar(node.head.func_call.callee)) |info| {
                            _ = info;
                            var numArgs: u32 = 1;
                            var arg_id = node.head.func_call.arg_head;
                            while (arg_id != NullId) : (numArgs += 1) {
                                const arg = self.nodes[arg_id];
                                _ = try self.semaExpr(arg_id, false);
                                arg_id = arg.next;
                            }

            //                 // Load callee after args so it can be easily discarded.
            //                 try self.genLoadLocal(info);
                            return AnyType;
                        } else {
                            var numArgs: u32 = 0;
                            var arg_id = node.head.func_call.arg_head;
                            while (arg_id != NullId) : (numArgs += 1) {
                                const arg = self.nodes[arg_id];
                                _ = try self.semaExpr(arg_id, false);
                                arg_id = arg.next;
                            }
                            return AnyType;
                        }
                    } else return self.reportError("Unsupported callee", .{}, node);
                } else return self.reportError("Unsupported named args", .{}, node);
            },
            .lambda_expr => {
                if (!discardTopExprReg) {
                    try self.pushSemaBlock();
                    defer self.endSemaBlock();

                    // Generate function body.
                    const func = self.funcDecls[node.head.func.decl_id];
                    try self.pushSemaFuncParamVars(func);
                    _ = try self.semaExpr(node.head.func.body_head, false);
                }
                return AnyType;
            },
            else => return self.reportError("Unsupported node", .{}, node),
        }
    }

    fn semaStmt(self: *VMcompiler, node: cy.Node, comptime discardTopExprReg: bool) !void {
        // log.debug("sema stmt {}", .{node.node_t});
        switch (node.node_t) {
            .pass_stmt => {
                return;
            },
            .expr_stmt => {
                _ = try self.semaExpr(node.head.child_head, discardTopExprReg);
            },
            .break_stmt => {
                return;
            },
            .add_assign_stmt => {
                const left = self.nodes[node.head.left_right.left];
                if (left.node_t == .ident) {
                    if (try self.semaReadVar(node.head.left_right.left)) |svar| {
                        const rtype = try self.semaExpr(node.head.left_right.right, false);
                        if (svar.vtype.typeT != .number and svar.vtype.typeT != .any and rtype.typeT != svar.vtype.typeT) {
                            return self.reportError("Type mismatch: Expected {}", .{svar.vtype.typeT}, node);
                        }
                    } else stdx.panic("variable not declared");
                } else {
                    stdx.panicFmt("unsupported assignment to left {}", .{left.node_t});
                }
            },
            .assign_stmt => {
                const left = self.nodes[node.head.left_right.left];
                if (left.node_t == .ident) {
                    if (try self.semaReadVar(node.head.left_right.left)) |svar| {
                        if (svar.isCaptured) {
                            return self.reportError("TODO: reassign to a captured variable.", .{}, left);
                        }
                    }

                    const rtype = try self.semaExpr(node.head.left_right.right, false);
                    _ = try self.semaSetVar(node.head.left_right.left, rtype);
                } else if (left.node_t == .arr_access_expr) {
                    _ = try self.semaExpr(left.head.left_right.left, false);
                    _ = try self.semaExpr(left.head.left_right.right, false);
                    _ = try self.semaExpr(node.head.left_right.right, false);
                } else if (left.node_t == .access_expr) {
                    _ = try self.semaExpr(left.head.left_right.left, false);
                    _ = try self.semaExpr(left.head.left_right.right, false);
                    _ = try self.semaExpr(node.head.left_right.right, false);
                } else {
                    stdx.panicFmt("unsupported assignment to left {}", .{left.node_t});
                }
            },
            .structDecl => {
                const nameN = self.nodes[node.head.structDecl.name];
                const name = self.getNodeTokenString(nameN);

                if (self.vm.getStruct(name) != null) {
                    log.debug("struct already exists", .{});
                    return error.CompileError;
                }

                var funcId = node.head.structDecl.funcsHead;
                while (funcId != NullId) {
                    const func = self.nodes[funcId];
                    const decl = self.funcDecls[func.head.func.decl_id];

                    if (decl.params.end > decl.params.start) {
                        const param = self.funcParams[decl.params.start];
                        const paramName = self.src[param.name.start..param.name.end];
                        if (std.mem.eql(u8, paramName, "self")) {
                            // Struct method.
                            try self.pushSemaBlock();
                            try self.pushSemaMethodParamVars(decl);
                            try self.semaStmts(func.head.func.body_head, false);
                            self.endSemaBlock();
                            funcId = func.next;
                            continue;
                        }
                    }
                    try self.pushSemaBlock();
                    try self.pushSemaFuncParamVars(decl);
                    try self.semaStmts(func.head.func.body_head, false);
                    self.endSemaBlock();
                    funcId = func.next;
                }
            },
            .func_decl => {
                const func = self.funcDecls[node.head.func.decl_id];

                try self.pushSemaBlock();
                try self.pushSemaFuncParamVars(func);
                try self.semaStmts(node.head.func.body_head, false);
                self.endSemaBlock();
            },
            .for_cond_stmt => {
                try self.pushSemaIterSubBlock();

                _ = try self.semaExpr(node.head.left_right.left, false);

                try self.semaStmts(node.head.left_right.right, false);

                try self.endSemaIterSubBlock();
            },
            .for_inf_stmt => {
                try self.pushSemaIterSubBlock();
                try self.semaStmts(node.head.child_head, false);
                try self.endSemaIterSubBlock();
            },
            .for_iter_stmt => {
                try self.pushSemaIterSubBlock();

                _ = try self.semaExpr(node.head.for_iter_stmt.iterable, false);

                const as_clause = self.nodes[node.head.for_iter_stmt.as_clause];
                _ = try self.semaEnsureVar(as_clause.head.as_iter_clause.value, NumberType);

                try self.semaStmts(node.head.for_iter_stmt.body_head, false);
                try self.endSemaIterSubBlock();
            },
            .for_range_stmt => {
                try self.pushSemaIterSubBlock();

                if (node.head.for_range_stmt.as_clause != NullId) {
                    const asClause = self.nodes[node.head.for_range_stmt.as_clause];
                    _ = try self.semaEnsureVar(asClause.head.as_range_clause.ident, NumberType);
                }

                const range_clause = self.nodes[node.head.for_range_stmt.range_clause];
                _ = try self.semaExpr(range_clause.head.left_right.left, false);
                _ = try self.semaExpr(range_clause.head.left_right.right, false);

                try self.semaStmts(node.head.for_range_stmt.body_head, false);
                try self.endSemaIterSubBlock();
            },
            .if_stmt => {
                _ = try self.semaExpr(node.head.left_right.left, false);

                try self.pushSemaSubBlock();
                try self.semaStmts(node.head.left_right.right, false);
                self.endSemaSubBlock();

                var elseClauseId = node.head.left_right.extra;
                while (elseClauseId != NullId) {
                    const elseClause = self.nodes[elseClauseId];
                    if (elseClause.head.else_clause.cond == NullId) {
                        try self.pushSemaSubBlock();
                        try self.semaStmts(elseClause.head.else_clause.body_head, false);
                        self.endSemaSubBlock();
                        break;
                    } else {
                        _ = try self.semaExpr(elseClause.head.else_clause.cond, false);

                        try self.pushSemaSubBlock();
                        try self.semaStmts(elseClause.head.else_clause.body_head, false);
                        self.endSemaSubBlock();
                        elseClauseId = elseClause.head.else_clause.else_clause;
                    }
                }
            },
            .return_stmt => {
                return;
            },
            .return_expr_stmt => {
                _ = try self.semaExpr(node.head.child_head, false);
            },
            else => return self.reportError("Unsupported node", .{}, node),
        }
    }

    fn semaStmts(self: *VMcompiler, head: cy.NodeId, comptime attachEnd: bool) anyerror!void {
        var cur_id = head;
        while (cur_id != NullId) {
            const node = self.nodes[cur_id];
            if (attachEnd) {
                if (node.next == NullId) {
                    try self.semaStmt(node, false);
                } else {
                    try self.semaStmt(node, true);
                }
            } else {
                try self.semaStmt(node, true);
            }
            cur_id = node.next;
        }
    }

    fn nextSemaSubBlock(self: *VMcompiler) void {
        self.curSemaSubBlockId = self.nextSemaSubBlockId;
        self.nextSemaSubBlockId += 1;

        const ssblock = self.curSemaSubBlock();
        for (ssblock.iterVarBeginTypes.items) |varAndType| {
            const svar = &self.vars.items[varAndType.id];
            svar.vtype = varAndType.vtype;
            svar.genIsDefined = true;
        }
    }

    fn prevSemaSubBlock(self: *VMcompiler) void {
        self.curSemaSubBlockId = self.curSemaSubBlock().prevSubBlockId;
    }

    fn nextSemaBlock(self: *VMcompiler) void {
        self.curSemaBlockId = self.nextSemaBlockId;
        self.nextSemaBlockId += 1;
        self.nextSemaSubBlock();
    }

    fn prevSemaBlock(self: *VMcompiler) void {
        self.curSemaBlockId = self.curSemaBlock().prevBlockId;
        self.prevSemaSubBlock();
    }

    /// Reserve locals upfront and gen var initializer if necessary.
    fn genInitLocals(self: *VMcompiler) !void {
        const sblock = self.curSemaBlock();

        // Reserve the locals.
        var numInitializers: u32 = 0;
        for (sblock.locals.items) |varId| {
            const svar = self.genGetVar(varId).?;
            _ = try self.reserveLocalVar(varId);
            if (svar.genInitializer) {
                numInitializers += 1;
            }
        }

        if (numInitializers > 0) {
            try self.buf.pushOp1(.setInitN, @intCast(u8, numInitializers));
            for (sblock.locals.items) |varId| {
                const svar = self.genGetVar(varId).?;
                if (svar.genInitializer) {
                    try self.buf.pushOperand(svar.local);
                }
            }
        }
    }

    pub fn compile(self: *VMcompiler, ast: cy.ParseResultView) !ResultView {
        self.buf.clear();
        self.blocks.clearRetainingCapacity();

        for (self.semaBlocks.items) |*block| {
            block.deinit(self.alloc);
        }
        self.semaBlocks.clearRetainingCapacity();

        for (self.semaSubBlocks.items) |*block| {
            block.deinit(self.alloc);
        }
        self.semaSubBlocks.clearRetainingCapacity();

        // Dummy first element to avoid len > 0 check during pop.
        try self.semaSubBlocks.append(self.alloc, SemaSubBlock.init(0, 0));
        try self.semaBlocks.append(self.alloc, SemaBlock.init(0));

        self.nodes = ast.nodes.items;
        self.funcDecls = ast.func_decls.items;
        self.funcParams = ast.func_params;
        self.src = ast.src;
        self.tokens = ast.tokens;
        self.semaBlockDepth = 0;

        const root = self.nodes[ast.root_id];

        try self.pushSemaBlock();
        self.semaStmts(root.head.child_head, true) catch {
            return ResultView{
                .buf = self.buf,
                .hasError = true,
            };
        };
        self.endSemaBlock();

        self.nextSemaBlockId = 1;
        self.nextSemaSubBlockId = 1;
        _ = self.nextSemaBlock();
        try self.pushBlock();
        try self.genInitLocals();
        self.genStatements(root.head.child_head, true) catch {
            if (dumpCompileErrorStackTrace) {
                std.debug.dumpStackTrace(@errorReturnTrace().?.*);
            }
            return ResultView{
                .buf = self.buf,
                .hasError = true,
            };
        };
        self.popBlock();
        self.buf.mainLocalSize = @intCast(u32, self.blockNumLocals());

        // Merge inst and const buffers.
        var reqLen = self.buf.ops.items.len + self.buf.consts.items.len * @sizeOf(cy.Const) + @alignOf(cy.Const) - 1;
        if (self.buf.ops.capacity < reqLen) {
            try self.buf.ops.ensureTotalCapacityPrecise(self.alloc, reqLen);
        }
        const constAddr = std.mem.alignForward(@ptrToInt(self.buf.ops.items.ptr) + self.buf.ops.items.len, @alignOf(cy.Const));
        const constDst = @intToPtr([*]cy.Const, constAddr)[0..self.buf.consts.items.len];
        std.mem.copy(cy.Const, constDst, self.buf.consts.toOwnedSlice(self.alloc));
        self.buf.mconsts = constDst;

        return ResultView{
            .buf = self.buf,
            .hasError = false,
        };
    }

    fn endLocals(self: *VMcompiler) !void {
        const sblock = self.curSemaBlock();
        for (sblock.params.items) |varId| {
            const svar = self.vars.items[varId];
            if (svar.lifetimeRcCandidate and !svar.isCaptured) {
                try self.buf.pushOp1(.release, svar.local);
            }
        }
        for (sblock.locals.items) |varId| {
            const svar = self.vars.items[varId];
            if (svar.lifetimeRcCandidate and svar.genIsDefined) {
                try self.buf.pushOp1(.release, svar.local);
            }
        }
    }

    fn pushContTo(self: *VMcompiler, toPc: usize) !void {
        const pc = self.buf.ops.items.len;
        try self.buf.pushOp2(.cont, 0, 0);
        self.buf.setOpArgU16(pc + 1, @intCast(u16, pc - toPc));
    }

    fn pushJumpBackTo(self: *VMcompiler, toPc: usize) !void {
        const pc = self.buf.ops.items.len;
        try self.buf.pushOp2(.jumpBack, 0, 0);
        self.buf.setOpArgU16(pc + 1, @intCast(u16, pc - toPc));
    }

    fn pushEmptyJump(self: *VMcompiler) !u32 {
        const start = @intCast(u32, self.buf.ops.items.len);
        try self.buf.pushOp2(.jump, 0, 0);
        return start;
    }

    fn pushEmptyJumpCondKeep(self: *VMcompiler) !u32 {
        const start = @intCast(u32, self.buf.ops.items.len);
        try self.buf.pushOp2(.jumpCondKeep, 0, 0);
        return start;
    }

    fn pushEmptyJumpNotCondKeep(self: *VMcompiler) !u32 {
        const start = @intCast(u32, self.buf.ops.items.len);
        try self.buf.pushOp2(.jumpNotCondKeep, 0, 0);
        return start;
    }

    fn pushEmptyJumpNotCond(self: *VMcompiler) !u32 {
        const start = @intCast(u32, self.buf.ops.items.len);
        try self.buf.pushOp2(.jumpNotCond, 0, 0);
        return start;
    }

    fn patchJumpToCurrent(self: *VMcompiler, jumpPc: u32) void {
        self.buf.setOpArgU16(jumpPc + 1, @intCast(u16, self.buf.ops.items.len - jumpPc));
    }

    fn patchSubBlockJumps(self: *VMcompiler, jumpStackStart: usize) void {
        for (self.subBlockJumpStack.items[jumpStackStart..]) |jump| {
            self.patchJumpToCurrent(jump.pc);
        }
    }

    fn patchBlockJumps(self: *VMcompiler, jumpStackStart: usize) void {
        for (self.blockJumpStack.items[jumpStackStart..]) |jump| {
            switch (jump.jumpT) {
                .jumpToEndLocals => {
                    self.buf.setOpArgU16(jump.pc + 1, @intCast(u16, self.curBlock.endLocalsPc - jump.pc));
                }
            }
        }
    }

    fn pushSemaSubBlock(self: *VMcompiler) !void {
        self.curSemaBlock().subBlockDepth += 1;
        const prev = self.curSemaSubBlockId;
        self.curSemaSubBlockId = @intCast(u32, self.semaSubBlocks.items.len);
        try self.semaSubBlocks.append(self.alloc, SemaSubBlock.init(prev, self.assignedVarStack.items.len));
    }

    fn pushSemaIterSubBlock(self: *VMcompiler) !void {
        try self.pushSemaSubBlock();
    }

    fn endSemaIterSubBlock(self: *VMcompiler) !void {
        const ssblock = self.curSemaSubBlock();
        for (self.assignedVarStack.items[ssblock.assignedVarStart..]) |varId| {
            const svar = self.vars.items[varId];
            if (ssblock.prevVarTypes.get(varId)) |prevt| {
                if (svar.vtype.typeT != prevt.typeT) {
                    // Type differs from prev scope type. Record change for iter block codegen.
                    try ssblock.iterVarBeginTypes.append(self.alloc, .{
                        .id = varId,
                        .vtype = AnyType,
                    });
                }
            } else {
                // First assigned in iter block. Record change for iter block codegen.
                try ssblock.iterVarBeginTypes.append(self.alloc, .{
                    .id = varId,
                    .vtype = svar.vtype,
                });
            }
        }
        self.endSemaSubBlock();
    }

    fn endSemaSubBlock(self: *VMcompiler) void {
        const sblock = self.curSemaBlock();
        sblock.subBlockDepth -= 1;
        const ssblock = self.curSemaSubBlock();

        // Merge types back to parent scope.
        for (self.assignedVarStack.items[ssblock.assignedVarStart..]) |varId| {
            const svar = &self.vars.items[varId];
            if (ssblock.prevVarTypes.get(varId)) |prevt| {
                if (svar.vtype.typeT != prevt.typeT) {
                    svar.vtype = AnyType;
                }
            }
        }
        ssblock.prevVarTypes.deinit(self.alloc);
        self.curSemaSubBlockId = ssblock.prevSubBlockId;
        self.assignedVarStack.items.len = ssblock.assignedVarStart;
    }

    fn curSemaSubBlock(self: *VMcompiler) *SemaSubBlock {
        return &self.semaSubBlocks.items[self.curSemaSubBlockId];
    }

    fn curSemaBlock(self: *VMcompiler) *SemaBlock {
        return &self.semaBlocks.items[self.curSemaBlockId];
    }

    fn pushSemaBlock(self: *VMcompiler) !void {
        try self.pushBlock();
        const prevId = self.curSemaBlockId;
        self.curSemaBlockId = @intCast(u32, self.semaBlocks.items.len);
        try self.semaBlocks.append(self.alloc, SemaBlock.init(prevId));
        self.semaBlockDepth += 1;
        try self.pushSemaSubBlock();
    }

    fn endSemaBlock(self: *VMcompiler) void {
        self.endSemaSubBlock();
        const sblock = self.curSemaBlock();
        sblock.nameToVar.deinit(self.alloc);
        self.curSemaBlockId = sblock.prevBlockId;
        self.popBlock();
        self.semaBlockDepth -= 1;
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

    fn blockNumLocals(self: *VMcompiler) usize {
        return self.curSemaBlock().locals.items.len + self.curSemaBlock().params.items.len;
    }

    fn semaSetVar(self: *VMcompiler, ident: cy.NodeId, vtype: Type) !void {
        const node = self.nodes[ident];
        const name = self.getNodeTokenString(node);
        // log.debug("set var {s}", .{name});
        if (self.curSemaBlock().nameToVar.get(name)) |varId| {
            const svar = &self.vars.items[varId];

            const ssblock = self.curSemaSubBlock();
            if (!ssblock.prevVarTypes.contains(varId)) {
                // Same variable but branched to sub block.
                try ssblock.prevVarTypes.put(self.alloc, varId, svar.vtype);
                try self.assignedVarStack.append(self.alloc, varId);
            }

            // Update current type after checking for branched assignment.
            if (svar.vtype.typeT != vtype.typeT) {
                svar.vtype = vtype;
                if (!svar.lifetimeRcCandidate and vtype.rcCandidate) {
                    svar.lifetimeRcCandidate = true;
                }
            }

            self.nodes[ident].head.ident.semaVarId = varId;
        } else {
            const id = try self.pushSemaLocalVar(name, vtype);
            const sblock = self.curSemaBlock();
            if (sblock.subBlockDepth > 1) {
                self.vars.items[id].genInitializer = true;
            }
            self.nodes[ident].head.ident.semaVarId = id;

            try self.assignedVarStack.append(self.alloc, id);
        }
    }

    /// Retrieve the SemaVar and set the id onto the node for codegen.
    /// First checks current scope and then the immediate parent scope.
    fn semaReadVar(self: *VMcompiler, ident: cy.NodeId) !?SemaVar {
        const node = self.nodes[ident];
        const name = self.getNodeTokenString(node);
        const sblock = self.curSemaBlock();
        if (sblock.nameToVar.get(name)) |varId| {
            self.nodes[ident].head.ident.semaVarId = varId;
            return self.vars.items[varId];
        }
        // Only check one block above.
        if (self.semaBlockDepth > 1) {
            const prev = self.semaBlocks.items[sblock.prevBlockId];
            if (prev.nameToVar.get(name)) |varId| {
                // Create a new captured variable.
                const svar = self.vars.items[varId];
                const id = try self.pushSemaCapturedVar(name, varId, svar.vtype);

                self.nodes[ident].head.ident.semaVarId = id;
                return self.vars.items[id];
            }
        }

        // Undefined var.
        self.nodes[ident].head.ident.semaVarId = NullId;
        return null;
    }

    fn genStatements(self: *VMcompiler, head: cy.NodeId, comptime attachEnd: bool) anyerror!void {
        var cur_id = head;
        while (cur_id != NullId) {
            const node = self.nodes[cur_id];
            if (attachEnd) {
                if (node.next == NullId) {
                    try self.genStatement(cur_id, false);
                } else {
                    try self.genStatement(cur_id, true);
                }
            } else {
                try self.genStatement(cur_id, true);
            }
            cur_id = node.next;
        }
        if (attachEnd) {
            try self.endLocals();
            try self.buf.pushOp(.end);
        }
    }

    fn semaGetVarPtr(self: *VMcompiler, name: []const u8) ?*SemaVar {
        if (self.curSemaBlock().nameToVar.get(name)) |varId| {
            return &self.vars.items[varId];
        } else return null;
    }

    fn genGetVarPtr(self: *const VMcompiler, id: SemaVarId) ?*SemaVar {
        if (id != NullId) {
            return &self.vars.items[id];
        } else {
            return null;
        }
    }

    fn genGetVar(self: *const VMcompiler, id: SemaVarId) ?SemaVar {
        if (id != NullId) {
            return self.vars.items[id];
        } else {
            return null;
        }
    }

    fn pushSemaCapturedVar(self: *VMcompiler, name: []const u8, parentVarId: SemaVarId, vtype: Type) !SemaVarId {
        const id = try self.pushSemaVar(name, vtype);
        self.vars.items[id].isCaptured = true;
        try self.capVarParents.put(self.alloc, id, parentVarId);
        try self.curSemaBlock().params.append(self.alloc, id);
        return id;
    }

    fn pushSemaLocalVar(self: *VMcompiler, name: []const u8, vtype: Type) !SemaVarId {
        const id = try self.pushSemaVar(name, vtype);
        try self.curSemaBlock().locals.append(self.alloc, id);
        return id;
    }

    fn semaEnsureVar(self: *VMcompiler, ident: cy.NodeId, vtype: Type) !SemaVarId {
        const node = self.nodes[ident];
        const name = self.getNodeTokenString(node);
        if (self.curSemaBlock().nameToVar.get(name)) |varId| {
            self.nodes[ident].head.ident.semaVarId = varId;
            return varId;
        } else {
            const id = try self.pushSemaLocalVar(name, vtype);
            self.nodes[ident].head.ident.semaVarId = id;
            return id;
        }
    }

    fn pushSemaVar(self: *VMcompiler, name: []const u8, vtype: Type) !SemaVarId {
        const sblock = self.curSemaBlock();
        const id = @intCast(u32, self.vars.items.len);
        const res = try sblock.nameToVar.getOrPut(self.alloc, name);
        if (res.found_existing) {
            return error.VarExists;
        } else {
            res.value_ptr.* = id;
            try self.vars.append(self.alloc, .{
                .name = name,
                .vtype = vtype,
                .lifetimeRcCandidate = vtype.rcCandidate,
            });
            return id;
        }
    }

    fn reserveLocalVar(self: *VMcompiler, varId: SemaVarId) !LocalId {
        const local = try self.curBlock.reserveLocal();
        self.vars.items[varId].local = local;
        return local;
    }

    fn genSetVar(self: *VMcompiler, varId: SemaVarId, vtype: Type) !u8 {
        // log.debug("gensetvar {}", .{varId});
        if (self.genGetVarPtr(varId)) |svar| {
            if (svar.genIsDefined) {
                if (svar.vtype.rcCandidate) {
                    // log.debug("releaseSet {} {}", .{varId, svar.vtype.typeT});
                    try self.buf.pushOp1(.releaseSet, svar.local);
                } else {
                    // log.debug("set {} {}", .{varId, svar.vtype.typeT});
                    try self.buf.pushOp1(.set, svar.local);
                }
                if (svar.vtype.typeT != vtype.typeT) {
                    svar.vtype = vtype;
                }
            } else {
                try self.buf.pushOp1(.set, svar.local);
                svar.genIsDefined = true;
                svar.vtype = vtype;
            }
            return svar.local;
        } else {
            log.debug("Undefined var.", .{});
            return error.CompileError;
        }
    }

    fn pushSemaMethodParamVars(self: *VMcompiler, func: cy.FuncDecl) !void {
        const sblock = self.curSemaBlock();

        if (func.params.end > func.params.start + 1) {
            for (self.funcParams[func.params.start + 2..func.params.end]) |param| {
                const paramName = self.src[param.name.start..param.name.end];
                const paramT = AnyType;
                const id = try self.pushSemaVar(paramName, paramT);
                try sblock.params.append(self.alloc, id);
            }

            // Add self receiver param.
            var id = try self.pushSemaVar("self", AnyType);
            try sblock.params.append(self.alloc, id);

            // First arg is copied to the end before the function call.
            const param = self.funcParams[func.params.start+1];
            const paramName = self.src[param.name.start..param.name.end];
            const paramT = AnyType;
            id = try self.pushSemaVar(paramName, paramT);
            try sblock.params.append(self.alloc, id);
        } else {
            // Add self receiver param.
            const id = try self.pushSemaVar("self", AnyType);
            try sblock.params.append(self.alloc, id);
        }
    }

    fn pushSemaFuncParamVars(self: *VMcompiler, func: cy.FuncDecl) !void {
        const sblock = self.curSemaBlock();

        if (func.params.end > func.params.start) {
            for (self.funcParams[func.params.start+1..func.params.end]) |param| {
                const paramName = self.src[param.name.start..param.name.end];
                const paramT = AnyType;
                const id = try self.pushSemaVar(paramName, paramT);
                try sblock.params.append(self.alloc, id);
            }
            // First arg is copied to the end before the function call.
            const param = self.funcParams[func.params.start];
            const paramName = self.src[param.name.start..param.name.end];
            const paramT = AnyType;
            const id = try self.pushSemaVar(paramName, paramT);
            try sblock.params.append(self.alloc, id);
        }
    }

    // Reserve params and captured vars.
    fn reserveFuncParams(self: *VMcompiler) !void {
        // First local is reserved for the return info.
        // This slightly reduces cycles at function call time.
        _ = try self.curBlock.reserveLocal();

        const sblock = self.curSemaBlock();
        for (sblock.params.items) |varId| {
            _ = try self.reserveLocalVar(varId);
        }
    }

    /// discardTopExprReg is usually true since statements aren't expressions and evaluating child expressions
    /// would just grow the register stack unnecessarily. However, the last main statement requires the
    /// resulting expr to persist to return from `eval`.
    fn genStatement(self: *VMcompiler, nodeId: cy.NodeId, comptime discardTopExprReg: bool) !void {
        // log.debug("gen stmt {}", .{node.node_t});

        const node = self.nodes[nodeId];
        switch (node.node_t) {
            .pass_stmt => {
                return;
            },
            .expr_stmt => {
                _ = try self.genExpr(node.head.child_head, discardTopExprReg);
            },
            .break_stmt => {
                const pc = try self.pushEmptyJump();
                try self.subBlockJumpStack.append(self.alloc, .{ .pc = pc });
            },
            .add_assign_stmt => {
                const left = self.nodes[node.head.left_right.left];
                if (left.node_t == .ident) {
                    if (self.genGetVar(left.head.ident.semaVarId)) |svar| {
                        const rtype = try self.genExpr(node.head.left_right.right, false);
                        if (svar.vtype.typeT != .number and svar.vtype.typeT != .any and rtype.typeT != svar.vtype.typeT) {
                            return self.reportError("Type mismatch: Expected {}", .{svar.vtype.typeT}, node);
                        }
                        try self.buf.pushOp1(.addSet, svar.local);
                    } else stdx.panic("variable not declared");
                } else {
                    stdx.panicFmt("unsupported assignment to left {}", .{left.node_t});
                }
            },
            .assign_stmt => {
                const left = self.nodes[node.head.left_right.left];
                if (left.node_t == .ident) {
                    if (self.genGetVar(left.head.ident.semaVarId)) |svar| {
                        if (svar.isCaptured) {
                            return self.reportError("Can not reassign to a captured variable.", .{}, left);
                        }
                    }
                    const rtype = try self.genMaybeRetainExpr(node.head.left_right.right, false);
                    _ = try self.genSetVar(left.head.ident.semaVarId, rtype);
                } else if (left.node_t == .arr_access_expr) {
                    _ = try self.genExpr(left.head.left_right.left, false);
                    _ = try self.genExpr(left.head.left_right.right, false);
                    _ = try self.genExpr(node.head.left_right.right, false);
                    try self.buf.pushOp(.setIndex);
                } else if (left.node_t == .access_expr) {
                    _ = try self.genExpr(left.head.left_right.left, false);

                    const accessRight = self.nodes[left.head.left_right.right];
                    if (accessRight.node_t != .ident) {
                        log.debug("Expected ident.", .{});
                        return error.CompileError;
                    }

                    _ = try self.genMaybeRetainExpr(node.head.left_right.right, false);

                    const fieldName = self.getNodeTokenString(accessRight);
                    const fieldId = try self.vm.ensureFieldSym(fieldName);
                    try self.buf.pushOp1(.releaseSetField, @intCast(u8, fieldId));
                    try self.pushDebugSym(nodeId);
                } else {
                    stdx.panicFmt("unsupported assignment to left {}", .{left.node_t});
                }
            },
            .structDecl => {
                const nameN = self.nodes[node.head.structDecl.name];
                const name = self.getNodeTokenString(nameN);

                const sid = try self.vm.ensureStruct(name);

                var i: u32 = 0;
                var fieldId = node.head.structDecl.fieldsHead;
                while (fieldId != NullId) : (i += 1) {
                    const field = self.nodes[fieldId];
                    fieldId = field.next;
                }
                const numFields = i;

                i = 0;
                fieldId = node.head.structDecl.fieldsHead;
                while (fieldId != NullId) : (i += 1) {
                    const field = self.nodes[fieldId];
                    const fieldName = self.getNodeTokenString(field);
                    const fieldSymId = try self.vm.ensureFieldSym(fieldName);
                    self.vm.setFieldSym(sid, fieldSymId, i, numFields <= 4);
                    fieldId = field.next;
                }

                self.vm.structs.buf[sid].numFields = i;

                var funcId = node.head.structDecl.funcsHead;
                var func: cy.Node = undefined;
                while (funcId != NullId) : (funcId = func.next) {
                    func = self.nodes[funcId];
                    const decl = self.funcDecls[func.head.func.decl_id];

                    const funcName = self.src[decl.name.start..decl.name.end];
                    if (decl.params.end > decl.params.start) {
                        const param = self.funcParams[decl.params.start];
                        const paramName = self.src[param.name.start..param.name.end];
                        if (std.mem.eql(u8, paramName, "self")) {
                            // Struct method.
                            try self.genMethodDecl(sid, func, decl, funcName);
                            continue;
                        }
                    }

                    const symPath = try std.fmt.allocPrint(self.alloc, "{s}.{s}", .{name, funcName});
                    try self.vm.funcSymNames.append(self.alloc, symPath);

                    try self.genFuncDecl(funcId, symPath);
                }
            },
            .func_decl => {
                const func = self.funcDecls[node.head.func.decl_id];
                const name = self.src[func.name.start..func.name.end];
                try self.genFuncDecl(nodeId, name);
            },
            .for_cond_stmt => {
                self.nextSemaSubBlock();
                defer self.prevSemaSubBlock();

                const top = @intCast(u32, self.buf.ops.items.len);
                _ = try self.genExpr(node.head.left_right.left, false);

                var jumpPc = try self.pushEmptyJumpNotCond();

                try self.genStatements(node.head.left_right.right, false);
                try self.pushJumpBackTo(top);

                self.patchJumpToCurrent(jumpPc);
            },
            .for_inf_stmt => {
                self.nextSemaSubBlock();

                const pcSave = @intCast(u32, self.buf.ops.items.len);
                const jumpStackSave = @intCast(u32, self.subBlockJumpStack.items.len);
                defer {
                    self.patchSubBlockJumps(jumpStackSave);
                    self.subBlockJumpStack.items.len = jumpStackSave;
                    self.prevSemaSubBlock();
                }

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
                try self.pushJumpBackTo(pcSave);
            },
            .for_iter_stmt => {
                self.nextSemaSubBlock();
                defer self.prevSemaSubBlock();

                _ = try self.genExpr(node.head.for_iter_stmt.iterable, false);

                const as_clause = self.nodes[node.head.for_iter_stmt.as_clause];
                const ident = self.nodes[as_clause.head.as_iter_clause.value];
                const val = self.genGetVar(ident.head.ident.semaVarId).?;

                const forPc = self.buf.ops.items.len;
                try self.buf.pushOp3(.forIter, val.local, 0, 0);

                const bodyPc = self.buf.ops.items.len;
                try self.genStatements(node.head.for_iter_stmt.body_head, false);
                try self.pushContTo(bodyPc);

                self.buf.setOpArgU16(forPc+2, @intCast(u16, self.buf.ops.items.len - forPc));
            },
            .for_range_stmt => {
                self.nextSemaSubBlock();
                defer self.prevSemaSubBlock();

                var local: u8 = NullByteId;
                if (node.head.for_range_stmt.as_clause != NullId) {
                    const asClause = self.nodes[node.head.for_range_stmt.as_clause];
                    const ident = self.nodes[asClause.head.as_range_clause.ident];
                    local = self.genGetVar(ident.head.ident.semaVarId).?.local;

                    // inc = as_clause.head.as_range_clause.inc;
                    // if (as_clause.head.as_range_clause.step != NullId) {
                    //     step = self.nodes[as_clause.head.as_range_clause.step];
                    // }
                }

                // Push range start/end.
                const range_clause = self.nodes[node.head.for_range_stmt.range_clause];
                _ = try self.genExpr(range_clause.head.left_right.left, false);
                _ = try self.genExpr(range_clause.head.left_right.right, false);

                // Push custom step.
                const stepConst = try self.buf.pushConst(.{ .val = f64One.val });
                try self.buf.pushOp1(.pushConst, @intCast(u8, stepConst));

                // Push for range op with asLocal.
                const forPc = self.buf.ops.items.len;
                try self.buf.pushOp3(.forRange, local, 0, 0);

                const bodyPc = self.buf.ops.items.len;
                try self.genStatements(node.head.for_range_stmt.body_head, false);
                try self.pushContTo(bodyPc);
                self.buf.setOpArgU16(forPc+2, @intCast(u16, self.buf.ops.items.len - forPc));
            },
            .if_stmt => {
                _ = try self.genExpr(node.head.left_right.left, false);

                var lastCondJump = try self.pushEmptyJumpNotCond();

                self.nextSemaSubBlock();
                try self.genStatements(node.head.left_right.right, false);
                self.prevSemaSubBlock();

                var elseClauseId = node.head.left_right.extra;
                if (elseClauseId != NullId) {
                    const jumpsStart = self.subBlockJumpStack.items.len;
                    defer {
                        self.patchSubBlockJumps(jumpsStart);
                        self.subBlockJumpStack.items.len = jumpsStart;
                    }

                    var endsWithElse = false;
                    while (elseClauseId != NullId) {
                        const pc = try self.pushEmptyJump();
                        try self.subBlockJumpStack.append(self.alloc, .{ .pc = pc });

                        self.patchJumpToCurrent(lastCondJump);

                        const elseClause = self.nodes[elseClauseId];
                        if (elseClause.head.else_clause.cond == NullId) {
                            self.nextSemaSubBlock();
                            try self.genStatements(elseClause.head.else_clause.body_head, false);
                            self.prevSemaSubBlock();
                            endsWithElse = true;
                            break;
                        } else {
                            _ = try self.genExpr(elseClause.head.else_clause.cond, false);

                            lastCondJump = try self.pushEmptyJumpNotCond();

                            self.nextSemaSubBlock();
                            try self.genStatements(elseClause.head.else_clause.body_head, false);
                            self.prevSemaSubBlock();
                            elseClauseId = elseClause.head.else_clause.else_clause;
                        }
                    }

                    if (!endsWithElse) {
                        self.patchJumpToCurrent(lastCondJump);
                    }
                } else {
                    self.patchJumpToCurrent(lastCondJump);
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
                _ = try self.genMaybeRetainExpr(node.head.child_head, false);

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

    fn genLoadLocal(self: *VMcompiler, local: LocalId) !void {
        try self.buf.pushOp1(.load, local);
    }

    fn genMaybeRetainExpr(self: *VMcompiler, nodeId: cy.NodeId, comptime discardTopExprReg: bool) anyerror!Type {
        const node = self.nodes[nodeId];
        if (node.node_t == .ident) {
            if (self.genGetVar(node.head.ident.semaVarId)) |svar| {
                if (svar.vtype.rcCandidate) {
                    try self.buf.pushOp1(.loadRetain, svar.local);
                } else {
                    try self.genLoadLocal(svar.local);
                }
                return svar.vtype;
            } else {
                try self.buf.pushOp(.pushNone);
                return AnyType;
            }
        } else if (node.node_t == .access_expr) {
            const left = self.nodes[node.head.left_right.left];
            _ = try self.genExpr(node.head.left_right.left, discardTopExprReg);

            // right should be an ident.
            const right = self.nodes[node.head.left_right.right];

            const name = self.getNodeTokenString(right);
            const fieldId = try self.vm.ensureFieldSym(name);

            if (!discardTopExprReg) {
                if (shouldReleaseAccessParent(left.node_t)) {
                    try self.buf.pushOp1(.pushFieldRetainParentRelease, @intCast(u8, fieldId));
                } else {
                    try self.buf.pushOp1(.pushFieldRetain, @intCast(u8, fieldId));
                }
            }

            return AnyType;
        } else if (node.node_t == .if_expr) {
            return self.genIfExpr(nodeId, discardTopExprReg, true);
        } else {
            return self.genExpr(nodeId, discardTopExprReg);
        }
    }

    fn genMethodDecl(self: *VMcompiler, structId: cy.StructId, node: cy.Node, func: cy.FuncDecl, name: []const u8) !void {
        // log.debug("gen method {s}", .{name});
        const methodId = try self.vm.ensureMethodSymKey(name);

        const jumpPc = try self.pushEmptyJump();

        try self.pushBlock();
        self.nextSemaBlock();

        const opStart = @intCast(u32, self.buf.ops.items.len);
        try self.reserveFuncParams();
        try self.genInitLocals();
        try self.genStatements(node.head.func.body_head, false);
        // TODO: Check last statement to skip adding ret.
        try self.endLocals();
        try self.buf.pushOp(.ret0);

        // Reserve another local for the call return info.
        const numParams = func.params.end - func.params.start;
        const numLocals = @intCast(u32, self.blockNumLocals() + 1 - numParams);

        self.prevSemaBlock();
        self.popBlock();

        self.patchJumpToCurrent(jumpPc);

        const sym = cy.SymbolEntry.initFunc(opStart, numLocals);
        try self.vm.addMethodSym(structId, methodId, sym);
    }

    fn genFuncDecl(self: *VMcompiler, nodeId: cy.NodeId, symName: []const u8) !void {
        const node = self.nodes[nodeId];
        const func = self.funcDecls[node.head.func.decl_id];
        const symId = try self.vm.ensureFuncSym(symName);

        const jumpPc = try self.pushEmptyJump();

        try self.pushBlock();
        self.nextSemaBlock();
        self.curBlock.frameLoc = nodeId;

        const jumpStackStart = self.blockJumpStack.items.len;
        defer {
            self.patchBlockJumps(jumpStackStart);
            self.blockJumpStack.items.len = jumpStackStart;

            self.prevSemaBlock();
            self.popBlock();
        }

        const opStart = @intCast(u32, self.buf.ops.items.len);
        try self.reserveFuncParams();
        try self.genInitLocals();
        try self.genStatements(node.head.func.body_head, false);
        // TODO: Check last statement to skip adding ret.
        self.curBlock.endLocalsPc = @intCast(u32, self.buf.ops.items.len);
        self.nodes[nodeId].head.func.genEndLocalsPc = self.curBlock.endLocalsPc;
        
        try self.endLocals();
        try self.buf.pushOp(.ret0);

        // Reserve another local for the call return info.
        const numParams = func.params.end - func.params.start;
        const numLocals = @intCast(u32, self.blockNumLocals() + 1 - numParams);

        self.patchJumpToCurrent(jumpPc);

        const sym = cy.FuncSymbolEntry.initFunc(opStart, numLocals);
        self.vm.setFuncSym(symId, sym);
    }

    fn genCallArgs(self: *VMcompiler, first: cy.NodeId) !u32 {
        var numArgs: u32 = 0;
        var argId = first;
        while (argId != NullId) : (numArgs += 1) {
            const arg = self.nodes[argId];
            _ = try self.genMaybeRetainExpr(argId, false);
            argId = arg.next;
        }
        return numArgs;
    }

    fn genCallExpr(self: *VMcompiler, nodeId: cy.NodeId, comptime discardTopExprReg: bool, comptime startFiber: bool) !Type {
        const node = self.nodes[nodeId];
        const callee = self.nodes[node.head.func_call.callee];
        if (!node.head.func_call.has_named_arg) {
            if (callee.node_t == .access_expr) {
                const right = self.nodes[callee.head.left_right.right];
                if (right.node_t == .ident) {
                    const numArgs = 1 + try self.genCallArgs(node.head.func_call.arg_head);

                    const rightName = self.getNodeTokenString(right);
                    const methodId = try self.vm.ensureMethodSymKey(rightName);

                    // var isStdCall = false;
                    const left = self.nodes[callee.head.left_right.left];
                    if (left.node_t == .ident) {
                        // Check if it's a symbol path.
                        const leftName = self.getNodeTokenString(left);
                        try self.u8Buf.resize(self.alloc, leftName.len + rightName.len + 1);
                        std.mem.copy(u8, self.u8Buf.items[0..leftName.len], leftName);
                        self.u8Buf.items[leftName.len] = '.';
                        std.mem.copy(u8, self.u8Buf.items[leftName.len+1..], rightName);

                        if (self.vm.getFuncSym(self.u8Buf.items)) |symId| {
                            if (discardTopExprReg) {
                                try self.buf.pushOp2(.pushCallSym0, @intCast(u8, symId), @intCast(u8, numArgs-1));
                            } else {
                                try self.buf.pushOp2(.pushCallSym1, @intCast(u8, symId), @intCast(u8, numArgs-1));
                            }
                            return AnyType;
                        }

                        // if (try self.readScopedVar(leftName)) |info| {
                        //     if (info.vtype.typeT == ListType.typeT) {
                        //         if (self.vm.hasMethodSym(cy.ListS, methodId)) {
                        //             isStdCall = true;
                        //         } 
                        //     }
                        // }
                    }
                    
                    // if (isStdCall) {
                    //     // Avoid retain/release for std call.
                    //     _ = try self.genExpr(left, false);
                    // } else {
                    //     _ = try self.genMaybeRetainExpr(left, false);
                    // }

                    // Retain is triggered during the function call if it ends up being a vm func.
                    _ = try self.genExpr(callee.head.left_right.left, false);

                    if (discardTopExprReg) {
                        try self.buf.pushOp2(.pushCallObjSym0, @intCast(u8, methodId), @intCast(u8, numArgs));
                        try self.pushDebugSym(nodeId);
                    } else {
                        try self.buf.pushOp2(.pushCallObjSym1, @intCast(u8, methodId), @intCast(u8, numArgs));
                        try self.pushDebugSym(nodeId);
                    }
                    return AnyType;
                } else return self.reportError("Unsupported callee", .{}, node);
            } else if (callee.node_t == .ident) {
                const token = self.tokens[callee.start_token];
                const name = self.src[token.start_pos..token.data.end_pos];

                if (self.genGetVar(callee.head.ident.semaVarId)) |svar| {
                    // Load callee first so it gets overwritten by the retInfo
                    // and avoids a copy operation on the first arg.
                    try self.genLoadLocal(svar.local);

                    var numArgs: u32 = 1;
                    var arg_id = node.head.func_call.arg_head;
                    // Second arg should be pushed first to match callSym layout.
                    if (arg_id != NullId) {
                        const arg = self.nodes[arg_id];
                        arg_id = arg.next;
                    }
                    while (arg_id != NullId) : (numArgs += 1) {
                        const arg = self.nodes[arg_id];
                        _ = try self.genMaybeRetainExpr(arg_id, false);
                        arg_id = arg.next;
                    }
                    if (node.head.func_call.arg_head != NullId) {
                        _ = try self.genMaybeRetainExpr(node.head.func_call.arg_head, false);
                        numArgs += 1;
                    }

                    if (discardTopExprReg) {
                        try self.buf.pushOp1(.pushCall0, @intCast(u8, numArgs));
                    } else {
                        try self.buf.pushOp1(.pushCall1, @intCast(u8, numArgs));
                    }
                    return AnyType;
                } else {
                    const numArgs = try self.genCallArgs(node.head.func_call.arg_head);

                    const costartPc = self.buf.ops.items.len;
                    if (startFiber) {
                        try self.buf.pushOp2(.pushCostart, @intCast(u8, numArgs), 0);
                    }

                    const symId = self.vm.getGlobalFuncSym(name) orelse (try self.vm.ensureFuncSym(name));
                    if (discardTopExprReg) {
                        try self.buf.pushOp2(.pushCallSym0, @intCast(u8, symId), @intCast(u8, numArgs));
                        try self.pushDebugSym(nodeId);
                    } else {
                        try self.buf.pushOp2(.pushCallSym1, @intCast(u8, symId), @intCast(u8, numArgs));
                        try self.pushDebugSym(nodeId);
                    }

                    if (startFiber) {
                        try self.buf.pushOp(.coreturn);
                        self.buf.setOpArgs1(costartPc + 2, @intCast(u8, self.buf.ops.items.len - costartPc));
                    }

                    return AnyType;
                }
            } else return self.reportError("Unsupported callee", .{}, node);
        } else return self.reportError("Unsupported named args", .{}, node);
    }

    fn genIfExpr(self: *VMcompiler, nodeId: cy.NodeId, comptime discardTopExprReg: bool, comptime maybeRetain: bool) !Type {
        const node = self.nodes[nodeId];
        _ = try self.genExpr(node.head.if_expr.cond, false);

        var jumpNotPc = try self.pushEmptyJumpNotCond();

        if (maybeRetain) {
            _ = try self.genMaybeRetainExpr(node.head.if_expr.body_expr, discardTopExprReg);
        } else {
            _ = try self.genExpr(node.head.if_expr.body_expr, discardTopExprReg);
        }

        const jumpPc = try self.pushEmptyJump();

        self.patchJumpToCurrent(jumpNotPc);
        if (node.head.if_expr.else_clause != NullId) {
            const else_clause = self.nodes[node.head.if_expr.else_clause];
            if (maybeRetain) {
                _ = try self.genMaybeRetainExpr(else_clause.head.child_head, discardTopExprReg);
            } else {
                _ = try self.genExpr(else_clause.head.child_head, discardTopExprReg);
            }
        } else {
            if (!discardTopExprReg) {
                try self.buf.pushOp(.pushNone);
            }
        }
        self.patchJumpToCurrent(jumpPc);

        return AnyType;
    }

    fn genExpr(self: *VMcompiler, nodeId: cy.NodeId, comptime discardTopExprReg: bool) anyerror!Type {
        const node = self.nodes[nodeId];
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
            .none => {
                if (!discardTopExprReg) {
                    try self.buf.pushOp(.pushNone);
                }
                return AnyType;
            },
            .arr_literal => {
                var expr_id = node.head.child_head;
                var i: u32 = 0;
                while (expr_id != NullId) : (i += 1) {
                    const expr = self.nodes[expr_id];
                    _ = try self.genExpr(expr_id, discardTopExprReg);
                    expr_id = expr.next;
                }

                if (!discardTopExprReg) {
                    try self.buf.pushOp1(.pushList, @intCast(u8, i));
                }
                return ListType;
            },
            .structInit => {
                const stype = self.nodes[node.head.structInit.structType];
                const sname = self.getNodeTokenString(stype);
                const sid = self.vm.getStruct(sname) orelse {
                    log.debug("Missing struct {s}", .{sname});
                    return error.CompileError;
                };

                const initializer = self.nodes[node.head.structInit.initializer];

                // TODO: Have sema sort the fields so eval can handle default values easier.
                // Push props onto stack.
                const operandStart = self.operandStack.items.len;
                defer self.operandStack.items.len = operandStart;

                var i: u32 = 0;
                var entryId = initializer.head.child_head;
                while (entryId != NullId) : (i += 1) {
                    var entry = self.nodes[entryId];
                    const prop = self.nodes[entry.head.left_right.left];

                    if (!discardTopExprReg) {
                        const propName = self.getNodeTokenString(prop);
                        const propIdx = self.vm.getStructFieldIdx(sid, propName) orelse {
                            log.debug("Missing field {s}", .{propName});
                            return error.CompileError;
                        };
                        try self.operandStack.append(self.alloc, cy.OpData.initArg(@intCast(u8, propIdx)));
                    }

                    _ = try self.genMaybeRetainExpr(entry.head.left_right.right, discardTopExprReg);
                    entryId = entry.next;
                }

                if (i != self.vm.structs.buf[sid].numFields) {
                    log.debug("Default values not supported. {} {}", .{i, self.vm.structs.buf[sid].numFields});
                    return error.CompileError;
                }

                if (!discardTopExprReg) {
                    try self.buf.pushOp2(.pushStructInitSmall, @intCast(u8, sid), @intCast(u8, i));
                    try self.buf.pushOperands(self.operandStack.items[operandStart..]);
                    // try self.buf.pushOp1(.pushStructInit, );
                }
                return AnyType;
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
                                try self.operandStack.append(self.alloc, cy.OpData.initArg(@intCast(u8, idx)));
                            },
                            else => stdx.panicFmt("unsupported key {}", .{key.node_t}),
                        }
                    }

                    _ = try self.genExpr(entry.head.left_right.right, discardTopExprReg);
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
                    const idx = try self.buf.pushConst(cy.Const.init(@bitCast(u64, val)));
                    try self.buf.pushOp1(.pushConst, @intCast(u8, idx));
                }
                return NumberType;
            },
            .string => {
                if (!discardTopExprReg) {
                    const token = self.tokens[node.start_token];
                    const literal = self.src[token.start_pos+1..token.data.end_pos-1];

                    // Unescape single quotes.
                    _ = try replaceIntoShorterList(u8, literal, "\\'", "'", &self.u8Buf, self.alloc);

                    const idx = try self.buf.pushStringConst(self.u8Buf.items);
                    try self.buf.pushOp1(.pushConst, @intCast(u8, idx));
                }
                return ConstStringType;
            },
            .stringTemplate => {
                const first = self.nodes[node.head.stringTemplate.partsHead];
                if (node.head.stringTemplate.firstIsString and first.next == NullId) {
                    // Just a string.
                    if (!discardTopExprReg) {
                        const str = self.getNodeTokenString(first);
                        const idx = try self.buf.pushStringConst(str);
                        try self.buf.pushOp1(.pushConst, @intCast(u8, idx));
                        return ConstStringType;
                    }
                }

                if (!node.head.stringTemplate.firstIsString) {
                    if (!discardTopExprReg) {
                        // Insert empty string.
                        const idx = try self.buf.pushStringConst("");
                        try self.buf.pushOp1(.pushConst, @intCast(u8, idx));
                    }
                }

                const exprStart = self.nodeStack.items.len;
                defer self.nodeStack.items.len = exprStart;

                var nextIsExpr = !node.head.stringTemplate.firstIsString;
                var curId = node.head.stringTemplate.partsHead;
                while (true) {
                    const cur = self.nodes[curId];
                    if (!nextIsExpr) {
                        if (!discardTopExprReg) {
                            const str = self.getNodeTokenString(cur);
                            const idx = try self.buf.pushStringConst(str);
                            try self.buf.pushOp1(.pushConst, @intCast(u8, idx));
                        }
                    } else {
                        // Push all exprs together.
                        try self.nodeStack.append(self.alloc, curId);
                    }
                    if (cur.next == NullId) {
                        if (nextIsExpr) {
                            // Insert empty string.
                            if (!discardTopExprReg) {
                                const idx = try self.buf.pushStringConst("");
                                try self.buf.pushOp1(.pushConst, @intCast(u8, idx));
                            }
                        }
                        break;
                    } else {
                        curId = cur.next;
                        nextIsExpr = !nextIsExpr;
                    }
                }

                const exprs = self.nodeStack.items[exprStart..];
                for (exprs) |exprId| {
                    _ = try self.genExpr(exprId, discardTopExprReg);
                }
                if (!discardTopExprReg) {
                    try self.buf.pushOp1(.pushStringTemplate, @intCast(u8, exprs.len));
                }
                return StringType;
            },
            .ident => {
                if (self.genGetVar(node.head.ident.semaVarId)) |svar| {
                    try self.genLoadLocal(svar.local);
                    return svar.vtype;
                } else {
                    try self.buf.pushOp(.pushNone);
                    return AnyType;
                }
            },
            .if_expr => {
                return self.genIfExpr(nodeId, discardTopExprReg, false);
            },
            .arr_range_expr => {
                _ = try self.genExpr(node.head.arr_range_expr.arr, discardTopExprReg);

                if (node.head.arr_range_expr.left == NullId) {
                    if (!discardTopExprReg) {
                        const idx = try self.buf.pushConst(.{ .val = 0 });
                        try self.buf.pushOp1(.pushConst, @intCast(u8, idx));
                    }
                } else {
                    _ = try self.genExpr(node.head.arr_range_expr.left, discardTopExprReg);
                }
                if (node.head.arr_range_expr.right == NullId) {
                    if (!discardTopExprReg) {
                        const idx = try self.buf.pushConst(.{ .val = f64NegOne.val });
                        try self.buf.pushOp1(.pushConst, @intCast(u8, idx));
                    }
                } else {
                    _ = try self.genExpr(node.head.arr_range_expr.right, discardTopExprReg);
                }

                if (!discardTopExprReg) {
                    try self.buf.pushOp(.pushSlice);
                }
                return ListType;
            },
            .access_expr => {
                const left = self.nodes[node.head.left_right.left];
                _ = try self.genExpr(node.head.left_right.left, discardTopExprReg);

                // right should be an ident.
                const right = self.nodes[node.head.left_right.right];

                const name = self.getNodeTokenString(right);
                const fieldId = try self.vm.ensureFieldSym(name);

                if (!discardTopExprReg) {
                    if (shouldReleaseAccessParent(left.node_t)) {
                        try self.buf.pushOp1(.pushFieldParentRelease, @intCast(u8, fieldId));
                    } else {
                        try self.buf.pushOp1(.pushField, @intCast(u8, fieldId));
                    }
                    try self.pushDebugSym(nodeId);
                }

                return AnyType;
            },
            .arr_access_expr => {
                _ = try self.genExpr(node.head.left_right.left, discardTopExprReg);

                const index = self.nodes[node.head.left_right.right];
                if (index.node_t == .unary_expr and index.head.unary.op == .minus) {
                    _ = try self.genExpr(index.head.unary.child, discardTopExprReg);
                    if (!discardTopExprReg) {
                        try self.buf.pushOp(.pushReverseIndex);
                    }
                } else {
                    _ = try self.genExpr(node.head.left_right.right, discardTopExprReg);
                    if (!discardTopExprReg) {
                        try self.buf.pushOp(.pushIndex);
                    }
                }
                return AnyType;
            },
            .unary_expr => {
                const op = node.head.unary.op;
                switch (op) {
                    .minus => {
                        _ = try self.genExpr(node.head.unary.child, discardTopExprReg);
                        if (!discardTopExprReg) {
                            try self.buf.pushOp(.pushNeg);
                        }
                        return NumberType;
                    },
                    .not => {
                        _ = try self.genExpr(node.head.unary.child, discardTopExprReg);
                        if (!discardTopExprReg) {
                            try self.buf.pushOp(.pushNot);
                        }
                        return BoolType;
                    },
                    // else => return self.reportError("Unsupported unary op: {}", .{op}, node),
                }
            },
            .bin_expr => {
                const left = node.head.left_right.left;
                const right = node.head.left_right.right;

                const op = @intToEnum(cy.BinaryExprOp, node.head.left_right.extra);
                switch (op) {
                    .plus => {
                        const ltype = try self.genExpr(left, discardTopExprReg);
                        _ = try self.genExpr(right, discardTopExprReg);
                        if (!discardTopExprReg) {
                            try self.buf.pushOp(.pushAdd);
                            try self.pushDebugSym(nodeId);
                        }
                        if (ltype.typeT == .string) {
                            return StringType;
                        } else {
                            return NumberType;
                        }
                    },
                    .star => {
                        _ = try self.genExpr(left, discardTopExprReg);
                        _ = try self.genExpr(right, discardTopExprReg);
                        if (!discardTopExprReg) {
                            try self.buf.pushOp(.pushMultiply);
                        }
                        return NumberType;
                    },
                    .slash => {
                        _ = try self.genExpr(left, discardTopExprReg);
                        _ = try self.genExpr(right, discardTopExprReg);
                        if (!discardTopExprReg) {
                            try self.buf.pushOp(.pushDivide);
                        }
                        return NumberType;
                    },
                    .percent => {
                        _ = try self.genExpr(left, discardTopExprReg);
                        _ = try self.genExpr(right, discardTopExprReg);
                        if (!discardTopExprReg) {
                            try self.buf.pushOp(.pushMod);
                        }
                        return NumberType;
                    },
                    .caret => {
                        _ = try self.genExpr(left, discardTopExprReg);
                        _ = try self.genExpr(right, discardTopExprReg);
                        if (!discardTopExprReg) {
                            try self.buf.pushOp(.pushPower);
                        }
                        return NumberType;
                    },
                    .minus => {
                        // Generating pushMinus1 for fib.cy increases performance ~10-12%.
                        var leftVar: u8 = 255;
                        const leftN = self.nodes[left];
                        if (leftN.node_t == .ident) {
                            if (self.genGetVar(leftN.head.ident.semaVarId)) |svar| {
                                leftVar = svar.local;
                            }
                        }
                        if (leftVar == 255) {
                            _ = try self.genExpr(left, discardTopExprReg);
                        }
                        var rightVar: u8 = 255;
                        const rightN = self.nodes[right];
                        if (rightN.node_t == .ident) {
                            if (self.genGetVar(rightN.head.ident.semaVarId)) |svar| {
                                rightVar = svar.local;
                            }
                        }
                        if (rightVar == 255) {
                            _ = try self.genExpr(right, discardTopExprReg);
                        }

                        if (!discardTopExprReg) {
                            if (leftVar == 255) {
                                if (rightVar == 255) {
                                    try self.buf.pushOp(.pushMinus);
                                } else {
                                    try self.buf.pushOp2(.pushMinus1, leftVar, rightVar);
                                }
                            } else {
                                if (rightVar == 255) {
                                    try self.buf.pushOp2(.pushMinus1, leftVar, rightVar);
                                } else {
                                    try self.buf.pushOp2(.pushMinus2, leftVar, rightVar);
                                }
                            }
                        }
                        return NumberType;
                    },
                    .bitwiseAnd => {
                        _ = try self.genExpr(left, discardTopExprReg);
                        _ = try self.genExpr(right, discardTopExprReg);
                        if (!discardTopExprReg) {
                            try self.buf.pushOp(.pushBitwiseAnd);
                        }
                        return NumberType;
                    },
                    .and_op => {
                        const ltype = try self.genExpr(left, discardTopExprReg);
                        const jumpPc = try self.pushEmptyJumpNotCondKeep();
                        const rtype = try self.genExpr(right, discardTopExprReg);
                        self.patchJumpToCurrent(jumpPc);

                        if (ltype.typeT == rtype.typeT) {
                            return ltype;
                        } else return AnyType;
                    },
                    .or_op => {
                        const ltype = try self.genExpr(left, discardTopExprReg);
                        const jumpPc = try self.pushEmptyJumpCondKeep();
                        const rtype = try self.genExpr(right, discardTopExprReg);
                        self.patchJumpToCurrent(jumpPc);

                        if (ltype.typeT == rtype.typeT) {
                            return ltype;
                        } else return AnyType;
                    },
                    .bang_equal => {
                        _ = try self.genExpr(left, discardTopExprReg);
                        _ = try self.genExpr(right, discardTopExprReg);
                        if (!discardTopExprReg) {
                            try self.buf.pushOp(.pushNotCompare);
                        }
                        return BoolType;
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
            .coyield => {
                const pc = self.buf.ops.items.len;
                try self.buf.pushOp2(.coyield, 0, 0);
                try self.blockJumpStack.append(self.alloc, .{ .jumpT = .jumpToEndLocals, .pc = @intCast(u32, pc) });

                // TODO: return coyield expression.
                if (!discardTopExprReg) {
                    try self.buf.pushOp(.pushNone);
                }
                return AnyType;
            },
            .costart => {
                _ = try self.genCallExpr(node.head.child_head, discardTopExprReg, true);
                return FiberType;
            },
            .call_expr => {
                return self.genCallExpr(nodeId, discardTopExprReg, false);
            },
            .lambda_expr => {
                if (!discardTopExprReg) {
                    const jumpPc = try self.pushEmptyJump();

                    self.nextSemaBlock();
                    try self.pushBlock();
                    const opStart = @intCast(u32, self.buf.ops.items.len);

                    // Generate function body.
                    const func = self.funcDecls[node.head.func.decl_id];
                    try self.reserveFuncParams();
                    try self.genInitLocals();
                    const numParams = @intCast(u8, func.params.end - func.params.start);
                    _ = try self.genMaybeRetainExpr(node.head.func.body_head, false);
                    try self.endLocals();
                    try self.buf.pushOp(.ret1);
                    self.patchJumpToCurrent(jumpPc);

                    // Push captured vars onto the stack.
                    const sblock = self.curSemaBlock();
                    for (sblock.params.items) |varId| {
                        const svar = self.vars.items[varId];
                        if (svar.isCaptured) {
                            const pId = self.capVarParents.get(varId).?;
                            const pvar = self.vars.items[pId];
                            if (svar.vtype.rcCandidate) {
                                try self.buf.pushOp1(.loadRetain, pvar.local);
                            } else {
                                try self.buf.pushOp1(.load, pvar.local);
                            }
                        }
                    }

                    const numLocals = @intCast(u8, self.blockNumLocals() + 1 - numParams);
                    const numCaptured = @intCast(u8, self.curSemaBlock().params.items.len - numParams);
                    self.popBlock();
                    self.prevSemaBlock();

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

    fn getNodeTokenString(self: *const VMcompiler, node: cy.Node) []const u8 {
        const token = self.tokens[node.start_token];
        return self.src[token.start_pos..token.data.end_pos];
    }

    fn pushDebugSym(self: *VMcompiler, nodeId: cy.NodeId) !void {
        try self.buf.pushDebugSym(self.buf.ops.items.len, 0, nodeId, self.curBlock.frameLoc);
    }

    fn pushDebugSymAt(self: *VMcompiler, pc: usize, nodeId: cy.NodeId) !void {
        try self.buf.pushDebugSym(pc, 0, nodeId, self.curBlock.frameLoc);
    }
};

pub const ResultView = struct {
    buf: cy.ByteCodeBuffer,
    hasError: bool,
};

const LocalId = u8;

// TODO: Rename to GenBlock
const Block = struct {
    numLocals: u32,
    frameLoc: cy.NodeId = NullId,
    endLocalsPc: u32,

    fn init() Block {
        return .{
            .numLocals = 0,
            .endLocalsPc = 0,
        };
    }

    fn deinit(self: *Block, alloc: std.mem.Allocator) void {
        _ = self;
        _ = alloc;
    }

    fn reserveLocal(self: *Block) !u8 {
        const idx = self.numLocals;
        self.numLocals += 1;
        if (idx <= std.math.maxInt(u8)) {
            return @intCast(u8, idx);
        } else {
            log.debug("Exceeded max local count.", .{});
            return error.CompileError;
        }
    }
};

const SemaVarId = u32;

const SemaVar = struct {
    /// The current type of the var as the ast is traversed.
    /// This is updated when there is a variable assignment or a child block returns.
    vtype: Type,

    name: []const u8,

    /// Whether this var is a captured function param.
    /// Currently, captured variables can not be reassigned to another value.
    /// This restriction avoids an extra retain/release op for closure calls.
    isCaptured: bool = false,

    /// Whether this is a function param. (Does not include captured vars.)
    isParam: bool = false,

    /// Indicates that at some point during the vars lifetime it was an rcCandidate.
    /// Since all exit paths jump to the same release inst, this flag is used to determine
    /// which vars need a release.
    lifetimeRcCandidate: bool,

    /// There are two cases where the compiler needs to implicitly generate var initializers.
    /// 1. Var is first assigned in a branched block. eg. Assigned inside if block.
    ///    Since the var needs to be released at the end of the root block,
    ///    it needs to have a defined value.
    /// 2. Var is first assigned in an iteration block.
    /// At the beginning of codegen for this block, these vars will be inited to the `none` value.
    genInitializer: bool = false,

    /// Local register offset assigned to this var.
    /// Locals are relative to the stack frame's start position.
    local: LocalId = undefined,

    /// Since the same sema var is used later by codegen,
    /// use a flag to indicate whether the var has been loosely defined in the block. (eg. assigned to lvalue)
    /// Note that assigning inside a branch counts as defined.
    /// Entering an iter block will auto mark those as defined since the var could have been assigned by a previous iteration.
    genIsDefined: bool = false,
};

const VarInfo = struct {

    hasStaticType: bool,
};

const TypeTag = enum {
    any,
    boolean,
    number,
    list,
    map,
    fiber,
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

const StringType = Type{
    .typeT = .string,
    .rcCandidate = true,
};

const FiberType = Type{
    .typeT = .fiber,
    .rcCandidate = true,
};

const ListType = Type{
    .typeT = .list,
    .rcCandidate = true,
};

const MapType = Type{
    .typeT = .map,
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

const BlockJumpType = enum {
    jumpToEndLocals,
};

const BlockJump = struct {
    jumpT: BlockJumpType,
    pc: u32,
};

const SubBlockJump = struct {
    pc: u32,
};

const Load = struct {
    pc: u32,
    tempOffset: u8,
};

// Same as std.mem.replace except writes to an ArrayList. Final result is also known to be at most the size of the original.
pub fn replaceIntoShorterList(comptime T: type, input: []const T, needle: []const T, replacement: []const T, output: *std.ArrayListUnmanaged(T), alloc: std.mem.Allocator) !usize {
    // Known upper bound.
    try output.resize(alloc, input.len);
    var i: usize = 0;
    var slide: usize = 0;
    var replacements: usize = 0;
    while (slide < input.len) {
        if (std.mem.indexOf(T, input[slide..], needle) == @as(usize, 0)) {
            std.mem.copy(u8, output.items[i..i+replacement.len], replacement);
            i += replacement.len;
            slide += needle.len;
            replacements += 1;
        } else {
            output.items[i] = input[slide];
            i += 1;
            slide += 1;
        }
    }
    output.items.len = i;
    return replacements;
}

const VarAndType = struct {
    id: SemaVarId,
    vtype: Type,
};

const SemaSubBlockId = u32;

const SemaSubBlock = struct {
    /// Save var start types for entering a codegen iter block.
    /// This can only be determined after the sema pass.
    /// This is used to initialize the var type when entering the codegen iter block so
    /// that the first `genSetVar` produces the correct `set` op.
    iterVarBeginTypes: std.ArrayListUnmanaged(VarAndType),

    /// Track which vars were assigned to in the current sub block.
    /// If the var was first assigned in a parent sub block, the type is saved in the map to
    /// be merged later with the ending var type.
    /// Can be freed after the end of block.
    prevVarTypes: std.AutoHashMapUnmanaged(SemaVarId, Type),

    /// Start of vars assigned in this block in `assignedVarStack`.
    /// When leaving this block, all assigned var types in this block are merged
    /// back to the parent scope.
    assignedVarStart: u32,

    /// Previous sema sub block.
    /// When this sub block ends, the previous sub block id is set as the current.
    prevSubBlockId: SemaSubBlockId,

    fn init(prevSubBlockId: SemaSubBlockId, assignedVarStart: usize) SemaSubBlock {
        return .{
            .assignedVarStart = @intCast(u32, assignedVarStart),
            .iterVarBeginTypes = .{},
            .prevVarTypes = .{},
            .prevSubBlockId = prevSubBlockId,
        };
    }

    fn deinit(self: *SemaSubBlock, alloc: std.mem.Allocator) void {
        self.iterVarBeginTypes.deinit(alloc);
    }
};

const SemaBlockId = u32;

const SemaBlock = struct {
    /// Local vars defined in this block. Does not include function params.
    locals: std.ArrayListUnmanaged(SemaVarId),

    /// Param vars for function blocks. Includes captured vars for closures.
    /// Codegen will reserve these first for the calling convention layout.
    params: std.ArrayListUnmanaged(SemaVarId),

    /// Name to var.
    /// This can be deinited after ending the sema block.
    nameToVar: std.StringHashMapUnmanaged(SemaVarId),

    /// Current sub block depth.
    subBlockDepth: u32,

    /// Previous sema block.
    /// When this block ends, the previous block id is set as the current.
    prevBlockId: SemaBlockId,

    fn init(prevBlockId: SemaBlockId) SemaBlock {
        return .{
            .nameToVar = .{},
            .locals = .{},
            .params = .{},
            .subBlockDepth = 0,
            .prevBlockId = prevBlockId,
        };
    }

    fn deinit(self: *SemaBlock, alloc: std.mem.Allocator) void {
        self.locals.deinit(alloc);
        self.params.deinit(alloc);
    }
};

fn shouldReleaseAccessParent(nodeT: cy.NodeType) bool {
    switch (nodeT) {
        .call_expr,
        .structInit => return true,
        else => return false,
    }
}