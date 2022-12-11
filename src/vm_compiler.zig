const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const bindings = @import("bindings.zig");

const log = stdx.log.scoped(.vm_compiler);

const NullId = std.math.maxInt(u32);
const NullIdU16 = std.math.maxInt(u16);
const NullIdU8 = std.math.maxInt(u8);
const f64NegOne = cy.Value.initF64(-1);
const f64One = cy.Value.initF64(1);

const dumpCompileErrorStackTrace = builtin.mode == .Debug and true;

const Root = @This();

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

    /// Tracks which temp locals need to be released at the end of the current ARC expression.
    arcTempLocalStack: std.ArrayListUnmanaged(LocalId),

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

    /// Local paths to syms.
    semaSyms: std.ArrayListUnmanaged(SemaSym),
    semaSymMap: std.StringHashMapUnmanaged(SemaSymId),
    
    /// Absolute path to syms and shared among modules.
    semaResolvedSyms: std.ArrayListUnmanaged(SemaResolvedSym),
    semaResolvedSymMap: std.StringHashMapUnmanaged(SemaResolvedSymId),

    semaSymToMod: std.AutoArrayHashMapUnmanaged(SemaSymId, ModuleId),

    /// Modules.
    modules: std.ArrayListUnmanaged(Module),
    /// Specifier to module.
    moduleMap: std.StringHashMapUnmanaged(ModuleId),

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
            .arcTempLocalStack = .{},
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
            .semaSyms = .{},
            .semaSymMap = .{},
            .semaResolvedSyms = .{},
            .semaResolvedSymMap = .{},
            .modules = .{},
            .moduleMap = .{},
            .semaSymToMod = .{},
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
        self.arcTempLocalStack.deinit(self.alloc);
        self.vars.deinit(self.alloc);
        self.capVarParents.deinit(self.alloc);

        for (self.semaSyms.items) |sym| {
            self.alloc.free(sym.path);
        }
        self.semaSyms.deinit(self.alloc);
        self.semaSymMap.deinit(self.alloc);

        for (self.semaResolvedSyms.items) |sym| {
            self.alloc.free(sym.path);
        }
        self.semaResolvedSyms.deinit(self.alloc);
        self.semaResolvedSymMap.deinit(self.alloc);

        for (self.modules.items) |*mod| {
            mod.deinit(self.alloc);
        }
        self.modules.deinit(self.alloc);
        self.moduleMap.deinit(self.alloc);
        self.semaSymToMod.deinit(self.alloc);
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
            .tagLiteral => {
                return TagLiteralType;
            },
            .tagInit => {
                const nameN = self.nodes[node.head.left_right.left];
                const name = self.getNodeTokenString(nameN);
                const tid = try self.vm.ensureTagType(name);
                return initTagType(tid);
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
                if (try self.semaIdentLocalVarOrNull(nodeId)) |varId| {
                    return self.vars.items[varId].vtype;
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
            .accessExpr => {
                _ = try self.semaExpr(node.head.accessExpr.left, discardTopExprReg);
                const right = self.nodes[node.head.accessExpr.right];
                if (right.node_t == .ident) {
                    const left = self.nodes[node.head.accessExpr.left];
                    if (left.node_t == .ident) {
                        if (left.head.ident.semaSymId != NullId) {
                            const leftSym = self.semaSyms.items[left.head.ident.semaSymId];
                            const rightName = self.getNodeTokenString(right);
                            self.u8Buf.clearRetainingCapacity();
                            const w = self.u8Buf.writer(self.alloc);
                            try std.fmt.format(w, "{s}.{s}", .{leftSym.path, rightName });

                            const symId = try self.ensureSemaSym(self.u8Buf.items, leftSym.leadSymId);
                            self.nodes[nodeId].head.accessExpr.semaSymId = symId;
                        }
                    } else if (left.node_t == .accessExpr) {
                        if (left.head.accessExpr.semaSymId != NullId) {
                            const leftSym = self.semaSyms.items[left.head.ident.semaSymId];
                            const rightName = self.getNodeTokenString(right);
                            self.u8Buf.clearRetainingCapacity();
                            const w = self.u8Buf.writer(self.alloc);
                            try std.fmt.format(w, "{s}.{s}", .{leftSym.path, rightName });

                            const symId = try self.ensureSemaSym(self.u8Buf.items, leftSym.leadSymId);
                            self.nodes[nodeId].head.accessExpr.semaSymId = symId;
                        }
                    }
                }
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
            .tryExpr => {
                _ = try self.semaExpr(node.head.child_head, discardTopExprReg);
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
                    .bitwiseNot => {
                        _ = try self.semaExpr(node.head.unary.child, discardTopExprReg);
                        return NumberType;
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
                    .bitwiseOr => {
                        _ = try self.semaExpr(left, discardTopExprReg);
                        _ = try self.semaExpr(right, discardTopExprReg);
                        return NumberType;
                    },
                    .bitwiseXor => {
                        _ = try self.semaExpr(left, discardTopExprReg);
                        _ = try self.semaExpr(right, discardTopExprReg);
                        return NumberType;
                    },
                    .bitwiseLeftShift => {
                        _ = try self.semaExpr(left, discardTopExprReg);
                        _ = try self.semaExpr(right, discardTopExprReg);
                        return NumberType;
                    },
                    .bitwiseRightShift => {
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
            .coresume => {
                _ = try self.semaExpr(node.head.child_head, false);
                return AnyType;
            },
            .coinit => {
                _ = try self.semaExpr(node.head.child_head, false);
                return FiberType;
            },
            .call_expr => {
                const callee = self.nodes[node.head.func_call.callee];
                if (!node.head.func_call.has_named_arg) {
                    if (callee.node_t == .accessExpr) {
                        _ = try self.semaExpr(node.head.func_call.callee, false);

                        var numArgs: u32 = 0;
                        var arg_id = node.head.func_call.arg_head;
                        while (arg_id != NullId) : (numArgs += 1) {
                            const arg = self.nodes[arg_id];
                            _ = try self.semaExpr(arg_id, false);
                            arg_id = arg.next;
                        }

                        return AnyType;
                    } else if (callee.node_t == .ident) {
                        if (try self.semaIdentLocalVarOrNull(node.head.func_call.callee)) |varId| {
                            _ = varId;
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
            .lambda_multi => {
                if (!discardTopExprReg) {
                    try self.pushSemaBlock();

                    // Generate function body.
                    const func = self.funcDecls[node.head.func.decl_id];
                    try self.pushSemaLambdaParamVars(func);
                    try self.semaStmts(node.head.func.body_head, false);

                    try self.endSemaBlock();
                }
                return AnyType;
            },
            .lambda_expr => {
                if (!discardTopExprReg) {
                    try self.pushSemaBlock();

                    // Generate function body.
                    const func = self.funcDecls[node.head.func.decl_id];
                    try self.pushSemaLambdaParamVars(func);
                    _ = try self.semaExpr(node.head.func.body_head, false);

                    try self.endSemaBlock();
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
                    if (try self.semaIdentLocalVarOrNull(node.head.left_right.left)) |varId| {
                        const svar = self.vars.items[varId];
                        const rtype = try self.semaExpr(node.head.left_right.right, false);
                        if (svar.vtype.typeT != .number and svar.vtype.typeT != .any and rtype.typeT != svar.vtype.typeT) {
                            return self.reportError("Type mismatch: Expected {}", .{svar.vtype.typeT}, node);
                        }
                    } else stdx.panic("variable not declared");
                } else if (left.node_t == .accessExpr) {
                    const accessLeft = try self.semaExpr(left.head.accessExpr.left, false);
                    const accessRight = try self.semaExpr(left.head.accessExpr.right, false);
                    const right = try self.semaExpr(node.head.left_right.right, false);
                    _ = accessLeft;
                    _ = accessRight;
                    _ = right;
                } else {
                    stdx.panicFmt("unsupported assignment to left {}", .{left.node_t});
                }
            },
            .assign_stmt => {
                const left = self.nodes[node.head.left_right.left];
                if (left.node_t == .ident) {
                    const rtype = try self.semaExpr(node.head.left_right.right, false);
                    _ = try self.semaAssignVar(node.head.left_right.left, rtype);
                } else if (left.node_t == .arr_access_expr) {
                    _ = try self.semaExpr(left.head.left_right.left, false);
                    _ = try self.semaExpr(left.head.left_right.right, false);
                    _ = try self.semaExpr(node.head.left_right.right, false);
                } else if (left.node_t == .accessExpr) {
                    _ = try self.semaExpr(left.head.accessExpr.left, false);
                    _ = try self.semaExpr(left.head.accessExpr.right, false);
                    _ = try self.semaExpr(node.head.left_right.right, false);
                } else {
                    stdx.panicFmt("unsupported assignment to left {}", .{left.node_t});
                }
            },
            .tagDecl => {
                const nameN = self.nodes[node.head.tagDecl.name];
                const name = self.getNodeTokenString(nameN);

                const tid = try self.vm.ensureTagType(name);

                var i: u32 = 0;
                var memberId = node.head.tagDecl.memberHead;
                while (memberId != NullId) : (i += 1) {
                    const member = self.nodes[memberId];
                    memberId = member.next;
                }
                const numMembers = i;
                self.vm.tagTypes.buf[tid].numMembers = numMembers;

                i = 0;
                memberId = node.head.tagDecl.memberHead;
                while (memberId != NullId) : (i += 1) {
                    const member = self.nodes[memberId];
                    const mName = self.getNodeTokenString(member);
                    const symId = try self.vm.ensureTagLitSym(mName);
                    self.vm.setTagLitSym(tid, symId, i);
                    memberId = member.next;
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
                            try self.endSemaBlock();
                            funcId = func.next;
                            continue;
                        }
                    }

                    // Struct function.

                    self.u8Buf.clearRetainingCapacity();
                    const w = self.u8Buf.writer(self.alloc);
                    const funcName = self.src[decl.name.start..decl.name.end];
                    try std.fmt.format(w, "{s}.{s}", .{name, funcName});
                    if (self.getSemaSym(self.u8Buf.items) == null) {
                        const symId = try self.ensureSemaSym(self.u8Buf.items, null);
                        self.semaSyms.items[symId].symT = .func;
                    } else {
                        return self.reportDebugError("Symbol already declared: {s}", .{self.u8Buf.items});
                    }

                    try self.pushSemaBlock();
                    try self.pushSemaFuncParamVars(decl);
                    try self.semaStmts(func.head.func.body_head, false);
                    try self.endSemaBlock();
                    funcId = func.next;
                }
            },
            .func_decl => {
                const func = self.funcDecls[node.head.func.decl_id];

                try self.pushSemaBlock();
                try self.pushSemaFuncParamVars(func);
                try self.semaStmts(node.head.func.body_head, false);
                try self.endSemaBlock();
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
                _ = try self.semaEnsureVar(as_clause.head.as_iter_clause.value, AnyType);

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
                try self.endSemaSubBlock();

                var elseClauseId = node.head.left_right.extra;
                while (elseClauseId != NullId) {
                    const elseClause = self.nodes[elseClauseId];
                    if (elseClause.head.else_clause.cond == NullId) {
                        try self.pushSemaSubBlock();
                        try self.semaStmts(elseClause.head.else_clause.body_head, false);
                        try self.endSemaSubBlock();
                        break;
                    } else {
                        _ = try self.semaExpr(elseClause.head.else_clause.cond, false);

                        try self.pushSemaSubBlock();
                        try self.semaStmts(elseClause.head.else_clause.body_head, false);
                        try self.endSemaSubBlock();
                        elseClauseId = elseClause.head.else_clause.else_clause;
                    }
                }
            },
            .importStmt => {
                const ident = self.nodes[node.head.left_right.left];
                const name = self.getNodeTokenString(ident);
                const symId = try self.ensureSemaSym(name, null);

                const spec = self.nodes[node.head.left_right.right];
                const specPath = self.getNodeTokenString(spec);

                const modId = try self.getOrLoadModule(specPath);
                try self.semaSymToMod.put(self.alloc, symId, modId);
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

    fn getOrLoadModule(self: *VMcompiler, spec: []const u8) !ModuleId {
        const res = try self.moduleMap.getOrPut(self.alloc, spec);
        if (res.found_existing) {
            return res.value_ptr.*;
        } else {
            const mod = try self.loadModule(spec);
            const id = @intCast(u32, self.modules.items.len);
            try self.modules.append(self.alloc, mod);
            res.key_ptr.* = spec;
            res.value_ptr.* = id;
            return id;
        }
    }

    fn loadModule(self: *VMcompiler, spec: []const u8) !Module {
        // Builtin modules.
        if (std.mem.eql(u8, "test", spec)) {
            var mod = Module{
                .syms = .{},
                .prefix = spec,
            };
            try mod.syms.put(self.alloc, "eq", .{
                .symT = .nativeFunc1,
                .inner = .{
                    .nativeFunc1 = .{
                        .func = bindings.testEq,
                    },
                },
            });
            try mod.syms.put(self.alloc, "eqNear", .{
                .symT = .nativeFunc1,
                .inner = .{
                    .nativeFunc1 = .{
                        .func = bindings.testEqNear,
                    },
                },
            });
            return mod;
        } else {
            return self.reportDebugError("Unsupported import. {s}", .{spec});
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
            // log.debug("{s} iter var", .{self.getVarName(varAndType.id)});
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

        self.curBlock.firstFreeTempLocal = @intCast(u8, self.curBlock.numLocals);
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
            if (dumpCompileErrorStackTrace) {
                std.debug.dumpStackTrace(@errorReturnTrace().?.*);
            }
            return ResultView{
                .buf = self.buf,
                .hasError = true,
            };
        };
        try self.endSemaBlock();

        // After sema pass, resolve used syms.
        for (self.semaSyms.items) |*sym| {
            if (self.semaSymToMod.get(sym.leadSymId)) |modId| {
                const modPrefix = self.modules.items[modId].prefix;
                if (std.mem.indexOfScalar(u8, sym.path, '.')) |idx| {
                    self.u8Buf.clearRetainingCapacity();
                    const w = self.u8Buf.writer(self.alloc);
                    try std.fmt.format(w, "{s}.{s}", .{ modPrefix, sym.path[idx+1..] });

                    if (try self.getOrTryResolveSym(self.u8Buf.items, modId)) |resolvedId| {
                        sym.resolvedSymId = resolvedId;
                        // log.debug("resolve {s} {} {}", .{sym.path, sym.leadSymId, modId});
                    }
                } else {
                    if (try self.getOrTryResolveSym(modPrefix, modId)) |resolvedId| {
                        sym.resolvedSymId = resolvedId;
                        // log.debug("resolve {s} {} {}", .{sym.path, sym.leadSymId, modId});
                    }
                }
            }
        }

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
        self.buf.mainStackSize = @intCast(u32, self.curBlock.getRequiredStackSize());

        // Merge inst and const buffers.
        var reqLen = self.buf.ops.items.len + self.buf.consts.items.len * @sizeOf(cy.Const) + @alignOf(cy.Const) - 1;
        if (self.buf.ops.capacity < reqLen) {
            try self.buf.ops.ensureTotalCapacityPrecise(self.alloc, reqLen);
        }
        const constAddr = std.mem.alignForward(@ptrToInt(self.buf.ops.items.ptr) + self.buf.ops.items.len, @alignOf(cy.Const));
        const constDst = @intToPtr([*]cy.Const, constAddr)[0..self.buf.consts.items.len];
        const constSrc = try self.buf.consts.toOwnedSlice(self.alloc);
        std.mem.copy(cy.Const, constDst, constSrc);
        self.alloc.free(constSrc);
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

    fn pushEmptyJumpCondNone(self: *VMcompiler, condLocal: LocalId) !u32 {
        const start = @intCast(u32, self.buf.ops.items.len);
        try self.buf.pushOp3(.jumpCondNone, 0, 0, condLocal);
        return start;
    }

    fn pushEmptyJumpCond(self: *VMcompiler, condLocal: LocalId) !u32 {
        const start = @intCast(u32, self.buf.ops.items.len);
        try self.buf.pushOp3(.jumpCond, 0, 0, condLocal);
        return start;
    }

    fn pushEmptyJumpNotCond(self: *VMcompiler, condLocal: LocalId) !u32 {
        const start = @intCast(u32, self.buf.ops.items.len);
        try self.buf.pushOp3(.jumpNotCond, 0, 0, condLocal);
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
        try self.endSemaSubBlock();
    }

    fn endSemaSubBlock(self: *VMcompiler) !void {
        const sblock = self.curSemaBlock();
        const ssblock = self.curSemaSubBlock();

        const curAssignedVars = self.assignedVarStack.items[ssblock.assignedVarStart..];
        self.assignedVarStack.items.len = ssblock.assignedVarStart;

        if (sblock.subBlockDepth > 1) {
            const pssblock = self.semaSubBlocks.items[ssblock.prevSubBlockId];

            // Merge types to parent sub block.
            for (curAssignedVars) |varId| {
                const svar = &self.vars.items[varId];
                // log.debug("merging {s}", .{self.getVarName(varId)});
                if (ssblock.prevVarTypes.get(varId)) |prevt| {
                    // Update current var type by merging.
                    if (svar.vtype.typeT != prevt.typeT) {
                        svar.vtype = AnyType;

                        // Previous sub block hasn't recorded the var assignment.
                        if (!pssblock.prevVarTypes.contains(varId)) {
                            try self.assignedVarStack.append(self.alloc, varId);
                        }
                    }
                } else {
                    // New variable assignment, propagate to parent block.
                    try self.assignedVarStack.append(self.alloc, varId);
                }
            }
        }
        ssblock.prevVarTypes.deinit(self.alloc);

        self.curSemaSubBlockId = ssblock.prevSubBlockId;
        sblock.subBlockDepth -= 1;
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

    fn endSemaBlock(self: *VMcompiler) !void {
        try self.endSemaSubBlock();
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

    fn semaAssignVar(self: *VMcompiler, ident: cy.NodeId, vtype: Type) !void {
        // log.debug("set var {s}", .{name});
        const node = self.nodes[ident];
        const name = self.getNodeTokenString(node);

        if (try self.semaIdentLocalVarOrNull(ident)) |varId| {
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
            // Create a new local.
            const id = try self.pushSemaLocalVar(name, vtype);
            const sblock = self.curSemaBlock();
            if (sblock.subBlockDepth > 1) {
                self.vars.items[id].genInitializer = true;
            }
            self.nodes[ident].head.ident.semaVarId = id;

            try self.assignedVarStack.append(self.alloc, id);
        }
    }

    const SemaVarResult = struct {
        id: SemaVarId,
        fromParentBlock: bool,
    };

    /// First checks current block and then the immediate parent block.
    fn semaLookupVar(self: *VMcompiler, name: []const u8) ?SemaVarResult {
        const sblock = self.curSemaBlock();
        if (sblock.nameToVar.get(name)) |varId| {
            return SemaVarResult{
                .id = varId,
                .fromParentBlock = false,
            };
        }

        // Only check one block above.
        if (self.semaBlockDepth > 1) {
            const prev = self.semaBlocks.items[sblock.prevBlockId];
            if (prev.nameToVar.get(name)) |varId| {
                return SemaVarResult{
                    .id = varId,
                    .fromParentBlock = true,
                };
            }
        }

        // Undefined var.
        return null;
    }

    /// Retrieve the SemaVarId that is local to the current block.
    /// If the var comes from a parent block, a local captured var is created and returned.
    /// Sets the resulting id onto the node for codegen.
    fn semaIdentLocalVarOrNull(self: *VMcompiler, ident: cy.NodeId) !?SemaVarId {
        const node = self.nodes[ident];
        const name = self.getNodeTokenString(node);

        if (self.semaLookupVar(name)) |res| {
            if (res.fromParentBlock) {
                // Create a local captured variable.
                const svar = self.vars.items[res.id];
                const id = try self.pushSemaCapturedVar(name, res.id, svar.vtype);
                self.nodes[ident].head.ident.semaVarId = id;
                return id;
            } else {
                self.nodes[ident].head.ident.semaVarId = res.id;
                return res.id;
            }
        } else {
            self.nodes[ident].head.ident.semaVarId = NullId;

            // Assume reference to global sym.
            const symId = try self.ensureSemaSym(name, null);
            self.nodes[ident].head.ident.semaSymId = symId;
            return null;
        }
    }

    fn getOrTryResolveSym(self: *VMcompiler, path: []const u8, modId: ModuleId) !?SemaResolvedSymId {
        if (self.semaResolvedSymMap.get(path)) |id| {
            return id;
        } else {
            const idx = std.mem.indexOfScalar(u8, path, '.') orelse return null;
            const subpath = path[idx+1..];
            if (self.modules.items[modId].syms.get(subpath)) |modSym| {
                const id = @intCast(u32, self.semaResolvedSyms.items.len);
                if (modSym.symT == .nativeFunc1) {
                    const pathDupe = try self.alloc.dupe(u8, path);
                    const rtSymId = try self.vm.ensureFuncSym(pathDupe);
                    const rtSym = cy.FuncSymbolEntry.initNativeFunc1(modSym.inner.nativeFunc1.func);
                    self.vm.setFuncSym(rtSymId, rtSym);
                    try self.semaResolvedSyms.append(self.alloc, .{
                        .symT = .func,
                        .path = pathDupe,
                    });
                    try self.semaResolvedSymMap.put(self.alloc, pathDupe, id);
                } else {
                    return self.reportDebugError("Unsupported module sym {}", .{modSym.symT});
                }
                return id;
            }
            return null;
        }
    }

    fn getSemaSym(self: *const VMcompiler, path: []const u8) ?SemaSymId {
        return self.semaSymMap.get(path);
    }

    fn ensureSemaSym(self: *VMcompiler, path: []const u8, leadSymId: ?SemaSymId) !SemaSymId {
        const res = try self.semaSymMap.getOrPut(self.alloc, path);
        if (res.found_existing) {
            return res.value_ptr.*;
        } else {
            const id = @intCast(u32, self.semaSyms.items.len);
            const ownPath = try self.alloc.dupe(u8, path);
            try self.semaSyms.append(self.alloc, .{
                .symT = .undefined,
                .path = ownPath,
                .leadSymId = leadSymId orelse id,
            });
            res.key_ptr.* = ownPath;
            res.value_ptr.* = id;
            return id;
        }
    }

    fn genStatements(self: *VMcompiler, head: cy.NodeId, comptime attachEnd: bool) anyerror!void {
        var cur_id = head;
        var node = self.nodes[cur_id];

        while (node.next != NullId) {
            try self.genStatement(cur_id, true);
            cur_id = node.next;
            node = self.nodes[cur_id];
        }

        // Check for last expression statement.
        if (node.node_t == .expr_stmt) {
            if (attachEnd) {
                const local = try self.genExprStmt(cur_id, true, false);
                try self.endLocals();
                try self.buf.pushOp1(.end, local);
            } else {
                _ = try self.genStatement(cur_id, true);
            }
        } else {
            if (attachEnd) {
                try self.genStatement(cur_id, false);
                try self.endLocals();
                try self.buf.pushOp1(.end, 255);
            } else {
                try self.genStatement(cur_id, true);
            }
        }
    }

    fn getVarName(self: *VMcompiler, varId: SemaVarId) []const u8 {
        if (builtin.mode == .Debug) {
            return self.vars.items[varId].name;
        } else {
            return "";
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
                .name = if (builtin.mode == .Debug) name else {},
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

    fn genSetBoxedVarToExpr(self: *VMcompiler, svar: *SemaVar, exprId: cy.NodeId) !void {
        // Retain rval.
        const exprv = try self.genRetainedTempExpr(exprId, false);
        if (svar.vtype.typeT != BoxType.typeT) {
            svar.vtype = exprv.vtype;
        }
        svar.genIsDefined = true;
        if (!svar.vtype.rcCandidate) {
            try self.buf.pushOp2(.setBoxValue, svar.local, exprv.local);
        } else {
            try self.buf.pushOp2(.setBoxValueRelease, svar.local, exprv.local);
        }
    }

    fn genSetVarToExpr(self: *VMcompiler, varId: SemaVarId, exprId: cy.NodeId, comptime discardTopExprReg: bool) !void {
        _ = discardTopExprReg;
        const expr = self.nodes[exprId];
        if (self.genGetVarPtr(varId)) |svar| {

            if (svar.isCaptured and !svar.isBoxed) {
                svar.isBoxed = true;
                svar.vtype = BoxType;
            }

            if (svar.isBoxed) {
                try self.genSetBoxedVarToExpr(svar, exprId);
                return;
            }

            if (expr.node_t == .ident) {
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
                        try self.buf.pushOp2(.copyRetainSrc, exprv.local, svar.local);
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
                    const symId = try self.vm.ensureTagLitSym(name);
                    const sym = self.vm.tagLitSyms.buf[symId];
                    if (sym.symT == .one and sym.inner.one.id == svar.vtype.inner.tagId) {
                        try self.buf.pushOp3(.tag, svar.vtype.inner.tagId, @intCast(u8, sym.inner.one.val), svar.local);
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
            log.debug("Undefined var.", .{});
            return error.CompileError;
        }
    }

    fn pushSemaLambdaParamVars(self: *VMcompiler, func: cy.FuncDecl) !void {
        const sblock = self.curSemaBlock();

        if (func.params.end > func.params.start) {
            for (self.funcParams[func.params.start..func.params.end]) |param| {
                const paramName = self.src[param.name.start..param.name.end];
                const paramT = AnyType;
                const id = try self.pushSemaVar(paramName, paramT);
                try sblock.params.append(self.alloc, id);
            }
        }

        // Add lambda receiver param.
        var id = try self.pushSemaVar("[callee]", AnyType);
        try sblock.params.append(self.alloc, id);
    }

    fn pushSemaMethodParamVars(self: *VMcompiler, func: cy.FuncDecl) !void {
        const sblock = self.curSemaBlock();

        if (func.params.end > func.params.start) {
            for (self.funcParams[func.params.start + 1..func.params.end]) |param| {
                const paramName = self.src[param.name.start..param.name.end];
                const paramT = AnyType;
                const id = try self.pushSemaVar(paramName, paramT);
                try sblock.params.append(self.alloc, id);
            }
        }

        // Add self receiver param.
        var id = try self.pushSemaVar("self", AnyType);
        try sblock.params.append(self.alloc, id);
    }

    fn pushSemaFuncParamVars(self: *VMcompiler, func: cy.FuncDecl) !void {
        const sblock = self.curSemaBlock();

        if (func.params.end > func.params.start) {
            for (self.funcParams[func.params.start..func.params.end]) |param| {
                const paramName = self.src[param.name.start..param.name.end];
                const paramT = AnyType;
                const id = try self.pushSemaVar(paramName, paramT);
                try sblock.params.append(self.alloc, id);
            }
        }
    }

    // Reserve params and captured vars.
    fn reserveFuncParams(self: *VMcompiler) !void {
        // First local is reserved for a single return value.
        _ = try self.curBlock.reserveLocal();

        // Second local is reserved for the return info.
        // This allows the vm to unwind with just the framePtr.
        _ = try self.curBlock.reserveLocal();

        const sblock = self.curSemaBlock();
        for (sblock.params.items) |varId| {
            _ = try self.reserveLocalVar(varId);
        }
    }

    fn beginArcExpr(self: *VMcompiler) u32 {
        self.curBlock.arcTempLocalStart = @intCast(u32, self.arcTempLocalStack.items.len);
        return self.curBlock.arcTempLocalStart;
    }

    fn endArcExpr(self: *VMcompiler, arcTempLocalStart: u32) !void {
        // Gen release ops.
        for (self.arcTempLocalStack.items[self.curBlock.arcTempLocalStart..]) |local| {
            try self.buf.pushOp1(.release, local);
        }
        self.arcTempLocalStack.items.len = self.curBlock.arcTempLocalStart;

        // Restore current local start.
        self.curBlock.arcTempLocalStart = arcTempLocalStart;
    }

    fn genExprStmt(self: *VMcompiler, nodeId: cy.NodeId, retainEscapeTop: bool, comptime discardTopExprReg: bool) !LocalId {
        const arcLocalStart = self.beginArcExpr();

        const node = self.nodes[nodeId];
        var val: GenValue = undefined;
        if (retainEscapeTop) {
            val = try self.genRetainedTempExpr(node.head.child_head, discardTopExprReg);
        } else {
            val = try self.genExpr(node.head.child_head, discardTopExprReg);
        }

        try self.endArcExpr(arcLocalStart);
        return val.local;
    }

    fn genBinOpAssignToField(self: *VMcompiler, code: cy.OpCode, leftId: cy.NodeId, rightId: cy.NodeId) !void {
        const left = self.nodes[leftId];

        const startTempLocal = self.curBlock.firstFreeTempLocal;
        defer self.computeNextTempLocalFrom(startTempLocal);

        const accessRight = self.nodes[left.head.accessExpr.right];
        if (accessRight.node_t != .ident) {
            log.debug("Expected ident.", .{});
            return error.CompileError;
        }
        const fieldName = self.getNodeTokenString(accessRight);
        const fieldId = try self.vm.ensureFieldSym(fieldName);

        const accessLeftv = try self.genExpr(left.head.accessExpr.left, false);
        const accessLocal = try self.nextFreeTempLocal();
        try self.buf.pushOp3(.field, @intCast(u8, fieldId), accessLeftv.local, accessLocal);

        const rightv = try self.genExpr(rightId, false);
        try self.buf.pushOp3(code, accessLocal, rightv.local, accessLocal);

        try self.buf.pushOp3(.setField, @intCast(u8, fieldId), accessLeftv.local, accessLocal);
        try self.pushDebugSym(leftId);
    }

    /// discardTopExprReg is usually true since statements aren't expressions and evaluating child expressions
    /// would just grow the register stack unnecessarily. However, the last main statement requires the
    /// resulting expr to persist to return from `eval`.
    fn genStatement(self: *VMcompiler, nodeId: cy.NodeId, comptime discardTopExprReg: bool) !void {
        // log.debug("gen stmt {}", .{node.node_t});

        self.resetNextFreeTemp();

        const node = self.nodes[nodeId];
        switch (node.node_t) {
            .pass_stmt => {
                return;
            },
            .expr_stmt => {
                _ = try self.genExprStmt(nodeId, false, discardTopExprReg);
            },
            .break_stmt => {
                const pc = try self.pushEmptyJump();
                try self.subBlockJumpStack.append(self.alloc, .{ .pc = pc });
            },
            .add_assign_stmt => {
                const left = self.nodes[node.head.left_right.left];
                if (left.node_t == .ident) {
                    if (self.genGetVarPtr(left.head.ident.semaVarId)) |svar| {
                        if (svar.isCaptured and !svar.isBoxed) {
                            svar.isBoxed = true;
                            svar.vtype = BoxType;
                        }

                        const right = try self.genExpr(node.head.left_right.right, false);
                        if (svar.isBoxed) {
                            const tempLocal = try self.nextFreeTempLocal();
                            try self.buf.pushOp2(.boxValue, svar.local, tempLocal);
                            try self.buf.pushOp3(.add, tempLocal, right.local, tempLocal);
                            try self.buf.pushOp2(.setBoxValue, svar.local, tempLocal);
                            return;
                        } else {
                            try self.buf.pushOp3(.add, svar.local, right.local, svar.local);
                        }
                    } else stdx.panic("variable not declared");
                } else if (left.node_t == .accessExpr) {
                    try self.genBinOpAssignToField(.add, node.head.left_right.left, node.head.left_right.right);
                } else {
                    stdx.panicFmt("unsupported assignment to left {}", .{left.node_t});
                }
            },
            .assign_stmt => {
                const left = self.nodes[node.head.left_right.left];
                if (left.node_t == .ident) {
                    try self.genSetVarToExpr(left.head.ident.semaVarId, node.head.left_right.right, false);
                } else if (left.node_t == .arr_access_expr) {
                    const startTempLocal = self.curBlock.firstFreeTempLocal;
                    defer self.setFirstFreeTempLocal(startTempLocal);

                    const leftv = try self.genExpr(left.head.left_right.left, false);
                    const indexv = try self.genExpr(left.head.left_right.right, false);
                    const rightv = try self.genExpr(node.head.left_right.right, false);
                    try self.buf.pushOp3(.setIndex, leftv.local, indexv.local, rightv.local);
                } else if (left.node_t == .accessExpr) {
                    const startTempLocal = self.curBlock.firstFreeTempLocal;
                    defer self.setFirstFreeTempLocal(startTempLocal);

                    const leftv = try self.genExpr(left.head.accessExpr.left, false);

                    const accessRight = self.nodes[left.head.accessExpr.right];
                    if (accessRight.node_t != .ident) {
                        log.debug("Expected ident.", .{});
                        return error.CompileError;
                    }

                    const rightv = try self.genRetainedTempExpr(node.head.left_right.right, false);

                    const fieldName = self.getNodeTokenString(accessRight);
                    const fieldId = try self.vm.ensureFieldSym(fieldName);
                    try self.buf.pushOp3(.setFieldRelease, @intCast(u8, fieldId), leftv.local, rightv.local);
                    try self.pushDebugSym(nodeId);
                } else {
                    stdx.panicFmt("unsupported assignment to left {}", .{left.node_t});
                }
            },
            .tagDecl => {
                // Nop.
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

                const topPc = @intCast(u32, self.buf.ops.items.len);
                const condv = try self.genExpr(node.head.left_right.left, false);

                var jumpPc = try self.pushEmptyJumpNotCond(condv.local);

                try self.genStatements(node.head.left_right.right, false);
                try self.pushJumpBackTo(topPc);

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

                const startTempLocal = self.curBlock.firstFreeTempLocal;
                defer self.setFirstFreeTempLocal(startTempLocal);
                const iterable = try self.genExpr(node.head.for_iter_stmt.iterable, false);

                const asClause = self.nodes[node.head.for_iter_stmt.as_clause];
                const ident = self.nodes[asClause.head.as_iter_clause.value];
                const val = self.genGetVar(ident.head.ident.semaVarId).?;

                // At this point the temp var is loosely defined.
                self.vars.items[ident.head.ident.semaVarId].genIsDefined = true;

                // // Reserve unreachable local for iterator.
                // const iterLocal = try self.nextFreeTempLocal();
                // try self.buf.pushOp2(.copy, iterable.local, iterLocal + 2);
                // try self.buf.pushOp3(.callObjSym1, @intCast(u8, self.vm.iteratorObjSym), iterLocal, 1);

                const forPc = self.buf.ops.items.len;

                // try self.buf.pushOp2(.copy, iterLocal, iterLocal + 3);
                // try self.buf.pushOp3(.callObjSym1, @intCast(u8, self.vm.nextObjSym), iterLocal + 1, 1);
                // try self.buf.pushOp2(.copyReleaseDst, iterLocal + 1, val.local);

                // const jumpNonePc = try self.pushEmptyJumpCondNone(val.local);
                // self.setFirstFreeTempLocal(iterLocal + 1);
                try self.buf.pushOpSlice(.forIter, &.{ startTempLocal, iterable.local, val.local, 0, 0 });

                const bodyPc = self.buf.ops.items.len;
                try self.genStatements(node.head.for_iter_stmt.body_head, false);
                try self.pushContTo(bodyPc);
                // try self.pushJumpBackTo(forPc);
                // self.patchJumpToCurrent(jumpNonePc);

                self.buf.setOpArgU16(forPc+4, @intCast(u16, self.buf.ops.items.len - forPc));
                // try self.buf.pushOp1(.release, iterLocal);
            },
            .for_range_stmt => {
                self.nextSemaSubBlock();
                defer self.prevSemaSubBlock();

                const startTempLocal = self.curBlock.firstFreeTempLocal;
                defer self.setFirstFreeTempLocal(startTempLocal);

                var local: u8 = NullIdU8;
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
                const rangeStart = try self.genExpr(range_clause.head.left_right.left, false);
                const rangeEnd = try self.genExpr(range_clause.head.left_right.right, false);

                // Push custom step.
                const stepConst = try self.buf.pushConst(.{ .val = f64One.val });
                const rangeStep = try self.nextFreeTempLocal();
                try self.buf.pushOp2(.constOp, @intCast(u8, stepConst), rangeStep);

                // try self.buf.pushOp2(.copy, rangeStart.local, local);

                // Push for range op with asLocal.
                const forPc = self.buf.ops.items.len;

                // Attempt to make use pure bytecode version of the for loop but it's still slower.
                // try self.buf.pushOp3(.lessNumber, local, rangeEnd.local, rangeStart.local);
                // const jumpNotCond = try self.pushEmptyJumpNotCond(rangeStart.local);
                // try self.buf.pushOp3(.addNumber, local, rangeStep, local);

                try self.buf.pushOpSlice(.forRange, &.{ local, rangeStart.local, rangeEnd.local, rangeStep, 0, 0 });

                const bodyPc = self.buf.ops.items.len;
                try self.genStatements(node.head.for_range_stmt.body_head, false);
                try self.pushContTo(bodyPc);
                self.buf.setOpArgU16(forPc+5, @intCast(u16, self.buf.ops.items.len - forPc));
                // try self.pushJumpBackTo(forPc);
                // self.patchJumpToCurrent(jumpNotCond);
            },
            .if_stmt => {
                const startTempLocal = self.curBlock.firstFreeTempLocal;

                const condv = try self.genExpr(node.head.left_right.left, false);
                var lastCondJump = try self.pushEmptyJumpNotCond(condv.local);
                self.setFirstFreeTempLocal(startTempLocal);

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
                            self.setFirstFreeTempLocal(startTempLocal);
                            break;
                        } else {
                            const elifCondv = try self.genExpr(elseClause.head.else_clause.cond, false);
                            lastCondJump = try self.pushEmptyJumpNotCond(elifCondv.local);

                            self.nextSemaSubBlock();
                            try self.genStatements(elseClause.head.else_clause.body_head, false);
                            self.prevSemaSubBlock();
                            elseClauseId = elseClause.head.else_clause.else_clause;

                            self.setFirstFreeTempLocal(startTempLocal);
                        }
                    }

                    if (!endsWithElse) {
                        self.patchJumpToCurrent(lastCondJump);
                    }
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
                self.setFirstFreeTempLocal(@intCast(u8, self.curBlock.numLocals));

                if (self.blocks.items.len == 1) {
                    const val = try self.genRetainedTempExpr(node.head.child_head, false);
                    try self.endLocals();
                    try self.buf.pushOp1(.end, @intCast(u8, val.local));
                } else {
                    _ = try self.genRetainedExprTo(node.head.child_head, 0, false);
                    try self.endLocals();
                    try self.buf.pushOp(.ret1);
                }
            },
            else => return self.reportError("Unsupported node", .{}, node),
        }
    }

    fn genExprToDestOrTempLocal(self: *VMcompiler, nodeId: cy.NodeId, dst: LocalId, usedDst: *bool, comptime discardTopExprReg: bool) !GenValue {
        const node = self.nodes[nodeId];
        if (isArcTempNode(node.node_t) or usedDst.*) {
            const finalDst = try self.userLocalOrNextTempLocal(nodeId);
            return self.genExprTo(nodeId, finalDst, false, discardTopExprReg);
        } else {
            const finalDst = self.userLocalOrDst(nodeId, dst, usedDst);
            return self.genExprTo(nodeId, finalDst, false, discardTopExprReg);
        }
    }

    /// Generates an instruction to put the root expression to a specific destination local.
    /// Also ensures that the expression is retained.
    fn genRetainedExprTo(self: *VMcompiler, nodeId: cy.NodeId, dst: LocalId, comptime discardTopExprReg: bool) anyerror!GenValue {
        return try self.genExprTo(nodeId, dst, true, discardTopExprReg);
    }

    /// Ensures that the expr value is retained and ends up in the next temp local.
    fn genRetainedTempExpr(self: *VMcompiler, nodeId: cy.NodeId, comptime discardTopExprReg: bool) anyerror!GenValue {
        const arcLocalStart = self.beginArcExpr();

        const dst = try self.nextFreeTempLocal();
        // ARC temps released at the end of this expr,
        // so the next free temp is guaranteed to be after dst.
        defer self.setFirstFreeTempLocal(dst + 1);

        const val = try self.genExprTo(nodeId, dst, true, discardTopExprReg);
        try self.endArcExpr(arcLocalStart);
        return val;
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
        const symId = try self.vm.ensureFuncSym(symName);

        const jumpPc = try self.pushEmptyJump();

        try self.pushBlock();
        self.nextSemaBlock();
        self.curBlock.frameLoc = nodeId;

        const jumpStackStart = self.blockJumpStack.items.len;

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
        const sblock = self.curSemaBlock();
        const numLocals = @intCast(u32, self.curBlock.numLocals + self.curBlock.numTempLocals);
        const func = self.funcDecls[node.head.func.decl_id];
        const numParams = @intCast(u8, func.params.end - func.params.start);
        const numCaptured = @intCast(u8, sblock.params.items.len - numParams);

        self.patchJumpToCurrent(jumpPc);

        self.patchBlockJumps(jumpStackStart);
        self.blockJumpStack.items.len = jumpStackStart;

        self.prevSemaBlock();
        self.popBlock();

        if (numCaptured > 0) {
            const operandStart = self.operandStack.items.len;
            defer self.operandStack.items.len = operandStart;

            // Push register operands to op.
            for (sblock.params.items) |varId| {
                const svar = self.vars.items[varId];
                if (svar.isCaptured) {
                    const pId = self.capVarParents.get(varId).?;
                    const pvar = &self.vars.items[pId];

                    if (svar.isBoxed and !pvar.isBoxed) {
                        // Lift var to boxed.
                        try self.buf.pushOp2(.box, pvar.local, pvar.local);
                        pvar.isBoxed = true;
                        pvar.vtype = BoxType;
                        pvar.lifetimeRcCandidate = true;

                        try self.buf.pushOp1(.retain, pvar.local);
                        try self.pushTempOperand(pvar.local);
                    } else {
                        if (svar.vtype.rcCandidate) {
                            try self.buf.pushOp1(.retain, pvar.local);
                        }
                        try self.pushTempOperand(pvar.local);
                    }
                }
            }
            try self.buf.pushOp3(.funcSymClosure, @intCast(u8, symId), @intCast(u8, numParams), numCaptured);
            try self.buf.pushOperands(self.operandStack.items[operandStart..]);
        }

        // A closure would update the symbol at runtime.
        const sym = cy.FuncSymbolEntry.initFunc(opStart, numLocals);
        self.vm.setFuncSym(symId, sym);
    }

    fn genCallArgs(self: *VMcompiler, first: cy.NodeId) !u32 {
        var numArgs: u32 = 0;
        var argId = first;
        while (argId != NullId) : (numArgs += 1) {
            const arg = self.nodes[argId];
            _ = try self.genRetainedTempExpr(argId, false);
            argId = arg.next;
        }
        return numArgs;
    }

    fn genGetResolvedSym(self: *const VMcompiler, semaSymId: SemaSymId) ?SemaResolvedSym {
        const sym = self.semaSyms.items[semaSymId];
        if (sym.resolvedSymId != NullId) {
            return self.semaResolvedSyms.items[sym.resolvedSymId];
        } else {
            if (sym.symT == .undefined) {
                return null;
            } else {
                return SemaResolvedSym{
                    .symT = switch (sym.symT) {
                        .func => .func,
                        .undefined => stdx.fatal(),
                    },
                    .path = sym.path,
                };
            }
        }
    }

    fn genCallExpr(self: *VMcompiler, nodeId: cy.NodeId, dst: LocalId, comptime discardTopExprReg: bool, comptime startFiber: bool) !GenValue {
        const startTempLocal = self.curBlock.firstFreeTempLocal;
        // Can assume temps after startTempLocal are not arc temps.
        defer self.setFirstFreeTempLocal(startTempLocal);

        var callStartLocal = self.advanceNextTempLocalPastArcTemps();

        // Reserve next two temps for return value and return info.
        if (dst + 1 == self.curBlock.firstFreeTempLocal) {
            callStartLocal -= 1;
        } else {
            _ = try self.nextFreeTempLocal();
        }
        _ = try self.nextFreeTempLocal();

        const genCallStartLocal = if (startFiber) 0 else callStartLocal;

        const node = self.nodes[nodeId];
        const callee = self.nodes[node.head.func_call.callee];
        if (!node.head.func_call.has_named_arg) {
            if (callee.node_t == .accessExpr) {
                if (callee.head.accessExpr.semaSymId != NullId) {
                    if (self.genGetResolvedSym(callee.head.accessExpr.semaSymId)) |semaSym| {
                        if (semaSym.symT == .func) {
                            // Symbol func call.
                            const numArgs = try self.genCallArgs(node.head.func_call.arg_head);
                            // var isStdCall = false;
                            if (self.vm.getFuncSym(semaSym.path)) |symId| {
                                if (discardTopExprReg) {
                                    try self.buf.pushOp3(.callSym0, @intCast(u8, symId), callStartLocal, @intCast(u8, numArgs));
                                    try self.pushDebugSym(nodeId);
                                } else {
                                    try self.buf.pushOp3(.callSym1, @intCast(u8, symId), callStartLocal, @intCast(u8, numArgs));
                                    try self.pushDebugSym(nodeId);
                                }
                                return GenValue.initTempValue(callStartLocal, AnyType);
                            } else {
                                return self.reportError("Unsupported callee", .{}, node);
                            }

                            // if (try self.readScopedVar(leftName)) |info| {
                            //     if (info.vtype.typeT == ListType.typeT) {
                            //         if (self.vm.hasMethodSym(cy.ListS, methodId)) {
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
                            return self.reportError("Unsupported callee", .{}, node);
                        }
                    } else {
                        const symPath = self.semaSyms.items[callee.head.accessExpr.semaSymId].path;
                        return self.reportError("Unsupported callee: {s}", .{symPath}, node);
                    }
                } else {
                    const right = self.nodes[callee.head.accessExpr.right];
                    if (right.node_t == .ident) {
                        // One more arg for receiver.
                        const numArgs = 1 + try self.genCallArgs(node.head.func_call.arg_head);

                        const rightName = self.getNodeTokenString(right);
                        const methodId = try self.vm.ensureMethodSymKey(rightName);

                        _ = try self.genRetainedTempExpr(callee.head.accessExpr.left, false);

                        if (discardTopExprReg) {
                            try self.buf.pushOp3(.callObjSym0, @intCast(u8, methodId), callStartLocal, @intCast(u8, numArgs));
                            try self.pushDebugSym(nodeId);
                        } else {
                            try self.buf.pushOp3(.callObjSym1, @intCast(u8, methodId), callStartLocal, @intCast(u8, numArgs));
                            try self.pushDebugSym(nodeId);
                        }
                        return GenValue.initTempValue(callStartLocal, AnyType);
                    } else return self.reportError("Unsupported callee", .{}, node);
                }
            } else if (callee.node_t == .ident) {
                if (self.genGetVar(callee.head.ident.semaVarId)) |_| {
                    var numArgs: u32 = 1;
                    var argId = node.head.func_call.arg_head;
                    while (argId != NullId) : (numArgs += 1) {
                        const arg = self.nodes[argId];
                        _ = try self.genRetainedTempExpr(argId, false);
                        argId = arg.next;
                    }

                    _ = try self.genRetainedTempExpr(node.head.func_call.callee, false);
                    self.setFirstFreeTempLocal(startTempLocal + 1);

                    if (discardTopExprReg) {
                        try self.buf.pushOp2(.call0, callStartLocal, @intCast(u8, numArgs));
                        return GenValue.initNoValue();
                    } else {
                        try self.buf.pushOp2(.call1, callStartLocal, @intCast(u8, numArgs));
                        return GenValue.initTempValue(callStartLocal, AnyType);
                    }
                } else {
                    const name = self.getNodeTokenString(callee);

                    const numArgs = try self.genCallArgs(node.head.func_call.arg_head);

                    const coinitPc = self.buf.ops.items.len;
                    if (startFiber) {
                        // Precompute first arg local since coinit doesn't need the startLocal.
                        var initialStackSize = numArgs + 2;
                        if (initialStackSize < 16) {
                            initialStackSize = 16;
                        }
                        try self.buf.pushOpSlice(.coinit, &.{ callStartLocal + 2, @intCast(u8, numArgs), 0, @intCast(u8, initialStackSize), dst });
                    }

                    const symId = self.vm.getGlobalFuncSym(name) orelse (try self.vm.ensureFuncSym(name));
                    if (discardTopExprReg) {
                        try self.buf.pushOp3(.callSym0, @intCast(u8, symId), genCallStartLocal, @intCast(u8, numArgs));
                        try self.pushDebugSym(nodeId);
                    } else {
                        try self.buf.pushOp3(.callSym1, @intCast(u8, symId), genCallStartLocal, @intCast(u8, numArgs));
                        try self.pushDebugSym(nodeId);
                    }

                    if (startFiber) {
                        try self.buf.pushOp(.coreturn);
                        self.buf.setOpArgs1(coinitPc + 3, @intCast(u8, self.buf.ops.items.len - coinitPc));
                    }

                    return GenValue.initTempValue(callStartLocal, AnyType);
                }
            } else return self.reportDebugError("Unsupported callee {}", .{callee.node_t});
        } else return self.reportDebugError("Unsupported named args", .{});
    }

    fn genIfExpr(self: *VMcompiler, nodeId: cy.NodeId, dst: LocalId, retainEscapeTop: bool, comptime discardTopExprReg: bool) !GenValue {
        const node = self.nodes[nodeId];
        const startTempLocal = self.curBlock.firstFreeTempLocal;
        defer self.computeNextTempLocalFrom(startTempLocal);

        const condv = try self.genExpr(node.head.if_expr.cond, false);
        var jumpNotPc = try self.pushEmptyJumpNotCond(condv.local);

        self.computeNextTempLocalFrom(startTempLocal);
    
        var truev = try self.genExprTo(node.head.if_expr.body_expr, dst, retainEscapeTop, false);
        const jumpPc = try self.pushEmptyJump();
        self.patchJumpToCurrent(jumpNotPc);

        self.computeNextTempLocalFrom(startTempLocal);

        var falsev: GenValue = undefined;
        if (node.head.if_expr.else_clause != NullId) {
            const else_clause = self.nodes[node.head.if_expr.else_clause];
            falsev = try self.genExprTo(else_clause.head.child_head, dst, retainEscapeTop, discardTopExprReg);
        } else {
            if (!discardTopExprReg) {
                falsev = try self.genNone(dst);
            }
        }

        self.patchJumpToCurrent(jumpPc);

        if (truev.vtype.typeT != falsev.vtype.typeT) {
            return self.initGenValue(dst, truev.vtype);
        } else {
            return self.initGenValue(dst, AnyType);
        }
    }

    fn genString(self: *VMcompiler, str: []const u8, dst: LocalId) !GenValue {
        const idx = try self.buf.pushStringConst(str);
        try self.buf.pushOp2(.constOp, @intCast(u8, idx), dst);
        return self.initGenValue(dst, ConstStringType);
    }

    fn genConst(self: *VMcompiler, val: f64, dst: LocalId) !GenValue {
        if (cy.Value.floatCanBeInteger(val)) {
            const i = @floatToInt(i64, val);
            if (i >= std.math.minInt(i8) and i <= std.math.maxInt(i8)) {
                try self.buf.pushOp2(.constI8, @bitCast(u8, @intCast(i8, i)), dst);
                return self.initGenValue(dst, NumberType);
            }
        }
        const idx = try self.buf.pushConst(cy.Const.init(@bitCast(u64, val)));
        try self.buf.pushOp2(.constOp, @intCast(u8, idx), dst);
        return self.initGenValue(dst, NumberType);
    }

    fn genNone(self: *VMcompiler, dst: LocalId) !GenValue {
        try self.buf.pushOp1(.none, dst);
        return self.initGenValue(dst, AnyType);
    }

    fn isArcTempLocal(self: *const VMcompiler, local: LocalId) bool {
        for (self.arcTempLocalStack.items[self.curBlock.arcTempLocalStart..]) |arcLocal| {
            if (arcLocal == local) {
                return true;
            }
        }
        return false;
    }

    fn userLocalOrDst(self: *VMcompiler, nodeId: cy.NodeId, dst: LocalId, usedDst: *bool) LocalId {
        if (self.nodes[nodeId].node_t == .ident) {
            if (self.genGetVar(self.nodes[nodeId].head.ident.semaVarId)) |svar| {
                return svar.local;
            }
        }
        usedDst.* = true;
        return dst;
    }

    fn userLocalOrNextTempLocal(self: *VMcompiler, nodeId: cy.NodeId) !LocalId {
        const node = self.nodes[nodeId];
        if (node.node_t == .ident) {
            if (self.genGetVar(self.nodes[nodeId].head.ident.semaVarId)) |svar| {
                return svar.local;
            }
        } else if (node.node_t == .call_expr) {
            // Since call expr args allocate arg locals past the arc temps,
            // select the call dst to be past the arc temps to skip generating an extra copy op.
            _ = self.advanceNextTempLocalPastArcTemps();
            return self.nextFreeTempLocal();
        }
        return self.nextFreeTempLocal();
    }

    fn advanceNextTempLocalPastArcTemps(self: *VMcompiler) LocalId {
        if (self.curBlock.arcTempLocalStart < self.arcTempLocalStack.items.len) {
            for (self.arcTempLocalStack.items[self.curBlock.arcTempLocalStart..]) |local| {
                if (self.curBlock.firstFreeTempLocal < local) {
                    self.curBlock.firstFreeTempLocal = local + 1;
                }
            }
        }
        return self.curBlock.firstFreeTempLocal;
    }

    /// TODO: Rename to reserveNextTempLocal.
    fn nextFreeTempLocal(self: *VMcompiler) !LocalId {
        if (self.curBlock.firstFreeTempLocal < 256) {
            if (self.curBlock.firstFreeTempLocal == self.curBlock.numLocals + self.curBlock.numTempLocals) {
                self.curBlock.numTempLocals += 1;
            }
            defer {
                // Advance to the next free temp considering reserved arc temps.
                self.curBlock.firstFreeTempLocal += 1;
                if (self.curBlock.arcTempLocalStart < self.arcTempLocalStack.items.len) {
                    while (self.isArcTempLocal(self.curBlock.firstFreeTempLocal)) {
                        self.curBlock.firstFreeTempLocal += 1;
                    }
                }
            }
            return @intCast(u8, self.curBlock.firstFreeTempLocal);
        } else {
            return self.reportDebugError("Exceeded max locals.", .{});
        }
    }

    fn computeNextTempLocalFrom(self: *VMcompiler, local: LocalId) void {
        self.curBlock.firstFreeTempLocal = local;
        if (self.curBlock.arcTempLocalStart < self.arcTempLocalStack.items.len) {
            while (self.isArcTempLocal(self.curBlock.firstFreeTempLocal)) {
                self.curBlock.firstFreeTempLocal += 1;
            }
        }
    }

    fn resetNextFreeTemp(self: *VMcompiler) void {
        self.curBlock.firstFreeTempLocal = @intCast(u8, self.curBlock.numLocals);
    }

    fn setFirstFreeTempLocal(self: *VMcompiler, local: LocalId) void {
        self.curBlock.firstFreeTempLocal = local;
    }

    /// Given two local values, determine the next destination temp local.
    /// The type of the dest value is left undefined to be set by caller.
    fn nextTempDestValue(self: *VMcompiler, src1: GenValue, src2: GenValue) !GenValue {
        if (src1.isTempLocal == src2.isTempLocal) {
            if (src1.isTempLocal) {
                const minTempLocal = std.math.min(src1.local, src2.local);
                self.setFirstFreeTempLocal(minTempLocal + 1);
                return GenValue.initTempValue(minTempLocal, undefined);
            } else {
                return GenValue.initTempValue(try self.nextFreeTempLocal(), undefined);
            }
        } else {
            if (src1.isTempLocal) {
                return GenValue.initTempValue(src1.local, undefined);
            } else {
                return GenValue.initTempValue(src2.local, undefined);
            }
        }
    }

    /// If the expression is a user local, the local is returned.
    /// Otherwise, the expression is allocated a temp local on the stack.
    fn genExpr(self: *VMcompiler, nodeId: cy.NodeId, comptime discardTopExprReg: bool) anyerror!GenValue {
        const dst = try self.userLocalOrNextTempLocal(nodeId);
        return self.genExprTo(nodeId, dst, false, discardTopExprReg);
    }

    fn canUseLocalAsTemp(self: *const VMcompiler, local: LocalId) bool {
        return local == 0 or local >= self.curBlock.numLocals;
    }

    fn isTempLocal(self: *const VMcompiler, local: LocalId) bool {
        return local >= self.curBlock.numLocals;
    }

    fn initGenValue(self: *const VMcompiler, local: LocalId, vtype: Type) GenValue {
        if (local >= self.curBlock.numLocals) {
            return GenValue.initTempValue(local, vtype);
        } else {
            return GenValue.initLocalValue(local, vtype);
        }
    }

    fn genPushBinOp(self: *VMcompiler, code: cy.OpCode, left: cy.NodeId, right: cy.NodeId, vtype: Type, dst: LocalId, comptime discardTopExprReg: bool) !GenValue {
        const startTempLocal = self.curBlock.firstFreeTempLocal;
        defer self.computeNextTempLocalFrom(startTempLocal);

        var usedDstAsTemp = !self.canUseLocalAsTemp(dst);
        const leftv = try self.genExprToDestOrTempLocal(left, dst, &usedDstAsTemp, discardTopExprReg);
        const rightv = try self.genExprToDestOrTempLocal(right, dst, &usedDstAsTemp, discardTopExprReg);
        if (!discardTopExprReg) {
            try self.buf.pushOp3(code, leftv.local, rightv.local, dst);
            return self.initGenValue(dst, vtype);
        } else return GenValue.initNoValue();
    }

    fn pushTempOperand(self: *VMcompiler, operand: u8) !void {
        try self.operandStack.append(self.alloc, cy.OpData.initArg(operand));
    }

    fn unescapeString(self: *VMcompiler, literal: []const u8) ![]const u8 {
        try self.u8Buf.resize(self.alloc, literal.len);
        return Root.unescapeString(self.u8Buf.items, literal);
    }

    /// `dst` indicates the local of the resulting value.
    /// `retainEscapeTop` indicates that the resulting val is meant to be used to escape the current scope. (eg. call args)
    /// If `retainEscapeTop` is false, the dst is a temp local, and the expr requires a retain (eg. call expr), it is added as an arcTempLocal.
    fn genExprTo(self: *VMcompiler, nodeId: cy.NodeId, dst: LocalId, retainEscapeTop: bool, comptime discardTopExprReg: bool) anyerror!GenValue {
        const node = self.nodes[nodeId];
        // log.debug("gen reg expr {}", .{node.node_t});
        switch (node.node_t) {
            .ident => {
                if (self.genGetVar(node.head.ident.semaVarId)) |svar| {
                    if (dst == svar.local) {
                        if (retainEscapeTop) {
                            stdx.panic("Unexpected retainEscapeTop.");
                        }
                        return GenValue.initSemaVar(svar);
                    } else {
                        if (retainEscapeTop and svar.vtype.rcCandidate) {
                            if (svar.isBoxed) {
                                try self.buf.pushOp2(.boxValueRetain, svar.local, dst);
                            } else {
                                try self.buf.pushOp2(.copyRetainSrc, svar.local, dst);
                            }
                        } else {
                            if (svar.isBoxed) {
                                try self.buf.pushOp2(.boxValue, svar.local, dst);
                            } else {
                                try self.buf.pushOp2(.copy, svar.local, dst);
                            }
                        }
                        return self.initGenValue(dst, svar.vtype);
                    }
                } else {
                    return self.genNone(dst);
                }
            },
            .true_literal => {
                if (!discardTopExprReg) {
                    try self.buf.pushOp1(.true, dst);
                    return self.initGenValue(dst, BoolType);
                } else return GenValue.initNoValue();
            },
            .false_literal => {
                if (!discardTopExprReg) {
                    try self.buf.pushOp1(.false, dst);
                    return self.initGenValue(dst, BoolType);
                } else return GenValue.initNoValue();
            },
            .number => {
                if (!discardTopExprReg) {
                    const literal = self.getNodeTokenString(node);
                    const val = try std.fmt.parseFloat(f64, literal);
                    return try self.genConst(val, dst);
                } else {
                    return GenValue.initNoValue();
                }
            },
            .tagLiteral => {
                const name = self.getNodeTokenString(node);
                const symId = try self.vm.ensureTagLitSym(name);
                try self.buf.pushOp2(.tagLiteral, @intCast(u8, symId), dst);
                return self.initGenValue(dst, TagLiteralType);
            },
            .string => {
                if (!discardTopExprReg) {
                    const literal = self.getNodeTokenString(node);
                    const str = try self.unescapeString(literal);
                    return self.genString(str, dst);
                } else {
                    return GenValue.initNoValue();
                }
            },
            .stringTemplate => {
                const first = self.nodes[node.head.stringTemplate.partsHead];
                if (node.head.stringTemplate.firstIsString and first.next == NullId) {
                    // Just a string.
                    if (!discardTopExprReg) {
                        const str = self.getNodeTokenString(first);
                        return self.genString(str, dst);
                    }
                }

                const operandStart = self.operandStack.items.len;
                defer self.operandStack.items.len = operandStart;

                if (!node.head.stringTemplate.firstIsString) {
                    if (!discardTopExprReg) {
                        // Insert empty string.
                        const idx = try self.buf.pushStringConst("");
                        try self.operandStack.append(self.alloc, cy.OpData.initArg(@intCast(u8, idx)));
                    }
                }

                const startTempLocal = self.curBlock.firstFreeTempLocal;
                defer self.computeNextTempLocalFrom(startTempLocal);

                const argStartLocal = self.advanceNextTempLocalPastArcTemps();

                var nextIsExpr = !node.head.stringTemplate.firstIsString;
                var curId = node.head.stringTemplate.partsHead;
                var numExprs: u32 = 0;
                while (true) {
                    const cur = self.nodes[curId];
                    if (!nextIsExpr) {
                        if (!discardTopExprReg) {
                            const str = self.getNodeTokenString(cur);
                            const idx = try self.buf.pushStringConst(str);
                            try self.operandStack.append(self.alloc, cy.OpData.initArg(@intCast(u8, idx)));
                        }
                    } else {
                        _ = try self.genRetainedTempExpr(curId, discardTopExprReg);
                        numExprs += 1;
                    }
                    if (cur.next == NullId) {
                        if (nextIsExpr) {
                            // Insert empty string.
                            if (!discardTopExprReg) {
                                const idx = try self.buf.pushStringConst("");
                                try self.operandStack.append(self.alloc, cy.OpData.initArg(@intCast(u8, idx)));
                            }
                        }
                        break;
                    } else {
                        curId = cur.next;
                        nextIsExpr = !nextIsExpr;
                    }
                }

                if (!discardTopExprReg) {
                    try self.buf.pushOp3(.stringTemplate, argStartLocal, @intCast(u8, numExprs), dst);
                    try self.buf.pushOperands(self.operandStack.items[operandStart..]);

                    if (!retainEscapeTop and self.isTempLocal(dst)) {
                        // Retain as arc temp.
                        try self.arcTempLocalStack.append(self.alloc, dst);
                    }
                    return self.initGenValue(dst, StringType);
                } else {
                    return GenValue.initNoValue();
                }
            },
            .none => {
                if (!discardTopExprReg) {
                    return self.genNone(dst);
                } else return GenValue.initNoValue();
            },
            .tagInit => {
                const name = self.nodes[node.head.left_right.left];
                const tname = self.getNodeTokenString(name);
                const tid = self.vm.tagTypeSignatures.get(tname) orelse {
                    log.debug("Missing tag type {s}", .{tname});
                    return error.CompileError;
                };

                const tagLit = self.nodes[node.head.left_right.right];
                const lname = self.getNodeTokenString(tagLit);
                const symId = self.vm.tagLitSymSignatures.get(lname) orelse {
                    log.debug("Missing tag literal {s}", .{lname});
                    return error.CompileError;
                };
                const sym = self.vm.tagLitSyms.buf[symId];
                if (sym.symT == .one) {
                    if (sym.inner.one.id == tid) {
                        try self.buf.pushOp3(.tag, @intCast(u8, tid), @intCast(u8, sym.inner.one.val), dst);
                        const vtype = initTagType(tid);
                        return self.initGenValue(dst, vtype);
                    }
                }

                log.debug("Tag {s} does not have member {s}", .{tname, lname});
                return error.CompileError;
            },
            .structInit => {
                const stype = self.nodes[node.head.structInit.name];
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

                const startTempLocal = self.curBlock.firstFreeTempLocal;
                defer self.computeNextTempLocalFrom(startTempLocal);

                const argStartLocal = self.advanceNextTempLocalPastArcTemps();

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

                    _ = try self.genRetainedTempExpr(entry.head.left_right.right, discardTopExprReg);
                    entryId = entry.next;
                }

                if (i != self.vm.structs.buf[sid].numFields) {
                    log.debug("Default values not supported. {} {}", .{i, self.vm.structs.buf[sid].numFields});
                    return error.CompileError;
                }

                if (!discardTopExprReg) {
                    try self.buf.pushOpSlice(.structSmall, &.{ @intCast(u8, sid), argStartLocal, @intCast(u8, i), dst });
                    try self.buf.pushOperands(self.operandStack.items[operandStart..]);
                    // try self.buf.pushOp1(.pushStructInit, );
                    if (!retainEscapeTop and self.isTempLocal(dst)) {
                        // Retain as arc temp.
                        try self.arcTempLocalStack.append(self.alloc, dst);
                    }
                    return GenValue.initTempValue(dst, AnyType);
                } else {
                    return GenValue.initNoValue();
                }
            },
            .if_expr => {
                return self.genIfExpr(nodeId, dst, retainEscapeTop, discardTopExprReg);
            },
            .map_literal => {
                const operandStart = self.operandStack.items.len;
                defer self.operandStack.items.len = operandStart;

                const startTempLocal = self.curBlock.firstFreeTempLocal;
                defer self.computeNextTempLocalFrom(startTempLocal);

                const argStartLocal = self.advanceNextTempLocalPastArcTemps();

                var i: u32 = 0;
                var entry_id = node.head.child_head;
                while (entry_id != NullId) : (i += 1) {
                    var entry = self.nodes[entry_id];
                    const key = self.nodes[entry.head.left_right.left];

                    if (!discardTopExprReg) {
                        switch (key.node_t) {
                            .ident => {
                                const name = self.getNodeTokenString(key);
                                const idx = try self.buf.pushStringConst(name);
                                try self.operandStack.append(self.alloc, cy.OpData.initArg(@intCast(u8, idx)));
                            },
                            .string => {
                                const name = self.getNodeTokenString(key);
                                const idx = try self.buf.pushStringConst(name);
                                try self.operandStack.append(self.alloc, cy.OpData.initArg(@intCast(u8, idx)));
                            },
                            else => stdx.panicFmt("unsupported key {}", .{key.node_t}),
                        }
                    }

                    _ = try self.genRetainedTempExpr(entry.head.left_right.right, discardTopExprReg);
                    entry_id = entry.next;
                }

                if (!discardTopExprReg) {
                    if (i == 0) {
                        try self.buf.pushOp1(.mapEmpty, dst);
                    } else {
                        try self.buf.pushOp3(.map, argStartLocal, @intCast(u8, i), dst);
                        try self.buf.pushOperands(self.operandStack.items[operandStart..]);
                    }

                    if (!retainEscapeTop and self.isTempLocal(dst)) {
                        try self.arcTempLocalStack.append(self.alloc, dst);
                    }
                    return self.initGenValue(dst, MapType);
                } else {
                    return GenValue.initNoValue();
                }
            },
            .arr_literal => {
                const startLocal = self.curBlock.firstFreeTempLocal;
                defer self.computeNextTempLocalFrom(startLocal);

                const argStartLocal = self.advanceNextTempLocalPastArcTemps();

                var exprId = node.head.child_head;
                var i: u32 = 0;
                while (exprId != NullId) : (i += 1) {
                    const expr = self.nodes[exprId];
                    _ = try self.genRetainedTempExpr(exprId, discardTopExprReg);
                    exprId = expr.next;
                }

                if (!discardTopExprReg) {
                    try self.buf.pushOp3(.list, argStartLocal, @intCast(u8, i), dst);
                    if (!retainEscapeTop and self.isTempLocal(dst)) {
                        try self.arcTempLocalStack.append(self.alloc, dst);
                    }
                    return self.initGenValue(dst, ListType);
                } else {
                    return GenValue.initNoValue();
                }
            },
            .arr_range_expr => {
                const startTempLocal = self.curBlock.firstFreeTempLocal;
                defer self.computeNextTempLocalFrom(startTempLocal);

                var usedDstAsTemp = !self.canUseLocalAsTemp(dst);

                // Parent value.
                const parentv = try self.genExprToDestOrTempLocal(node.head.arr_range_expr.arr, dst, &usedDstAsTemp, discardTopExprReg);

                // Range left value.
                var leftv: GenValue = undefined;
                if (node.head.arr_range_expr.left == NullId) {
                    if (!discardTopExprReg) {
                        if (usedDstAsTemp) {
                            const leftDst = try self.nextFreeTempLocal();
                            leftv = try self.genConst(0, leftDst);
                        } else {
                            leftv = try self.genConst(0, dst);
                            usedDstAsTemp = true;
                        }
                    }
                } else {
                    leftv = try self.genExprToDestOrTempLocal(node.head.arr_range_expr.left, dst, &usedDstAsTemp, discardTopExprReg);
                }

                // Range right value.
                var rightv: GenValue = undefined;
                if (node.head.arr_range_expr.right == NullId) {
                    if (!discardTopExprReg) {
                        if (usedDstAsTemp) {
                            const rightDst = try self.nextFreeTempLocal();
                            rightv = try self.genConst(-1, rightDst);
                        } else {
                            rightv = try self.genConst(-1, dst);
                            usedDstAsTemp = true;
                        }
                    }
                } else {
                    rightv = try self.genExprToDestOrTempLocal(node.head.arr_range_expr.right, dst, &usedDstAsTemp, discardTopExprReg);
                }

                if (!discardTopExprReg) {
                    try self.buf.pushOpSlice(.slice, &.{ parentv.local, leftv.local, rightv.local, dst });
                    if (!retainEscapeTop and self.isTempLocal(dst)) {
                        try self.arcTempLocalStack.append(self.alloc, dst);
                    }
                    return self.initGenValue(dst, ListType);
                } else {
                    return GenValue.initNoValue();
                }
            },
            .arr_access_expr => {
                const startTempLocal = self.curBlock.firstFreeTempLocal;
                defer self.computeNextTempLocalFrom(startTempLocal);

                var usedDstAsTemp = !self.canUseLocalAsTemp(dst);

                // Gen left.
                const leftv = try self.genExprToDestOrTempLocal(node.head.left_right.left, dst, &usedDstAsTemp, discardTopExprReg);

                // Gen index.
                const index = self.nodes[node.head.left_right.right];
                const isReverseIndex = index.node_t == .unary_expr and index.head.unary.op == .minus;
                const indexv = try self.genExprToDestOrTempLocal(node.head.left_right.right, dst, &usedDstAsTemp, discardTopExprReg);

                if (!discardTopExprReg) {
                    if (isReverseIndex) {
                        if (retainEscapeTop) {
                            try self.buf.pushOp3(.reverseIndexRetain, leftv.local, indexv.local, dst);
                        } else {
                            try self.buf.pushOp3(.reverseIndex, leftv.local, indexv.local, dst);
                        }
                    } else {
                        if (retainEscapeTop) {
                            try self.buf.pushOp3(.indexRetain, leftv.local, indexv.local, dst);
                        } else {
                            try self.buf.pushOp3(.index, leftv.local, indexv.local, dst);
                        }
                    }
                    return self.initGenValue(dst, AnyType);
                } else {
                    return GenValue.initNoValue();
                }
            },
            .accessExpr => {
                const startTempLocal = self.curBlock.firstFreeTempLocal;
                defer self.computeNextTempLocalFrom(startTempLocal);

                var usedDstAsTemp = !self.canUseLocalAsTemp(dst);

                // Left side.
                const leftv = try self.genExprToDestOrTempLocal(node.head.accessExpr.left, dst, &usedDstAsTemp, discardTopExprReg);

                // Right should be an ident.
                const right = self.nodes[node.head.accessExpr.right];

                const name = self.getNodeTokenString(right);
                const fieldId = try self.vm.ensureFieldSym(name);

                if (!discardTopExprReg) {
                    if (retainEscapeTop) {
                        try self.buf.pushOp3(.fieldRetain, @intCast(u8, fieldId), leftv.local, dst);
                    } else {
                        try self.buf.pushOp3(.field, @intCast(u8, fieldId), leftv.local, dst);
                    }
                    try self.pushDebugSym(nodeId);
                    return self.initGenValue(dst, AnyType);
                } else {
                    return GenValue.initNoValue();
                }
            },
            .tryExpr => {
                const child = try self.genExpr(node.head.child_head, false);
                try self.buf.pushOp2(.tryValue, child.local, dst);
                return self.initGenValue(dst, AnyType);
            },
            .unary_expr => {
                const op = node.head.unary.op;
                switch (op) {
                    .minus => {
                        const child = try self.genExpr(node.head.unary.child, discardTopExprReg);
                        if (!discardTopExprReg) {
                            try self.buf.pushOp2(.neg, child.local, dst);
                            return self.initGenValue(dst, NumberType);
                        } else return GenValue.initNoValue();
                    },
                    .not => {
                        const child = try self.genExpr(node.head.unary.child, discardTopExprReg);
                        if (!discardTopExprReg) {
                            try self.buf.pushOp2(.not, child.local, dst);
                            return self.initGenValue(dst, BoolType);
                        } else return GenValue.initNoValue();
                    },
                    .bitwiseNot => {
                        const child = try self.genExpr(node.head.unary.child, discardTopExprReg);
                        if (!discardTopExprReg) {
                            try self.buf.pushOp2(.bitwiseNot, child.local, dst);
                            return self.initGenValue(dst, NumberType);
                        } else return GenValue.initNoValue();
                    },
                    // else => return self.reportError("Unsupported unary op: {}", .{op}, node),
                }
            },
            .bin_expr => {
                const left = node.head.left_right.left;
                const right = node.head.left_right.right;

                const op = @intToEnum(cy.BinaryExprOp, node.head.left_right.extra);
                switch (op) {
                    .slash => {
                        return self.genPushBinOp(.div, left, right, NumberType, dst, discardTopExprReg);
                    },
                    .percent => {
                        return self.genPushBinOp(.mod, left, right, NumberType, dst, discardTopExprReg);
                    },
                    .caret => {
                        return self.genPushBinOp(.pow, left, right, NumberType, dst, discardTopExprReg);
                    },
                    .star => {
                        return self.genPushBinOp(.mul, left, right, NumberType, dst, discardTopExprReg);
                    },
                    .plus => {
                        const startTempLocal = self.curBlock.firstFreeTempLocal;
                        defer self.computeNextTempLocalFrom(startTempLocal);

                        var usedDstAsTemp = !self.canUseLocalAsTemp(dst);
                        const leftv = try self.genExprToDestOrTempLocal(left, dst, &usedDstAsTemp, discardTopExprReg);
                        const rightv = try self.genExprToDestOrTempLocal(right, dst, &usedDstAsTemp, discardTopExprReg);
                        if (!discardTopExprReg) {
                            try self.buf.pushOp3(.add, leftv.local, rightv.local, dst);
                            try self.pushDebugSym(nodeId);
                            if (leftv.vtype.typeT == .string) {
                                return self.initGenValue(dst, StringType);
                            } else {
                                return self.initGenValue(dst, NumberType);
                            }
                        } else {
                            return GenValue.initNoValue();
                        }
                    },
                    .minus => {
                        return self.genPushBinOp(.minus, left, right, NumberType, dst, discardTopExprReg);
                    },
                    .equal_equal => {
                        return self.genPushBinOp(.compare, left, right, BoolType, dst, discardTopExprReg);
                    },
                    .bang_equal => {
                        return self.genPushBinOp(.compareNot, left, right, BoolType, dst, discardTopExprReg);
                    },
                    .less => {
                        return self.genPushBinOp(.less, left, right, BoolType, dst, discardTopExprReg);
                    },
                    .less_equal => {
                        return self.genPushBinOp(.lessEqual, left, right, BoolType, dst, discardTopExprReg);
                    },
                    .greater => {
                        return self.genPushBinOp(.greater, left, right, BoolType, dst, discardTopExprReg);
                    },
                    .greater_equal => {
                        return self.genPushBinOp(.greaterEqual, left, right, BoolType, dst, discardTopExprReg);
                    },
                    .bitwiseAnd => {
                        return self.genPushBinOp(.bitwiseAnd, left, right, NumberType, dst, discardTopExprReg);
                    },
                    .bitwiseOr => {
                        return self.genPushBinOp(.bitwiseOr, left, right, NumberType, dst, discardTopExprReg);
                    },
                    .bitwiseXor => {
                        return self.genPushBinOp(.bitwiseXor, left, right, NumberType, dst, discardTopExprReg);
                    },
                    .bitwiseLeftShift => {
                        return self.genPushBinOp(.bitwiseLeftShift, left, right, NumberType, dst, discardTopExprReg);
                    },
                    .bitwiseRightShift => {
                        return self.genPushBinOp(.bitwiseRightShift, left, right, NumberType, dst, discardTopExprReg);
                    },
                    .and_op => {
                        const startTempLocal = self.curBlock.firstFreeTempLocal;
                        defer self.computeNextTempLocalFrom(startTempLocal);

                        const leftv = try self.genExprTo(left, dst, false, discardTopExprReg);
                        const jumpPc = try self.pushEmptyJumpNotCond(leftv.local);
                        const rightv = try self.genExprTo(right, dst, false, discardTopExprReg);
                        self.patchJumpToCurrent(jumpPc);

                        if (leftv.vtype.typeT == rightv.vtype.typeT) {
                            if (retainEscapeTop and leftv.vtype.rcCandidate) {
                                try self.buf.pushOp1(.retain, dst);
                            }
                            return self.initGenValue(dst, leftv.vtype);
                        } else {
                            if (retainEscapeTop and (leftv.vtype.rcCandidate or rightv.vtype.rcCandidate)) {
                                try self.buf.pushOp1(.retain, dst);
                            }
                            return self.initGenValue(dst, AnyType);
                        }
                    },
                    .or_op => {
                        const startTempLocal = self.curBlock.firstFreeTempLocal;
                        defer self.computeNextTempLocalFrom(startTempLocal);

                        const leftv = try self.genExprTo(left, dst, retainEscapeTop, discardTopExprReg);
                        const jumpPc = try self.pushEmptyJumpCond(leftv.local);
                        const rightv = try self.genExprTo(right, dst, retainEscapeTop, discardTopExprReg);
                        self.patchJumpToCurrent(jumpPc);

                        if (leftv.vtype.typeT == rightv.vtype.typeT) {
                            return self.initGenValue(dst, leftv.vtype);
                        } else return self.initGenValue(dst, AnyType);
                    },
                    else => return self.reportError("Unsupported binary op: {}", .{op}, node),
                }
            },
            .call_expr => {
                const val = try self.genCallExpr(nodeId, dst, discardTopExprReg, false);
                if (dst != val.local) {
                    try self.buf.pushOp2(.copy, val.local, dst);
                }
                if (!discardTopExprReg and !retainEscapeTop and self.isTempLocal(dst)) {
                    try self.arcTempLocalStack.append(self.alloc, dst);
                }
                return self.initGenValue(dst, val.vtype);
            },
            .lambda_multi => {
                if (!discardTopExprReg) {
                    const jumpPc = try self.pushEmptyJump();

                    self.nextSemaBlock();
                    try self.pushBlock();
                    self.curBlock.frameLoc = nodeId;

                    const jumpStackStart = self.blockJumpStack.items.len;
                    const opStart = @intCast(u32, self.buf.ops.items.len);

                    // Generate function body.
                    const func = self.funcDecls[node.head.func.decl_id];
                    try self.reserveFuncParams();
                    try self.genInitLocals();
                    const numParams = @intCast(u8, func.params.end - func.params.start);

                    try self.genStatements(node.head.func.body_head, false);

                    try self.endLocals();
                    try self.buf.pushOp(.ret0);
                    self.patchJumpToCurrent(jumpPc);

                    const sblock = self.curSemaBlock();
                    const numLocals = @intCast(u8, self.curBlock.numLocals + self.curBlock.numTempLocals);
                    const numCaptured = @intCast(u8, sblock.params.items.len - numParams - 1);

                    self.patchBlockJumps(jumpStackStart);
                    self.blockJumpStack.items.len = jumpStackStart;

                    self.popBlock();
                    self.prevSemaBlock();

                    if (numCaptured == 0) {
                        const funcPcOffset = @intCast(u8, self.buf.ops.items.len - opStart);
                        try self.buf.pushOpSlice(.lambda, &.{ funcPcOffset, numParams, numLocals, dst });
                        if (!retainEscapeTop and self.isTempLocal(dst)) {
                            try self.arcTempLocalStack.append(self.alloc, dst);
                        }
                        return self.initGenValue(dst, AnyType);
                    } else {
                        const operandStart = self.operandStack.items.len;
                        defer self.operandStack.items.len = operandStart;

                        // Retain captured vars.
                        for (sblock.params.items) |varId| {
                            const svar = self.vars.items[varId];
                            if (svar.isCaptured) {
                                const pId = self.capVarParents.get(varId).?;
                                const pvar = &self.vars.items[pId];

                                if (svar.isBoxed and !pvar.isBoxed) {
                                    // Lift var to boxed.
                                    try self.buf.pushOp2(.box, pvar.local, pvar.local);
                                    pvar.isBoxed = true;
                                    pvar.vtype = BoxType;
                                    pvar.lifetimeRcCandidate = true;

                                    try self.buf.pushOp1(.retain, pvar.local);
                                    try self.pushTempOperand(pvar.local);
                                } else {
                                    if (svar.vtype.rcCandidate) {
                                        try self.buf.pushOp1(.retain, pvar.local);
                                    }
                                    try self.pushTempOperand(pvar.local);
                                }
                            }
                        }

                        const funcPcOffset = @intCast(u8, self.buf.ops.items.len - opStart);
                        try self.buf.pushOpSlice(.closure, &.{ funcPcOffset, numParams, numCaptured, numLocals, dst });
                        try self.buf.pushOperands(self.operandStack.items[operandStart..]);
                        if (!retainEscapeTop and self.isTempLocal(dst)) {
                            try self.arcTempLocalStack.append(self.alloc, dst);
                        }
                        return self.initGenValue(dst, AnyType);
                    }
                } else {
                    return GenValue.initNoValue();
                }
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
                    _ = try self.genRetainedExprTo(node.head.func.body_head, 0, false);
                    try self.endLocals();
                    try self.buf.pushOp(.ret1);
                    self.patchJumpToCurrent(jumpPc);

                    const sblock = self.curSemaBlock();
                    const numLocals = @intCast(u8, self.curBlock.numLocals + self.curBlock.numTempLocals);
                    const numCaptured = @intCast(u8, sblock.params.items.len - numParams - 1);

                    self.popBlock();
                    self.prevSemaBlock();

                    if (numCaptured == 0) {
                        const funcPcOffset = @intCast(u8, self.buf.ops.items.len - opStart);
                        try self.buf.pushOpSlice(.lambda, &.{ funcPcOffset, numParams, numLocals, dst });
                        if (!retainEscapeTop and self.isTempLocal(dst)) {
                            try self.arcTempLocalStack.append(self.alloc, dst);
                        }
                        return self.initGenValue(dst, AnyType);
                    } else {
                        const operandStart = self.operandStack.items.len;
                        defer self.operandStack.items.len = operandStart;

                        // Retain captured vars.
                        for (sblock.params.items) |varId| {
                            const svar = self.vars.items[varId];
                            if (svar.isCaptured) {
                                const pId = self.capVarParents.get(varId).?;
                                const pvar = &self.vars.items[pId];

                                if (svar.isBoxed and !pvar.isBoxed) {
                                    // Lift var to boxed.
                                    try self.buf.pushOp2(.box, pvar.local, pvar.local);
                                    pvar.isBoxed = true;
                                    pvar.vtype = BoxType;
                                    pvar.lifetimeRcCandidate = true;

                                    try self.buf.pushOp1(.retain, pvar.local);
                                    try self.pushTempOperand(pvar.local);
                                } else {
                                    if (svar.vtype.rcCandidate) {
                                        try self.buf.pushOp1(.retain, pvar.local);
                                    }
                                    try self.pushTempOperand(pvar.local);
                                }
                            }
                        }

                        const funcPcOffset = @intCast(u8, self.buf.ops.items.len - opStart);
                        try self.buf.pushOpSlice(.closure, &.{ funcPcOffset, numParams, numCaptured, numLocals, dst });
                        try self.buf.pushOperands(self.operandStack.items[operandStart..]);
                        if (!retainEscapeTop and self.isTempLocal(dst)) {
                            try self.arcTempLocalStack.append(self.alloc, dst);
                        }
                        return self.initGenValue(dst, AnyType);
                    }
                } else {
                    return GenValue.initNoValue();
                }
            },
            .coresume => {
                const fiber = try self.genRetainedTempExpr(node.head.child_head, false);
                try self.buf.pushOp2(.coresume, fiber.local, dst);
                return self.initGenValue(dst, AnyType);
            },
            .coyield => {
                const pc = self.buf.ops.items.len;
                try self.buf.pushOp2(.coyield, 0, 0);
                try self.blockJumpStack.append(self.alloc, .{ .jumpT = .jumpToEndLocals, .pc = @intCast(u32, pc) });

                // TODO: return coyield expression.
                if (!discardTopExprReg) {
                    return try self.genNone(dst);
                } else {
                    return GenValue.initNoValue();
                }
            },
            .coinit => {
                _ = try self.genCallExpr(node.head.child_head, dst, discardTopExprReg, true);
                return self.initGenValue(dst, FiberType);
            },
            else => {
                return self.reportDebugError("Unsupported {}", .{node.node_t});
            }
        }
    }

    /// Temporary way to log an error without touching std.fmt in release builds.
    fn reportDebugError(self: *const VMcompiler, comptime fmt: []const u8, args: anytype) error{CompileError} {
        _ = self;
        log.debug(fmt, args);
        return error.CompileError;
    }

    fn reportError(self: *VMcompiler, comptime fmt: []const u8, args: anytype, node: cy.Node) anyerror {
        const token = self.tokens[node.start_token];
        const customMsg = try std.fmt.allocPrint(self.alloc, fmt, args);
        defer self.alloc.free(customMsg);
        self.alloc.free(self.lastErr);
        self.lastErr = try std.fmt.allocPrint(self.alloc, "{s}: {} at {}", .{customMsg, node.node_t, token.pos()});
        return error.CompileError;
    }

    fn getNodeTokenString(self: *const VMcompiler, node: cy.Node) []const u8 {
        const token = self.tokens[node.start_token];
        return self.src[token.pos()..token.data.end_pos];
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
    /// This includes the return info, function params, captured params, and local vars.
    /// Does not include temp locals.
    numLocals: u32,
    frameLoc: cy.NodeId = NullId,
    endLocalsPc: u32,

    /// These are used for rvalues and function args.
    /// At the end of the block, the total stack size needed for the function body is known.
    numTempLocals: u8,

    /// Starts at `numLocals`.
    /// Temp locals are allocated from the end of the user locals towards the right.
    firstFreeTempLocal: u8,

    /// Start of the first retained temp local of the current ARC expr.
    /// This must be kept updated as codegen walks the ast so that sub expressions
    /// knows which ARC expr they belong to.
    arcTempLocalStart: u32,

    fn init() Block {
        return .{
            .numLocals = 0,
            .endLocalsPc = 0,
            .numTempLocals = 0,
            .firstFreeTempLocal = 0,
            .arcTempLocalStart = 0,
        };
    }

    fn deinit(self: *Block, alloc: std.mem.Allocator) void {
        _ = self;
        _ = alloc;
    }

    fn getRequiredStackSize(self: *const Block) u8 {
        return @intCast(u8, self.numLocals + self.numTempLocals);
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

/// TODO: Rename to SemaLocal to indicate a local var rather than a global var.
const SemaVar = struct {
    /// The current type of the var as the ast is traversed.
    /// This is updated when there is a variable assignment or a child block returns.
    vtype: Type,

    /// Whether this var is a captured function param.
    /// Currently, captured variables can not be reassigned to another value.
    /// This restriction avoids an extra retain/release op for closure calls.
    isCaptured: bool = false,

    /// A captured var needs to be boxed if:
    /// 1. It can become a primitive value during its lifetime
    /// 2. It was assigned to a value in a closure.
    /// This is updated only in codegen since a captured var can start off unboxed and later lifted.
    isBoxed: bool = false,

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

    name: if (builtin.mode == .Debug) []const u8 else void,
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
    box,
    tag,
    tagLiteral,
};

const Type = struct {
    typeT: TypeTag,
    rcCandidate: bool,
    inner: packed union {
        tagId: u8,
    } = undefined,
};

const BoxType = Type{
    .typeT = .box,
    .rcCandidate = true,
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

const TagLiteralType = Type{
    .typeT = .tagLiteral,
    .rcCandidate = false,
};

fn initTagType(tagId: u32) Type {
    return .{
        .typeT = .tag,
        .rcCandidate = false,
        .inner = .{
            .tagId = @intCast(u8, tagId),
        },
    };
}

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

fn isArcTempNode(nodeT: cy.NodeType) bool {
    switch (nodeT) {
        .call_expr,
        .arr_literal,
        .map_literal,
        .stringTemplate,
        .coinit,
        .structInit => return true,
        else => return false,
    }
}

const GenValue = struct {
    vtype: Type,
    local: LocalId,
    isTempLocal: bool,

    fn initNoValue() GenValue {
        return .{
            .vtype = undefined,
            .local = 0,
            .isTempLocal = false,
        };
    }

    fn initLocalValue(local: LocalId, vtype: Type) GenValue {
        return .{
            .vtype = vtype,
            .local = local,
            .isTempLocal = false,
        };
    }

    fn initTempValue(local: LocalId, vtype: Type) GenValue {
        return .{
            .vtype = vtype,
            .local = local,
            .isTempLocal = true,
        };
    }

    fn initSemaVar(svar: SemaVar) GenValue {
        return .{
            .vtype = svar.vtype,
            .local = svar.local,
            .isTempLocal = false,
        };
    }
};

test "Internals." {
    try t.eq(@sizeOf(SemaVar), 32);
    try t.eq(@sizeOf(Type), 3);
}

const SemaSymId = u32;
const SemaSymType = enum {
    func,
    undefined,
};

const SemaSym = struct {
    symT: SemaSymType,
    path: []const u8,

    /// Points to the first symId that starts the path.
    leadSymId: SemaSymId,

    /// After the sema pass, all semaSyms are resolved.
    resolvedSymId: SemaResolvedSymId = NullId,
};

const SemaResolvedSymId = u32;
const SemaResolvedSym = struct {
    symT: ResolvedSymType,
    path: []const u8,
};

const ResolvedSymType = enum {
    func,
};

const ModuleSymType = enum {
    nativeFunc1,
};

const ModuleSym = struct {
    symT: ModuleSymType,
    inner: union {
        nativeFunc1: struct {
            func: *const fn (*cy.UserVM, [*]const cy.Value, u8) cy.Value,
        },
    },
};

const ModuleId = u32;
const Module = struct {
    syms: std.StringHashMapUnmanaged(ModuleSym), 
    prefix: []const u8,

    fn deinit(self: *Module, alloc: std.mem.Allocator) void {
        self.syms.deinit(alloc);
    }
};

/// `buf` is assumed to be big enough.
pub fn unescapeString(buf: []u8, literal: []const u8) []const u8 {
    var newIdx: u32 = 0; 
    var i: u32 = 0;
    while (i < literal.len) : (newIdx += 1) {
        if (literal[i] == '\\') {
            if (literal[i + 1] == 'n') {
                buf[newIdx] = '\n';
            } else {
                buf[newIdx] = literal[i + 1];
            }
            i += 2;
        } else {
            buf[newIdx] = literal[i];
            i += 1;
        }
    }
    return buf[0..newIdx];
}