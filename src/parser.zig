const std = @import("std");
const stdx = @import("stdx");
const t = stdx.testing;
const fatal = stdx.fatal;

pub const NodeId = u32;
const NullId = std.math.maxInt(u32);
const log = stdx.log.scoped(.parser);
const IndexSlice = stdx.IndexSlice(u32);

const keywords = std.ComptimeStringMap(TokenType, .{
    .{ "return", .return_k },
    .{ "if", .if_k },
    .{ "then", .then_k },
    .{ "else", .else_k },
    .{ "for", .for_k },
    .{ "func", .func_k },
    .{ "break", .break_k },
    .{ "await", .await_k },
    .{ "true", .true_k },
    .{ "false", .false_k },
    .{ "or", .or_k },
    .{ "and", .and_k },
    .{ "not", .not_k },
    .{ "as", .as_k },
    .{ "pass", .pass_k },
    .{ "none", .none_k },
});

const BlockState = struct {
    indent_spaces: u32,
    vars: std.StringHashMapUnmanaged(void),

    fn deinit(self: *BlockState, alloc: std.mem.Allocator) void {
        self.vars.deinit(alloc);
    }
};

/// Parses source code into AST.
pub const Parser = struct {
    alloc: std.mem.Allocator,

    /// Context vars.
    src: std.ArrayListUnmanaged(u8),
    next_pos: u32,
    savePos: u32,
    tokens: std.ArrayListUnmanaged(Token),
    nodes: std.ArrayListUnmanaged(Node),
    last_err: []const u8,
    last_err_pos: u32,
    block_stack: std.ArrayListUnmanaged(BlockState),
    cur_indent: u32,
    func_params: std.ArrayListUnmanaged(FunctionParam),
    func_decls: std.ArrayListUnmanaged(FunctionDeclaration),

    // TODO: This should be implemented by user callbacks.
    /// @name arg.
    name: []const u8,
    /// Variable dependencies.
    deps: std.StringHashMapUnmanaged(NodeId),

    tokenizeOpts: TokenizeOptions,

    /// For custom functions.
    user: struct {
        ctx: *anyopaque,
        advanceChar: std.meta.FnPtr(fn (*anyopaque) void),
        peekChar: std.meta.FnPtr(fn (*anyopaque) u8),
        peekCharAhead: std.meta.FnPtr(fn (*anyopaque, u32) ?u8),
        isAtEndChar: std.meta.FnPtr(fn (*anyopaque) bool),
        getSubStrFromDelta: std.meta.FnPtr(fn (*anyopaque, u32) []const u8),
        savePos: std.meta.FnPtr(fn (*anyopaque) void),
        restorePos: std.meta.FnPtr(fn (*anyopaque) void),
    },

    pub fn init(alloc: std.mem.Allocator) Parser {
        return .{
            .alloc = alloc,
            .src = .{},
            .next_pos = undefined,
            .savePos = undefined,
            .tokens = .{},
            .nodes = .{},
            .last_err = "",
            .last_err_pos = 0,
            .block_stack = .{},
            .cur_indent = 0,
            .func_params = .{},
            .func_decls = .{},
            .name = "",
            .deps = .{},
            .user = undefined,
            .tokenizeOpts = .{},
        };
    }

    pub fn deinit(self: *Parser) void {
        self.tokens.deinit(self.alloc);
        self.nodes.deinit(self.alloc);
        self.src.deinit(self.alloc);
        self.alloc.free(self.last_err);
        self.block_stack.deinit(self.alloc);
        self.func_params.deinit(self.alloc);
        self.func_decls.deinit(self.alloc);
        self.deps.deinit(self.alloc);
    }

    pub fn parseNoErr(self: *Parser, src: []const u8) !ResultView {
        const res = try self.parse(src);
        if (res.has_error) {
            log.debug("{s}", .{res.err_msg});
            return error.ParseError;
        }
        return res;
    }

    pub fn parse(self: *Parser, src: []const u8) !ResultView {
        self.src.clearRetainingCapacity();
        try self.src.appendSlice(self.alloc, src);
        self.name = "";
        self.deps.clearRetainingCapacity();

        const tokenizeOpts = TokenizeOptions{
            .ignoreErrors = false,
        };
        Tokenizer(.{ .user = false }).tokenize(self, tokenizeOpts) catch |err| {
            log.debug("tokenize error: {}", .{err});
            return ResultView{
                .has_error = true,
                .err_msg = self.last_err,
                .root_id = NullId,
                .nodes = &self.nodes,
                .func_decls = &self.func_decls,
                .func_params = self.func_params.items,
                .tokens = &.{},
                .src = self.src.items,
                .name = self.name,
                .deps = &self.deps,
            };
        };
        const root_id = self.parseRoot() catch |err| {
            log.debug("parse error: {} {s}", .{err, self.last_err});
            logSrcPos(self.src.items, self.last_err_pos, 20);
            return ResultView{
                .has_error = true,
                .err_msg = self.last_err,
                .root_id = NullId,
                .nodes = &self.nodes,
                .func_decls = &self.func_decls,
                .func_params = self.func_params.items,
                .tokens = &.{},
                .src = self.src.items,
                .name = self.name,
                .deps = &self.deps,
            };
        };
        return ResultView{
            .has_error = false,
            .err_msg = "",
            .root_id = root_id,
            .nodes = &self.nodes,
            .tokens = self.tokens.items,
            .src = self.src.items,
            .func_decls = &self.func_decls,
            .func_params = self.func_params.items,
            .name = self.name,
            .deps = &self.deps,
        };
    }

    fn parseRoot(self: *Parser) !NodeId {
        self.next_pos = 0;
        self.nodes.clearRetainingCapacity();
        self.block_stack.clearRetainingCapacity();
        self.func_decls.clearRetainingCapacity();
        self.func_params.clearRetainingCapacity();
        self.cur_indent = 0;

        const root_id = self.pushNode(.root, 0);
        const first_stmt = try self.parseBodyStatements(0);
        self.nodes.items[root_id].head = .{
            .child_head = first_stmt,
        };
        return 0;
    }

    /// Returns number of spaces that precedes a statement.
    /// If current line is consumed if there is no statement.
    fn consumeIndentBeforeStmt(self: *Parser) u32 {
        while (true) {
            var res: u32 = 0;
            var token = self.peekToken();
            while (token.token_t == .indent) {
                res += token.data.indent;
                self.advanceToken();
                token = self.peekToken();
            }
            if (token.token_t == .new_line) {
                self.advanceToken();
                continue;
            } else if (token.token_t == .none) {
                return res;
            } else {
                return res;
            }
        }
    }

    fn pushBlock(self: *Parser, indent: u32) !void {
        try self.block_stack.append(self.alloc, .{
            .indent_spaces = indent,
            .vars = .{},
        });
    }

    fn popBlock(self: *Parser) void {
        self.block_stack.items[self.block_stack.items.len-1].deinit(self.alloc);
        _ = self.block_stack.pop();
    }

    /// Like parseIndentedBodyStatements but the body indentation is already known.
    fn parseBodyStatements(self: *Parser, body_indent: u32) !NodeId {
        try self.pushBlock(body_indent);
        defer self.popBlock();

        var indent = self.consumeIndentBeforeStmt();
        if (indent != body_indent) {
            return self.reportTokenError(error.IndentError, "Unexpected indentation.", self.peekToken());
        }
        var first_stmt = (try self.parseStatement()) orelse return NullId;
        var last_stmt = first_stmt;

        while (true) {
            indent = self.consumeIndentBeforeStmt();
            if (indent == body_indent) {
                const id = (try self.parseStatement()) orelse break;
                self.nodes.items[last_stmt].next = id;
                last_stmt = id;
            } else {
                return self.reportTokenError(error.IndentError, "Unexpected indentation.", self.peekToken());
            }
        }
        return first_stmt;
    }

    /// Returns the first statement or NullId.
    fn parseIndentedBodyStatements(self: *Parser, start_indent: u32) !NodeId {
        // New block. Indent spaces is determines by the first body statement.
        try self.pushBlock(0);
        defer {
            self.popBlock();
            self.cur_indent = start_indent;
        }

        // Parse first statement and determine the body indentation.
        var body_indent: u32 = undefined;
        var start = self.next_pos;
        var indent = self.consumeIndentBeforeStmt();
        if (indent <= start_indent) {
            // End of body. Rewind and return.
            self.next_pos = start;
            return self.reportTokenError(error.SyntaxError, "Block requires at least one statement. Use the `pass` statement as a placeholder.", self.peekToken());
        } else {
            body_indent = indent;
            self.cur_indent = body_indent;
        }
        var first_stmt = (try self.parseStatement()) orelse {
            return self.reportTokenError(error.SyntaxError, "Block requires at least one statement. Use the `pass` statement as a placeholder.", self.peekToken());
        };
        var last_stmt = first_stmt;

        // Parse the rest of the body statements and enforce the body indentation.
        while (true) {
            start = self.next_pos;
            indent = self.consumeIndentBeforeStmt();
            if (indent == body_indent) {
                const id = (try self.parseStatement()) orelse break;
                self.nodes.items[last_stmt].next = id;
                last_stmt = id;
            } else if (indent <= start_indent) {
                self.next_pos = start;
                break;
            } else {
                return self.reportTokenError(error.IndentError, "Unexpected indent.", self.peekToken());
            }
        }
        return first_stmt;
    }

    fn parseLambdaFuncWithParam(self: *Parser, paramIdent: NodeId) !NodeId {
        const start = self.next_pos;
        // Assumes first token is `=>`.
        self.advanceToken();

        var decl = FunctionDeclaration{
            .name = undefined,
            .params = stdx.IndexSlice(u32).init(@intCast(u32, self.func_params.items.len), @intCast(u32, self.func_params.items.len+1)),
            .return_type = null,
        };

        const param = self.nodes.items[paramIdent];
        const token = self.tokens.items[param.start_token];
        const name = IndexSlice.init(token.start_pos, token.data.end_pos);
        try self.func_params.append(self.alloc, .{
            .name = name,
        });

        // Parse body expr.
        const body_expr = (try self.parseExpr(.{})) orelse {
            return self.reportTokenError(error.SyntaxError, "Expected lambda body expression.", self.peekToken());
        };
        
        const decl_id = @intCast(u32, self.func_decls.items.len);
        try self.func_decls.append(self.alloc, decl);

        const id = self.pushNode(.lambda_expr, start);
        self.nodes.items[id].head = .{
            .func = .{
                .decl_id = decl_id,
                .body_head = body_expr,
            },
        };
        return id;
    }

    fn parseNoParamLambdaFunc(self: *Parser) !NodeId {
        const start = self.next_pos;
        // Assumes first token is `=>`.
        self.advanceToken();

        var decl = FunctionDeclaration{
            .name = undefined,
            .params = stdx.IndexSlice(u32).init(0, 0),
            .return_type = null,
        };

        // Parse body expr.
        const body_expr = (try self.parseExpr(.{})) orelse {
            return self.reportTokenError(error.SyntaxError, "Expected lambda body expression.", self.peekToken());
        };
        
        const decl_id = @intCast(u32, self.func_decls.items.len);
        try self.func_decls.append(self.alloc, decl);

        const id = self.pushNode(.lambda_expr, start);
        self.nodes.items[id].head = .{
            .func = .{
                .decl_id = decl_id,
                .body_head = body_expr,
            },
        };
        return id;
    }

    fn parseLambdaFunction(self: *Parser) !NodeId {
        const start = self.next_pos;

        var decl = FunctionDeclaration{
            .name = undefined,
            .params = undefined,
            .return_type = null,
        };

        decl.params = try self.parseFunctionParams();

        var token = self.peekToken();
        if (token.token_t != .equal_greater) {
            return self.reportTokenError(error.SyntaxError, "Expected =>.", token);
        }
        self.advanceToken();

        // Parse body expr.
        const body_expr = (try self.parseExpr(.{})) orelse {
            return self.reportTokenError(error.SyntaxError, "Expected lambda body expression.", self.peekToken());
        };
        
        const decl_id = @intCast(u32, self.func_decls.items.len);
        try self.func_decls.append(self.alloc, decl);

        const id = self.pushNode(.lambda_expr, start);
        self.nodes.items[id].head = .{
            .func = .{
                .decl_id = decl_id,
                .body_head = body_expr,
            },
        };
        return id;
    }

    fn parseFunctionParams(self: *Parser) !IndexSlice {
        var token = self.peekToken();
        if (token.token_t != .left_paren) {
            return self.reportTokenError(error.SyntaxError, "Expected open parenthesis.", token);
        }
        self.advanceToken();

        // Parse params.
        const param_start = @intCast(u32, self.func_params.items.len);
        outer: {
            token = self.peekToken();
            if (token.token_t == .ident) {
                self.advanceToken();
                const name = IndexSlice.init(token.start_pos, token.data.end_pos);
                try self.func_params.append(self.alloc, .{
                    .name = name,
                });
            } else if (token.token_t == .right_paren) {
                self.advanceToken();
                break :outer;
            } else return self.reportTokenError(error.SyntaxError, "Unexpected token in function param list.", token);
            while (true) {
                token = self.peekToken();
                switch (token.token_t) {
                    .comma => {
                        self.advanceToken();
                    },
                    .right_paren => {
                        self.advanceToken();
                        break;
                    },
                    else => return self.reportTokenError(error.SyntaxError, "Unexpected token in function param list.", token),
                }

                token = self.peekToken();
                if (token.token_t != .ident) {
                    return self.reportTokenError(error.SyntaxError, "Expected param identifier.", token);
                }
                self.advanceToken();
                const name = IndexSlice.init(token.start_pos, token.data.end_pos);
                try self.func_params.append(self.alloc, .{
                    .name = name,
                });
            }
        }
        return IndexSlice.init(param_start, @intCast(u32, self.func_params.items.len));
    }

    fn parseFunctionReturn(self: *Parser) !?IndexSlice {
        var token = self.peekToken();
        if (token.token_t == .colon) {
            self.advanceToken();
            return null;
        } else if (token.token_t == .ident) {
            const return_type = IndexSlice.init(token.start_pos, token.data.end_pos);
            self.advanceToken();
            token = self.peekToken();
            if (token.token_t != .colon) {
                return self.reportTokenError(error.SyntaxError, "Expected colon.", token);
            }
            self.advanceToken();
            return return_type;
        } else {
            return self.reportTokenError(error.SyntaxError, "Expected colon or type.", token);
        }
    }

    fn parseFunctionDeclaration(self: *Parser) !NodeId {
        const start = self.next_pos;
        // Assumes first token is the `func` keyword.
        self.advanceToken();

        var decl = FunctionDeclaration{
            .name = undefined,
            .params = undefined,
            .return_type = null,
        };

        // Parse function name.
        var token = self.peekToken();
        const left_pos = self.next_pos;
        if (token.token_t == .ident) {
            decl.name = IndexSlice.init(token.start_pos, token.data.end_pos);
            self.advanceToken();
        } else return self.reportTokenError(error.SyntaxError, "Expected function name identifier.", token);

        token = self.peekToken();
        if (token.token_t == .dot) {
            // Parse lambda assign decl.
            var left = self.pushNode(.ident, left_pos);
            self.advanceToken();
            while (true) {
                token = self.peekToken();
                if (token.token_t == .ident) {
                    const ident = self.pushNode(.ident, self.next_pos);
                    const expr = self.pushNode(.access_expr, left_pos);
                    self.nodes.items[expr].head = .{
                        .left_right = .{
                            .left = left,
                            .right = ident,
                        },
                    };
                    left = expr;
                } else {
                    return self.reportTokenError(error.SyntaxError, "Expected ident.", token);
                }

                self.advanceToken();
                token = self.peekToken();
                if (token.token_t == .left_paren) {
                    break;
                } else if (token.token_t == .dot) {
                    continue;
                } else {
                    return self.reportTokenError(error.SyntaxError, "Expected open paren.", token);
                }
            }

            decl.params = try self.parseFunctionParams();
            decl.return_type = try self.parseFunctionReturn();
            const first_stmt = try self.parseIndentedBodyStatements(self.cur_indent);

            const decl_id = @intCast(u32, self.func_decls.items.len);
            try self.func_decls.append(self.alloc, decl);

            const id = self.pushNode(.lambda_assign_decl, start);
            self.nodes.items[id].head = .{
                .lambda_assign_decl = .{
                    .decl_id = decl_id,
                    .body_head = first_stmt,
                    .assign_expr = left,
                },
            };
            return id;
        } else if (token.token_t == .left_paren) {
            decl.params = try self.parseFunctionParams();
            decl.return_type = try self.parseFunctionReturn();

            // Parse body.
            const first_stmt = try self.parseIndentedBodyStatements(self.cur_indent);
            
            const decl_id = @intCast(u32, self.func_decls.items.len);
            try self.func_decls.append(self.alloc, decl);

            const id = self.pushNode(.func_decl, start);
            self.nodes.items[id].head = .{
                .func = .{
                    .decl_id = decl_id,
                    .body_head = first_stmt,
                },
            };

            const name = self.src.items[decl.name.start..decl.name.end];
            const block = &self.block_stack.items[self.block_stack.items.len-1];
            try block.vars.put(self.alloc, name, {});
            return id;
        } else {
            return self.reportTokenError(error.SyntaxError, "Expected left paren.", token);
        }
    }

    fn parseIfStmtElseClause(self: *Parser) anyerror!NodeId {
        const save = self.next_pos;
        const indent = self.consumeIndentBeforeStmt();
        if (indent != self.cur_indent) {
            self.next_pos = save;
            return NullId;
        }

        var token = self.peekToken();
        if (token.token_t == .else_k) {
            const else_clause = self.pushNode(.else_clause, self.next_pos);
            self.advanceToken();

            token = self.peekToken();
            if (token.token_t == .colon) {
                // else block.
                self.advanceToken();

                // TODO: Parse statements on the same line.

                token = self.peekToken();
                if (token.token_t != .new_line) {
                    return self.reportTokenError(error.SyntaxError, "Expected new line.", token);
                }
                self.advanceToken();

                const first_stmt = try self.parseIndentedBodyStatements(self.cur_indent);
                self.nodes.items[else_clause].head = .{
                    .else_clause = .{
                        .body_head = first_stmt,
                        .cond = NullId,
                        .else_clause = NullId,
                    },
                };
                return else_clause;
            } else {
                // else if block.
                const cond = (try self.parseExpr(.{})) orelse {
                    return self.reportTokenError(error.SyntaxError, "Expected else if condition.", self.peekToken());
                };
                token = self.peekToken();
                if (token.token_t == .colon) {
                    self.advanceToken();
                    token = self.peekToken();
                    if (token.token_t != .new_line) {
                        return self.reportTokenError(error.SyntaxError, "Expected new line.", token);
                    }
                    self.advanceToken();

                    const first_stmt = try self.parseIndentedBodyStatements(self.cur_indent);
                    self.nodes.items[else_clause].head = .{
                        .else_clause = .{
                            .body_head = first_stmt,
                            .cond = cond,
                            .else_clause = NullId,
                        },
                    };

                    const nested_else = try self.parseIfStmtElseClause();
                    if (nested_else != NullId) {
                        self.nodes.items[else_clause].head.else_clause.else_clause = nested_else;
                        return else_clause;
                    } else {
                        return else_clause;
                    }
                } else {
                    return self.reportTokenError(error.SyntaxError, "Expected colon after else if condition.", token);
                }
            }
        } else {
            self.next_pos = save;
            return NullId;
        }
    }

    fn parseIfStatement(self: *Parser) !NodeId {
        const start = self.next_pos;
        // Assumes first token is the `if` keyword.
        self.advanceToken();

        const if_cond = (try self.parseExpr(.{})) orelse {
            return self.reportTokenError(error.SyntaxError, "Expected if condition.", self.peekToken());
        };

        var token = self.peekToken();
        if (token.token_t == .then_k) {
            const if_expr = try self.parseIfThenExpr(if_cond, start);
            const expr_stmt = self.pushNode(.expr_stmt, start);
            self.nodes.items[expr_stmt].head = .{
                .child_head = if_expr,
            };
            return expr_stmt;
        } else if (token.token_t != .colon) {
            return self.reportTokenError(error.SyntaxError, "Expected colon after if condition.", token);
        }
        self.advanceToken();

        const if_stmt = self.pushNode(.if_stmt, start);

        // TODO: Parse statements on the same line.

        token = self.peekToken();
        if (token.token_t != .new_line) {
            return self.reportTokenError(error.SyntaxError, "Expected new line.", token);
        }
        self.advanceToken();

        var first_stmt = try self.parseIndentedBodyStatements(self.cur_indent);
        self.nodes.items[if_stmt].head = .{
            .left_right = .{
                .left = if_cond,
                .right = first_stmt,
            },
        };

        const else_clause = try self.parseIfStmtElseClause();
        if (else_clause != NullId) {
            self.nodes.items[if_stmt].head.left_right.extra = else_clause;
            return if_stmt;
        } else {
            return if_stmt;
        }
    }

    fn parseForStatement(self: *Parser) !NodeId {
        const start = self.next_pos;
        // Assumes first token is the `for` keyword.
        self.advanceToken();

        var token = self.peekToken();
        if (token.token_t == .colon) {
            self.advanceToken();

            // Infinite loop.
            const first_stmt = try self.parseIndentedBodyStatements(self.cur_indent);
            const for_stmt = self.pushNode(.for_inf_stmt, start);
            self.nodes.items[for_stmt].head = .{
                .child_head = first_stmt,
            };
            return for_stmt;
        } else {
            // Parse next token as expression.
            const expr_pos = self.next_pos;
            const expr_id = (try self.parseExpr(.{})) orelse {
                return self.reportTokenError(error.SyntaxError, "Expected condition expression.", token);
            };

            token = self.peekToken();
            if (token.token_t == .colon) {
                self.advanceToken();
                const first_stmt = try self.parseIndentedBodyStatements(self.cur_indent);
                const for_stmt = self.pushNode(.for_cond_stmt, start);
                self.nodes.items[for_stmt].head = .{
                    .left_right = .{
                        .left = expr_id,
                        .right = first_stmt,
                    },
                };
                return for_stmt;
            } else if (token.token_t == .dot_dot) {
                self.advanceToken();
                const right_range_expr = (try self.parseExpr(.{})) orelse {
                    return self.reportTokenError(error.SyntaxError, "Expected right range expression.", token);
                };
                const range_clause = self.pushNode(.range_clause, expr_pos);
                self.nodes.items[range_clause].head = .{
                    .left_right = .{
                        .left = expr_id,
                        .right = right_range_expr,
                    },
                };

                token = self.peekToken();
                if (token.token_t == .colon) {
                    self.advanceToken();

                    const first_stmt = try self.parseIndentedBodyStatements(self.cur_indent);
                    const for_stmt = self.pushNode(.for_range_stmt, start);
                    self.nodes.items[for_stmt].head = .{
                        .for_range_stmt = .{
                            .range_clause = range_clause,
                            .body_head = first_stmt,
                            .as_clause = NullId,
                        },
                    };
                    return for_stmt;
                } else if (token.token_t == .as_k) {
                    self.advanceToken();

                    token = self.peekToken();
                    const ident = (try self.parseExpr(.{})) orelse {
                        return self.reportTokenError(error.SyntaxError, "Expected ident.", token);
                    };
                    if (self.nodes.items[ident].node_t == .ident) {
                        token = self.peekToken();
                        if (token.token_t == .colon) {
                            self.advanceToken();
                            const first_stmt = try self.parseIndentedBodyStatements(self.cur_indent);

                            const as_clause = self.pushNode(.as_range_clause, start);
                            self.nodes.items[as_clause].head = .{
                                .as_range_clause = .{
                                    .ident = ident,
                                    .step = NullId,
                                    .inc = true,
                                }
                            };

                            const for_stmt = self.pushNode(.for_range_stmt, start);
                            self.nodes.items[for_stmt].head = .{
                                .for_range_stmt = .{
                                    .range_clause = range_clause,
                                    .body_head = first_stmt,
                                    .as_clause = as_clause,
                                },
                            };
                            return for_stmt;
                        } else if (token.token_t == .plus_equal) {
                            self.advanceToken();
                            const step_expr = (try self.parseExpr(.{})) orelse {
                                return self.reportTokenError(error.SyntaxError, "Expected step expr.", token);
                            };
                            token = self.peekToken();
                            if (token.token_t == .colon) {
                                self.advanceToken();
                                const first_stmt = try self.parseIndentedBodyStatements(self.cur_indent);

                                const as_clause = self.pushNode(.as_range_clause, start);
                                self.nodes.items[as_clause].head = .{
                                    .as_range_clause = .{
                                        .ident = ident,
                                        .step = step_expr,
                                        .inc = true,
                                    }
                                };

                                const for_stmt = self.pushNode(.for_range_stmt, start);
                                self.nodes.items[for_stmt].head = .{
                                    .for_range_stmt = .{
                                        .range_clause = range_clause,
                                        .body_head = first_stmt,
                                        .as_clause = as_clause,
                                    },
                                };
                                return for_stmt;
                            } else {
                                return self.reportTokenError(error.SyntaxError, "Expected :.", token);
                            }
                        } else {
                            return self.reportTokenError(error.SyntaxError, "Expected :.", token);
                        }
                    } else {
                        return self.reportTokenError(error.SyntaxError, "Expected ident.", token);
                    }
                } else {
                    return self.reportTokenError(error.SyntaxError, "Expected :.", token);
                }
            } else if (token.token_t == .as_k) {
                self.advanceToken();
                token = self.peekToken();
                const ident = (try self.parseExpr(.{})) orelse {
                    return self.reportTokenError(error.SyntaxError, "Expected ident.", token);
                };
                if (self.nodes.items[ident].node_t == .ident) {
                    token = self.peekToken();
                    if (token.token_t == .colon) {
                        self.advanceToken();
                        const body_head = try self.parseIndentedBodyStatements(self.cur_indent);

                        const as_clause = self.pushNode(.as_iter_clause, start);
                        self.nodes.items[as_clause].head = .{
                            .as_iter_clause = .{
                                .value = ident,
                                .key = NullId,
                            }
                        };

                        const for_stmt = self.pushNode(.for_iter_stmt, start);
                        self.nodes.items[for_stmt].head = .{
                            .for_iter_stmt = .{
                                .iterable = expr_id,
                                .body_head = body_head,
                                .as_clause = as_clause,
                            },
                        };
                        return for_stmt;
                    } else {
                        return self.reportTokenError(error.SyntaxError, "Expected :.", token);
                    }
                } else {
                    return self.reportTokenError(error.SyntaxError, "Expected ident.", token);
                }
            } else {
                return self.reportTokenError(error.SyntaxError, "Expected :.", token);
            }
        }
    }

    fn parseBlock(self: *Parser) !NodeId {
        const start = self.next_pos;
        // Assumes first token is the ident.
        const name = self.pushNode(.ident, start);
        self.advanceToken();
        // Assumes second token is colon.
        self.advanceToken();

        // Parse body.
        const first_stmt = try self.parseIndentedBodyStatements(self.cur_indent);
        
        const id = self.pushNode(.label_decl, start);
        self.nodes.items[id].head = .{
            .left_right = .{
                .left = name,
                .right = first_stmt,
            },
        };
        return id;
    }

    fn parseStatement(self: *Parser) anyerror!?NodeId {
        var token = self.peekToken();
        if (token.token_t == .none) {
            return null;
        }
        switch (token.token_t) {
            .new_line => {
                // Skip newlines.
                while (true) {
                    self.advanceToken();
                    token = self.peekToken();
                    if (token.token_t == .none) {
                        return null;
                    }
                    if (token.token_t != .new_line) {
                        break;
                    }
                }
                return try self.parseStatement();
            },
            .ident => {
                const token2 = self.peekTokenAhead(1);
                if (token2.token_t == .colon) {
                    return try self.parseBlock();
                } else {
                    if (try self.parseExprOrAssignStatement()) |id| {
                        return id;
                    }
                }
            },
            .at => {
                const start = self.next_pos;
                self.advanceToken();
                token = self.peekToken();
                if (token.token_t == .ident) {
                    const name_token = self.tokens.items[self.next_pos];
                    const name = self.src.items[name_token.start_pos .. name_token.data.end_pos];
                    var skip_compile = false;

                    const ident = self.pushNode(.ident, self.next_pos);
                    self.advanceToken();
                    const at_ident = self.pushNode(.at_ident, start);
                    self.nodes.items[at_ident].head = .{
                        .annotation = .{
                            .child = ident,
                        },
                    };

                    const child: NodeId = b: {
                        // Parse as call expr.
                        const call_id = try self.parseAnyCallExpr(at_ident);
                        const call_expr = self.nodes.items[call_id];
                        if (call_expr.head.func_call.arg_head == NullId) {
                            return self.reportTokenError(error.SyntaxError, "Expected arg for @name.", token);
                        }
                        const arg = self.nodes.items[call_expr.head.func_call.arg_head];
                        if (std.mem.eql(u8, "name", name)) {
                            if (arg.node_t == .ident) {
                                const arg_token = self.tokens.items[arg.start_token];
                                self.name = self.src.items[arg_token.start_pos .. arg_token.data.end_pos];
                                skip_compile = true;
                            } else {
                                return self.reportTokenError(error.SyntaxError, "Expected ident arg for @name.", token);
                            }
                        } else if (std.mem.eql(u8, "compileError", name)) {
                            skip_compile = true;
                        }
                        break :b call_id;
                    };

                    const id = self.pushNode(.at_stmt, start);
                    self.nodes.items[id].head = .{
                        .at_stmt = .{
                            .child = child,
                            .skip_compile = skip_compile,
                        },
                    };

                    token = self.peekToken();
                    if (token.token_t == .new_line) {
                        self.advanceToken();
                        return id;
                    } else if (token.token_t == .none) {
                        return id;
                    } else return self.reportTokenError(error.BadToken, "Expected end of line or file", token);
                }

                // Reparse as expression.
                self.next_pos = start;
                if (try self.parseExprOrAssignStatement()) |id| {
                    return id;
                }
            },
            .func_k => {
                return try self.parseFunctionDeclaration();
            },
            .if_k => {
                return try self.parseIfStatement();
            },
            .for_k => {
                return try self.parseForStatement();
            },
            .pass_k => {
                const id = self.pushNode(.pass_stmt, self.next_pos);
                self.advanceToken();
                token = self.peekToken();
                switch (token.token_t) {
                    .none => return id,
                    .new_line => {
                        self.advanceToken();
                        return id;
                    },
                    else => {
                        return self.reportTokenError(error.SyntaxError, "Expected end of statement.", token);
                    },
                }
            },
            .break_k => {
                const id = self.pushNode(.break_stmt, self.next_pos);
                self.advanceToken();
                token = self.peekToken();
                switch (token.token_t) {
                    .none => return id,
                    .new_line => {
                        self.advanceToken();
                        return id;
                    },
                    else => {
                        return self.reportTokenError(error.SyntaxError, "Expected end of statement.", token);
                    },
                }
            },
            .return_k => {
                const id = try self.parseReturnStatement();
                token = self.peekToken();
                switch (token.token_t) {
                    .none => return id,
                    .new_line => {
                        self.advanceToken();
                        return id;
                    },
                    else => {
                        return self.reportTokenError(error.SyntaxError, "Expected end of statement.", token);
                    },
                }
            },
            else => {
                if (try self.parseExprOrAssignStatement()) |id| {
                    return id;
                }
            },
        }
        self.last_err = std.fmt.allocPrint(self.alloc, "unknown token: {} at {}", .{token.token_t, token.start_pos}) catch fatal();
        return error.UnknownToken;
    }

    fn reportTokenError(self: *Parser, err: anyerror, msg: []const u8, token: Token) anyerror {
        self.alloc.free(self.last_err);
        self.last_err = std.fmt.allocPrint(self.alloc, "{s}: {} at {}", .{msg, token.token_t, token.start_pos}) catch fatal();
        self.last_err_pos = token.start_pos;
        return err;
    }

    fn parseMapEntry(self: *Parser, key_node_t: NodeType) !NodeId {
        const start = self.next_pos;
        self.advanceToken();
        var token = self.peekToken();
        if (token.token_t != .colon) {
            return self.reportTokenError(error.SyntaxError, "Expected colon.", token);
        }
        self.advanceToken();
        const val_id = (try self.parseExpr(.{})) orelse {
            return self.reportTokenError(error.SyntaxError, "Expected map value.", token);
        };
        const key_id = self.pushNode(key_node_t, start);
        const entry_id = self.pushNode(.map_entry, start);
        self.nodes.items[entry_id].head = .{
            .left_right = .{
                .left = key_id,
                .right = val_id,
            }
        };
        return entry_id;
    }

    fn consumeWhitespaceTokens(self: *Parser) void {
        var token = self.peekToken();
        while (token.token_t != .none) {
            switch (token.token_t) {
                .new_line,
                .indent => {
                    self.advanceToken();
                    token = self.peekToken();
                    continue;
                },
                else => return,
            }
        }
    }

    fn parseArrayLiteral(self: *Parser) !NodeId {
        const start = self.next_pos;
        // Assume first token is left bracket.
        self.advanceToken();

        var last_entry: NodeId = undefined;
        var first_entry: NodeId = NullId;
        outer: {
            self.consumeWhitespaceTokens();
            var token = self.peekToken();

            if (token.token_t == .right_bracket) {
                // Empty array.
                break :outer;
            } else {
                first_entry = (try self.parseExpr(.{})) orelse {
                    return self.reportTokenError(error.SyntaxError, "Expected array item.", token);
                };
                last_entry = first_entry;
            }

            while (true) {
                self.consumeWhitespaceTokens();
                token = self.peekToken();
                if (token.token_t == .comma) {
                    self.advanceToken();
                } else if (token.token_t == .right_bracket) {
                    break :outer;
                }

                token = self.peekToken();
                if (token.token_t == .right_bracket) {
                    break :outer;
                } else {
                    const expr_id = (try self.parseExpr(.{})) orelse {
                        return self.reportTokenError(error.SyntaxError, "Expected array item.", token);
                    };
                    self.nodes.items[last_entry].next = expr_id;
                    last_entry = expr_id;
                }
            }
        }

        const arr_id = self.pushNode(.arr_literal, start);
        self.nodes.items[arr_id].head = .{
            .child_head = first_entry,
        };

        // Parse closing bracket.
        const token = self.peekToken();
        if (token.token_t == .right_bracket) {
            self.advanceToken();
            return arr_id;
        } else return self.reportTokenError(error.SyntaxError, "Expected closing bracket.", token);
    }

    fn parseMapLiteral(self: *Parser) !NodeId {
        const start = self.next_pos;
        // Assume first token is left brace.
        self.advanceToken();

        var last_entry: NodeId = undefined;
        var first_entry: NodeId = NullId;
        outer: {
            self.consumeWhitespaceTokens();
            var token = self.peekToken();
            switch (token.token_t) {
                .ident => {
                    first_entry = try self.parseMapEntry(.ident);
                    last_entry = first_entry;
                },
                .string => {
                    first_entry = try self.parseMapEntry(.string);
                    last_entry = first_entry;
                },
                .number => {
                    first_entry = try self.parseMapEntry(.number);
                    last_entry = first_entry;
                },
                .right_brace => {
                    break :outer;
                },
                else => return self.reportTokenError(error.SyntaxError, "Expected map key.", token),
            }

            while (true) {
                self.consumeWhitespaceTokens();
                token = self.peekToken();
                if (token.token_t == .comma) {
                    self.advanceToken();
                } else if (token.token_t == .right_brace) {
                    break :outer;
                }

                token = self.peekToken();
                switch (token.token_t) {
                    .ident => {
                        const entry_id = try self.parseMapEntry(.ident);
                        self.nodes.items[last_entry].next = entry_id;
                        last_entry = entry_id;
                    },
                    .string => {
                        const entry_id = try self.parseMapEntry(.string);
                        self.nodes.items[last_entry].next = entry_id;
                        last_entry = entry_id;
                    },
                    .number => {
                        const entry_id = try self.parseMapEntry(.number);
                        self.nodes.items[last_entry].next = entry_id;
                        last_entry = entry_id;
                    },
                    .right_brace => {
                        break :outer;
                    },
                    else => return self.reportTokenError(error.SyntaxError, "Expected map key.", token),
                }
            }
        }

        const map_id = self.pushNode(.map_literal, start);
        self.nodes.items[map_id].head = .{
            .child_head = first_entry,
        };

        // Parse closing brace.
        const token = self.peekToken();
        if (token.token_t == .right_brace) {
            self.advanceToken();
            return map_id;
        } else return self.reportTokenError(error.SyntaxError, "Expected closing brace.", token);
    }

    fn parseCallArg(self: *Parser) !?NodeId {
        self.consumeWhitespaceTokens();
        const start = self.next_pos;
        const token = self.peekToken();
        if (token.token_t == .ident) {
            if (self.peekTokenAhead(1).token_t == .colon) {
                // Named arg.
                const name = self.pushNode(.ident, start);
                _ = self.consumeToken();
                _ = self.consumeToken();
                var arg = (try self.parseExpr(.{})) orelse {
                    return self.reportTokenError(error.SyntaxError, "Expected arg expression.", self.peekToken());
                };
                const named_arg = self.pushNode(.named_arg, start);
                self.nodes.items[named_arg].head = .{
                    .left_right = .{
                        .left = name,
                        .right = arg,
                    },
                };
                return named_arg;
            } 
        }

        return try self.parseExpr(.{});
    }

    fn parseAnyCallExpr(self: *Parser, callee: NodeId) !NodeId {
        const token = self.peekToken();
        if (token.token_t == .left_paren) {
            return try self.parseCallExpression(callee);
        } else {
            return try self.parseNoParenCallExpression(callee);
        }
    }

    fn parseCallExpression(self: *Parser, left_id: NodeId) !NodeId {
        // Assume first token is left paren.
        self.advanceToken();

        const expr_start = self.nodes.items[left_id].start_token;
        const expr_id = self.pushNode(.call_expr, expr_start);

        var has_named_arg = false;
        var first: NodeId = NullId;
        inner: {
            first = (try self.parseCallArg()) orelse {
                break :inner;
            };
            if (self.nodes.items[first].node_t == .named_arg) {
                has_named_arg = true;
            }
            var last_arg_id = first;
            while (true) {
                const token = self.peekToken();
                if (token.token_t != .comma and token.token_t != .new_line) {
                    break;
                }
                self.advanceToken();
                const arg_id = (try self.parseCallArg()) orelse {
                    break;
                };
                self.nodes.items[last_arg_id].next = arg_id;
                last_arg_id = arg_id;
                if (self.nodes.items[last_arg_id].node_t == .named_arg) {
                    has_named_arg = true;
                }
            }
        }
        // Parse closing paren.
        self.consumeWhitespaceTokens();
        const token = self.peekToken();
        if (token.token_t == .right_paren) {
            self.advanceToken();
            self.nodes.items[expr_id].head = .{
                .func_call = .{
                    .callee = left_id,
                    .arg_head = first,
                    .has_named_arg = has_named_arg,
                },
            };
            return expr_id;
        } else return self.reportTokenError(error.SyntaxError, "Expected closing parenthesis.", token);
    }

    /// Assumes first arg exists.
    fn parseNoParenCallExpression(self: *Parser, left_id: NodeId) !NodeId {
        const expr_start = self.nodes.items[left_id].start_token;
        const expr_id = self.pushNode(.call_expr, expr_start);

        const start = self.next_pos;
        var token = self.consumeToken();
        var last_arg_id = switch (token.token_t) {
            .ident => self.pushNode(.ident, start),
            .string => self.pushNode(.string, start),
            .number => self.pushNode(.number, start),
            else => return self.reportTokenError(error.BadToken, "Expected arg token", token),
        };
        self.nodes.items[expr_id].head = .{
            .func_call = .{
                .callee = left_id,
                .arg_head = last_arg_id,
                .has_named_arg = false,
            },
        };

        while (true) {
            token = self.peekToken();
            const arg_id = switch (token.token_t) {
                .ident => self.pushNode(.ident, self.next_pos),
                .string => self.pushNode(.string, self.next_pos),
                .number => self.pushNode(.number, self.next_pos),
                .new_line => break,
                .none => break,
                else => return self.reportTokenError(error.BadToken, "Expected arg token", token),
            };
            self.nodes.items[last_arg_id].next = arg_id;
            last_arg_id = arg_id;
            self.advanceToken();
        }
        return expr_id;
    }

    /// Parses the right expression of a BinaryExpression.
    fn parseRightExpression(self: *Parser, left_op: BinaryExprOp) anyerror!NodeId {
        var start = self.next_pos;
        const expr_id = try self.parseTermExpr();

        // Check if next token is an operator with higher precedence.
        const token = self.peekToken();
        if (token.token_t == .operator) {
            const op_prec = getBinOpPrecedence(left_op);
            const right_op = toBinExprOp(token.data.operator_t);
            const right_op_prec = getBinOpPrecedence(right_op);
            if (right_op_prec > op_prec) {
                // Continue parsing right.
                _ = self.consumeToken();
                start = self.next_pos;
                const right_id = try self.parseRightExpression(right_op);

                const bin_expr = self.pushNode(.bin_expr, start);
                self.nodes.items[bin_expr].head = .{
                    .left_right = .{
                        .left = expr_id,
                        .right = right_id,
                        .extra = @enumToInt(right_op),
                    },
                };
                return bin_expr;
            }
        }
        return expr_id;
    }

    fn isVarDeclaredFromScope(self: *Parser, name: []const u8) bool {
        var i = self.block_stack.items.len;
        while (i > 0) {
            i -= 1;
            if (self.block_stack.items[i].vars.contains(name)) {
                return true;
            }
        }
        return false;
    }

    fn parseIfThenExpr(self: *Parser, if_cond: NodeId, start: u32) !NodeId {
        // Assume first token is `then`
        self.advanceToken();

        const if_expr = self.pushNode(.if_expr, start);

        const if_body = (try self.parseExpr(.{})) orelse {
            return self.reportTokenError(error.SyntaxError, "Expected if body.", self.peekToken());
        };
        self.nodes.items[if_expr].head = .{
            .if_expr = .{
                .cond = if_cond,
                .body_expr = if_body,
                .else_clause = NullId,
            },
        };

        const token = self.peekToken();
        if (token.token_t == .else_k) {
            const else_clause = self.pushNode(.else_clause, self.next_pos);
            self.advanceToken();

            const else_body = (try self.parseExpr(.{})) orelse {
                return self.reportTokenError(error.SyntaxError, "Expected else body.", self.peekToken());
            };
            self.nodes.items[else_clause].head = .{
                .child_head = else_body,
            };

            self.nodes.items[if_expr].head.if_expr.else_clause = else_clause;
        }
        return if_expr;
    }

    fn parseTermExpr(self: *Parser) anyerror!NodeId {
        var start = self.next_pos;
        var token = self.peekToken();

        var left_id = switch (token.token_t) {
            .ident => b: {
                self.advanceToken();
                const id = self.pushNode(.ident, start);

                const name_token = self.tokens.items[start];
                const name = self.src.items[name_token.start_pos..name_token.data.end_pos];
                if (!self.isVarDeclaredFromScope(name)) {
                    try self.deps.put(self.alloc, name, id);
                }

                break :b id;
            },
            .true_k => {
                self.advanceToken();
                return self.pushNode(.true_literal, start);
            },
            .false_k => {
                self.advanceToken();
                return self.pushNode(.false_literal, start);
            },
            .none_k => {
                self.advanceToken();
                return self.pushNode(.none, start);
            },
            .number => b: {
                self.advanceToken();
                break :b self.pushNode(.number, start);
            },
            .string => b: {
                self.advanceToken();
                break :b self.pushNode(.string, start);
            },
            .at => b: {
                self.advanceToken();
                token = self.peekToken();
                if (token.token_t == .ident) {
                    const ident = self.pushNode(.ident, self.next_pos);
                    self.advanceToken();
                    const at_ident = self.pushNode(.at_ident, start);
                    self.nodes.items[at_ident].head = .{
                        .annotation = .{
                            .child = ident,
                        },
                    };
                    break :b at_ident;
                } else {
                    return self.reportTokenError(error.SyntaxError, "Expected identifier.", token);
                }
            },
            .await_k => {
                // Await expression.
                const expr_id = self.pushNode(.await_expr, start);
                self.advanceToken();
                const term_id = try self.parseTermExpr();
                self.nodes.items[expr_id].head = .{
                    .child_head = term_id,
                };
                return expr_id;
            },
            .func_k => {
                // Lambda function.
                return self.parseLambdaFunction();
            },
            .if_k => {
                self.advanceToken();
                const if_cond = (try self.parseExpr(.{})) orelse {
                    return self.reportTokenError(error.SyntaxError, "Expected if condition.", self.peekToken());
                };

                token = self.peekToken();
                if (token.token_t == .then_k) {
                    return try self.parseIfThenExpr(if_cond, start);
                } else {
                    return self.reportTokenError(error.SyntaxError, "Expected then keyword.", token);
                }
            },
            .left_paren => b: {
                _ = self.consumeToken();
                token = self.peekToken();

                const expr_id = (try self.parseExpr(.{})) orelse {
                    token = self.peekToken();
                    if (token.token_t == .right_paren) {
                        _ = self.consumeToken();
                    } else {
                        return self.reportTokenError(error.SyntaxError, "Expected expression.", token);
                    }
                    // Assume empty args for lambda.
                    token = self.peekToken();
                    if (token.token_t == .equal_greater) {
                        return self.parseNoParamLambdaFunc();
                    } else {
                        return self.reportTokenError(error.SyntaxError, "Unexpected paren.", token);
                    }
                };
                token = self.peekToken();
                if (token.token_t == .right_paren) {
                    _ = self.consumeToken();
                    break :b expr_id;
                } else if (token.token_t == .comma) {
                    self.next_pos = start;
                    return self.parseLambdaFunction();
                } else {
                    return self.reportTokenError(error.SyntaxError, "Expected right parenthesis.", token);
                }
            },
            .left_brace => b: {
                // Map literal.
                const map_id = try self.parseMapLiteral();
                break :b map_id;
            },
            .left_bracket => b: {
                // Array literal.
                const arr_id = try self.parseArrayLiteral();
                break :b arr_id;
            },
            .not_k => {
                self.advanceToken();
                const expr = self.pushNode(.unary_expr, start);
                const child = try self.parseTermExpr();
                self.nodes.items[expr].head = .{
                    .unary = .{
                        .child = child,
                        .op = .not,
                    },
                };
                return expr;
            },
            .operator => {
                if (token.data.operator_t == .minus) {
                    self.advanceToken();
                    const expr_id = self.pushNode(.unary_expr, start);
                    const term_id = try self.parseTermExpr();
                    self.nodes.items[expr_id].head = .{
                        .unary = .{
                            .child = term_id,
                            .op = .minus,
                        },
                    };
                    return expr_id;
                } else return self.reportTokenError(error.SyntaxError, "Unexpected operator.", token);
            },
            .none => return self.reportTokenError(error.SyntaxError, "Expected term expr.", token),
            else => return self.reportTokenError(error.SyntaxError, "Expected term expr.", token),
        };

        while (true) {
            const next = self.peekToken();
            switch (next.token_t) {
                .equal_greater => {
                    const left = self.nodes.items[left_id];
                    if (left.node_t == .ident) {
                        // Lambda.
                        return self.parseLambdaFuncWithParam(left_id);
                    } else {
                        return self.reportTokenError(error.SyntaxError, "Unexpected `=>` token", next);
                    }
                },
                .dot => {
                    // AccessExpression.
                    self.advanceToken();
                    const next2 = self.peekToken();
                    if (next2.token_t == .ident) {
                        const right_id = self.pushNode(.ident, self.next_pos);
                        const expr_id = self.pushNode(.access_expr, start);
                        self.nodes.items[expr_id].head = .{
                            .left_right = .{
                                .left = left_id,
                                .right = right_id,
                            },
                        };
                        left_id = expr_id;
                        self.advanceToken();
                        start = self.next_pos;
                    } else return self.reportTokenError(error.BadToken, "Expected ident", next2);
                },
                .left_bracket => {
                    // If left is an accessor expression or identifier, parse as access expression.
                    const left_t = self.nodes.items[left_id].node_t;
                    if (left_t == .ident or left_t == .access_expr) {
                        // Consume left bracket.
                        self.advanceToken();

                        token = self.peekToken();
                        if (token.token_t == .dot_dot) {
                            // Start of list to end index slice.
                            self.advanceToken();
                            const right_range = (try self.parseExpr(.{})) orelse {
                                return self.reportTokenError(error.SyntaxError, "Expected expression.", self.peekToken());
                            };

                            token = self.peekToken();
                            if (token.token_t == .right_bracket) {
                                self.advanceToken();
                                const res = self.pushNode(.arr_range_expr, start);
                                self.nodes.items[res].head = .{
                                    .arr_range_expr = .{
                                        .arr = left_id,
                                        .left = NullId,
                                        .right = right_range,
                                    },
                                };
                                left_id = res;
                                start = self.next_pos;
                            } else {
                                return self.reportTokenError(error.SyntaxError, "Expected right bracket.", token);
                            }
                        } else {
                            const expr_id = (try self.parseExpr(.{})) orelse {
                                return self.reportTokenError(error.SyntaxError, "Expected expression.", self.peekToken());
                            };

                            token = self.peekToken();
                            if (token.token_t == .right_bracket) {
                                self.advanceToken();
                                const access_id = self.pushNode(.arr_access_expr, start);
                                self.nodes.items[access_id].head = .{
                                    .left_right = .{
                                        .left = left_id,
                                        .right = expr_id,
                                    },
                                };
                                left_id = access_id;
                                start = self.next_pos;
                            } else if (token.token_t == .dot_dot) {
                                self.advanceToken();
                                token = self.peekToken();
                                if (token.token_t == .right_bracket) {
                                    // Start index to end of list slice.
                                    self.advanceToken();
                                    const res = self.pushNode(.arr_range_expr, start);
                                    self.nodes.items[res].head = .{
                                        .arr_range_expr = .{
                                            .arr = left_id,
                                            .left = expr_id,
                                            .right = NullId,
                                        },
                                    };
                                    left_id = res;
                                    start = self.next_pos;
                                } else {
                                    const right_expr = (try self.parseExpr(.{})) orelse {
                                        return self.reportTokenError(error.SyntaxError, "Expected expression.", self.peekToken());
                                    };
                                    token = self.peekToken();
                                    if (token.token_t == .right_bracket) {
                                        self.advanceToken();
                                        const res = self.pushNode(.arr_range_expr, start);
                                        self.nodes.items[res].head = .{
                                            .arr_range_expr = .{
                                                .arr = left_id,
                                                .left = expr_id,
                                                .right = right_expr,
                                            },
                                        };
                                        left_id = res;
                                        start = self.next_pos;
                                    } else {
                                        return self.reportTokenError(error.SyntaxError, "Expected right bracket.", token);
                                    }
                                }
                            } else {
                                return self.reportTokenError(error.SyntaxError, "Expected right bracket.", token);
                            }
                        }
                    } else return self.reportTokenError(error.SyntaxError, "Expected variable to left of access expression.", next);
                },
                .left_paren => {
                    // If left is an accessor expression or identifier, parse as call expression.
                    const left_t = self.nodes.items[left_id].node_t;
                    if (left_t == .ident or left_t == .access_expr or left_t == .at_ident) {
                        const call_id = try self.parseCallExpression(left_id);
                        left_id = call_id;
                    } else return self.reportTokenError(error.SyntaxError, "Expected variable to left of call expression.", next);
                },
                .dot_dot,
                .right_bracket,
                .right_paren,
                .right_brace,
                .else_k,
                .comma,
                .colon,
                .plus_equal,
                .equal,
                .operator,
                .logic_op,
                .then_k,
                .as_k => break,
                .ident,
                .number,
                .string => {
                    // CallExpression.
                    left_id = try self.parseNoParenCallExpression(left_id);
                    start = self.next_pos;
                },
                .new_line,
                .none => break,
                else => return self.reportTokenError(error.UnknownToken, "Unknown token", next),
            }
        }
        return left_id;
    }

    fn parseExpr(self: *Parser, opts: ParseExprOptions) anyerror!?NodeId {
        var start = self.next_pos;
        var token = self.peekToken();

        var left_id = switch (token.token_t) {
            .if_k => {
                return try self.parseTermExpr();
            },
            .none => return null,
            .right_paren => return null,
            .right_bracket => return null,
            else => try self.parseTermExpr(),
        };

        while (true) {
            const next = self.peekToken();
            switch (next.token_t) {
                .plus_equal,
                .equal => {
                    // If left is an accessor expression or identifier, parse as assignment statement.
                    if (opts.returnLeftAssignExpr) {
                        switch (self.nodes.items[left_id].node_t) {
                            .arr_access_expr,
                            .ident => {
                                opts.outIsAssignStmt.* = true;
                                return left_id;
                            },
                            else => {
                                return self.reportTokenError(error.SyntaxError, "Expected variable to left of assignment operator.", next);
                            },
                        }
                    } else {
                        break;
                    }
                },
                .operator => {
                    // BinaryExpression.
                    const op_t = next.data.operator_t;
                    const bin_op = toBinExprOp(op_t);
                    self.advanceToken();
                    const right_id = try self.parseRightExpression(bin_op);

                    const bin_expr = self.pushNode(.bin_expr, start);
                    self.nodes.items[bin_expr].head = .{
                        .left_right = .{
                            .left = left_id,
                            .right = right_id,
                            .extra = @enumToInt(bin_op),
                        },
                    };
                    left_id = bin_expr;
                },
                .and_k => {
                    self.advanceToken();
                    const right_id = try self.parseRightExpression(.and_op);
                    const bin_expr = self.pushNode(.bin_expr, start);
                    self.nodes.items[bin_expr].head = .{
                        .left_right = .{
                            .left = left_id,
                            .right = right_id,
                            .extra = @enumToInt(BinaryExprOp.and_op),
                        },
                    };
                    left_id = bin_expr;
                },
                .or_k => {
                    self.advanceToken();
                    const right_id = try self.parseRightExpression(.or_op);
                    const bin_expr = self.pushNode(.bin_expr, start);
                    self.nodes.items[bin_expr].head = .{
                        .left_right = .{
                            .left = left_id,
                            .right = right_id,
                            .extra = @enumToInt(BinaryExprOp.or_op),
                        },
                    };
                    left_id = bin_expr;
                },
                .logic_op => {
                    // BinaryExpression.
                    const op_t = next.data.logic_op_t;
                    const bin_op = try toBinExprOpFromLogicOp(op_t);
                    self.advanceToken();
                    const right_id = try self.parseRightExpression(bin_op);

                    const bin_expr = self.pushNode(.bin_expr, start);
                    self.nodes.items[bin_expr].head = .{
                        .left_right = .{
                            .left = left_id,
                            .right = right_id,
                            .extra = @enumToInt(bin_op),
                        },
                    };
                    left_id = bin_expr;
                },
                .right_bracket,
                .right_paren,
                .right_brace,
                .else_k,
                .then_k,
                .comma,
                .colon,
                .dot_dot,
                .as_k,
                .new_line,
                .none => break,
                else => return self.reportTokenError(error.UnknownToken, "Unknown token", next),
            }
        }
        return left_id;
    }

    /// Assumes next token is the return token.
    fn parseReturnStatement(self: *Parser) !u32 {
        const start = self.next_pos;
        self.advanceToken();
        if (try self.parseExpr(.{})) |expr_id| {
            const id = self.pushNode(.return_expr_stmt, start);
            self.nodes.items[id].head = .{
                .child_head = expr_id,
            };
            return id;
        } else {
            return self.pushNode(.return_stmt, start);
        }
    }

    fn parseExprOrAssignStatement(self: *Parser) !?u32 {
        var is_assign_stmt = false;
        const expr_id = (try self.parseExpr(.{ .returnLeftAssignExpr = true, .outIsAssignStmt = &is_assign_stmt})) orelse return null;

        if (is_assign_stmt) {
            var token = self.peekToken();
            // Assumes next token is an assignment operator: =, +=.
            self.advanceToken();
            const right_expr_id = (try self.parseExpr(.{ .allowMultilineLambda = true })) orelse {
                return self.reportTokenError(error.SyntaxError, "Expected right expression for assignment statement.", self.peekToken());
            };
            const start = self.nodes.items[expr_id].start_token;
            const id = switch (token.token_t) {
                .equal => self.pushNode(.assign_stmt, start),
                .plus_equal => self.pushNode(.add_assign_stmt, start),
                else => return self.reportTokenError(error.Unsupported, "Unsupported assignment operator.", token),
            };
            self.nodes.items[id].head = .{
                .left_right = .{
                    .left = expr_id,
                    .right = right_expr_id,
                },
            };

            const left = self.nodes.items[expr_id];
            if (left.node_t == .ident) {
                const name_token = self.tokens.items[left.start_token];
                const name = self.src.items[name_token.start_pos..name_token.data.end_pos];
                const block = &self.block_stack.items[self.block_stack.items.len-1];
                if (self.deps.get(name)) |node_id| {
                    if (node_id == expr_id) {
                        // Remove dependency now that it's recognized as assign statement.
                        _ = self.deps.remove(name);
                    }
                }
                try block.vars.put(self.alloc, name, {});
            }

            token = self.peekToken();
            if (token.token_t == .new_line) {
                self.advanceToken();
                return id;
            } else if (token.token_t == .none) {
                return id;
            } else return self.reportTokenError(error.BadToken, "Expected end of line or file", token);
        } else {
            const start = self.nodes.items[expr_id].start_token;
            const id = self.pushNode(.expr_stmt, start);
            self.nodes.items[id].head = .{
                .child_head = expr_id,
            };

            const token = self.peekToken();
            if (token.token_t == .new_line) {
                self.advanceToken();
                return id;
            } else if (token.token_t == .none) {
                return id;
            } else return self.reportTokenError(error.BadToken, "Expected end of line or file", token);
        }
    }

    pub fn pushNode(self: *Parser, node_t: NodeType, start: u32) NodeId {
        const id = self.nodes.items.len;
        self.nodes.append(self.alloc, .{
            .node_t = node_t,
            .start_token = start,
            .next = NullId,
            .head = undefined,
        }) catch fatal();
        return @intCast(NodeId, id);
    }

    inline fn pushIdentToken(self: *Parser, start_pos: u32, end_pos: u32) void {
        self.tokens.append(self.alloc, .{
            .token_t = .ident,
            .start_pos = start_pos,
            .data = .{
                .end_pos = end_pos,
            },
        }) catch fatal();
    }

    inline fn pushNumberToken(self: *Parser, start_pos: u32, end_pos: u32) void {
        self.tokens.append(self.alloc, .{
            .token_t = .number,
            .start_pos = start_pos,
            .data = .{
                .end_pos = end_pos,
            },
        }) catch fatal();
    }

    inline fn pushStringToken(self: *Parser, start_pos: u32, end_pos: u32) void {
        self.tokens.append(self.alloc, .{
            .token_t = .string,
            .start_pos = start_pos,
            .data = .{
                .end_pos = end_pos,
            },
        }) catch fatal();
    }

    inline fn pushLogicOpToken(self: *Parser, logic_op_t: LogicOpType, start_pos: u32) void {
        self.tokens.append(self.alloc, .{
            .token_t = .logic_op,
            .start_pos = start_pos,
            .data = .{
                .logic_op_t = logic_op_t,
            },
        }) catch fatal();
    }

    inline fn pushOpToken(self: *Parser, operator_t: OperatorType, start_pos: u32) void {
        self.tokens.append(self.alloc, .{
            .token_t = .operator,
            .start_pos = start_pos,
            .data = .{
                .operator_t = operator_t,
            },
        }) catch fatal();
    }

    inline fn pushIndentToken(self: *Parser, num_spaces: u32, start_pos: u32) void {
        self.tokens.append(self.alloc, .{
            .token_t = .indent,
            .start_pos = start_pos,
            .data = .{
                .indent = num_spaces,
            },
        }) catch fatal();
    }

    inline fn pushToken(self: *Parser, token_t: TokenType, start_pos: u32) void {
        self.tokens.append(self.alloc, .{
            .token_t = token_t,
            .start_pos = start_pos,
            .data = .{
                .nothing = {},
            },
        }) catch fatal();
    }

    /// When n=0, this is equivalent to peekToken.
    inline fn peekTokenAhead(self: Parser, n: u32) Token {
        if (self.next_pos + n < self.tokens.items.len) {
            return self.tokens.items[self.next_pos + n];
        } else {
            return Token{
                .token_t = .none,
                .start_pos = self.next_pos,
                .data = .{
                    .nothing = {},
                },
            };
        }
    }

    inline fn peekToken(self: Parser) Token {
        if (!self.isAtEndToken()) {
            return self.tokens.items[self.next_pos];
        } else {
            return Token{
                .token_t = .none,
                .start_pos = self.next_pos,
                .data = .{
                    .nothing = {},
                },
            };
        }
    }

    inline fn advanceToken(self: *Parser) void {
        self.next_pos += 1;
    }

    inline fn isAtEndToken(self: Parser) bool {
        return self.tokens.items.len == self.next_pos;
    }

    inline fn consumeToken(self: *Parser) Token {
        const token = self.tokens.items[self.next_pos];
        self.next_pos += 1;
        return token;
    }
};

pub const OperatorType = enum(u3) {
    plus,
    minus,
    star,
    star_star,
    slash,
    percent,
};

const LogicOpType = enum(u3) {
    bang = 0,
    bang_equal = 1,
    less = 2,
    less_equal = 3,
    greater = 4,
    greater_equal = 5,
    equal_equal = 6,
};

pub const TokenType = enum {
    ident,
    number,
    string,
    operator,
    at,
    left_paren,
    right_paren,
    left_brace,
    right_brace,
    left_bracket,
    right_bracket,
    equal_greater,
    comma,
    colon,
    dot,
    dot_dot,
    logic_op,
    equal,
    plus_equal,
    new_line,
    indent,
    return_k,
    break_k,
    if_k,
    then_k,
    else_k,
    for_k,
    await_k,
    true_k,
    false_k,
    or_k,
    and_k,
    not_k,
    as_k,
    pass_k,
    none_k,
    func_k,
    // Error token, returned if ignoreErrors = true.
    err,
    /// Used to indicate no token.
    none,
};

pub const Token = struct {
    token_t: TokenType,
    start_pos: u32,
    data: union {
        end_pos: u32,
        operator_t: OperatorType,
        logic_op_t: LogicOpType,
        // Num indent spaces.
        indent: u32,
        nothing: void,
    },
};

const NodeType = enum {
    root,
    expr_stmt,
    assign_stmt,
    add_assign_stmt,
    pass_stmt,
    break_stmt,
    return_stmt,
    return_expr_stmt,
    at_stmt,
    ident,
    at_ident,
    true_literal,
    false_literal,
    none,
    string,
    await_expr,
    access_expr,
    arr_access_expr,
    arr_range_expr,
    call_expr,
    named_arg,
    bin_expr,
    unary_expr,
    number,
    if_expr,
    if_stmt,
    else_clause,
    for_inf_stmt,
    for_cond_stmt,
    for_range_stmt,
    for_iter_stmt,
    range_clause,
    as_range_clause,
    as_iter_clause,
    label_decl,
    func_decl,
    lambda_assign_decl,
    lambda_expr, 
    lambda_multi,
    map_literal,
    map_entry,
    arr_literal,
};

pub const BinaryExprOp = enum {
    plus,
    minus,
    star,
    star_star,
    slash,
    percent,
    bang_equal,
    less,
    less_equal,
    greater,
    greater_equal,
    equal_equal,
    and_op,
    or_op,
    dummy,
};

const UnaryOp = enum {
    minus,
    not,
};

pub const Node = struct {
    node_t: NodeType,
    start_token: u32,
    next: NodeId,
    /// Fixed size. TODO: Rename to `data`.
    head: union {
        left_right: struct {
            left: NodeId,
            right: NodeId,
            extra: u32 = NullId,
        },
        func_call: struct {
            callee: NodeId,
            arg_head: NodeId,
            has_named_arg: bool,
        },
        unary: struct {
            child: NodeId,
            op: UnaryOp,
        },
        child_head: NodeId,
        annotation: struct {
            child: NodeId,
        },
        at_stmt: struct {
            child: NodeId,
            skip_compile: bool,
        },
        func: struct {
            decl_id: FuncDeclId,
            body_head: NodeId,
        },
        lambda_assign_decl: struct {
            decl_id: FuncDeclId,
            body_head: NodeId,
            assign_expr: NodeId,
        },
        for_range_stmt: struct {
            range_clause: NodeId,
            body_head: NodeId,
            as_clause: NodeId,
        },
        for_iter_stmt: struct {
            iterable: NodeId,
            body_head: NodeId,
            as_clause: NodeId,
        },
        as_range_clause: struct {
            ident: NodeId,
            step: NodeId,
            inc: bool,
        },
        as_iter_clause: struct {
            value: NodeId,
            key: NodeId,
        },
        arr_range_expr: struct {
            arr: NodeId,
            left: NodeId,
            right: NodeId,
        },
        if_expr: struct {
            cond: NodeId,
            body_expr: NodeId,
            else_clause: NodeId,
        },
        else_clause: struct {
            body_head: NodeId,
            // for else ifs only.
            cond: NodeId,
            else_clause: NodeId,
        },
    },
};

pub const Result = struct {
    inner: ResultView,
    
    pub fn init(alloc: std.mem.Allocator, view: ResultView) !Result {
        const arr = try view.nodes.clone(alloc);
        const nodes = try alloc.create(std.ArrayListUnmanaged(Node));
        nodes.* = arr;

        const new_src = try alloc.dupe(u8, view.src);

        const deps = try alloc.create(std.StringHashMapUnmanaged(NodeId));
        deps.* = .{};
        var iter = view.deps.iterator();
        while (iter.next()) |entry| {
            const dep = entry.key_ptr.*;
            const offset = @ptrToInt(dep.ptr) - @ptrToInt(view.src.ptr);
            try deps.put(alloc, new_src[offset..offset+dep.len], entry.value_ptr.*);
        }

        const func_decls_arr = try view.func_decls.clone(alloc);
        const func_decls = try alloc.create(std.ArrayListUnmanaged(FunctionDeclaration));
        func_decls.* = func_decls_arr;

        return Result{
            .inner = .{
                .has_error = view.has_error,
                .err_msg = try alloc.dupe(u8, view.err_msg),
                .root_id = view.root_id,
                .nodes = nodes,
                .src = new_src,
                .tokens = try alloc.dupe(Token, view.tokens),
                .func_decls = func_decls,
                .func_params = try alloc.dupe(FunctionParam, view.func_params),
                .name = try alloc.dupe(u8, view.name),
                .deps = deps,
            },
        };
    }

    pub fn deinit(self: Result, alloc: std.mem.Allocator) void {
        alloc.free(self.inner.err_msg);
        self.inner.nodes.deinit(alloc);
        alloc.destroy(self.inner.nodes);
        alloc.free(self.inner.tokens);
        alloc.free(self.inner.src);
        self.inner.func_decls.deinit(alloc);
        alloc.destroy(self.inner.func_decls);
        alloc.free(self.inner.func_params);
        alloc.free(self.inner.name);
        self.inner.deps.deinit(alloc);
        alloc.destroy(self.inner.deps);
    }
};

/// Result data is not owned.
pub const ResultView = struct {
    has_error: bool,
    err_msg: []const u8,
    root_id: NodeId,

    /// ArrayList is returned so resulting ast can be modified.
    nodes: *std.ArrayListUnmanaged(Node),
    tokens: []const Token,
    src: []const u8,
    func_decls: *std.ArrayListUnmanaged(FunctionDeclaration),
    func_params: []const FunctionParam,

    name: []const u8,
    deps: *std.StringHashMapUnmanaged(NodeId),

    pub fn getTokenString(self: ResultView, token_id: u32) []const u8 {
        // Assumes token with end_pos.
        const token = self.tokens[token_id];
        return self.src[token.start_pos..token.data.end_pos];
    }

    pub fn dupe(self: ResultView, alloc: std.mem.Allocator) !Result {
        return try Result.init(alloc, self);
    }

    pub fn pushNode(self: ResultView, alloc: std.mem.Allocator, node_t: NodeType, start: u32) NodeId {
        return pushNodeToList(alloc, self.nodes, node_t, start);
    }

    pub fn assertOnlyOneStmt(self: ResultView, node_id: NodeId) ?NodeId {
        var count: u32 = 0;
        var stmt_id: NodeId = undefined;
        var cur_id = node_id;
        while (cur_id != NullId) {
            const cur = self.nodes.items[cur_id];
            if (cur.node_t == .at_stmt and cur.head.at_stmt.skip_compile) {
                cur_id = cur.next;
                continue;
            }
            count += 1;
            stmt_id = cur_id;
            if (count > 1) {
                return null;
            }
            cur_id = cur.next;
        }
        if (count == 1) {
            return stmt_id;
        } else return null;
    }
};

const FuncDeclId = u32;

pub const FunctionDeclaration = struct {
    name: IndexSlice,
    params: IndexSlice,
    return_type: ?IndexSlice,
};

pub const FunctionParam = struct {
    name: IndexSlice,
};

fn toBinExprOp(op: OperatorType) BinaryExprOp {
    return switch (op) {
        .plus => .plus,
        .minus => .minus,
        .star => .star,
        .star_star => .star_star,
        .slash => .slash,
        .percent => .percent,
    };
}

fn toBinExprOpFromLogicOp(op: LogicOpType) !BinaryExprOp {
    return switch (op) {
        .bang_equal => .bang_equal,
        .less => .less,
        .less_equal => .less_equal,
        .greater => .greater,
        .greater_equal => .greater_equal,
        .equal_equal => .equal_equal,
        else => {
            log.debug("unsupported logic op: {}", .{op});
            return error.Unsupported;
        },
    };
}

pub fn getBinOpPrecedence(op: BinaryExprOp) u8 {
    switch (op) {
        .slash,
        .star_star,
        .star => {
            return 2;
        },
        .minus,
        .plus => {
            return 1;
        },
        .greater,
        .greater_equal,
        .less,
        .less_equal,
        .bang_equal,
        .equal_equal => {
            return 0;
        },
        else => return 0,
    }
}

pub fn getLastStmt(nodes: []const Node, head: NodeId, out_prev: *NodeId) NodeId {
    var prev: NodeId = NullId;
    var cur_id = head;
    while (cur_id != NullId) {
        const node = nodes[cur_id];
        if (node.next == NullId) {
            out_prev.* = prev;
            return cur_id;
        }
        prev = cur_id;
        cur_id = node.next;
    }
    out_prev.* = NullId;
    return NullId;
}

pub fn pushNodeToList(alloc: std.mem.Allocator, nodes: *std.ArrayListUnmanaged(Node), node_t: NodeType, start: u32) NodeId {
    const id = nodes.items.len;
    nodes.append(alloc, .{
        .node_t = node_t,
        .start_token = start,
        .next = NullId,
        .head = undefined,
    }) catch fatal();
    return @intCast(NodeId, id);
}

test "Parse dependency variables" {
    var parser = Parser.init(t.alloc);
    defer parser.deinit();

    var res = try parser.parseNoErr(
        \\foo
    );
    try t.eq(res.deps.size, 1);
    try t.eq(res.deps.contains("foo"), true);

    // Assign statement.
    res = try parser.parseNoErr(
        \\foo = 123
        \\foo
    );
    try t.eq(res.deps.size, 0);

    // Function call.
    res = try parser.parseNoErr(
        \\foo()
    );
    try t.eq(res.deps.size, 1);
    try t.eq(res.deps.contains("foo"), true);

    // Function call after declaration.
    res = try parser.parseNoErr(
        \\func foo():
        \\  pass
        \\foo()
    );
    try t.eq(res.deps.size, 0);
}

pub fn logSrcPos(src: []const u8, start: u32, len: u32) void {
    if (start + len > src.len) {
        log.debug("{s}", .{ src[start..] });
    } else {
        log.debug("{s}", .{ src[start..start+len] });
    }
}

pub const TokenizeState = enum {
    start,
    token,
    end,
};

const TokenizerConfig = struct {
    /// Use provided functions to read buffer and advance position.
    user: bool,
};

/// Made generic in case there is a need to use a different src buffer. TODO: substring still needs to be abstracted into user fn.
pub fn Tokenizer(comptime Config: TokenizerConfig) type {
    return struct {
        inline fn isAtEndChar(p: *const Parser) bool {
            if (Config.user) {
                return p.user.isAtEndChar(p.user.ctx);
            } else {
                return p.src.items.len == p.next_pos;
            }
        }

        inline fn savePos(p: *Parser) void {
            if (Config.user) {
                p.user.savePos(p.user.ctx);
            } else {
                p.savePos = p.next_pos;
            }
        }

        inline fn restorePos(p: *Parser) void {
            if (Config.user) {
                p.user.restorePos(p.user.ctx);
            } else {
                p.next_pos = p.savePos;
            }
        }

        inline fn isNextChar(p: *const Parser, ch: u8) bool {
            if (isAtEndChar(p)) {
                return false;
            }
            return peekChar(p) == ch;
        }

        inline fn consumeChar(p: *Parser) u8 {
            const ch = peekChar(p);
            advanceChar(p);
            return ch;
        }

        inline fn peekChar(p: *const Parser) u8 {
            if (Config.user) {
                return p.user.peekChar(p.user.ctx);
            } else {
                return p.src.items[p.next_pos];
            }
        }

        inline fn getSubStrFrom(p: *const Parser, start: u32) []const u8 {
            if (Config.user) {
                return p.user.getSubStrFromDelta(p.user.ctx, p.next_pos - start);
            } else {
                return p.src.items[start..p.next_pos];
            }
        }

        inline fn peekCharAhead(p: *const Parser, steps: u32) ?u8 {
            if (Config.user) {
                return p.user.peekCharAhead(p.user.ctx, steps);
            } else {
                if (p.next_pos < p.src.items.len - steps) {
                    return p.src.items[p.next_pos + steps];
                } else return null;
            }
        }

        inline fn advanceChar(p: *Parser) void {
            if (Config.user) {
                p.user.advanceChar(p.user.ctx);
            } else {
                p.next_pos += 1;
            }
        }

        /// Returns true if a token was parsed.
        /// Returns false if the end was reached or a character was skipped.
        fn tokenizeOne(p: *Parser, parsedNewLine: *bool) !bool {
            parsedNewLine.* = false;
            const start = p.next_pos;
            const ch = consumeChar(p);
            switch (ch) {
                '(' => p.pushToken(.left_paren, start),
                ')' => p.pushToken(.right_paren, start),
                '{' => p.pushToken(.left_brace, start),
                '}' => p.pushToken(.right_brace, start),
                '[' => p.pushToken(.left_bracket, start),
                ']' => p.pushToken(.right_bracket, start),
                ',' => p.pushToken(.comma, start),
                '.' => {
                    if (peekChar(p) == '.') {
                        advanceChar(p);
                        p.pushToken(.dot_dot, start);
                    } else {
                        p.pushToken(.dot, start);
                    }
                },
                ':' => p.pushToken(.colon, start),
                '@' => p.pushToken(.at, start),
                '-' => p.pushOpToken(.minus, start),
                '%' => p.pushOpToken(.percent, start),
                '+' => {
                    if (peekChar(p) == '=') {
                        advanceChar(p);
                        p.pushToken(.plus_equal, start);
                    } else {
                        p.pushOpToken(.plus, start);
                    }
                },
                '*' => {
                    if (peekChar(p) == '*') {
                        advanceChar(p);
                        p.pushOpToken(.star_star, start);
                    } else {
                        p.pushOpToken(.star, start);
                    }
                },
                '/' => {
                    if (peekChar(p) == '/') {
                        // Single line comment. Ignore chars until eol.
                        while (!isAtEndChar(p)) {
                            _ = consumeChar(p);
                            if (peekChar(p) == '\n') {
                                // Don't consume new line or the current indentation could augment with the next line.
                                return tokenizeOne(p, parsedNewLine);
                            }
                        }
                        return false;
                    } else {
                        p.pushOpToken(.slash, start);
                    }
                },
                '!' => {
                    if (isNextChar(p, '=')) {
                        p.pushLogicOpToken(.bang_equal, start);
                        advanceChar(p);
                    } else {
                        p.pushLogicOpToken(.bang, start);
                    }
                },
                '=' => {
                    if (!isAtEndChar(p)) {
                        switch (peekChar(p)) {
                            '=' => {
                                advanceChar(p);
                                p.pushLogicOpToken(.equal_equal, start);
                            },
                            '>' => {
                                advanceChar(p);
                                p.pushToken(.equal_greater, start);
                            },
                            else => {
                                p.pushToken(.equal, start);
                            }
                        }
                    } else {
                        p.pushToken(.equal, start);
                    }
                },
                '<' => {
                    if (isNextChar(p, '=')) {
                        p.pushLogicOpToken(.less_equal, start);
                        advanceChar(p);
                    } else {
                        p.pushLogicOpToken(.less, start);
                    }
                },
                '>' => {
                    if (isNextChar(p, '=')) {
                        p.pushLogicOpToken(.greater_equal, start);
                        advanceChar(p);
                    } else {
                        p.pushLogicOpToken(.greater, start);
                    }
                },
                ' ',
                '\r',
                '\t' => {
                    // Consume whitespace.
                    while (!isAtEndChar(p)) {
                        var ch2 = peekChar(p);
                        switch (ch2) {
                            ' ',
                            '\r',
                            '\t' => advanceChar(p),
                            else => return tokenizeOne(p, parsedNewLine),
                        }
                    }
                    return false;
                },
                '\n' => {
                    p.pushToken(.new_line, start);
                    parsedNewLine.* = true;
                },
                '`' => {
                    savePos(p);
                    while (true) {
                        if (isAtEndChar(p)) {
                            if (p.tokenizeOpts.ignoreErrors) {
                                restorePos(p);
                                p.pushToken(.err, start);
                                return true;
                            } else return error.UnterminatedString;
                        }
                        const ch_ = consumeChar(p);
                        switch (ch_) {
                            '`' => {
                                p.pushStringToken(start, p.next_pos);
                                break;
                            },
                            '\\' => {
                                // Escape the next character.
                                if (isAtEndChar(p)) {
                                    if (p.tokenizeOpts.ignoreErrors) {
                                        restorePos(p);
                                        p.pushToken(.err, start);
                                        return true;
                                    } else return error.UnterminatedString;
                                }
                                _ = consumeChar(p);
                                continue;
                            },
                            else => {},
                        }
                    }
                },
                '\'' => {
                    savePos(p);
                    while (true) {
                        if (isAtEndChar(p)) {
                            if (p.tokenizeOpts.ignoreErrors) {
                                restorePos(p);
                                p.pushToken(.err, start);
                                return true;
                            } else return error.UnterminatedString;
                        }
                        const ch_ = consumeChar(p);
                        switch (ch_) {
                            '\'' => {
                                p.pushStringToken(start, p.next_pos);
                                break;
                            },
                            '\\' => {
                                // Escape the next character.
                                if (isAtEndChar(p)) {
                                    if (p.tokenizeOpts.ignoreErrors) {
                                        restorePos(p);
                                        p.pushToken(.err, start);
                                        return true;
                                    } else return error.UnterminatedString;
                                }
                                _ = consumeChar(p);
                                continue;
                            },
                            '\n' => {
                                if (p.tokenizeOpts.ignoreErrors) {
                                    restorePos(p);
                                    p.pushToken(.err, start);
                                    return true;
                                } else return error.UnterminatedString;
                            },
                            else => {},
                        }
                    }
                },
                else => {
                    if (std.ascii.isAlpha(ch)) {
                        tokenizeKeywordOrIdent(p, start);
                        return true;
                    }
                    if (ch >= '0' and ch <= '9') {
                        tokenizeNumber(p, start);
                        return true;
                    }
                    if (p.tokenizeOpts.ignoreErrors) {
                        p.pushToken(.err, start);
                        return true;
                    } else {
                        p.last_err = std.fmt.allocPrint(p.alloc, "unknown character: {c} ({}) at {}", .{ch, ch, start}) catch fatal();
                        return error.UnknownChar;
                    }
                }
            }
            return true;
        }

        /// Returns true if an indent or new line token was parsed.
        fn tokenizeIndentOne(p: *Parser) bool {
            const ch = peekChar(p);
            switch (ch) {
                ' ' => {
                    const start = p.next_pos;
                    advanceChar(p);
                    var count: u32 = 1;
                    while (true) {
                        const ch_ = peekChar(p);
                        if (ch_ == ' ') {
                            count += 1;
                            advanceChar(p);
                        } else break;
                    }
                    p.pushIndentToken(count, start);
                    return true;
                },
                '\n' => {
                    p.pushToken(.new_line, p.next_pos);
                    advanceChar(p);
                    return true;
                },
                else => return false,
            }
        }

        /// Step tokenizer with provided state.
        pub fn tokenizeStep(p: *Parser, state: TokenizeState) anyerror!TokenizeState {
            if (isAtEndChar(p)) {
                return .end;
            }
            switch (state) {
                .start => {
                    if (tokenizeIndentOne(p)) {
                        return .start;
                    } else {
                        return try tokenizeStep(p, .token);
                    }
                },
                .token => {
                    var parsedNewLine: bool = undefined;
                    if (try tokenizeOne(p, &parsedNewLine)) {
                        if (!parsedNewLine) {
                            return .token;
                        } else {
                            return .start;
                        }
                    } else {
                        while (!isAtEndChar(p)) {
                            if (try tokenizeOne(p, &parsedNewLine)) {
                                if (!parsedNewLine) {
                                    return .token;
                                } else {
                                    return .start;
                                }
                            }
                        }
                        return .end;
                    }
                },
                .end => return error.AtEnd,
            }
        }

        fn tokenize(p: *Parser, opts: TokenizeOptions) !void {
            p.tokenizeOpts = opts;
            p.tokens.clearRetainingCapacity();
            p.next_pos = 0;

            var parsedNewLine = false;
            while (!isAtEndChar(p)) {
                // First parse indent spaces.
                while (!isAtEndChar(p)) {
                    if (!tokenizeIndentOne(p)) {
                        break;
                    }
                }
                while (!isAtEndChar(p)) {
                    if (try tokenizeOne(p, &parsedNewLine)) {
                        if (parsedNewLine) {
                            break;
                        }
                    } else break;
                }
            }
        }

        fn tokenizeKeywordOrIdent(p: *Parser, start: u32) void {
            // Consume alpha.
            while (true) {
                if (isAtEndChar(p)) {
                    if (keywords.get(getSubStrFrom(p, start))) |token_t| {
                        p.pushToken(token_t, start);
                    } else {
                        p.pushIdentToken(start, p.next_pos);
                    }
                    return;
                }
                const ch = peekChar(p);
                if (std.ascii.isAlpha(ch)) {
                    advanceChar(p);
                    continue;
                } else break;
            }

            // Consume alpha, numeric, underscore.
            while (true) {
                if (isAtEndChar(p)) {
                    if (keywords.get(getSubStrFrom(p, start))) |token_t| {
                        p.pushToken(token_t, start);
                    } else {
                        p.pushIdentToken(start, p.next_pos);
                    }
                    return;
                }
                const ch = peekChar(p);
                if (std.ascii.isAlNum(ch)) {
                    advanceChar(p);
                    continue;
                }
                if (ch == '_') {
                    advanceChar(p);
                    continue;
                }
                if (keywords.get(getSubStrFrom(p, start))) |token_t| {
                    p.pushToken(token_t, start);
                } else {
                    p.pushIdentToken(start, p.next_pos);
                }
                return;
            }
        }

        fn tokenizeNumber(p: *Parser, start: u32) void {
            while (true) {
                if (isAtEndChar(p)) {
                    p.pushNumberToken(start, p.next_pos);
                    return;
                }
                const ch = peekChar(p);
                if (ch >= '0' and ch <= '9') {
                    advanceChar(p);
                    continue;
                } else break;
            }
            // Check for decimal.
            if (peekCharAhead(p, 1)) |ch2| {
                const ch = peekChar(p);
                if (ch == '.' and ch2 >= '0' and ch2 <= '9') {
                    advanceChar(p);
                    advanceChar(p);
                    while (true) {
                        if (isAtEndChar(p)) {
                            break;
                        }
                        const ch_ = peekChar(p);
                        if (ch_ >= '0' and ch_ <= '9') {
                            advanceChar(p);
                            continue;
                        } else break;
                    }
                }
            }
            p.pushNumberToken(start, p.next_pos);
        }
    };
}

const TokenizeOptions = struct {
    /// Used for syntax highlighting.
    ignoreErrors: bool = false,
};

const ParseExprOptions = struct {
    allowMultilineLambda: bool = false,
    returnLeftAssignExpr: bool = false,
    outIsAssignStmt: *bool = undefined,
};