const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const t = stdx.testing;
const fatal = stdx.fatal;
const fmt = @import("fmt.zig");
const v = fmt.v;
const cy = @import("cyber.zig");

pub const NodeId = u32;
const NullId = std.math.maxInt(u32);
const NullIdU16 = std.math.maxInt(u16);
const log = stdx.log.scoped(.parser);
const IndexSlice = stdx.IndexSlice(u32);

const dumpParseErrorStackTrace = builtin.mode == .Debug and !cy.isWasm and true;

const keywords = std.ComptimeStringMap(TokenType, .{
    .{ "and", .and_k },
    .{ "as", .as_k },
    .{ "await", .await_k },
    .{ "break", .break_k },
    .{ "catch", .catch_k },
    .{ "coinit", .coinit_k },
    .{ "compt", .compt_k },
    .{ "continue", .continue_k },
    .{ "coresume", .coresume_k },
    .{ "coyield", .coyield_k },
    .{ "each", .each_k },
    .{ "false", .false_k },
    .{ "for", .for_k },
    .{ "while", .while_k },
    .{ "func", .func_k },
    .{ "else", .else_k },
    .{ "if", .if_k },
    .{ "import", .import_k },
    .{ "is", .is_k },
    .{ "static", .static_k },
    .{ "capture", .capture_k },
    .{ "none", .none_k },
    .{ "not", .not_k },
    .{ "object", .object_k },
    .{ "or", .or_k },
    .{ "pass", .pass_k },
    .{ "recover", .recover_k },
    .{ "return", .return_k },
    .{ "tagtype", .tagtype_k },
    .{ "then", .then_k },
    .{ "true", .true_k },
    .{ "try", .try_k },
    .{ "var", .var_k },
    .{ "match", .match_k },
});

const BlockState = struct {
    placeholder: u32 = 0,
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
    func_decls: std.ArrayListUnmanaged(FuncDecl),

    /// Use the parser pass to record var declarations.
    varDecls: std.ArrayListUnmanaged(NodeId),

    // TODO: This should be implemented by user callbacks.
    /// @name arg.
    name: []const u8,
    /// Variable dependencies.
    deps: std.StringHashMapUnmanaged(NodeId),

    tokenizeOpts: TokenizeOptions,

    /// For custom functions.
    user: struct {
        ctx: *anyopaque,
        advanceChar: *const fn (*anyopaque) void,
        peekChar: *const fn (*anyopaque) u8,
        peekCharAhead: *const fn (*anyopaque, u32) ?u8,
        isAtEndChar: *const fn (*anyopaque) bool,
        getSubStrFromDelta: *const fn (*anyopaque, u32) []const u8,
        savePos: *const fn (*anyopaque) void,
        restorePos: *const fn (*anyopaque) void,
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
            .varDecls = .{},
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
        self.varDecls.deinit(self.alloc);
    }

    fn dumpTokensToCurrent(self: *Parser) void {
        for (self.tokens.items[0..self.next_pos+1]) |token| {
            log.debug("{}", .{token.token_t});
        }
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
        try self.src.ensureTotalCapacity(self.alloc, 1024);
        try self.src.appendSlice(self.alloc, src);
        self.name = "";
        self.deps.clearRetainingCapacity();

        const tokenizeOpts = TokenizeOptions{
            .ignoreErrors = false,
        };
        Tokenizer(.{ .user = false }).tokenize(self, tokenizeOpts) catch |err| {
            log.debug("tokenize error: {}", .{err});
            if (dumpParseErrorStackTrace and !cy.silentError) {
                std.debug.dumpStackTrace(@errorReturnTrace().?.*);
            }
            return ResultView{
                .has_error = true,
                .isTokenError = true,
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
            // self.dumpTokensToCurrent();
            logSrcPos(self.src.items, self.last_err_pos, 20);
            if (dumpParseErrorStackTrace and !cy.silentError) {
                std.debug.dumpStackTrace(@errorReturnTrace().?.*);
            }
            return ResultView{
                .has_error = true,
                .isTokenError = false,
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
            .isTokenError = false,
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

        const root_id = try self.pushNode(.root, 0);

        try self.pushBlock();
        defer self.popBlock();
        const first_stmt = try self.parseBodyStatements(0);
        self.nodes.items[root_id].head = .{
            .child_head = first_stmt,
        };
        return 0;
    }

    /// Returns number of spaces that precedes a statement.
    /// If current line is consumed if there is no statement.
    fn consumeIndentBeforeStmt(self: *Parser) !u32 {
        while (true) {
            var res: u32 = 0;
            var token = self.peekToken();
            if (token.tag() == .indent) {
                res = token.data.indent;
                self.advanceToken();
                token = self.peekToken();
            }
            if (token.tag() == .new_line) {
                self.advanceToken();
                continue;
            } else if (token.tag() == .indent) {
                // If another indent token is encountered, it would be a different type.
                return self.reportParseError("Can not mix tabs and spaces for indentation.", &.{});
            } else if (token.tag() == .none) {
                return 0;
            } else {
                return res;
            }
        }
    }

    fn pushBlock(self: *Parser) !void {
        try self.block_stack.append(self.alloc, .{
            .vars = .{},
        });
    }

    fn popBlock(self: *Parser) void {
        self.block_stack.items[self.block_stack.items.len-1].deinit(self.alloc);
        _ = self.block_stack.pop();
    }

    fn parseSingleOrIndentedBodyStmts(self: *Parser) !NodeId {
        var token = self.peekToken();
        if (token.tag() != .new_line) {
            // Parse single statement only.
            return (try self.parseStatement()) orelse {
                return self.reportParseError("Expected one statement after the block.", &.{});
            };
        } else {
            self.advanceToken();
            return self.parseIndentedBodyStatements();
        }
    }

    /// Indent is determined by the first body statement.
    fn parseIndentedBodyStatements(self: *Parser) !NodeId {
        const reqIndent = try self.parseFirstChildIndent(self.cur_indent);
        return self.parseBodyStatements(reqIndent);
    }

    // Assumes the first indent is already consumed.
    fn parseBodyStatements(self: *Parser, reqIndent: u32) !NodeId {
        const prevIndent = self.cur_indent;
        self.cur_indent = reqIndent;
        defer self.cur_indent = prevIndent;

        var first_stmt = (try self.parseStatement()) orelse return NullId;
        var last_stmt = first_stmt;

        // Parse body statements until indentation goes back to at least the previous indent.
        while (true) {
            const start = self.next_pos;
            const indent = try self.consumeIndentBeforeStmt();
            if (indent == reqIndent) {
                const id = (try self.parseStatement()) orelse break;
                self.nodes.items[last_stmt].next = id;
                last_stmt = id;
            } else if (indent <= prevIndent) {
                self.next_pos = start;
                break;
            } else {
                return self.reportParseError("Unexpected indentation.", &.{});
            }
        }
        return first_stmt;
    }

    /// Parses the first child indent and returns the indent size.
    fn parseFirstChildIndent(self: *Parser, fromIndent: u32) !u32 {
        const indent = try self.consumeIndentBeforeStmt();
        if (indent > fromIndent) {
            return indent;
        } else {
            return self.reportParseError("Block requires at least one statement. Use the `pass` statement as a placeholder.", &.{});
        }
    }

    fn parseLambdaFuncWithParam(self: *Parser, paramIdent: NodeId) !NodeId {
        const start = self.next_pos;
        // Assumes first token is `=>`.
        self.advanceToken();

        var decl = FuncDecl{
            .name = undefined,
            .params = stdx.IndexSlice(u32).init(@intCast(u32, self.func_params.items.len), @intCast(u32, self.func_params.items.len+1)),
            .return_type = null,
        };

        const param = self.nodes.items[paramIdent];
        const token = self.tokens.items[param.start_token];
        const name = IndexSlice.init(token.pos(), token.data.end_pos);
        try self.func_params.append(self.alloc, .{
            .name = name,
            .typeName = IndexSlice.init(0, 0),
        });

        // Parse body expr.
        const body_expr = (try self.parseExpr(.{})) orelse {
            return self.reportParseError("Expected lambda body expression.", &.{});
        };
        
        const decl_id = @intCast(u32, self.func_decls.items.len);
        try self.func_decls.append(self.alloc, decl);

        const id = try self.pushNode(.lambda_expr, start);
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

        var decl = FuncDecl{
            .name = undefined,
            .params = stdx.IndexSlice(u32).init(0, 0),
            .return_type = null,
        };

        // Parse body expr.
        const body_expr = (try self.parseExpr(.{})) orelse {
            return self.reportParseError("Expected lambda body expression.", &.{});
        };
        
        const decl_id = @intCast(u32, self.func_decls.items.len);
        try self.func_decls.append(self.alloc, decl);

        const id = try self.pushNode(.lambda_expr, start);
        self.nodes.items[id].head = .{
            .func = .{
                .decl_id = decl_id,
                .body_head = body_expr,
            },
        };
        return id;
    }

    fn parseMultilineLambdaFunction(self: *Parser) !NodeId {
        const start = self.next_pos;

        // Assume first token is `func`.
        self.advanceToken();

        var decl = FuncDecl{
            .name = undefined,
            .params = undefined,
            .return_type = null,
        };

        decl.params = try self.parseFunctionParams();
        decl.return_type = try self.parseFunctionReturn();

        try self.pushBlock();
        const firstChild = try self.parseSingleOrIndentedBodyStmts();
        self.popBlock();
            
        const decl_id = @intCast(u32, self.func_decls.items.len);
        try self.func_decls.append(self.alloc, decl);

        const id = try self.pushNode(.lambda_multi, start);
        self.nodes.items[id].head = .{
            .func = .{
                .decl_id = decl_id,
                .body_head = firstChild,
            },
        };
        return id;
    }

    fn parseLambdaFunction(self: *Parser) !NodeId {
        const start = self.next_pos;

        var decl = FuncDecl{
            .name = undefined,
            .params = undefined,
            .return_type = null,
        };

        decl.params = try self.parseFunctionParams();

        var token = self.peekToken();
        if (token.tag() != .equal_greater) {
            return self.reportParseError("Expected =>.", &.{});
        }
        self.advanceToken();

        // Parse body expr.
        const body_expr = (try self.parseExpr(.{})) orelse {
            return self.reportParseError("Expected lambda body expression.", &.{});
        };
        
        const decl_id = @intCast(u32, self.func_decls.items.len);
        try self.func_decls.append(self.alloc, decl);

        const id = try self.pushNode(.lambda_expr, start);
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
        if (token.tag() != .left_paren) {
            return self.reportParseError("Expected open parenthesis.", &.{});
        }
        self.advanceToken();

        // Parse params.
        const param_start = @intCast(u32, self.func_params.items.len);
        outer: {
            token = self.peekToken();
            if (token.tag() == .ident) {
                self.advanceToken();
                const name = IndexSlice.init(token.pos(), token.data.end_pos);

                // Check for type.
                token = self.peekToken();
                var typeName = IndexSlice.init(0, 0);
                if (token.tag() == .ident) {
                    self.advanceToken();
                    typeName = IndexSlice.init(token.pos(), token.data.end_pos);
                }

                try self.func_params.append(self.alloc, .{
                    .name = name,
                    .typeName = typeName,
                });
            } else if (token.tag() == .right_paren) {
                self.advanceToken();
                break :outer;
            } else return self.reportParseError("Unexpected token in function param list.", &.{});
            while (true) {
                token = self.peekToken();
                switch (token.tag()) {
                    .comma => {
                        self.advanceToken();
                    },
                    .right_paren => {
                        self.advanceToken();
                        break;
                    },
                    else => return self.reportParseError("Unexpected token in function param list.", &.{}),
                }

                token = self.peekToken();
                if (token.tag() != .ident) {
                    return self.reportParseError("Expected param identifier.", &.{});
                }
                self.advanceToken();
                const name = IndexSlice.init(token.pos(), token.data.end_pos);

                // Check for type.
                token = self.peekToken();
                var typeName = IndexSlice.init(0, 0);
                if (token.tag() == .ident) {
                    self.advanceToken();
                    typeName = IndexSlice.init(token.pos(), token.data.end_pos);
                }

                try self.func_params.append(self.alloc, .{
                    .name = name,
                    .typeName = typeName,
                });
            }
        }
        return IndexSlice.init(param_start, @intCast(u32, self.func_params.items.len));
    }

    fn parseFunctionReturn(self: *Parser) !?IndexSlice {
        var token = self.peekToken();
        if (token.tag() == .colon) {
            self.advanceToken();
            return null;
        } else if (token.tag() == .ident) {
            const return_type = IndexSlice.init(token.pos(), token.data.end_pos);
            self.advanceToken();
            token = self.peekToken();
            if (token.tag() != .colon) {
                return self.reportParseError("Expected colon.", &.{});
            }
            self.advanceToken();
            return return_type;
        } else {
            return self.reportParseError("Expected colon or type.", &.{});
        }
    }

    fn parseTagMember(self: *Parser) !?NodeId {
        const start = self.next_pos;
        var token = self.peekToken();
        if (token.tag() == .ident) {
            const name = try self.pushIdentNode(self.next_pos);
            self.advanceToken();

            token = self.peekToken();
            if (token.tag() == .new_line) {
                self.advanceToken();
                const field = try self.pushNode(.tagMember, start);
                self.nodes.items[field].head = .{
                    .tagMember = .{
                        .name = name,
                    },
                };
                return field;
            } else {
                return self.reportParseError("Unexpected token.", &.{});
            }
        } else return null;
    }

    fn parseStructField(self: *Parser) !?NodeId {
        const start = self.next_pos;
        var token = self.peekToken();
        if (token.tag() == .ident) {
            const name = try self.pushIdentNode(self.next_pos);
            self.advanceToken();

            token = self.peekToken();
            if (token.tag() == .ident) {
                const typeN = try self.pushIdentNode(self.next_pos);
                self.advanceToken();

                token = self.peekToken();
                if (token.tag() == .new_line) {
                    self.advanceToken();
                } else {
                    return self.reportParseError("Expected new line.", &.{});
                }

                const field = try self.pushNode(.structField, start);
                self.nodes.items[field].head = .{
                    .structField = .{
                        .name = name,
                        .fieldType = typeN,
                    },
                };
                return field;
            } else if (token.tag() == .new_line) {
                self.advanceToken();
                const field = try self.pushNode(.structField, start);
                self.nodes.items[field].head = .{
                    .structField = .{
                        .name = name,
                        .fieldType = NullId,
                    },
                };
                return field;
            } else {
                return self.reportParseError("Expected type.", &.{});
            }
        } else return null;
    }

    fn parseTagDecl(self: *Parser) !NodeId {
        const start = self.next_pos;
        // Assumes first token is the `tagtype` keyword.
        self.advanceToken();

        // Parse tag name.
        var token = self.peekToken();
        var name: NodeId = NullId;
        if (token.tag() == .ident) {
            name = try self.pushIdentNode(self.next_pos);
            self.advanceToken();
        } else return self.reportParseError("Expected tag name identifier.", &.{});

        token = self.peekToken();
        if (token.tag() == .colon) {
            self.advanceToken();
        } else {
            return self.reportParseError("Expected colon.", &.{});
        }

        const reqIndent = try self.parseFirstChildIndent(self.cur_indent);
        const prevIndent = self.cur_indent;
        self.cur_indent = reqIndent;
        defer self.cur_indent = prevIndent;

        var firstMember = (try self.parseTagMember()) orelse {
            return self.reportParseError("Expected tag member.", &.{});
        };
        var lastMember = firstMember;

        while (true) {
            const start2 = self.next_pos;
            const indent = try self.consumeIndentBeforeStmt();
            if (indent == reqIndent) {
                const id = (try self.parseTagMember()) orelse break;
                self.nodes.items[lastMember].next = id;
                lastMember = id;
            } else if (indent <= prevIndent) {
                self.next_pos = start2;
                const id = try self.pushNode(.tagDecl, start);
                self.nodes.items[id].head = .{
                    .tagDecl = .{
                        .name = name,
                        .memberHead = firstMember,
                    },
                };
                return id;
            } else {
                return self.reportParseError("Unexpected indentation.", &.{});
            }
        }
        unreachable;
    }

    fn parseStructDecl(self: *Parser) !NodeId {
        const start = self.next_pos;
        // Assumes first token is the `type` keyword.
        self.advanceToken();

        // Parse struct name.
        var token = self.peekToken();
        var name: NodeId = NullId;
        if (token.tag() == .ident) {
            name = try self.pushIdentNode(self.next_pos);
            self.advanceToken();
        } else return self.reportParseError("Expected type name identifier.", &.{});

        token = self.peekToken();
        if (token.tag() == .colon) {
            self.advanceToken();
        } else {
            return self.reportParseError("Expected colon.", &.{});
        }

        const reqIndent = try self.parseFirstChildIndent(self.cur_indent);
        const prevIndent = self.cur_indent;
        self.cur_indent = reqIndent;
        defer self.cur_indent = prevIndent;

        var firstField = (try self.parseStructField()) orelse NullId;
        if (firstField != NullId) {
            var lastField = firstField;

            while (true) {
                const start2 = self.next_pos;
                const indent = try self.consumeIndentBeforeStmt();
                if (indent == reqIndent) {
                    const id = (try self.parseStructField()) orelse break;
                    self.nodes.items[lastField].next = id;
                    lastField = id;
                } else if (indent <= prevIndent) {
                    self.next_pos = start2;
                    const id = try self.pushNode(.structDecl, start);
                    self.nodes.items[id].head = .{
                        .structDecl = .{
                            .name = name,
                            .fieldsHead = firstField,
                            .funcsHead = NullId,
                        },
                    };
                    return id;
                } else {
                    return self.reportParseError("Unexpected indentation.", &.{});
                }
            }
        }

        token = self.peekToken();
        if (token.tag() == .func_k) {
            var firstFunc = try self.parseFunctionDecl();
            var lastFunc = firstFunc;

            while (true) {
                const start2 = self.next_pos;
                const indent = try self.consumeIndentBeforeStmt();
                if (indent == reqIndent) {
                    token = self.peekToken();
                    if (token.tag() == .func_k) {
                        const id = try self.parseFunctionDecl();
                        self.nodes.items[lastFunc].next = id;
                        lastFunc = id;
                    } else return self.reportParseError("Unexpected token.", &.{});
                } else if (indent <= prevIndent) {
                    self.next_pos = start2;
                    break;
                } else {
                    return self.reportParseError("Unexpected indentation.", &.{});
                }
            }

            const id = try self.pushNode(.structDecl, start);
            self.nodes.items[id].head = .{
                .structDecl = .{
                    .name = name,
                    .fieldsHead = firstField,
                    .funcsHead = firstFunc,
                },
            };
            return id;
        } else {
            return self.reportParseError("Unexpected token.", &.{});
        }
    }

    fn parseFunctionDecl(self: *Parser) !NodeId {
        const start = self.next_pos;
        // Assumes first token is the `func` keyword.
        self.advanceToken();

        var decl = FuncDecl{
            .name = undefined,
            .params = undefined,
            .return_type = null,
        };

        // Parse function name.
        var token = self.peekToken();
        const left_pos = self.next_pos;
        if (token.tag() == .ident) {
            decl.name = IndexSlice.init(token.pos(), token.data.end_pos);
            self.advanceToken();
        } else return self.reportParseError("Expected function name identifier.", &.{});

        token = self.peekToken();
        if (token.tag() == .dot) {
            // Parse lambda assign decl.
            var left = try self.pushIdentNode(left_pos);
            self.advanceToken();
            while (true) {
                token = self.peekToken();
                if (token.tag() == .ident) {
                    const ident = try self.pushIdentNode(self.next_pos);
                    const expr = try self.pushNode(.accessExpr, left_pos);
                    self.nodes.items[expr].head = .{
                        .accessExpr = .{
                            .left = left,
                            .right = ident,
                        },
                    };
                    left = expr;
                } else {
                    return self.reportParseError("Expected ident.", &.{});
                }

                self.advanceToken();
                token = self.peekToken();
                if (token.tag() == .left_paren) {
                    break;
                } else if (token.tag() == .dot) {
                    continue;
                } else {
                    return self.reportParseError("Expected open paren.", &.{});
                }
            }

            decl.params = try self.parseFunctionParams();
            decl.return_type = try self.parseFunctionReturn();

            try self.pushBlock();
            defer self.popBlock();
            const first_stmt = try self.parseIndentedBodyStatements();

            const decl_id = @intCast(u32, self.func_decls.items.len);
            try self.func_decls.append(self.alloc, decl);

            const id = try self.pushNode(.lambda_assign_decl, start);
            self.nodes.items[id].head = .{
                .lambda_assign_decl = .{
                    .decl_id = decl_id,
                    .body_head = first_stmt,
                    .assign_expr = left,
                },
            };
            return id;
        } else if (token.tag() == .left_paren) {
            decl.params = try self.parseFunctionParams();
            decl.return_type = try self.parseFunctionReturn();

            const name = self.src.items[decl.name.start..decl.name.end];
            const block = &self.block_stack.items[self.block_stack.items.len-1];
            try block.vars.put(self.alloc, name, {});

            try self.pushBlock();
            const firstChild = try self.parseSingleOrIndentedBodyStmts();
            self.popBlock();
            
            const decl_id = @intCast(u32, self.func_decls.items.len);
            try self.func_decls.append(self.alloc, decl);

            const id = try self.pushNode(.func_decl, start);
            self.nodes.items[id].head = .{
                .func = .{
                    .decl_id = decl_id,
                    .body_head = firstChild,
                },
            };
            return id;
        } else {
            return self.reportParseError("Expected left paren.", &.{});
        }
    }

    fn parseElseStmt(self: *Parser) anyerror!NodeId {
        const save = self.next_pos;
        const indent = try self.consumeIndentBeforeStmt();
        if (indent != self.cur_indent) {
            self.next_pos = save;
            return NullId;
        }

        var token = self.peekToken();
        if (token.tag() == .else_k) {
            const else_clause = try self.pushNode(.else_clause, self.next_pos);
            self.advanceToken();

            token = self.peekToken();
            if (token.tag() == .colon) {
                // else block.
                self.advanceToken();

                try self.pushBlock();
                defer self.popBlock();
                const firstChild = try self.parseSingleOrIndentedBodyStmts();
                self.nodes.items[else_clause].head = .{
                    .else_clause = .{
                        .body_head = firstChild,
                        .cond = NullId,
                        .else_clause = NullId,
                    },
                };
                return else_clause;
            } else {
                // else if block.
                const cond = (try self.parseExpr(.{})) orelse {
                    return self.reportParseError("Expected else if condition.", &.{});
                };
                token = self.peekToken();
                if (token.tag() == .colon) {
                    self.advanceToken();

                    try self.pushBlock();
                    const firstChild = try self.parseSingleOrIndentedBodyStmts();
                    self.popBlock();
                    self.nodes.items[else_clause].head = .{
                        .else_clause = .{
                            .body_head = firstChild,
                            .cond = cond,
                            .else_clause = NullId,
                        },
                    };

                    const nested_else = try self.parseElseStmt();
                    if (nested_else != NullId) {
                        self.nodes.items[else_clause].head.else_clause.else_clause = nested_else;
                        return else_clause;
                    } else {
                        return else_clause;
                    }
                } else {
                    return self.reportParseError("Expected colon after else if condition.", &.{});
                }
            }
        } else {
            self.next_pos = save;
            return NullId;
        }
    }

    fn parseMatchStatement(self: *Parser) !NodeId {
        const start = self.next_pos;
        // Assumes first token is the `match` keyword.
        self.advanceToken();

        const expr = (try self.parseExpr(.{})) orelse {
            return self.reportParseError("Expected match expression.", &.{});
        };
        var token = self.peekToken();
        if (token.tag() != .colon) {
            return self.reportParseError("Expected colon after if condition.", &.{});
        }
        self.advanceToken();

        const reqIndent = try self.parseFirstChildIndent(self.cur_indent);

        // Like `parseBodyStatements` but only parses case blocks.
        {
            const prevIndent = self.cur_indent;
            self.cur_indent = reqIndent;
            defer self.cur_indent = prevIndent;

            var firstCase = try self.parseCaseBlock();
            var lastCase = firstCase;

            // Parse body statements until indentation goes back to at least the previous indent.
            while (true) {
                const save = self.next_pos;
                const indent = try self.consumeIndentBeforeStmt();
                if (indent == reqIndent) {
                    const case = try self.parseCaseBlock();
                    self.nodes.items[lastCase].next = case;
                    lastCase = case;
                } else if (indent <= prevIndent) {
                    self.next_pos = save;
                    break;
                } else {
                    return self.reportParseError("Unexpected indentation.", &.{});
                }
            }

            const match = try self.pushNode(.matchBlock, start);
            self.nodes.items[match].head = .{
                .matchBlock = .{
                    .expr = expr,
                    .firstCase = firstCase,
                },
            };
            return match;
        }
    }

    fn parseIfStatement(self: *Parser) !NodeId {
        const start = self.next_pos;
        // Assumes first token is the `if` keyword.
        self.advanceToken();

        const if_cond = (try self.parseExpr(.{})) orelse {
            return self.reportParseError("Expected if condition.", &.{});
        };

        var token = self.peekToken();
        if (token.tag() == .then_k) {
            const if_expr = try self.parseIfThenExpr(if_cond, start);
            const expr_stmt = try self.pushNode(.expr_stmt, start);
            self.nodes.items[expr_stmt].head = .{
                .child_head = if_expr,
            };
            return expr_stmt;
        } else if (token.tag() != .colon) {
            return self.reportParseError("Expected colon after if condition.", &.{});
        }
        self.advanceToken();

        const if_stmt = try self.pushNode(.if_stmt, start);

        try self.pushBlock();
        var firstChild = try self.parseSingleOrIndentedBodyStmts();
        self.popBlock();
        self.nodes.items[if_stmt].head = .{
            .left_right = .{
                .left = if_cond,
                .right = firstChild,
            },
        };

        const else_clause = try self.parseElseStmt();
        if (else_clause != NullId) {
            self.nodes.items[if_stmt].head.left_right.extra = else_clause;
            return if_stmt;
        } else {
            return if_stmt;
        }
    }

    fn parseImportStmt(self: *Parser) !NodeId {
        const start = self.next_pos;
        // Assumes first token is the `import` keyword.
        self.advanceToken();

        var token = self.peekToken();
        if (token.token_t == .ident) {
            const ident = try self.pushIdentNode(self.next_pos);
            self.advanceToken();

            const fromId = (try self.parseExpr(.{})) orelse {
                return self.reportParseError("Expected from identifier.", &.{});
            };
            const from = self.nodes.items[fromId];
            if (from.node_t == .string) {
                try self.consumeNewLineOrEnd();
                const import = try self.pushNode(.importStmt, start);
                self.nodes.items[import].head = .{
                    .left_right = .{
                        .left = ident,
                        .right = fromId,
                    },
                };
                return import;
            } else {
                return self.reportParseError("Expected from identifier to be a string. {}", &.{fmt.v(from.node_t)});
            }
        } else {
            return self.reportParseError("Expected import clause.", &.{});
        }
    }

    fn parseWhileStatement(self: *Parser) !NodeId {
        const start = self.next_pos;
        // Assumes first token is the `while` keyword.
        self.advanceToken();

        var token = self.peekToken();
        if (token.tag() == .colon) {
            self.advanceToken();

            // Infinite loop.
            try self.pushBlock();
            const firstChild = try self.parseSingleOrIndentedBodyStmts();
            self.popBlock();

            const whileStmt = try self.pushNode(.whileInfStmt, start);
            self.nodes.items[whileStmt].head = .{
                .child_head = firstChild,
            };
            return whileStmt;
        } else {
            // Parse next token as expression.
            const expr_id = (try self.parseExpr(.{})) orelse {
                return self.reportParseError("Expected condition expression.", &.{});
            };

            token = self.peekToken();
            if (token.tag() == .colon) {
                self.advanceToken();
                try self.pushBlock();
                const firstChild = try self.parseSingleOrIndentedBodyStmts();
                self.popBlock();

                const whileStmt = try self.pushNode(.whileCondStmt, start);
                self.nodes.items[whileStmt].head = .{
                    .whileCondStmt = .{
                        .cond = expr_id,
                        .bodyHead = firstChild,
                    },
                };
                return whileStmt;
            } else {
                return self.reportParseError("Expected :.", &.{});
            }
        }
    }

    fn parseForStatement(self: *Parser) !NodeId {
        const start = self.next_pos;
        // Assumes first token is the `for` keyword.
        self.advanceToken();

        var token = self.peekToken();
        // Parse next token as expression.
        const expr_pos = self.next_pos;
        const expr_id = (try self.parseExpr(.{})) orelse {
            return self.reportParseError("Expected condition expression.", &.{});
        };

        token = self.peekToken();
        if (token.tag() == .colon) {
            self.advanceToken();
            try self.pushBlock();
            const firstChild = try self.parseSingleOrIndentedBodyStmts();
            self.popBlock();

            const forStmt = try self.pushNode(.forOptStmt, start);
            self.nodes.items[forStmt].head = .{
                .forOptStmt = .{
                    .opt = expr_id,
                    .bodyHead = firstChild,
                    .as = NullId,
                },
            };
            return forStmt;
        } else if (token.tag() == .as_k) {
            self.advanceToken();
            token = self.peekToken();
            const ident = (try self.parseExpr(.{})) orelse {
                return self.reportParseError("Expected ident.", &.{});
            };
            if (self.nodes.items[ident].node_t == .ident) {
                token = self.peekToken();
                if (token.tag() == .colon) {
                    self.advanceToken();
                    try self.pushBlock();
                    const firstChild = try self.parseSingleOrIndentedBodyStmts();
                    self.popBlock();

                    const forStmt = try self.pushNode(.forOptStmt, start);
                    self.nodes.items[forStmt].head = .{
                        .forOptStmt = .{
                            .opt = expr_id,
                            .bodyHead = firstChild,
                            .as = ident,
                        },
                    };
                    return forStmt;
                } else {
                    return self.reportParseError("Expected :.", &.{});
                }
            } else {
                return self.reportParseError("Expected ident.", &.{});
            }
        } else if (token.tag() == .dot_dot) {
            self.advanceToken();
            const right_range_expr = (try self.parseExpr(.{})) orelse {
                return self.reportParseError("Expected right range expression.", &.{});
            };
            const range_clause = try self.pushNode(.range_clause, expr_pos);
            self.nodes.items[range_clause].head = .{
                .left_right = .{
                    .left = expr_id,
                    .right = right_range_expr,
                },
            };

            token = self.peekToken();
            if (token.tag() == .colon) {
                self.advanceToken();

                try self.pushBlock();
                const firstChild = try self.parseSingleOrIndentedBodyStmts();
                self.popBlock();

                const for_stmt = try self.pushNode(.for_range_stmt, start);
                self.nodes.items[for_stmt].head = .{
                    .for_range_stmt = .{
                        .range_clause = range_clause,
                        .body_head = firstChild,
                        .eachClause = NullId,
                    },
                };
                return for_stmt;
            } else if (token.tag() == .each_k) {
                self.advanceToken();

                token = self.peekToken();
                const ident = (try self.parseExpr(.{})) orelse {
                    return self.reportParseError("Expected ident.", &.{});
                };
                if (self.nodes.items[ident].node_t == .ident) {
                    token = self.peekToken();
                    if (token.tag() == .colon) {
                        self.advanceToken();

                        try self.pushBlock();
                        const firstChild = try self.parseSingleOrIndentedBodyStmts();
                        self.popBlock();

                        const eachClause = try self.pushNode(.eachClause, start);
                        self.nodes.items[eachClause].head = .{
                            .eachClause = .{
                                .value = ident,
                                .key = NullId,
                            }
                        };

                        const for_stmt = try self.pushNode(.for_range_stmt, start);
                        self.nodes.items[for_stmt].head = .{
                            .for_range_stmt = .{
                                .range_clause = range_clause,
                                .body_head = firstChild,
                                .eachClause = eachClause,
                            },
                        };
                        return for_stmt;
                    } else {
                        return self.reportParseError("Expected :.", &.{});
                    }
                } else {
                    return self.reportParseErrorAt("Expected ident.", &.{}, token.pos());
                }
            } else {
                return self.reportParseError("Expected :.", &.{});
            }
        } else if (token.tag() == .each_k) {
            self.advanceToken();
            token = self.peekToken();
            const ident = (try self.parseExpr(.{})) orelse {
                return self.reportParseError("Expected ident.", &.{});
            };
            if (self.nodes.items[ident].node_t == .ident) {
                token = self.peekToken();
                if (token.tag() == .colon) {
                    self.advanceToken();
                    try self.pushBlock();
                    const firstChild = try self.parseSingleOrIndentedBodyStmts();
                    self.popBlock();

                    const each = try self.pushNode(.eachClause, start);
                    self.nodes.items[each].head = .{
                        .eachClause = .{
                            .value = ident,
                            .key = NullId,
                        }
                    };

                    const for_stmt = try self.pushNode(.for_iter_stmt, start);
                    self.nodes.items[for_stmt].head = .{
                        .for_iter_stmt = .{
                            .iterable = expr_id,
                            .body_head = firstChild,
                            .eachClause = each,
                        },
                    };
                    return for_stmt;
                } else if (token.tag() == .comma) {
                    self.advanceToken();
                    const secondIdent = (try self.parseExpr(.{})) orelse {
                        return self.reportParseError("Expected ident.", &.{});
                    };
                    if (self.nodes.items[secondIdent].node_t == .ident) {
                        token = self.peekToken();
                        if (token.tag() == .colon) {
                            self.advanceToken();
                            try self.pushBlock();
                            const firstChild = try self.parseSingleOrIndentedBodyStmts();
                            self.popBlock();

                            const each = try self.pushNode(.eachClause, start);
                            self.nodes.items[each].head = .{
                                .eachClause = .{
                                    .value = secondIdent,
                                    .key = ident,
                                }
                            };

                            const for_stmt = try self.pushNode(.for_iter_stmt, start);
                            self.nodes.items[for_stmt].head = .{
                                .for_iter_stmt = .{
                                    .iterable = expr_id,
                                    .body_head = firstChild,
                                    .eachClause = each,
                                },
                            };
                            return for_stmt;
                        } else {
                            return self.reportParseError("Expected :.", &.{});
                        }
                    } else {
                        return self.reportParseError("Expected ident.", &.{});
                    }
                } else {
                    return self.reportParseError("Expected :.", &.{});
                }
            } else {
                return self.reportParseErrorAt("Expected ident.", &.{}, token.pos());
            }
        } else {
            return self.reportParseError("Expected :.", &.{});
        }
    }

    fn parseBlock(self: *Parser) !NodeId {
        const start = self.next_pos;
        // Assumes first token is the ident.
        const name = try self.pushIdentNode(start);
        self.advanceToken();
        // Assumes second token is colon.
        self.advanceToken();

        // Parse body.
        try self.pushBlock();
        const first_stmt = try self.parseIndentedBodyStatements();
        self.popBlock();
        
        const id = try self.pushNode(.label_decl, start);
        self.nodes.items[id].head = .{
            .left_right = .{
                .left = name,
                .right = first_stmt,
            },
        };
        return id;
    }

    fn parseCaseBlock(self: *Parser) !NodeId {
        const start = self.next_pos;
        var token = self.peekToken();
        var firstCond: NodeId = undefined;
        if (token.token_t == .else_k) {
            self.advanceToken();
            firstCond = try self.pushNode(.elseCase, start);
        } else {
            firstCond = (try self.parseExpr(.{})) orelse {
                return self.reportParseError("Expected case condition.", &.{});
            };
        }
        var lastCond = firstCond;
        while (true) {
            token = self.peekToken();
            if (token.tag() == .colon) {
                self.advanceToken();
                break;
            } else if (token.tag() == .comma) {
                self.advanceToken();
                var cond: NodeId = undefined;
                if (token.token_t == .else_k) {
                    self.advanceToken();
                    cond = try self.pushNode(.elseCase, start);
                } else {
                    cond = (try self.parseExpr(.{})) orelse {
                        return self.reportParseError("Expected case condition.", &.{});
                    };
                }
                self.nodes.items[lastCond].next = cond;
                lastCond = cond;
            } else {
                return self.reportParseError("Expected comma or colon.", &.{});
            }
        }

        // Parse body.
        const firstChild = try self.parseSingleOrIndentedBodyStmts();

        const case = try self.pushNode(.caseBlock, start);
        self.nodes.items[case].head = .{
            .caseBlock = .{
                .firstCond = firstCond,
                .firstChild = firstChild,
            },
        };
        return case;
    }

    fn parseStatement(self: *Parser) anyerror!?NodeId {
        var token = self.peekToken();
        if (token.tag() == .none) {
            return null;
        }
        switch (token.tag()) {
            .new_line => {
                // Skip newlines.
                while (true) {
                    self.advanceToken();
                    token = self.peekToken();
                    if (token.tag() == .none) {
                        return null;
                    }
                    if (token.tag() != .new_line) {
                        break;
                    }
                }
                return try self.parseStatement();
            },
            .ident => {
                const token2 = self.peekTokenAhead(1);
                if (token2.tag() == .colon) {
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
                if (token.tag() == .ident) {
                    const name_token = self.tokens.items[self.next_pos];
                    const name = self.src.items[name_token.pos() .. name_token.data.end_pos];
                    var skip_compile = false;

                    const ident = try self.pushIdentNode(self.next_pos);
                    self.advanceToken();
                    const at_ident = try self.pushNode(.at_ident, start);
                    self.nodes.items[at_ident].head = .{
                        .annotation = .{
                            .child = ident,
                        },
                    };

                    const child: NodeId = b: {
                        // Parse as call expr.
                        const callStart = self.next_pos;
                        const call_id = try self.parseAnyCallExpr(at_ident);
                        const call_expr = self.nodes.items[call_id];
                        if (call_expr.head.func_call.arg_head == NullId) {
                            return self.reportParseErrorAt("Expected arg for @name.", &.{}, callStart);
                        }
                        const arg = self.nodes.items[call_expr.head.func_call.arg_head];
                        if (std.mem.eql(u8, "name", name)) {
                            if (arg.node_t == .ident) {
                                const arg_token = self.tokens.items[arg.start_token];
                                self.name = self.src.items[arg_token.pos() .. arg_token.data.end_pos];
                                skip_compile = true;
                            } else {
                                return self.reportParseErrorAt("Expected ident arg for @name.", &.{}, callStart);
                            }
                        } else if (std.mem.eql(u8, "compileError", name)) {
                            skip_compile = true;
                        }
                        break :b call_id;
                    };

                    const id = try self.pushNode(.at_stmt, start);
                    self.nodes.items[id].head = .{
                        .at_stmt = .{
                            .child = child,
                            .skip_compile = skip_compile,
                        },
                    };

                    token = self.peekToken();
                    if (token.tag() == .new_line) {
                        self.advanceToken();
                        return id;
                    } else if (token.tag() == .none) {
                        return id;
                    } else return self.reportParseError("Expected end of line or file", &.{});
                }

                // Reparse as expression.
                self.next_pos = start;
                if (try self.parseExprOrAssignStatement()) |id| {
                    return id;
                }
            },
            .object_k => {
                return try self.parseStructDecl();
            },
            .tagtype_k => {
                return try self.parseTagDecl();
            },
            .func_k => {
                return try self.parseFunctionDecl();
            },
            .if_k => {
                return try self.parseIfStatement();
            },
            .match_k => {
                return try self.parseMatchStatement();
            },
            .for_k => {
                return try self.parseForStatement();
            },
            .while_k => {
                return try self.parseWhileStatement();
            },
            .import_k => {
                return try self.parseImportStmt();
            },
            .pass_k => {
                const id = try self.pushNode(.pass_stmt, self.next_pos);
                self.advanceToken();
                token = self.peekToken();
                try self.consumeNewLineOrEnd();
                return id;
            },
            .continue_k => {
                const id = try self.pushNode(.continueStmt, self.next_pos);
                self.advanceToken();
                try self.consumeNewLineOrEnd();
                return id;
            },
            .break_k => {
                const id = try self.pushNode(.breakStmt, self.next_pos);
                self.advanceToken();
                try self.consumeNewLineOrEnd();
                return id;
            },
            .return_k => {
                const id = try self.parseReturnStatement();
                token = self.peekToken();
                try self.consumeNewLineOrEnd();
                return id;
            },
            .var_k => {
                const start = self.next_pos;
                self.advanceToken();

                // Static var name.
                token = self.peekToken();
                var name: NodeId = undefined;
                if (token.tag() == .ident) {
                    name = try self.pushIdentNode(self.next_pos);
                    self.advanceToken();
                } else return self.reportParseError("Expected local name identifier.", &.{});

                token = self.peekToken();
                if (token.tag() != .equal) {
                    return self.reportParseError("Expected `=` after local variable name.", &.{});
                }
                self.advanceToken();

                var right: NodeId = undefined;
                if (self.peekToken().tag() == .func_k) {
                    // Multi-line lambda.
                    right = try self.parseMultilineLambdaFunction();
                } else {
                    right = (try self.parseExpr(.{})) orelse {
                        return self.reportParseError("Expected right expression for assignment statement.", &.{});
                    };
                }
                const decl = try self.pushNode(.varDecl, start);
                self.nodes.items[decl].head = .{
                    .varDecl = .{
                        .left = name,
                        .right = right,
                    },
                };
                try self.varDecls.append(self.alloc, decl);
                return decl;
            },
            .capture_k => {
                const start = self.next_pos;
                self.advanceToken();

                // Local name.
                token = self.peekToken();
                var name: NodeId = undefined;
                if (token.tag() == .ident) {
                    name = try self.pushIdentNode(self.next_pos);
                    self.advanceToken();
                } else return self.reportParseError("Expected local variable identifier.", &.{});

                token = self.peekToken();
                if (token.tag() != .equal) {
                    try self.consumeNewLineOrEnd();
                    const decl = try self.pushNode(.captureDecl, start);
                    self.nodes.items[decl].head = .{
                        .left_right = .{
                            .left = name,
                            .right = NullId,
                        },
                    };
                    return decl;
                }
                self.advanceToken();

                var right: NodeId = undefined;
                if (self.peekToken().tag() == .func_k) {
                    // Multi-line lambda.
                    right = try self.parseMultilineLambdaFunction();
                } else {
                    right = (try self.parseExpr(.{})) orelse {
                        return self.reportParseError("Expected right expression for assignment statement.", &.{});
                    };
                }
                try self.consumeNewLineOrEnd();
                const decl = try self.pushNode(.captureDecl, start);
                self.nodes.items[decl].head = .{
                    .left_right = .{
                        .left = name,
                        .right = right,
                    },
                };
                return decl;
            },
            .static_k => {
                const start = self.next_pos;
                self.advanceToken();

                // Local name.
                token = self.peekToken();
                var name: NodeId = undefined;
                if (token.tag() == .ident) {
                    name = try self.pushIdentNode(self.next_pos);
                    self.advanceToken();
                } else return self.reportParseError("Expected variable name identifier.", &.{});

                token = self.peekToken();
                if (token.tag() != .equal) {
                    try self.consumeNewLineOrEnd();
                    const decl = try self.pushNode(.captureDecl, start);
                    self.nodes.items[decl].head = .{
                        .left_right = .{
                            .left = name,
                            .right = NullId,
                        },
                    };
                    return decl;
                }
                self.advanceToken();

                var right: NodeId = undefined;
                if (self.peekToken().tag() == .func_k) {
                    // Multi-line lambda.
                    right = try self.parseMultilineLambdaFunction();
                } else {
                    right = (try self.parseExpr(.{})) orelse {
                        return self.reportParseError("Expected right expression for assignment statement.", &.{});
                    };
                }
                try self.consumeNewLineOrEnd();
                const decl = try self.pushNode(.staticDecl, start);
                self.nodes.items[decl].head = .{
                    .left_right = .{
                        .left = name,
                        .right = right,
                    },
                };
                return decl;
            },
            else => {
                if (try self.parseExprOrAssignStatement()) |id| {
                    return id;
                }
            },
        }
        self.last_err = try fmt.allocFormat(self.alloc, "unknown token: {} at {}", &.{fmt.v(token.tag()), fmt.v(token.pos())});
        return error.UnknownToken;
    }

    fn reportTokenError(self: *Parser, format: []const u8, args: []const fmt.FmtValue) error{TokenError} {
        return self.reportTokenErrorAt(format, args, self.next_pos);
    }

    fn reportTokenErrorAt(self: *Parser, format: []const u8, args: []const fmt.FmtValue, pos: u32) error{TokenError} {
        self.alloc.free(self.last_err);
        self.last_err = fmt.allocFormat(self.alloc, format, args) catch fatal();
        self.last_err_pos = pos;
        return error.TokenError;
    }

    fn reportParseError(self: *Parser, format: []const u8, args: []const fmt.FmtValue) error{ParseError, FormatError, OutOfMemory} {
        return self.reportParseErrorAt(format, args, self.next_pos);
    }

    fn reportParseErrorAt(self: *Parser, format: []const u8, args: []const fmt.FmtValue, tokenPos: u32) error{ParseError, FormatError, OutOfMemory} {
        self.alloc.free(self.last_err);
        self.last_err = try fmt.allocFormat(self.alloc, format, args);
        if (tokenPos >= self.tokens.items.len) {
            self.last_err_pos = @intCast(u32, self.src.items.len);
        } else {
            self.last_err_pos = self.tokens.items[tokenPos].pos();
        }
        return error.ParseError;
    }

    fn parseMapEntry(self: *Parser, key_node_t: NodeType) !NodeId {
        const start = self.next_pos;
        self.advanceToken();
        var token = self.peekToken();
        if (token.tag() != .colon) {
            return self.reportParseError("Expected colon.", &.{});
        }
        self.advanceToken();
        const val_id = (try self.parseExpr(.{})) orelse {
            return self.reportParseError("Expected map value.", &.{});
        };
        const key_id = try self.pushNode(key_node_t, start);
        const entry_id = try self.pushNode(.mapEntry, start);
        self.nodes.items[entry_id].head = .{
            .mapEntry = .{
                .left = key_id,
                .right = val_id,
            }
        };
        return entry_id;
    }

    fn consumeNewLineOrEnd(self: *Parser) !void {
        var tag = self.peekToken().tag();
        if (tag == .new_line) {
            self.advanceToken();
            return;
        }
        if (tag == .none) {
            return;
        }
        return self.reportParseError("Expected new line or end.", &.{});
    }

    fn consumeWhitespaceTokens(self: *Parser) void {
        var token = self.peekToken();
        while (token.tag() != .none) {
            switch (token.tag()) {
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

            if (token.tag() == .right_bracket) {
                // Empty array.
                break :outer;
            } else {
                first_entry = (try self.parseExpr(.{})) orelse {
                    return self.reportParseError("Expected array item.", &.{});
                };
                last_entry = first_entry;
            }

            while (true) {
                self.consumeWhitespaceTokens();
                token = self.peekToken();
                if (token.tag() == .comma) {
                    self.advanceToken();
                } else if (token.tag() == .right_bracket) {
                    break :outer;
                }

                token = self.peekToken();
                if (token.tag() == .right_bracket) {
                    break :outer;
                } else {
                    const expr_id = (try self.parseExpr(.{})) orelse {
                        return self.reportParseError("Expected array item.", &.{});
                    };
                    self.nodes.items[last_entry].next = expr_id;
                    last_entry = expr_id;
                }
            }
        }

        const arr_id = try self.pushNode(.arr_literal, start);
        self.nodes.items[arr_id].head = .{
            .child_head = first_entry,
        };

        // Parse closing bracket.
        const token = self.peekToken();
        if (token.tag() == .right_bracket) {
            self.advanceToken();
            return arr_id;
        } else return self.reportParseError("Expected closing bracket.", &.{});
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
            switch (token.tag()) {
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
                else => return self.reportParseError("Expected map key.", &.{}),
            }

            while (true) {
                self.consumeWhitespaceTokens();
                token = self.peekToken();
                if (token.tag() == .comma) {
                    self.advanceToken();
                } else if (token.tag() == .right_brace) {
                    break :outer;
                }

                token = self.peekToken();
                switch (token.tag()) {
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
                    .new_line => {
                        continue;
                    },
                    else => return self.reportParseError("Expected map key.", &.{}),
                }
            }
        }

        const map_id = try self.pushNode(.map_literal, start);
        self.nodes.items[map_id].head = .{
            .child_head = first_entry,
        };

        // Parse closing brace.
        const token = self.peekToken();
        if (token.tag() == .right_brace) {
            self.advanceToken();
            return map_id;
        } else return self.reportParseError("Expected closing brace.", &.{});
    }

    fn parseCallArg(self: *Parser) !?NodeId {
        self.consumeWhitespaceTokens();
        const start = self.next_pos;
        const token = self.peekToken();
        if (token.tag() == .ident) {
            if (self.peekTokenAhead(1).tag() == .colon) {
                // Named arg.
                const name = try self.pushIdentNode(start);
                _ = self.consumeToken();
                _ = self.consumeToken();
                var arg = (try self.parseExpr(.{})) orelse {
                    return self.reportParseError("Expected arg expression.", &.{});
                };
                const named_arg = try self.pushNode(.named_arg, start);
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
        if (token.tag() == .left_paren) {
            return try self.parseCallExpression(callee);
        } else {
            return try self.parseNoParenCallExpression(callee);
        }
    }

    fn parseCallExpression(self: *Parser, left_id: NodeId) !NodeId {
        // Assume first token is left paren.
        self.advanceToken();

        const expr_start = self.nodes.items[left_id].start_token;
        const expr_id = try self.pushNode(.call_expr, expr_start);

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
                if (token.tag() != .comma and token.tag() != .new_line) {
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
        if (token.tag() == .right_paren) {
            self.advanceToken();
            self.nodes.items[expr_id].head = .{
                .func_call = .{
                    .callee = left_id,
                    .arg_head = first,
                    .has_named_arg = has_named_arg,
                },
            };
            return expr_id;
        } else return self.reportParseError("Expected closing parenthesis.", &.{});
    }

    /// Assumes first arg exists.
    fn parseNoParenCallExpression(self: *Parser, left_id: NodeId) !NodeId {
        const expr_start = self.nodes.items[left_id].start_token;
        const expr_id = try self.pushNode(.call_expr, expr_start);

        const firstArg = try self.parseTightTermExpr();
        var last_arg_id = firstArg;
        self.nodes.items[expr_id].head = .{
            .func_call = .{
                .callee = left_id,
                .arg_head = firstArg,
                .has_named_arg = false,
            },
        };

        while (true) {
            const token = self.peekToken();
            switch (token.tag()) {
                .new_line => break,
                .none => break,
                else => {
                    const arg = try self.parseTightTermExpr();
                    self.nodes.items[last_arg_id].next = arg;
                    last_arg_id = arg;
                },
            }
        }
        return expr_id;
    }

    /// Parses the right expression of a BinaryExpression.
    fn parseRightExpression(self: *Parser, left_op: BinaryExprOp) anyerror!NodeId {
        var start = self.next_pos;
        const expr_id = try self.parseTermExpr();

        // Check if next token is an operator with higher precedence.
        var token = self.peekToken();
        if (token.tag() == .operator) {
            const op_prec = getBinOpPrecedence(left_op);
            const right_op = toBinExprOp(token.data.operator_t);
            const right_op_prec = getBinOpPrecedence(right_op);
            if (right_op_prec > op_prec) {
                // Continue parsing right.
                _ = self.consumeToken();
                start = self.next_pos;
                const right_id = try self.parseRightExpression(right_op);

                const binExpr = try self.pushNode(.binExpr, start);
                self.nodes.items[binExpr].head = .{
                    .binExpr = .{
                        .left = expr_id,
                        .right = right_id,
                        .op = right_op,
                    },
                };

                // Before returning, perform left recursion with the right if the op prec is the same.
                // eg. a + b * c * d
                // The result should be a + ((b * c) * d).
                var left = binExpr;
                while (true) {
                    token = self.peekToken();
                    if (token.tag() == .operator) {
                        const right2_op = toBinExprOp(token.data.operator_t);
                        const right2_op_prec = getBinOpPrecedence(right2_op);
                        if (right2_op_prec == right_op_prec) {
                            self.advanceToken();
                            const rightExpr = try self.parseRightExpression(right_op);
                            const newBinExpr = try self.pushNode(.binExpr, start);
                            self.nodes.items[newBinExpr].head = .{
                                .binExpr = .{
                                    .left = left,
                                    .right = rightExpr,
                                    .op = right2_op,
                                },
                            };
                            left = newBinExpr;
                            continue;
                        }
                    }
                    break;
                }
                return left;
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

        const if_expr = try self.pushNode(.if_expr, start);

        const if_body = (try self.parseExpr(.{})) orelse {
            return self.reportParseError("Expected if body.", &.{});
        };
        self.nodes.items[if_expr].head = .{
            .if_expr = .{
                .cond = if_cond,
                .body_expr = if_body,
                .else_clause = NullId,
            },
        };

        const token = self.peekToken();
        if (token.tag() == .else_k) {
            const else_clause = try self.pushNode(.else_clause, self.next_pos);
            self.advanceToken();

            const else_body = (try self.parseExpr(.{})) orelse {
                return self.reportParseError("Expected else body.", &.{});
            };
            self.nodes.items[else_clause].head = .{
                .child_head = else_body,
            };

            self.nodes.items[if_expr].head.if_expr.else_clause = else_clause;
        }
        return if_expr;
    }

    /// A string template begins and ends with .templateString token.
    /// Inside the template, two template expressions can be adjacent to each other.
    fn parseStringTemplate(self: *Parser) !NodeId {
        const start = self.next_pos;

        const id = try self.pushNode(.stringTemplate, start);

        // First determine the first token type.
        var first: NodeId = undefined;
        var token = self.peekToken();
        if (token.tag() == .templateString) {
            first = try self.pushNode(.string, start);
        } else return self.reportParseError("Expected template string or expression.", &.{});

        self.nodes.items[id].head = .{
            .stringTemplate = .{
                .partsHead = first,
            },
        };
        var lastWasStringPart = true;
        var last = first;

        self.advanceToken();
        token = self.peekToken();

        while (true) {
            const tag = token.tag();
            if (tag == .templateString) {
                if (lastWasStringPart) {
                    // End of this template.
                    break;
                }
                const str = try self.pushNode(.string, self.next_pos);
                self.nodes.items[last].next = str;
                last = str;
                lastWasStringPart = true;
            } else if (tag == .templateExprStart) {
                self.advanceToken();
                const expr = (try self.parseExpr(.{})) orelse {
                    return self.reportParseError("Expected expression.", &.{});
                };
                token = self.peekToken();
                if (token.tag() != .right_brace) {
                    return self.reportParseError("Expected right brace.", &.{});
                }
                self.nodes.items[last].next = expr;
                last = expr;
                lastWasStringPart = false;
            } else {
                break;
            }
            self.advanceToken();
            token = self.peekToken();
        }
        return id;
    }

    /// An expression term doesn't contain a binary expression at the top.
    fn parseTermExpr(self: *Parser) anyerror!NodeId {
        const start = self.next_pos;
        const token = self.peekToken();
        switch (token.tag()) {
            .await_k => {
                // Await expression.
                const expr_id = try self.pushNode(.await_expr, start);
                self.advanceToken();
                const term_id = try self.parseTermExpr();
                self.nodes.items[expr_id].head = .{
                    .child_head = term_id,
                };
                return expr_id;
            },
            .not_k => {
                self.advanceToken();
                const expr = try self.pushNode(.unary_expr, start);
                const child = try self.parseTermExpr();
                self.nodes.items[expr].head = .{
                    .unary = .{
                        .child = child,
                        .op = .not,
                    },
                };
                return expr;
            },
            .compt_k => {
                self.advanceToken();
                const expr = try self.pushNode(.comptExpr, start);
                const child = try self.parseTermExpr();
                self.nodes.items[expr].head = .{
                    .child_head = child,
                };
                return expr;
            },
            .try_k => {
                self.advanceToken();
                const expr = try self.pushNode(.tryExpr, start);
                const child = try self.parseTermExpr();
                self.nodes.items[expr].head = .{
                    .child_head = child,
                };
                return expr;
            },
            .coresume_k => {
                self.advanceToken();
                const coresume = try self.pushNode(.coresume, start);
                const fiberExpr = try self.parseTermExpr();
                self.nodes.items[coresume].head = .{
                    .child_head = fiberExpr,
                };
                return coresume;
            },
            .coyield_k => {
                self.advanceToken();
                const coyield = try self.pushNode(.coyield, start);
                return coyield;
            },
            .coinit_k => {
                self.advanceToken();
                const callExprId = try self.parseExpr(.{}) orelse {
                    return self.reportParseError("Expected call expression.", &.{});
                };
                const callExpr = self.nodes.items[callExprId];
                if (callExpr.node_t != .call_expr) {
                    return self.reportParseError("Expected call expression.", &.{});
                }
                const coinit = try self.pushNode(.coinit, start);
                self.nodes.items[coinit].head = .{
                    .child_head = callExprId,
                };
                return coinit;
            },
            else => {
                return self.parseTightTermExpr();
            },
        }
    }

    /// A tight term expr also doesn't include various top expressions
    /// that are separated by whitespace. eg. coinit <expr>
    fn parseTightTermExpr(self: *Parser) anyerror!NodeId {
        var start = self.next_pos;
        var token = self.peekToken();
        if (token.tag() == .new_line) {
            self.consumeWhitespaceTokens();
            start = self.next_pos;
            token = self.peekToken();
        }

        var left_id = switch (token.tag()) {
            .ident => b: {
                self.advanceToken();
                const id = try self.pushIdentNode(start);

                const name_token = self.tokens.items[start];
                const name = self.src.items[name_token.pos()..name_token.data.end_pos];
                if (!self.isVarDeclaredFromScope(name)) {
                    try self.deps.put(self.alloc, name, id);
                }

                break :b id;
            },
            .true_k => {
                self.advanceToken();
                return try self.pushNode(.true_literal, start);
            },
            .false_k => {
                self.advanceToken();
                return try self.pushNode(.false_literal, start);
            },
            .none_k => {
                self.advanceToken();
                return try self.pushNode(.none, start);
            },
            .number => b: {
                self.advanceToken();
                break :b try self.pushNode(.number, start);
            },
            .nonDecInt => b: {
                self.advanceToken();
                break :b try self.pushNode(.nonDecInt, start);
            },
            .tag => {
                self.advanceToken();
                return try self.pushNode(.tagLiteral, start);
            },
            .string => b: {
                self.advanceToken();
                break :b try self.pushNode(.string, start);
            },
            .templateString => b: {
                break :b try self.parseStringTemplate();
            },
            .at => b: {
                self.advanceToken();
                token = self.peekToken();
                if (token.tag() == .ident) {
                    const ident = try self.pushIdentNode(self.next_pos);
                    self.advanceToken();
                    const at_ident = try self.pushNode(.at_ident, start);
                    self.nodes.items[at_ident].head = .{
                        .annotation = .{
                            .child = ident,
                        },
                    };
                    break :b at_ident;
                } else {
                    return self.reportParseError("Expected identifier.", &.{});
                }
            },
            .if_k => {
                self.advanceToken();
                const if_cond = (try self.parseExpr(.{})) orelse {
                    return self.reportParseError("Expected if condition.", &.{});
                };

                token = self.peekToken();
                if (token.tag() == .then_k) {
                    return try self.parseIfThenExpr(if_cond, start);
                } else {
                    return self.reportParseError("Expected then keyword.", &.{});
                }
            },
            .left_paren => b: {
                _ = self.consumeToken();
                token = self.peekToken();

                const expr_id = (try self.parseExpr(.{})) orelse {
                    token = self.peekToken();
                    if (token.tag() == .right_paren) {
                        _ = self.consumeToken();
                    } else {
                        return self.reportParseError("Expected expression.", &.{});
                    }
                    // Assume empty args for lambda.
                    token = self.peekToken();
                    if (token.tag() == .equal_greater) {
                        return self.parseNoParamLambdaFunc();
                    } else {
                        return self.reportParseError("Unexpected paren.", &.{});
                    }
                };
                token = self.peekToken();
                if (token.tag() == .right_paren) {
                    _ = self.consumeToken();

                    token = self.peekToken();
                    if (self.nodes.items[expr_id].node_t == .ident and token.tag() == .equal_greater) {
                        return self.parseLambdaFuncWithParam(expr_id);
                    }

                    const group = try self.pushNode(.group, start);
                    self.nodes.items[group].head = .{
                        .child_head = expr_id,
                    };
                    break :b group;
                } else if (token.tag() == .comma) {
                    self.next_pos = start;
                    return self.parseLambdaFunction();
                } else {
                    return self.reportParseError("Expected right parenthesis.", &.{});
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
            .operator => {
                if (token.data.operator_t == .minus) {
                    self.advanceToken();
                    const expr_id = try self.pushNode(.unary_expr, start);
                    const term_id = try self.parseTermExpr();
                    self.nodes.items[expr_id].head = .{
                        .unary = .{
                            .child = term_id,
                            .op = .minus,
                        },
                    };
                    return expr_id;
                } else if (token.data.operator_t == .tilde) {
                    self.advanceToken();
                    const expr_id = try self.pushNode(.unary_expr, start);
                    const term_id = try self.parseTermExpr();
                    self.nodes.items[expr_id].head = .{
                        .unary = .{
                            .child = term_id,
                            .op = .bitwiseNot,
                        },
                    };
                    return expr_id;
                } else if (token.data.operator_t == .bang) {
                    self.advanceToken();
                    const expr = try self.pushNode(.unary_expr, start);
                    const child = try self.parseTermExpr();
                    self.nodes.items[expr].head = .{
                        .unary = .{
                            .child = child,
                            .op = .not,
                        },
                    };
                    return expr;
                } else return self.reportParseError("Unexpected operator.", &.{});
            },
            else => return self.reportParseError("Expected term expr. Parsed {}.", &.{fmt.v(token.tag())}),
        };

        while (true) {
            const next = self.peekToken();
            switch (next.tag()) {
                .equal_greater => {
                    const left = self.nodes.items[left_id];
                    if (left.node_t == .ident) {
                        // Lambda.
                        return self.parseLambdaFuncWithParam(left_id);
                    } else {
                        return self.reportParseError("Unexpected `=>` token", &.{});
                    }
                },
                .tag => {
                    // Tag init.
                    const left = self.nodes.items[left_id];
                    if (left.node_t == .ident) {
                        const tagLiteral = try self.pushNode(.tagLiteral, self.next_pos);
                        self.advanceToken();

                        const tagInit = try self.pushNode(.tagInit, start);
                        self.nodes.items[tagInit].head = .{
                            .left_right = .{
                                .left = left_id,
                                .right = tagLiteral,
                            },
                        };
                        return tagInit;
                    } else {
                        return self.reportParseError("Expected left ident.", &.{});
                    }
                },
                .dot => {
                    // AccessExpression.
                    self.advanceToken();
                    const next2 = self.peekToken();
                    if (next2.tag() == .ident) {
                        const right_id = try self.pushIdentNode(self.next_pos);
                        const expr_id = try self.pushNode(.accessExpr, start);
                        self.nodes.items[expr_id].head = .{
                            .accessExpr = .{
                                .left = left_id,
                                .right = right_id,
                            },
                        };
                        left_id = expr_id;
                        self.advanceToken();
                        start = self.next_pos;
                    } else return self.reportParseError("Expected ident", &.{});
                },
                .left_bracket => {
                    // Index or slice operator.

                    // Consume left bracket.
                    self.advanceToken();

                    token = self.peekToken();
                    if (token.tag() == .dot_dot) {
                        // Start of list to end index slice.
                        self.advanceToken();
                        const right_range = (try self.parseExpr(.{})) orelse {
                            return self.reportParseError("Expected expression.", &.{});
                        };

                        token = self.peekToken();
                        if (token.tag() == .right_bracket) {
                            self.advanceToken();
                            const res = try self.pushNode(.arr_range_expr, start);
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
                            return self.reportParseError("Expected right bracket.", &.{});
                        }
                    } else {
                        const expr_id = (try self.parseExpr(.{})) orelse {
                            return self.reportParseError("Expected expression.", &.{});
                        };

                        token = self.peekToken();
                        if (token.tag() == .right_bracket) {
                            self.advanceToken();
                            const access_id = try self.pushNode(.arr_access_expr, start);
                            self.nodes.items[access_id].head = .{
                                .left_right = .{
                                    .left = left_id,
                                    .right = expr_id,
                                },
                            };
                            left_id = access_id;
                            start = self.next_pos;
                        } else if (token.tag() == .dot_dot) {
                            self.advanceToken();
                            token = self.peekToken();
                            if (token.tag() == .right_bracket) {
                                // Start index to end of list slice.
                                self.advanceToken();
                                const res = try self.pushNode(.arr_range_expr, start);
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
                                    return self.reportParseError("Expected expression.", &.{});
                                };
                                token = self.peekToken();
                                if (token.tag() == .right_bracket) {
                                    self.advanceToken();
                                    const res = try self.pushNode(.arr_range_expr, start);
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
                                    return self.reportParseError("Expected right bracket.", &.{});
                                }
                            }
                        } else {
                            return self.reportParseError("Expected right bracket.", &.{});
                        }
                    }
                },
                .left_paren => {
                    const call_id = try self.parseCallExpression(left_id);
                    left_id = call_id;
                },
                .left_brace => {
                    if (self.nodes.items[left_id].node_t == .ident) {
                        const props = try self.parseMapLiteral();
                        const initN = try self.pushNode(.structInit, start);
                        self.nodes.items[initN].head = .{
                            .structInit = .{
                                .name = left_id,
                                .initializer = props,
                            },
                        };
                        left_id = initN;
                    } else {
                        return self.reportParseError("Expected struct type to the left for initializer.", &.{});
                    }
                },
                .dot_dot,
                .right_bracket,
                .right_paren,
                .right_brace,
                .else_k,
                .comma,
                .colon,
                .is_k,
                .equal,
                .operator,
                .or_k,
                .and_k,
                .then_k,
                .as_k,
                .each_k,
                .string,
                .number,
                .ident,
                .templateString,
                .new_line,
                .none => break,
                else => return self.reportParseError("Unknown token", &.{}),
            }
        }
        return left_id;
    }

    fn returnLeftAssignExpr(self: *Parser, leftId: NodeId, outIsAssignStmt: *bool) !NodeId {
        switch (self.nodes.items[leftId].node_t) {
            .accessExpr,
            .arr_access_expr,
            .ident => {
                outIsAssignStmt.* = true;
                return leftId;
            },
            else => {
                return self.reportParseError("Expected variable to left of assignment operator.", &.{});
            },
        }
    }

    /// An error can be returned during the expr parsing.
    /// If null is returned instead, no token begins an expression
    /// and the caller can assume next_pos did not change. Instead of reporting
    /// a generic error message, it delegates that to the caller.
    fn parseExpr(self: *Parser, opts: ParseExprOptions) anyerror!?NodeId {
        var start = self.next_pos;
        var token = self.peekToken();

        var left_id: NodeId = undefined;
        switch (token.tag()) {
            .none => return null,
            .right_paren => return null,
            .right_bracket => return null,
            else => {
                left_id = try self.parseTermExpr();
            },
        }

        while (true) {
            const next = self.peekToken();
            switch (next.tag()) {
                .equal => {
                    // If left is an accessor expression or identifier, parse as assignment statement.
                    if (opts.returnLeftAssignExpr) {
                        return try self.returnLeftAssignExpr(left_id, opts.outIsAssignStmt);
                    } else {
                        break;
                    }
                },
                .operator => {
                    const op_t = next.data.operator_t;
                    switch (op_t) {
                        .plus,
                        .minus,
                        .star,
                        .slash => {
                            if (self.peekTokenAhead(1).token_t == .equal) {
                                if (opts.returnLeftAssignExpr) {
                                    return try self.returnLeftAssignExpr(left_id, opts.outIsAssignStmt);
                                } else {
                                    break;
                                }
                            }
                        },
                        else => {},
                    }
                    // BinaryExpression.
                    const bin_op = toBinExprOp(op_t);
                    self.advanceToken();
                    const right_id = try self.parseRightExpression(bin_op);

                    const bin_expr = try self.pushNode(.binExpr, start);
                    self.nodes.items[bin_expr].head = .{
                        .binExpr = .{
                            .left = left_id,
                            .right = right_id,
                            .op = bin_op,
                        },
                    };
                    left_id = bin_expr;
                },
                .and_k => {
                    self.advanceToken();
                    const right_id = try self.parseRightExpression(.and_op);
                    const bin_expr = try self.pushNode(.binExpr, start);
                    self.nodes.items[bin_expr].head = .{
                        .binExpr = .{
                            .left = left_id,
                            .right = right_id,
                            .op = BinaryExprOp.and_op,
                        },
                    };
                    left_id = bin_expr;
                },
                .or_k => {
                    self.advanceToken();
                    const right_id = try self.parseRightExpression(.or_op);
                    const bin_expr = try self.pushNode(.binExpr, start);
                    self.nodes.items[bin_expr].head = .{
                        .binExpr = .{
                            .left = left_id,
                            .right = right_id,
                            .op = BinaryExprOp.or_op,
                        },
                    };
                    left_id = bin_expr;
                },
                .is_k => {
                    self.advanceToken();
                    token = self.peekToken();
                    var binOp = BinaryExprOp.equal_equal;
                    if (token.tag() == .not_k) {
                        binOp = BinaryExprOp.bang_equal;
                        self.advanceToken();
                    }
                    const right_id = try self.parseRightExpression(binOp);

                    const bin_expr = try self.pushNode(.binExpr, start);
                    self.nodes.items[bin_expr].head = .{
                        .binExpr = .{
                            .left = left_id,
                            .right = right_id,
                            .op = binOp,
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
                .each_k,
                .new_line,
                .none => break,
                else => {
                    // Attempt to parse as no paren call expr.
                    const left = self.nodes.items[left_id];
                    switch (left.node_t) {
                        .accessExpr,
                        .ident => {
                            return try self.parseNoParenCallExpression(left_id);
                        },
                        else => {
                            return self.reportParseError("Unsupported shorthand caller {}", &.{v(left.node_t)});
                        }
                    }
                }
            }
        }
        return left_id;
    }

    /// Assumes next token is the return token.
    fn parseReturnStatement(self: *Parser) !u32 {
        const start = self.next_pos;
        self.advanceToken();
        const token = self.peekToken();
        if (token.tag() == .new_line or token.tag() == .none) {
            return try self.pushNode(.return_stmt, start);
        } else {
            const expr = try self.parseExpr(.{}) orelse {
                return self.reportParseError("Expected expression.", &.{});
            };
            const id = try self.pushNode(.return_expr_stmt, start);
            self.nodes.items[id].head = .{
                .child_head = expr,
            };
            return id;
        }
    }

    fn parseExprOrAssignStatement(self: *Parser) !?u32 {
        var is_assign_stmt = false;
        const expr_id = (try self.parseExpr(.{
            .returnLeftAssignExpr = true,
            .outIsAssignStmt = &is_assign_stmt
        })) orelse {
            return null;
        };

        if (is_assign_stmt) {
            var token = self.peekToken();
            const opStart = self.next_pos;
            const assignTag = token.tag();
            // Assumes next token is an assignment operator: =, +=.
            self.advanceToken();

            const start = self.nodes.items[expr_id].start_token;
            var assignStmt: NodeId = undefined;
            var rightExpr: NodeId = undefined;
            switch (assignTag) {
                .equal => {
                    assignStmt = try self.pushNode(.assign_stmt, start);
                    if (self.peekToken().tag() == .func_k) {
                        // Multi-line lambda.
                        rightExpr = try self.parseMultilineLambdaFunction();
                    } else {
                        rightExpr = (try self.parseExpr(.{})) orelse {
                            return self.reportParseError("Expected right expression for assignment statement.", &.{});
                        };
                    }
                    self.nodes.items[assignStmt].head = .{
                        .left_right = .{
                            .left = expr_id,
                            .right = rightExpr,
                        },
                    };
                },
                .operator => {
                    const op_t = token.data.operator_t;
                    switch (op_t) {
                        .plus,
                        .minus,
                        .star,
                        .slash => {
                            self.advanceToken();
                            rightExpr = (try self.parseExpr(.{})) orelse {
                                return self.reportParseError("Expected right expression for assignment statement.", &.{});
                            };
                            assignStmt = try self.pushNode(.opAssignStmt, start);
                            self.nodes.items[assignStmt].head = .{
                                .opAssignStmt = .{
                                    .left = expr_id,
                                    .right = rightExpr,
                                    .op = toBinExprOp(op_t),
                                },
                            };
                        },
                        else => fmt.panic("Unexpected operator assignment.", &.{}),
                    }
                },
                else => return self.reportParseErrorAt("Unsupported assignment operator.", &.{}, opStart),
            }


            const left = self.nodes.items[expr_id];
            if (left.node_t == .ident) {
                const name_token = self.tokens.items[left.start_token];
                const name = self.src.items[name_token.pos()..name_token.data.end_pos];
                const block = &self.block_stack.items[self.block_stack.items.len-1];
                if (self.deps.get(name)) |node_id| {
                    if (node_id == expr_id) {
                        // Remove dependency now that it's recognized as assign statement.
                        _ = self.deps.remove(name);
                    }
                }
                try block.vars.put(self.alloc, name, {});
            }

            if (self.nodes.items[rightExpr].node_t != .lambda_multi) {
                token = self.peekToken();
                if (token.tag() == .new_line) {
                    self.advanceToken();
                    return assignStmt;
                } else if (token.tag() == .none) {
                    return assignStmt;
                } else return self.reportParseError("Expected end of line or file, got {}", &.{fmt.v(token.tag())});
            } else {
                return assignStmt;
            }
        } else {
            const start = self.nodes.items[expr_id].start_token;
            const id = try self.pushNode(.expr_stmt, start);
            self.nodes.items[id].head = .{
                .child_head = expr_id,
            };

            const token = self.peekToken();
            if (token.tag() == .new_line) {
                self.advanceToken();
                return id;
            } else if (token.tag() == .none) {
                return id;
            } else return self.reportParseError("Expected end of line or file", &.{});
        }
    }

    pub fn pushNode(self: *Parser, node_t: NodeType, start: u32) !NodeId {
        const id = self.nodes.items.len;
        try self.nodes.append(self.alloc, .{
            .node_t = node_t,
            .start_token = start,
            .next = NullId,
            .head = undefined,
        });
        return @intCast(NodeId, id);
    }

    fn pushIdentNode(self: *Parser, start: u32) !NodeId {
        const id = try self.pushNode(.ident, start);
        self.nodes.items[id].head = .{
            .ident = .{},
        };
        return id;
    }

    inline fn pushTagToken(self: *Parser, start_pos: u32, end_pos: u32) !void {
        try self.tokens.append(self.alloc, .{
            .token_t = .tag,
            .start_pos = @intCast(u26, start_pos),
            .data = .{
                .end_pos = end_pos,
            },
        });
    }

    inline fn pushIdentToken(self: *Parser, start_pos: u32, end_pos: u32) !void {
        try self.tokens.append(self.alloc, .{
            .token_t = .ident,
            .start_pos = @intCast(u26, start_pos),
            .data = .{
                .end_pos = end_pos,
            },
        });
    }

    inline fn pushNonDecimalIntegerToken(self: *Parser, start_pos: u32, end_pos: u32) !void {
        try self.tokens.append(self.alloc, .{
            .token_t = .nonDecInt,
            .start_pos = @intCast(u26, start_pos),
            .data = .{
                .end_pos = end_pos,
            },
        });
    }

    inline fn pushNumberToken(self: *Parser, start_pos: u32, end_pos: u32) !void {
        try self.tokens.append(self.alloc, .{
            .token_t = .number,
            .start_pos = @intCast(u26, start_pos),
            .data = .{
                .end_pos = end_pos,
            },
        });
    }

    inline fn pushTemplateStringToken(self: *Parser, start_pos: u32, end_pos: u32) !void {
        try self.tokens.append(self.alloc, .{
            .token_t = .templateString,
            .start_pos = @intCast(u26, start_pos),
            .data = .{
                .end_pos = end_pos,
            },
        });
    }

    inline fn pushStringToken(self: *Parser, start_pos: u32, end_pos: u32) !void {
        try self.tokens.append(self.alloc, .{
            .token_t = .string,
            .start_pos = @intCast(u26, start_pos),
            .data = .{
                .end_pos = end_pos,
            },
        });
    }

    inline fn pushOpToken(self: *Parser, operator_t: OperatorType, start_pos: u32) !void {
        try self.tokens.append(self.alloc, .{
            .token_t = .operator,
            .start_pos = @intCast(u26, start_pos),
            .data = .{
                .operator_t = operator_t,
            },
        });
    }

    inline fn pushIndentToken(self: *Parser, num_spaces: u32, start_pos: u32, spaces: bool) void {
        self.tokens.append(self.alloc, .{
            .token_t = .indent,
            .start_pos = @intCast(u26, start_pos),
            .data = .{
                .indent = if (spaces) num_spaces else num_spaces + 100,
            },
        }) catch fatal();
    }

    inline fn pushToken(self: *Parser, token_t: TokenType, start_pos: u32) !void {
        try self.tokens.append(self.alloc, .{
            .token_t = token_t,
            .start_pos = @intCast(u26, start_pos),
            .data = .{
                .end_pos = NullId,
            },
        });
    }

    /// When n=0, this is equivalent to peekToken.
    inline fn peekTokenAhead(self: Parser, n: u32) Token {
        if (self.next_pos + n < self.tokens.items.len) {
            return self.tokens.items[self.next_pos + n];
        } else {
            return Token{
                .token_t = .none,
                .start_pos = @intCast(u26, self.next_pos),
                .data = .{
                    .end_pos = NullId,
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
                .start_pos = @intCast(u26, self.next_pos),
                .data = .{
                    .end_pos = NullId,
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

pub const OperatorType = enum {
    plus,
    minus,
    star,
    caret,
    slash,
    percent,
    ampersand,
    verticalBar,
    doubleVerticalBar,
    tilde,
    lessLess,
    greaterGreater,
    bang,
    bang_equal,
    less,
    less_equal,
    greater,
    greater_equal,
    equal_equal,
};

pub const TokenType = enum(u6) {
    ident,
    number,
    nonDecInt,
    string,
    templateString,
    templateExprStart,
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
    new_line,
    indent,
    return_k,
    break_k,
    continue_k,
    if_k,
    then_k,
    else_k,
    for_k,
    while_k,
    await_k,
    true_k,
    each_k,
    false_k,
    or_k,
    and_k,
    not_k,
    as_k,
    pass_k,
    none_k,
    object_k,
    tagtype_k,
    func_k,
    is_k,
    coinit_k,
    coyield_k,
    coresume_k,
    tag,
    import_k,
    try_k,
    catch_k,
    recover_k,
    compt_k,
    static_k,
    capture_k,
    var_k,
    match_k,
    // Error token, returned if ignoreErrors = true.
    err,
    /// Used to indicate no token.
    none,
};

pub const Token = packed struct {
    token_t: TokenType,
    start_pos: u26,
    data: packed union {
        end_pos: u32,
        operator_t: OperatorType,
        // Num indent spaces.
        indent: u32,
    },

    pub inline fn tag(self: Token) TokenType {
        return self.token_t;
    }

    pub inline fn pos(self: Token) u32 {
        return self.start_pos;
    }
};

pub const NodeType = enum {
    root,
    expr_stmt,
    assign_stmt,
    opAssignStmt,
    staticDecl,
    captureDecl,
    varDecl,
    pass_stmt,
    breakStmt,
    continueStmt,
    return_stmt,
    return_expr_stmt,
    at_stmt,
    ident,
    at_ident,
    true_literal,
    false_literal,
    none,
    string,
    stringTemplate,
    await_expr,
    accessExpr,
    arr_access_expr,
    arr_range_expr,
    call_expr,
    named_arg,
    binExpr,
    unary_expr,
    number,
    nonDecInt,
    if_expr,
    if_stmt,
    else_clause,
    whileInfStmt,
    whileCondStmt,
    forOptStmt,
    for_range_stmt,
    for_iter_stmt,
    range_clause,
    eachClause,
    label_decl,
    func_decl,
    structDecl,
    structField,
    structInit,
    tagDecl,
    tagMember,
    tagInit,
    tagLiteral,
    lambda_assign_decl,
    lambda_expr, 
    lambda_multi,
    map_literal,
    mapEntry,
    arr_literal,
    coinit,
    coyield,
    coresume,
    importStmt,
    tryExpr,
    comptExpr,
    group,
    caseBlock,
    matchBlock,
    elseCase,
};

pub const BinaryExprOp = enum {
    plus,
    minus,
    star,
    caret,
    slash,
    percent,
    bitwiseAnd,
    bitwiseOr,
    bitwiseXor,
    bitwiseLeftShift,
    bitwiseRightShift,
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
    bitwiseNot,
};

pub const Node = struct {
    node_t: NodeType,
    start_token: u32,
    next: NodeId,
    /// Fixed size. TODO: Rename to `data`.
    head: union {
        binExpr: struct {
            left: NodeId,
            right: NodeId,
            op: BinaryExprOp,
            semaCanRequestIntegerOperands: bool = false,
        },
        opAssignStmt: struct {
            left: NodeId,
            right: NodeId,
            op: BinaryExprOp,
        },
        mapEntry: struct {
            left: NodeId,
            right: NodeId,
        },
        caseBlock: struct {
            firstCond: NodeId,
            firstChild: NodeId,
        },
        matchBlock: struct {
            expr: NodeId,
            firstCase: NodeId,
        },
        left_right: struct {
            left: NodeId,
            right: NodeId,
            extra: u32 = NullId,
        },
        accessExpr: struct {
            left: NodeId,
            right: NodeId,
            /// Symbol id of a var or func. NullIdU16 if it does not point to a symbol.
            semaSymId: u32 = NullId,
        },
        func_call: struct {
            callee: NodeId,
            arg_head: NodeId,
            has_named_arg: bool,
        },
        ident: struct {
            semaVarId: u32 = NullId,
            semaSymId: u32 = NullId,
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
            genEndLocalsPc: u32 = NullId,
        },
        lambda_assign_decl: struct {
            decl_id: FuncDeclId,
            body_head: NodeId,
            assign_expr: NodeId,
        },
        structInit: struct {
            name: NodeId,
            initializer: NodeId,
        },
        structField: struct {
            name: NodeId,
            fieldType: NodeId,
        },
        structDecl: struct {
            name: NodeId,
            fieldsHead: NodeId,
            funcsHead: NodeId,
        },
        varDecl: struct {
            left: NodeId,
            right: NodeId,
        },
        tagMember: struct {
            name: NodeId,
        },
        tagDecl: struct {
            name: NodeId,
            memberHead: NodeId,
        },
        whileCondStmt: struct {
            cond: NodeId,
            bodyHead: NodeId,
        },
        forOptStmt: struct {
            opt: NodeId,
            bodyHead: NodeId,
            as: NodeId,
        },
        for_range_stmt: struct {
            range_clause: NodeId,
            body_head: NodeId,
            eachClause: NodeId,
        },
        for_iter_stmt: struct {
            iterable: NodeId,
            body_head: NodeId,
            eachClause: NodeId,
        },
        eachClause: struct {
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
        stringTemplate: struct {
            partsHead: NodeId,
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
        const func_decls = try alloc.create(std.ArrayListUnmanaged(FuncDecl));
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
    root_id: NodeId,
    err_msg: []const u8,
    has_error: bool,
    isTokenError: bool,

    /// ArrayList is returned so resulting ast can be modified.
    nodes: *std.ArrayListUnmanaged(Node),
    tokens: []const Token,
    src: []const u8,
    func_decls: *std.ArrayListUnmanaged(FuncDecl),
    func_params: []const FunctionParam,

    name: []const u8,
    deps: *std.StringHashMapUnmanaged(NodeId),

    pub fn getTokenString(self: ResultView, token_id: u32) []const u8 {
        // Assumes token with end_pos.
        const token = self.tokens[token_id];
        return self.src[token.pos()..token.data.end_pos];
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

const StructDeclId = u32;
pub const StructDecl = struct {
    name: IndexSlice,
    fields: IndexSlice,
    funcs: IndexSlice,
    methods: IndexSlice,
};

const FuncDeclId = u32;

pub const FuncDecl = struct {
    name: IndexSlice,
    params: IndexSlice,
    return_type: ?IndexSlice,
    semaSymId: u32 = NullId,
    semaResolvedFuncSymId: u32 = NullId,
};

pub const FunctionParam = struct {
    name: IndexSlice,
    typeName: IndexSlice,
};

fn toBinExprOp(op: OperatorType) BinaryExprOp {
    return switch (op) {
        .plus => .plus,
        .minus => .minus,
        .star => .star,
        .caret => .caret,
        .slash => .slash,
        .percent => .percent,
        .ampersand => .bitwiseAnd,
        .verticalBar => .bitwiseOr,
        .doubleVerticalBar => .bitwiseXor,
        .lessLess => .bitwiseLeftShift,
        .greaterGreater => .bitwiseRightShift,
        .bang_equal => .bang_equal,
        .less => .less,
        .less_equal => .less_equal,
        .greater => .greater,
        .greater_equal => .greater_equal,
        .equal_equal => .equal_equal,
        .bang,
        .tilde => unreachable,
    };
}

pub fn getBinOpPrecedence(op: BinaryExprOp) u8 {
    switch (op) {
        .caret => return 6,
        .slash,
        .percent,
        .star => {
            return 5;
        },
        .minus,
        .plus => {
            return 4;
        },

        .bitwiseLeftShift,
        .bitwiseRightShift => return 3,

        .bitwiseAnd,
        .bitwiseOr => return 2,

        .greater,
        .greater_equal,
        .less,
        .less_equal,
        .bang_equal,
        .equal_equal => {
            return 1;
        },

        .or_op,
        .and_op => {
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

const StringDelim = enum(u2) {
    single,
    double,
    triple,
};

pub const TokenizeState = struct {
    stateT: TokenizeStateTag,

    /// For string interpolation, open braces can accumulate so the end of a template expression can be determined.
    openBraces: u8 = 0,

    /// For string interpolation, if true the delim is a double quote otherwise it's a backtick.
    stringDelim: StringDelim = .single,
    hadTemplateExpr: u1 = 0,
};

pub const TokenizeStateTag = enum {
    start,
    token,
    templateString,
    templateExpr,
    templateExprToken,
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

        /// Consumes the next token skipping whitespace and returns the next tokenizer state.
        fn tokenizeOne(p: *Parser, state: TokenizeState) !TokenizeState {
            if (isAtEndChar(p)) {
                return .{
                    .stateT = .end,
                };
            }

            const start = p.next_pos;
            const ch = consumeChar(p);
            switch (ch) {
                '(' => {
                    try p.pushToken(.left_paren, start);
                },
                ')' => {
                    try p.pushToken(.right_paren, start);
                },
                '{' => {
                    try p.pushToken(.left_brace, start);
                    if (state.stateT == .templateExprToken) {
                        var next = state;
                        next.openBraces += 1;
                        return next;
                    }
                },
                '}' => {
                    try p.pushToken(.right_brace, start);
                    if (state.stateT == .templateExprToken) {
                        var next = state;
                        if (state.openBraces == 0) {
                            next.stateT = .templateString;
                            next.openBraces = 0;
                            return next;
                        } else {
                            next.openBraces -= 1;
                            return next;
                        }
                    }
                },
                '[' => try p.pushToken(.left_bracket, start),
                ']' => try p.pushToken(.right_bracket, start),
                ',' => try p.pushToken(.comma, start),
                '.' => {
                    if (peekChar(p) == '.') {
                        advanceChar(p);
                        try p.pushToken(.dot_dot, start);
                    } else {
                        try p.pushToken(.dot, start);
                    }
                },
                ':' => {
                    try p.pushToken(.colon, start);
                },
                '@' => try p.pushToken(.at, start),
                '-' => {
                    if (peekChar(p) == '-') {
                        advanceChar(p);
                        // Single line comment. Ignore chars until eol.
                        while (!isAtEndChar(p)) {
                            if (peekChar(p) == '\n') {
                                // Don't consume new line or the current indentation could augment with the next line.
                                return tokenizeOne(p, state);
                            }
                            advanceChar(p);
                        }
                        return .{ .stateT = .end };
                    } else {
                        try p.pushOpToken(.minus, start);
                    }
                },
                '%' => try p.pushOpToken(.percent, start),
                '&' => try p.pushOpToken(.ampersand, start),
                '|' => {
                    if (peekChar(p) == '|') {
                        advanceChar(p);
                        try p.pushOpToken(.doubleVerticalBar, start);
                    } else {
                        try p.pushOpToken(.verticalBar, start);
                    }
                },
                '~' => try p.pushOpToken(.tilde, start),
                '+' => {
                    try p.pushOpToken(.plus, start);
                },
                '^' => {
                    try p.pushOpToken(.caret, start);
                },
                '*' => {
                    try p.pushOpToken(.star, start);
                },
                '/' => {
                    try p.pushOpToken(.slash, start);
                },
                '!' => {
                    if (isNextChar(p, '=')) {
                        try p.pushOpToken(.bang_equal, start);
                        advanceChar(p);
                    } else {
                        try p.pushOpToken(.bang, start);
                    }
                },
                '=' => {
                    if (!isAtEndChar(p)) {
                        switch (peekChar(p)) {
                            '=' => {
                                advanceChar(p);
                                try p.pushOpToken(.equal_equal, start);
                            },
                            '>' => {
                                advanceChar(p);
                                try p.pushToken(.equal_greater, start);
                            },
                            else => {
                                try p.pushToken(.equal, start);
                            }
                        }
                    } else {
                        try p.pushToken(.equal, start);
                    }
                },
                '<' => {
                    const ch2 = peekChar(p);
                    if (ch2 == '=') {
                        try p.pushOpToken(.less_equal, start);
                        advanceChar(p);
                    } else if (ch2 == '<') {
                        try p.pushOpToken(.lessLess, start);
                        advanceChar(p);
                    } else {
                        try p.pushOpToken(.less, start);
                    }
                },
                '>' => {
                    const ch2 = peekChar(p);
                    if (ch2 == '=') {
                        try p.pushOpToken(.greater_equal, start);
                        advanceChar(p);
                    } else if (ch2 == '>') {
                        try p.pushOpToken(.greaterGreater, start);
                        advanceChar(p);
                    } else {
                        try p.pushOpToken(.greater, start);
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
                            else => return tokenizeOne(p, state),
                        }
                    }
                    return .{ .stateT = .end };
                },
                '\n' => {
                    try p.pushToken(.new_line, start);
                    return .{ .stateT = .start };
                },
                '"' => {
                    return tokenizeTemplateStringOne(p, .{
                        .stateT = state.stateT,
                        .stringDelim = .double,
                    });
                },
                '\'' => {
                    if (state.stateT == .templateExprToken) {
                        // Only allow string literals inside template expressions.
                        try tokenizeString(p, p.next_pos, '\'');
                        return state;
                    } else {
                        if (peekChar(p) == '\'') {
                            if (peekCharAhead(p, 1)) |ch2| {
                                if (ch2 == '\'') {
                                    _ = consumeChar(p);
                                    _ = consumeChar(p);
                                    return tokenizeTemplateStringOne(p, .{
                                        .stateT = state.stateT,
                                        .stringDelim = .triple,
                                    });
                                }
                            }
                        }
                        return tokenizeTemplateStringOne(p, .{
                            .stateT = state.stateT,
                            .stringDelim = .single,
                        });
                    }
                },
                '#' => {
                    try tokenizeTag(p, p.next_pos);
                    return .{ .stateT = .token };
                },
                else => {
                    if (std.ascii.isAlphabetic(ch)) {
                        try tokenizeKeywordOrIdent(p, start);
                        return .{ .stateT = .token };
                    }
                    if (ch >= '0' and ch <= '9') {
                        try tokenizeNumber(p, start);
                        return .{ .stateT = .token };
                    }
                    if (p.tokenizeOpts.ignoreErrors) {
                        try p.pushToken(.err, start);
                        return .{ .stateT = .token };
                    } else {
                        return p.reportTokenErrorAt("unknown character: {} ({}) at {}", &.{fmt.char(ch), fmt.v(ch), fmt.v(start)}, start);
                    }
                }
            }
            return .{ .stateT = .token };
        }

        /// Returns true if an indent or new line token was parsed.
        fn tokenizeIndentOne(p: *Parser) !bool {
            if (isAtEndChar(p)) {
                return false;
            }
            var ch = peekChar(p);
            switch (ch) {
                ' ' => {
                    const start = p.next_pos;
                    advanceChar(p);
                    var count: u32 = 1;
                    while (true) {
                        ch = peekChar(p);
                        if (ch == ' ') {
                            count += 1;
                            advanceChar(p);
                        } else break;
                    }
                    p.pushIndentToken(count, start, true);
                    return true;
                },
                '\t' => {
                    const start = p.next_pos;
                    advanceChar(p);
                    var count: u32 = 1;
                    while (true) {
                        ch = peekChar(p);
                        if (ch == '\t') {
                            count += 1;
                            advanceChar(p);
                        } else break;
                    }
                    p.pushIndentToken(count, start, false);
                    return true;
                },
                '\n' => {
                    try p.pushToken(.new_line, p.next_pos);
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
                    return tokenizeOne(p, state);
                },
                .templateToken => {
                    return tokenizeOne(p, state);
                },
                .end => return error.AtEnd,
            }
        }

        fn tokenize(p: *Parser, opts: TokenizeOptions) !void {
            p.tokenizeOpts = opts;
            p.tokens.clearRetainingCapacity();
            p.next_pos = 0;

            if (p.src.items.len > 2 and p.src.items[0] == '#' and p.src.items[1] == '!') {
                // Ignore shebang line.
                while (!isAtEndChar(p)) {
                    if (peekChar(p) == '\n') {
                        advanceChar(p);
                        break;
                    }
                    advanceChar(p);
                }
            }

            var state = TokenizeState{
                .stateT = .start,
            };
            while (true) {
                switch (state.stateT) {
                    .start => {
                        // First parse indent spaces.
                        while (true) {
                            if (!(try tokenizeIndentOne(p))) {
                                state.stateT = .token;
                                break;
                            }
                        }
                    },
                    .token => {
                        while (true) {
                            state = try tokenizeOne(p, state);
                            if (state.stateT != .token) {
                                break;
                            }
                        }
                    },
                    .templateString => {
                        state = try tokenizeTemplateStringOne(p, state);
                    },
                    .templateExpr => {
                        state = try tokenizeTemplateExprOne(p, state);
                    },
                    .templateExprToken => {
                        while (true) {
                            const nextState = try tokenizeOne(p, state);
                            if (nextState.stateT != .token) {
                                state = nextState;
                                break;
                            }
                        }
                    },
                    .end => {
                        break;
                    },
                }
            }
        }

        fn tokenizeTemplateExprOne(p: *Parser, state: TokenizeState) !TokenizeState {
            var ch = peekChar(p);
            if (ch == '{') {
                advanceChar(p);
                try p.pushToken(.templateExprStart, p.next_pos);
                var next = state;
                next.stateT = .templateExprToken;
                next.openBraces = 0;
                next.hadTemplateExpr = 1;
                return next;
            } else {
                stdx.panicFmt("Expected template expr '{{'", .{});
            }
        }

        /// Returns the next tokenizer state.
        fn tokenizeTemplateStringOne(p: *Parser, state: TokenizeState) !TokenizeState {
            const start = p.next_pos;
            savePos(p);

            while (true) {
                if (isAtEndChar(p)) {
                    if (p.tokenizeOpts.ignoreErrors) {
                        restorePos(p);
                        try p.pushToken(.err, start);
                        return .{ .stateT = .token };
                    } else return p.reportTokenErrorAt("UnterminatedString", &.{}, start);
                }
                const ch = peekChar(p);
                switch (ch) {
                    '\'' => {
                        if (state.stringDelim == .single) {
                            if (state.hadTemplateExpr == 1) {
                                try p.pushTemplateStringToken(start, p.next_pos);
                            } else {
                                try p.pushStringToken(start, p.next_pos);
                            }
                            _ = consumeChar(p);
                            return .{ .stateT = .token };
                        } else if (state.stringDelim == .triple) {
                            var ch2 = peekCharAhead(p, 1) orelse 0;
                            if (ch2 == '\'') {
                                ch2 = peekCharAhead(p, 2) orelse 0;
                                if (ch2 == '\'') {
                                    if (state.hadTemplateExpr == 1) {
                                        try p.pushTemplateStringToken(start, p.next_pos);
                                    } else {
                                        try p.pushStringToken(start, p.next_pos);
                                    }
                                    _ = consumeChar(p);
                                    _ = consumeChar(p);
                                    _ = consumeChar(p);
                                    return .{ .stateT = .token };
                                }
                            }
                        }
                        _ = consumeChar(p);
                    },
                    '"' => {
                        if (state.stringDelim == .double) {
                            if (state.hadTemplateExpr == 1) {
                                try p.pushTemplateStringToken(start, p.next_pos);
                            } else {
                                try p.pushStringToken(start, p.next_pos);
                            }
                            _ = consumeChar(p);
                            return .{ .stateT = .token };
                        } else {
                            _ = consumeChar(p);
                        }
                    },
                    '{' => {
                        try p.pushTemplateStringToken(start, p.next_pos);
                        var next = state;
                        next.stateT = .templateExpr;
                        next.openBraces = 0;
                        next.hadTemplateExpr = 1;
                        return next;
                    },
                    '\\' => {
                        // Escape the next character.
                        _ = consumeChar(p);
                        if (isAtEndChar(p)) {
                            if (p.tokenizeOpts.ignoreErrors) {
                                restorePos(p);
                                try p.pushToken(.err, start);
                                return .{ .stateT = .token };
                            } else return p.reportTokenErrorAt("UnterminatedString", &.{}, start);
                        }
                        _ = consumeChar(p);
                        continue;
                    },
                    '\n' => {
                        if (state.stringDelim == .single) {
                            if (p.tokenizeOpts.ignoreErrors) {
                                restorePos(p);
                                try p.pushToken(.err, start);
                                return .{ .stateT = .token };
                            } else return p.reportTokenErrorAt("UnterminatedString", &.{}, start);
                        }
                        _ = consumeChar(p);
                    },
                    else => {
                        _ = consumeChar(p);
                    },
                }
            }
        }

        fn tokenizeTag(p: *Parser, start: u32) !void {
            // Consume alpha, numeric, underscore.
            while (true) {
                if (isAtEndChar(p)) {
                    try p.pushTagToken(start, p.next_pos);
                    return;
                }
                const ch = peekChar(p);
                if (std.ascii.isAlphanumeric(ch)) {
                    advanceChar(p);
                    continue;
                }
                if (ch == '_') {
                    advanceChar(p);
                    continue;
                }
                try p.pushTagToken(start, p.next_pos);
                return;
            }
        }

        fn tokenizeKeywordOrIdent(p: *Parser, start: u32) !void {
            // Consume alpha.
            while (true) {
                if (isAtEndChar(p)) {
                    if (keywords.get(getSubStrFrom(p, start))) |token_t| {
                        try p.pushToken(token_t, start);
                    } else {
                        try p.pushIdentToken(start, p.next_pos);
                    }
                    return;
                }
                const ch = peekChar(p);
                if (std.ascii.isAlphabetic(ch)) {
                    advanceChar(p);
                    continue;
                } else break;
            }

            // Consume alpha, numeric, underscore.
            while (true) {
                if (isAtEndChar(p)) {
                    if (keywords.get(getSubStrFrom(p, start))) |token_t| {
                        try p.pushToken(token_t, start);
                    } else {
                        try p.pushIdentToken(start, p.next_pos);
                    }
                    return;
                }
                const ch = peekChar(p);
                if (std.ascii.isAlphanumeric(ch)) {
                    advanceChar(p);
                    continue;
                }
                if (ch == '_') {
                    advanceChar(p);
                    continue;
                }
                if (keywords.get(getSubStrFrom(p, start))) |token_t| {
                    try p.pushToken(token_t, start);
                } else {
                    try p.pushIdentToken(start, p.next_pos);
                }
                return;
            }
        }

        fn tokenizeString(p: *Parser, start: u32, delim: u8) !void {
            savePos(p);
            while (true) {
                if (isAtEndChar(p)) {
                    if (p.tokenizeOpts.ignoreErrors) {
                        restorePos(p);
                        try p.pushToken(.err, start);
                    } else return p.reportTokenErrorAt("UnterminatedString", &.{}, start);
                }
                if (peekChar(p) == delim) {
                    try p.pushStringToken(start, p.next_pos);
                    advanceChar(p);
                    return;
                } else {
                    advanceChar(p);
                }
            }
        }

        fn consumeDigits(p: *Parser) void {
            while (true) {
                if (isAtEndChar(p)) {
                    return;
                }
                const ch = peekChar(p);
                if (ch >= '0' and ch <= '9') {
                    advanceChar(p);
                    continue;
                } else break;
            }
        }

        /// Assumes first digit is consumed.
        fn tokenizeNumber(p: *Parser, start: u32) !void {
            if (isAtEndChar(p)) {
                try p.pushNumberToken(start, p.next_pos);
                return;
            }
            var ch = peekChar(p);
            if ((ch >= '0' and ch <= '9') or ch == '.') {
                // Decimal notation.
                consumeDigits(p);
                if (isAtEndChar(p)) {
                    try p.pushNumberToken(start, p.next_pos);
                    return;
                }
                ch = peekChar(p);
                const ch2 = peekCharAhead(p, 1) orelse 0;
                // Differentiate decimal from range operator.
                if (ch == '.' and (ch2 >= '0' and ch2 <= '9')) {
                    advanceChar(p);
                    advanceChar(p);
                    consumeDigits(p);
                    if (isAtEndChar(p)) {
                        try p.pushNumberToken(start, p.next_pos);
                        return;
                    }
                    ch = peekChar(p);
                }
                if (ch == 'e') {
                    advanceChar(p);
                    if (isAtEndChar(p)) {
                        return p.reportTokenError("Expected number.", &.{});
                    }
                    ch = peekChar(p);
                    if (ch == '-') {
                        advanceChar(p);
                        if (isAtEndChar(p)) {
                            return p.reportTokenError("Expected number.", &.{});
                        }
                        ch = peekChar(p);
                    }
                    if (ch < '0' and ch > '9') {
                        return p.reportTokenError("Expected number.", &.{});
                    }
                    consumeDigits(p);
                }
                try p.pushNumberToken(start, p.next_pos);
                return;
            } else {
                if (p.src.items[p.next_pos-1] == '0') {
                    if (ch == 'x') {
                        // Hex integer.
                        advanceChar(p);
                        while (true) {
                            if (isAtEndChar(p)) {
                                break;
                            }
                            ch = peekChar(p);
                            if ((ch >= '0' and ch <= '9') or (ch >= 'A' and ch <= 'Z') or (ch >= 'a' and ch <= 'z')) {
                                advanceChar(p);
                                continue;
                            } else break;
                        }
                        try p.pushNonDecimalIntegerToken(start, p.next_pos);
                        return;
                    } else if (ch == 'o') {
                        // Oct integer.
                        advanceChar(p);
                        while (true) {
                            if (isAtEndChar(p)) {
                                break;
                            }
                            ch = peekChar(p);
                            if (ch >= '0' and ch <= '8') {
                                advanceChar(p);
                                continue;
                            } else break;
                        }
                        try p.pushNonDecimalIntegerToken(start, p.next_pos);
                        return;
                    } else if (ch == 'b') {
                        // Bin integer.
                        advanceChar(p);
                        while (true) {
                            if (isAtEndChar(p)) {
                                break;
                            }
                            ch = peekChar(p);
                            if (ch == '0' or ch == '1') {
                                advanceChar(p);
                                continue;
                            } else break;
                        }
                        try p.pushNonDecimalIntegerToken(start, p.next_pos);
                        return;
                    }
                }
                try p.pushNumberToken(start, p.next_pos);
                return;
            }
        }
    };
}

const TokenizeOptions = struct {
    /// Used for syntax highlighting.
    ignoreErrors: bool = false,
};

const ParseExprOptions = struct {
    returnLeftAssignExpr: bool = false,
    outIsAssignStmt: *bool = undefined,
};

test "Internals." {
    try t.eq(@sizeOf(Token), 8);
    try t.eq(@alignOf(Token), 8);
    try t.eq(@sizeOf(Node), 28);
    try t.eq(@sizeOf(TokenizeState), 4);

    try t.eq(std.enums.values(TokenType).len, 60);
    try t.eq(keywords.kvs.len, 34);
}