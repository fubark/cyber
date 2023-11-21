const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const t = stdx.testing;
const fatal = cy.fatal;
const fmt = @import("fmt.zig");
const v = fmt.v;
const cy = @import("cyber.zig");

const NodeId = cy.NodeId;
const TokenId = u32;
const NullId = cy.NullId;
const log = cy.log.scoped(.parser);
const IndexSlice = cy.IndexSlice(u32);

const dumpParseErrorStackTrace = builtin.mode == .Debug and !cy.isWasm and true;

const annotations = std.ComptimeStringMap(cy.ast.AnnotationType, .{
    .{ "host", .host },
});

const keywords = std.ComptimeStringMap(TokenType, .{
    .{ "and", .and_k },
    .{ "as", .as_k },
    .{ "my", .my_k },
    // .{ "await", .await_k },
    .{ "break", .break_k },
    .{ "case", .case_k },
    .{ "catch", .catch_k },
    .{ "coinit", .coinit_k },
    .{ "continue", .continue_k },
    .{ "coresume", .coresume_k },
    .{ "coyield", .coyield_k },
    .{ "else", .else_k },
    .{ "enum", .enum_k },
    .{ "error", .error_k },
    .{ "false", .false_k },
    .{ "for", .for_k },
    .{ "func", .func_k },
    .{ "if", .if_k },
    .{ "import", .import_k },
    .{ "is", .is_k },
    .{ "switch", .switch_k },
    .{ "none", .none_k },
    .{ "object", .object_k },
    .{ "or", .or_k },
    .{ "pass", .pass_k },
    .{ "not", .not_k },
    .{ "return", .return_k },
    .{ "throw", .throw_k },
    .{ "true", .true_k },
    .{ "try", .try_k },
    .{ "type", .type_k },
    .{ "var", .var_k },
    .{ "while", .while_k },
});

const Block = struct {
    vars: std.StringHashMapUnmanaged(void),

    fn deinit(self: *Block, alloc: std.mem.Allocator) void {
        self.vars.deinit(alloc);
    }
};

/// Parses source code into AST.
pub const Parser = struct {
    alloc: std.mem.Allocator,

    /// Context vars.
    src: []const u8,
    next_pos: u32,
    savePos: u32,
    tokens: std.ArrayListUnmanaged(Token),
    nodes: std.ArrayListUnmanaged(cy.Node),
    last_err: []const u8,
    /// The last error's src char pos.
    last_err_pos: u32,
    blockStack: std.ArrayListUnmanaged(Block),
    cur_indent: u32,

    /// Use the parser pass to record static declarations.
    staticDecls: std.ArrayListUnmanaged(StaticDecl),

    // TODO: This should be implemented by user callbacks.
    /// @name arg.
    name: []const u8,
    /// Variable dependencies.
    deps: std.StringHashMapUnmanaged(NodeId),

    tokenizeOpts: TokenizeOptions,

    inObjectDecl: bool,

    /// Whether to parse and accumulate comment tokens in `comments`.
    parseComments: bool,
    comments: std.ArrayListUnmanaged(cy.IndexSlice(u32)),

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
            .src = "",
            .next_pos = undefined,
            .savePos = undefined,
            .tokens = .{},
            .nodes = .{},
            .last_err = "",
            .last_err_pos = 0,
            .blockStack = .{},
            .cur_indent = 0,
            .name = "",
            .deps = .{},
            .user = undefined,
            .tokenizeOpts = .{},
            .staticDecls = .{},
            .inObjectDecl = false,
            .parseComments = false,
            .comments = .{},
        };
    }

    pub fn deinit(self: *Parser) void {
        self.tokens.deinit(self.alloc);
        self.nodes.deinit(self.alloc);
        self.alloc.free(self.last_err);
        for (self.blockStack.items) |*block| {
            block.deinit(self.alloc);
        }
        self.blockStack.deinit(self.alloc);
        self.deps.deinit(self.alloc);
        self.staticDecls.deinit(self.alloc);
        self.comments.deinit(self.alloc);
    }

    fn dumpTokensToCurrent(self: *Parser) void {
        for (self.tokens.items[0..self.next_pos+1]) |token| {
            log.debug("{}", .{token.tag()});
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
        self.src = src;
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
                .tokens = &.{},
                .src = self.src,
                .name = self.name,
                .deps = &self.deps,
            };
        };
        const root_id = self.parseRoot() catch |err| {
            log.debug("parse error: {} {s}", .{err, self.last_err});
            // self.dumpTokensToCurrent();
            logSrcPos(self.src, self.last_err_pos, 20);
            if (dumpParseErrorStackTrace and !cy.silentError) {
                std.debug.dumpStackTrace(@errorReturnTrace().?.*);
            }
            return ResultView{
                .has_error = true,
                .isTokenError = false,
                .err_msg = self.last_err,
                .root_id = NullId,
                .nodes = &self.nodes,
                .tokens = &.{},
                .src = self.src,
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
            .src = self.src,
            .name = self.name,
            .deps = &self.deps,
        };
    }

    fn parseRoot(self: *Parser) !NodeId {
        self.next_pos = 0;
        self.nodes.clearRetainingCapacity();
        self.blockStack.clearRetainingCapacity();
        self.cur_indent = 0;

        const root_id = try self.pushNode(.root, 0);

        const indent = (try self.consumeIndentBeforeStmt()) orelse {
            self.nodes.items[root_id].head = .{
                .root = .{
                    .headStmt = cy.NullId,
                },
            };
            return 0;
        };
        if (indent != 0) {
            return self.reportParseError("Unexpected indentation.", &.{});
        }

        try self.pushBlock();
        const res = try self.parseBodyStatements(0);

        // Mark last expression stmt.
        const last = self.nodes.items[res.last];
        if (last.node_t == .exprStmt) {
            self.nodes.items[res.last].head.exprStmt.isLastRootStmt = true;
        }

        const block = self.popBlock();
        _ = block;

        self.nodes.items[root_id].head = .{
            .root = .{
                .headStmt = res.first,
            },
        };
        return 0;
    }

    /// Returns number of spaces that precedes a statement.
    /// The current line is consumed if there is no statement.
    fn consumeIndentBeforeStmt(self: *Parser) !?u32 {
        while (true) {
            // Spaces, count = 0.
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
                return null;
            } else {
                return res;
            }
        }
    }

    fn pushBlock(self: *Parser) !void {
        try self.blockStack.append(self.alloc, .{
            .vars = .{},
        });
    }

    fn popBlock(self: *Parser) Block {
        var block = self.blockStack.pop();
        block.deinit(self.alloc);
        return block;
    }

    fn parseSingleOrIndentedBodyStmts(self: *Parser) !FirstLastStmt {
        var token = self.peekToken();
        if (token.tag() != .new_line) {
            // Parse single statement only.
            const stmt = try self.parseStatement();
            return .{
                .first = stmt,
                .last = stmt,
            };
        } else {
            self.advanceToken();
            return self.parseIndentedBodyStatements();
        }
    }

    /// Indent is determined by the first body statement.
    fn parseIndentedBodyStatements(self: *Parser) !FirstLastStmt {
        const reqIndent = try self.parseFirstChildIndent(self.cur_indent);
        return self.parseBodyStatements(reqIndent);
    }

    // Assumes the first indent is already consumed.
    fn parseBodyStatements(self: *Parser, reqIndent: u32) !FirstLastStmt {
        const prevIndent = self.cur_indent;
        self.cur_indent = reqIndent;
        defer self.cur_indent = prevIndent;

        var first = try self.parseStatement();
        var last = first;

        // Parse body statements until indentation goes back to at least the previous indent.
        while (true) {
            const start = self.next_pos;
            const indent = (try self.consumeIndentBeforeStmt()) orelse break;
            if (indent == reqIndent) {
                const id = try self.parseStatement();
                self.nodes.items[last].next = id;
                last = id;
            } else if (try isRecedingIndent(self, prevIndent, reqIndent, indent)) {
                self.next_pos = start;
                break;
            } else {
                return self.reportParseError("Unexpected indentation.", &.{});
            }
        }
        return .{
            .first = first,
            .last = last,
        };
    }

    /// Parses the first child indent and returns the indent size.
    fn parseFirstChildIndent(self: *Parser, fromIndent: u32) !u32 {
        const indent = (try self.consumeIndentBeforeStmt()) orelse {
            return self.reportParseError("Block requires at least one statement. Use the `pass` statement as a placeholder.", &.{});
        };
        if ((fromIndent ^ indent < 0x80000000) or fromIndent == 0) {
            // Either same indent style or indenting from root.
            if (indent > fromIndent) {
                return indent;
            } else {
                return self.reportParseError("Block requires at least one statement. Use the `pass` statement as a placeholder.", &.{});
            }
        } else {
            if (fromIndent & 0x80000000 == 0x80000000) {
                return self.reportParseError("Expected tabs for indentation.", &.{});
            } else {
                return self.reportParseError("Expected spaces for indentation.", &.{});
            }
        }
    }

    fn parseLambdaFuncWithParam(self: *Parser, paramIdent: NodeId) !NodeId {
        const start = self.next_pos;
        // Assumes first token is `=>`.
        self.advanceToken();
        
        const id = try self.pushNode(.lambda_expr, start);

        // Parse body expr.
        try self.pushBlock();
        const expr = (try self.parseExpr(.{})) orelse {
            return self.reportParseError("Expected lambda body expression.", &.{});
        };
        const block = self.popBlock();
        _ = block;

        const identPos = self.nodes.items[paramIdent].start_token;
        const param = try self.pushNode(.funcParam, identPos);
        self.nodes.items[param].head = .{
            .funcParam = .{
                .name = paramIdent,
                .typeSpecHead = NullId,
            },
        };

        const header = try self.pushNode(.funcHeader, start);
        self.nodes.items[header].head = .{
            .funcHeader = .{
                .name = cy.NullId,
                .paramHead = param,
                .ret = cy.NullId,
            },
        };

        self.nodes.items[id].head = .{
            .func = .{
                .header = header,
                .bodyHead = expr,
            },
        };
        return id;
    }

    fn parseNoParamLambdaFunc(self: *Parser) !NodeId {
        const start = self.next_pos;
        // Assumes first token is `=>`.
        self.advanceToken();

        const id = try self.pushNode(.lambda_expr, start);

        // Parse body expr.
        try self.pushBlock();
        const expr = (try self.parseExpr(.{})) orelse {
            return self.reportParseError("Expected lambda body expression.", &.{});
        };
        _ = self.popBlock();
        
        const header = try self.pushNode(.funcHeader, start);
        self.nodes.items[header].head = .{
            .funcHeader = .{
                .name = cy.NullId,
                .paramHead = cy.NullId,
                .ret = cy.NullId,
            },
        };

        self.nodes.items[id].head = .{
            .func = .{
                .header = header,
                .bodyHead = expr,
            },
        };
        return id;
    }

    fn parseMultilineLambdaFunction(self: *Parser) !NodeId {
        const start = self.next_pos;

        // Assume first token is `func`.
        self.advanceToken();

        const paramHead = try self.parseFuncParams();
        const ret = try self.parseFuncReturn();

        if (self.peekToken().tag() == .colon) {
            self.advanceToken();
        } else {
            return self.reportParseError("Expected colon.", &.{});
        }

        const id = try self.pushNode(.lambda_multi, start);

        try self.pushBlock();
        const res = try self.parseSingleOrIndentedBodyStmts();
        _ = self.popBlock();

        const header = try self.pushNode(.funcHeader, start);
        self.nodes.items[header].head = .{
            .funcHeader = .{
                .name = cy.NullId,
                .paramHead = paramHead orelse cy.NullId,
                .ret = ret orelse cy.NullId,
            },
        };

        self.nodes.items[id].head = .{
            .func = .{
                .header = header,
                .bodyHead = res.first,
            },
        };
        return id;
    }

    fn parseLambdaFunction(self: *Parser) !NodeId {
        const start = self.next_pos;

        const paramHead = try self.parseFuncParams();
        const ret = try self.parseFuncReturn();

        var token = self.peekToken();
        if (token.tag() != .equal_greater) {
            return self.reportParseError("Expected =>.", &.{});
        }
        self.advanceToken();

        const id = try self.pushNode(.lambda_expr, start);

        // Parse body expr.
        try self.pushBlock();
        const expr = (try self.parseExpr(.{})) orelse {
            return self.reportParseError("Expected lambda body expression.", &.{});
        };
        const block = self.popBlock();
        _ = block;

        const header = try self.pushNode(.funcHeader, start);
        self.nodes.items[header].head = .{
            .funcHeader = .{
                .name = cy.NullId,
                .paramHead = paramHead orelse cy.NullId,
                .ret = ret orelse cy.NullId,
            },
        };
        
        self.nodes.items[id].head = .{
            .func = .{
                .header = header,
                .bodyHead = expr,
            },
        };
        return id;
    }

    fn parseFuncParams(self: *Parser) !?NodeId {
        var token = self.peekToken();
        if (token.tag() != .left_paren) {
            return self.reportParseError("Expected open parenthesis.", &.{});
        }
        self.advanceToken();

        // Parse params.
        token = self.peekToken();
        if (token.tag() == .ident) {
            var start = self.next_pos;
            var name = try self.pushIdentNode(start);

            self.advanceToken();
            var typeSpecHead = (try self.parseOptNamePath()) orelse cy.NullId;

            const paramHead = try self.pushNode(.funcParam, start);
            self.nodes.items[paramHead].head = .{
                .funcParam = .{
                    .name = name,
                    .typeSpecHead = typeSpecHead,
                },
            };

            var last = paramHead;
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
                    else => return self.reportParseError("Unexpected token {} in function param list.", &.{v(token.tag())}),
                }

                token = self.peekToken();
                start = self.next_pos;
                if (token.tag() != .ident and token.tag() != .type_k) {
                    return self.reportParseError("Expected param identifier.", &.{});
                }

                name = try self.pushIdentNode(start);
                self.advanceToken();

                typeSpecHead = (try self.parseOptNamePath()) orelse cy.NullId;

                const param = try self.pushNode(.funcParam, start);
                self.nodes.items[param].head = .{
                    .funcParam = .{
                        .name = name,
                        .typeSpecHead = typeSpecHead,
                    },
                };
                self.nodes.items[last].next = param;
                last = param;
            }
            return paramHead;
        } else if (token.tag() == .right_paren) {
            self.advanceToken();
            return null;
        } else return self.reportParseError("Unexpected token in function param list.", &.{});
    }

    fn parseFuncReturn(self: *Parser) !?NodeId {
        return self.parseOptNamePath();
    }

    fn parseOptName(self: *Parser) !?NodeId {
        const start = self.next_pos;
        var token = self.peekToken();
        if (token.tag() == .ident) {
            self.advanceToken();
            return try self.pushIdentNode(start);
        } else if (token.tag() == .none_k) {
            self.advanceToken();
            return try self.pushIdentNode(start);
        } else if (token.tag() == .error_k) {
            self.advanceToken();
            return try self.pushIdentNode(start);
        } else if (token.tag() == .type_k) {
            self.advanceToken();
            return try self.pushIdentNode(start);
        } else if (token.tag() == .string) {
            self.advanceToken();
            return try self.pushIdentNode(start);
        }
        return null;
    }

    fn parseOptNamePath(self: *Parser) !?NodeId {
        const first = (try self.parseOptName()) orelse {
            return null;
        };

        var token = self.peekToken();
        if (token.tag() != .dot) {
            return first;
        }
        
        var last = first;
        while (token.tag() == .dot) {
            self.advanceToken();
            const name = (try self.parseOptName()) orelse {
                return self.reportParseError("Expected name.", &.{});
            };
            self.nodes.items[last].next = name;
            last = name;
            token = self.peekToken();
        }
        return first;
    }

    fn parseEnumMember(self: *Parser) !NodeId {
        const start = self.next_pos;
        if (self.peekToken().tag() != .case_k) {
            return self.reportParseError("Expected case keyword.", &.{});
        }
        self.advanceToken();
        if (self.peekToken().tag() != .ident) {
            return self.reportParseError("Expected member identifier.", &.{});
        }
        const name = try self.pushIdentNode(self.next_pos);
        self.advanceToken();

        try self.consumeNewLineOrEnd();

        const field = try self.pushNode(.enumMember, start);
        self.nodes.items[field].head = .{
            .enumMember = .{
                .name = name,
            },
        };
        return field;
    }

    fn parseObjectField(self: *Parser) !?NodeId {
        const start = self.next_pos;

        var token = self.peekToken();
        if (token.tag() != .var_k and token.tag() != .my_k) {
            return null;
        }
        const typed = token.tag() == .var_k;
        self.advanceToken();

        const name = (try self.parseOptName()) orelse {
            return self.reportParseError("Expected field identifier.", &.{});
        };

        var typeSpecHead: cy.NodeId = cy.NullId;
        if (typed) {
            if (try self.parseOptNamePath()) |namePath| {
                try self.consumeNewLineOrEnd();
                typeSpecHead = namePath;
            }
        }

        const field = try self.pushNode(.objectField, start);
        self.nodes.items[field].head = .{
            .objectField = .{
                .name = name,
                .typeSpecHead = typeSpecHead,
                .typed = typed,
            },
        };
        return field;
    }

    fn parseTypeDecl(self: *Parser, modifierHead: cy.NodeId) !NodeId {
        const start = self.next_pos;
        // Assumes first token is the `type` keyword.
        self.advanceToken();

        // Parse name.
        const name = (try self.parseOptName()) orelse {
            return self.reportParseError("Expected type name identifier.", &.{});
        };

        var token = self.peekToken();
        switch (token.tag()) {
            .enum_k => {
                return self.parseEnumDecl(start, name);
            },
            .object_k => {
                return self.parseObjectDecl(start, name, modifierHead);
            },
            else => {
                return self.parseTypeAliasDecl(start, name);
            }
        }
    }

    fn parseTypeAliasDecl(self: *Parser, start: TokenId, name: NodeId) !NodeId {
        const typeSpecHead = (try self.parseOptNamePath()) orelse {
            return self.reportParseError("Expected type specifier.", &.{});
        };

        const id = try self.pushNode(.typeAliasDecl, start);
        self.nodes.items[id].head = .{
            .typeAliasDecl = .{
                .name = name,
                .typeSpecHead = typeSpecHead,
            },
        };

        try self.staticDecls.append(self.alloc, .{
            .declT = .typeAlias,
            .nodeId = id,
            .data = undefined,
        });

        return id;
    }

    fn parseEnumDecl(self: *Parser, start: TokenId, name: NodeId) !NodeId {
        // Assumes first token is the `enum` keyword.
        self.advanceToken();

        var token = self.peekToken();
        if (token.tag() == .colon) {
            self.advanceToken();
        } else {
            return self.reportParseError("Expected colon.", &.{});
        }

        const reqIndent = try self.parseFirstChildIndent(self.cur_indent);
        const prevIndent = self.cur_indent;
        self.cur_indent = reqIndent;
        defer self.cur_indent = prevIndent;

        var firstMember = try self.parseEnumMember();
        var lastMember = firstMember;
        var numMembers: u32 = 1;

        while (true) {
            const start2 = self.next_pos;
            const indent = (try self.consumeIndentBeforeStmt()) orelse break;
            if (indent == reqIndent) {
                const id = try self.parseEnumMember();
                self.nodes.items[lastMember].next = id;
                lastMember = id;
                numMembers += 1;
            } else if (try isRecedingIndent(self, prevIndent, reqIndent, indent)) {
                self.next_pos = start2;
                break;
            } else {
                return self.reportParseError("Unexpected indentation.", &.{});
            }
        }
        const id = try self.pushNode(.enumDecl, start);
        self.nodes.items[id].head = .{
            .enumDecl = .{
                .name = name,
                .memberHead = firstMember,
                .numMembers = @intCast(numMembers),
            },
        };
        try self.staticDecls.append(self.alloc, .{
            .declT = .enumT,
            .nodeId = id,
            .data = undefined,
        });
        return id;
    }

    fn pushObjectDecl(self: *Parser, start: TokenId, name: NodeId, modifierHead: NodeId, fieldsHead: NodeId, numFields: u32, funcsHead: NodeId) !NodeId {
        const body = try self.pushNode(.objectDeclBody, start);
        self.nodes.items[body].head = .{
            .objectDeclBody = .{
                .fieldsHead = fieldsHead,
                .funcsHead = funcsHead,
                .numFields = numFields,
            },
        };

        const id = try self.pushNode(.objectDecl, start);
        self.nodes.items[id].head = .{
            .objectDecl = .{
                .name = name,
                .modifierHead = modifierHead,
                .body = body,
            },
        };
        try self.staticDecls.append(self.alloc, .{
            .declT = .object,
            .nodeId = id,
            .data = undefined,
        });
        return id;
    }

    fn parseObjectDecl(self: *Parser, start: TokenId, name: NodeId, modifierHead: cy.NodeId) !NodeId {
        self.inObjectDecl = true;
        defer self.inObjectDecl = false;

        // Assumes first token is the `object` keyword.
        self.advanceToken();

        // Parse struct name.
        var token = self.peekToken();
        if (token.tag() == .colon) {
            self.advanceToken();
        } else {
            // Only declaration. No members.
            return self.pushObjectDecl(start, name, modifierHead, cy.NullId, 0, cy.NullId);
        }

        const reqIndent = try self.parseFirstChildIndent(self.cur_indent);
        const prevIndent = self.cur_indent;
        self.cur_indent = reqIndent;
        defer self.cur_indent = prevIndent;

        var firstField = (try self.parseObjectField()) orelse NullId;
        var numFields: u32 = 1;
        if (firstField != NullId) {
            var lastField = firstField;

            while (true) {
                const start2 = self.next_pos;
                const indent = (try self.consumeIndentBeforeStmt()) orelse {
                    return self.pushObjectDecl(start, name, modifierHead, firstField, numFields, NullId);
                };
                if (indent == reqIndent) {
                    const id = (try self.parseObjectField()) orelse break;
                    numFields += 1;
                    self.nodes.items[lastField].next = id;
                    lastField = id;
                } else if (try isRecedingIndent(self, prevIndent, reqIndent, indent)) {
                    self.next_pos = start2;
                    return self.pushObjectDecl(start, name, modifierHead, firstField, numFields, NullId);
                } else {
                    return self.reportParseError("Unexpected indentation.", &.{});
                }
            }
        }

        token = self.peekToken();
        const firstFunc = try self.parseStatement();
        var nodeT = self.nodes.items[firstFunc].node_t;
        if (nodeT == .funcDecl or nodeT == .funcDeclInit) {
            var lastFunc = firstFunc;

            while (true) {
                const start2 = self.next_pos;
                const indent = (try self.consumeIndentBeforeStmt()) orelse break;
                if (indent == reqIndent) {
                    token = self.peekToken();
                    const func = try self.parseStatement();
                    nodeT = self.nodes.items[func].node_t;
                    if (nodeT == .funcDecl or nodeT == .funcDeclInit) {
                        self.nodes.items[lastFunc].next = func;
                        lastFunc = func;
                    } else return self.reportParseError("Expected function.", &.{});
                } else if (try isRecedingIndent(self, prevIndent, reqIndent, indent)) {
                    self.next_pos = start2;
                    break;
                } else {
                    return self.reportParseError("Unexpected indentation.", &.{});
                }
            }
            return self.pushObjectDecl(start, name, modifierHead, firstField, numFields, firstFunc);
        } else {
            return self.reportParseErrorAt("Expected function.", &.{}, self.nodes.items[firstFunc].start_token);
        }
    }

    fn parseFuncDecl(self: *Parser, modifierHead: cy.NodeId) !NodeId {
        const start = self.next_pos;
        // Assumes first token is the `func` keyword.
        self.advanceToken();

        // Parse function name.
        const name = (try self.parseOptNamePath()) orelse {
            return self.reportParseError("Expected function name identifier.", &.{});
        };

        var token = self.peekToken();
        if (token.tag() == .left_paren) {
            const paramHead = try self.parseFuncParams();
            const ret = try self.parseFuncReturn();

            const nameToken = self.tokens.items[self.nodes.items[name].start_token];
            const nameStr = self.src[nameToken.pos()..nameToken.data.end_pos];
            const block = &self.blockStack.items[self.blockStack.items.len-1];
            try block.vars.put(self.alloc, nameStr, {});

            token = self.peekToken();
            if (token.tag() == .colon) {
                self.advanceToken();

                try self.pushBlock();
                const res = try self.parseSingleOrIndentedBodyStmts();
                _ = self.popBlock();

                const header = try self.pushNode(.funcHeader, start);
                self.nodes.items[header].head = .{
                    .funcHeader = .{
                        .name = name,
                        .paramHead = paramHead orelse cy.NullId,
                        .ret = ret orelse cy.NullId,
                    },
                };
                self.nodes.items[header].next = modifierHead;

                const id = try self.pushNode(.funcDecl, start);
                self.nodes.items[id].head = .{
                    .func = .{
                        .header = header,
                        .bodyHead = res.first,
                    },
                };

                if (!self.inObjectDecl) {
                    try self.staticDecls.append(self.alloc, .{
                        .declT = .func,
                        .nodeId = id,
                        .data = undefined,
                    });
                }
                return id;
            } else {
                // Just a declaration, no body.
                const header = try self.pushNode(.funcHeader, start);
                self.nodes.items[header].head = .{
                    .funcHeader = .{
                        .name = name,
                        .paramHead = paramHead orelse cy.NullId,
                        .ret = ret orelse cy.NullId,
                    },
                };
                self.nodes.items[header].next = modifierHead;

                const id = try self.pushNode(.funcDeclInit, start);
                self.nodes.items[id].head = .{
                    .func = .{
                        .header = header,
                        .bodyHead = cy.NullId,
                    },
                };

                if (!self.inObjectDecl) {
                    try self.staticDecls.append(self.alloc, .{
                        .declT = .funcInit,
                        .nodeId = id,
                        .data = undefined,
                    });
                }
                return id;
            }
        } else {
            return self.reportParseError("Expected left paren.", &.{});
        }
    }

    fn parseElseStmt(self: *Parser, outNumElseBlocks: *u32) anyerror!NodeId {
        const save = self.next_pos;
        const indent = try self.consumeIndentBeforeStmt();
        if (indent != self.cur_indent) {
            self.next_pos = save;
            return NullId;
        }

        var token = self.peekToken();
        if (token.tag() != .else_k) {
            self.next_pos = save;
            return NullId;
        }

        const elseBlock = try self.pushNode(.elseBlock, self.next_pos);
        outNumElseBlocks.* += 1;
        self.advanceToken();

        token = self.peekToken();
        if (token.tag() == .colon) {
            // else block.
            self.advanceToken();

            const res = try self.parseSingleOrIndentedBodyStmts();
            self.nodes.items[elseBlock].head = .{
                .elseBlock = .{
                    .bodyHead = res.first,
                    .cond = NullId,
                },
            };
            return elseBlock;
        } else {
            // else if block.
            const cond = (try self.parseExpr(.{})) orelse {
                return self.reportParseError("Expected else if condition.", &.{});
            };
            token = self.peekToken();
            if (token.tag() == .colon) {
                self.advanceToken();

                const res = try self.parseSingleOrIndentedBodyStmts();
                self.nodes.items[elseBlock].head = .{
                    .elseBlock = .{
                        .bodyHead = res.first,
                        .cond = cond,
                    },
                };

                const nested_else = try self.parseElseStmt(outNumElseBlocks);
                if (nested_else != NullId) {
                    self.nodes.items[elseBlock].next = nested_else;
                }
                return elseBlock;
            } else {
                return self.reportParseError("Expected colon after else if condition.", &.{});
            }
        }
    }

    fn parseSwitchStatement(self: *Parser) !NodeId {
        const start = self.next_pos;
        // Assumes first token is the `switch` keyword.
        self.advanceToken();

        const expr = (try self.parseExpr(.{})) orelse {
            return self.reportParseError("Expected switch expression.", &.{});
        };
        if (self.peekToken().tag() != .colon) {
            return self.reportParseError("Expected colon after if condition.", &.{});
        }
        self.advanceToken();

        var indent = (try self.consumeIndentBeforeStmt()) orelse {
            return self.reportParseError("Expected case or else block.", &.{});
        };
        if (self.cur_indent != indent) {
            return self.reportParseError("Expected case or else block.", &.{});
        }

        var firstCase = (try self.parseCaseBlock()) orelse {
            return self.reportParseError("Expected case or else block.", &.{});
        };
        var lastCase = firstCase;
        var numCases: u32 = 1;

        // Parse body statements until no more case blocks indentation recedes.
        while (true) {
            const save = self.next_pos;
            indent = (try self.consumeIndentBeforeStmt()) orelse break;
            if (self.cur_indent == indent) {
                const case = (try self.parseCaseBlock()) orelse {
                    self.next_pos = save;
                    break;
                };
                numCases += 1;
                self.nodes.items[lastCase].next = case;
                lastCase = case;
            } else {
                self.next_pos = save;
                break;
            }
        }

        const switchBlock = try self.pushNode(.switchBlock, start);
        self.nodes.items[switchBlock].head = .{
            .switchBlock = .{
                .expr = expr,
                .caseHead = firstCase,
                .numCases = @intCast(numCases),
            },
        };
        return switchBlock;
    }

    fn parseTryStmt(self: *Parser) !NodeId {
        const start = self.next_pos;
        // Assumes first tokens are `try` and `:`.
        self.advanceToken();
        self.advanceToken();

        const stmt = try self.pushNode(.tryStmt, start);

        const tryStmts = try self.parseSingleOrIndentedBodyStmts();

        const indent = try self.consumeIndentBeforeStmt();
        if (indent != self.cur_indent) {
            return self.reportParseError("Expected catch block.", &.{});
        }

        var token = self.peekToken();
        if (token.tag() != .catch_k) {
            return self.reportParseError("Expected catch block.", &.{});
        }
        self.advanceToken();

        token = self.peekToken();
        var errorVar: NodeId = cy.NullId;
        if (token.tag() == .ident) {
            errorVar = try self.pushIdentNode(self.next_pos);
            self.advanceToken();
        }

        token = self.peekToken();
        if (token.tag() != .colon) {
            return self.reportParseError("Expected colon.", &.{});
        }
        self.advanceToken();

        const catchStmts = try self.parseSingleOrIndentedBodyStmts();

        self.nodes.items[stmt].head = .{
            .tryStmt = .{
                .tryFirstStmt = tryStmts.first,
                .errorVar = errorVar,
                .catchFirstStmt = catchStmts.first,
            },
        };
        return stmt;
    }

    fn parseIfStatement(self: *Parser) !NodeId {
        const start = self.next_pos;
        // Assumes first token is the `if` keyword.
        self.advanceToken();

        const ifCond = (try self.parseExpr(.{})) orelse {
            return self.reportParseError("Expected if condition.", &.{});
        };

        var token = self.peekToken();
        if (token.tag() != .colon) {
            return self.reportParseError("Expected colon after if condition.", &.{});
        }
        self.advanceToken();

        const ifStmt = try self.pushNode(.ifStmt, start);

        var res = try self.parseSingleOrIndentedBodyStmts();

        var numElseBlocks: u32 = 0;
        const elseBlock = try self.parseElseStmt(&numElseBlocks);

        self.nodes.items[ifStmt].head = .{
            .ifStmt = .{
                .cond = ifCond,
                .bodyHead = res.first,
                .elseHead = elseBlock,
                .numElseBlocks = @intCast(numElseBlocks),
            },
        };
        return ifStmt;
    }

    fn parseImportStmt(self: *Parser) !NodeId {
        const start = self.next_pos;
        // Assumes first token is the `import` keyword.
        self.advanceToken();

        var token = self.peekToken();
        if (token.tag() == .ident) {
            const ident = try self.pushIdentNode(self.next_pos);
            self.advanceToken();

            token = self.peekToken();
            var spec: cy.NodeId = cy.NullId;
            if (token.tag() != .new_line) {
                spec = (try self.parseExpr(.{})) orelse {
                    return self.reportParseError("Expected import specifier.", &.{});
                };
                const specN = self.nodes.items[spec];
                if (specN.node_t == .string) {
                    try self.consumeNewLineOrEnd();
                } else {
                    return self.reportParseError("Expected import specifier to be a string. {}", &.{fmt.v(specN.node_t)});
                }
            } else {
                self.advanceToken();
            }

            const import = try self.pushNode(.importStmt, start);
            self.nodes.items[import].head = .{
                .left_right = .{
                    .left = ident,
                    .right = spec,
                },
            };

            try self.staticDecls.append(self.alloc, .{
                .declT = .import,
                .nodeId = import,
                .data = undefined,
            });
            return import;
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
            const res = try self.parseSingleOrIndentedBodyStmts();

            const whileStmt = try self.pushNode(.whileInfStmt, start);
            self.nodes.items[whileStmt].head = .{
                .child_head = res.first,
            };
            return whileStmt;
        }

        // Parse next token as expression.
        const expr_id = (try self.parseExpr(.{})) orelse {
            return self.reportParseError("Expected condition expression.", &.{});
        };

        token = self.peekToken();
        if (token.tag() == .colon) {
            self.advanceToken();
            const res = try self.parseSingleOrIndentedBodyStmts();

            const whileStmt = try self.pushNode(.whileCondStmt, start);
            self.nodes.items[whileStmt].head = .{
                .whileCondStmt = .{
                    .cond = expr_id,
                    .bodyHead = res.first,
                },
            };
            return whileStmt;
        } else if (token.tag() == .capture) {
            self.advanceToken();
            token = self.peekToken();
            const ident = (try self.parseExpr(.{})) orelse {
                return self.reportParseError("Expected ident.", &.{});
            };
            if (self.nodes.items[ident].node_t != .ident) {
                return self.reportParseError("Expected ident.", &.{});
            }
            token = self.peekToken();
            if (token.tag() != .colon) {
                return self.reportParseError("Expected :.", &.{});
            }
            self.advanceToken();
            const res = try self.parseSingleOrIndentedBodyStmts();

            const whileStmt = try self.pushNode(.whileOptStmt, start);
            self.nodes.items[whileStmt].head = .{
                .whileOptStmt = .{
                    .opt = expr_id,
                    .bodyHead = res.first,
                    .some = ident,
                },
            };
            return whileStmt;
        } else {
            return self.reportParseError("Expected :.", &.{});
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
            const res = try self.parseSingleOrIndentedBodyStmts();

            const forIterHeader = try self.pushNode(.forIterHeader, start);
            self.nodes.items[forIterHeader].head = .{ .forIterHeader = .{
                .iterable = expr_id,
                .eachClause = cy.NullId,
                .count = cy.NullId,
            }};

            const forStmt = try self.pushNode(.forIterStmt, start);
            self.nodes.items[forStmt].head = .{
                .forIterStmt = .{
                    .header = forIterHeader,
                    .bodyHead = res.first,
                },
            };
            return forStmt;
        } else if (token.tag() == .dot_dot or token.tag() == .minusDotDot) {
            self.advanceToken();
            const right_range_expr = (try self.parseExpr(.{})) orelse {
                return self.reportParseError("Expected right range expression.", &.{});
            };
            const range_clause = try self.pushNode(.range_clause, expr_pos);
            self.nodes.items[range_clause].head = .{
                .forRange = .{
                    .left = expr_id,
                    .right = right_range_expr,
                    .increment = token.tag() == .dot_dot,
                },
            };

            token = self.peekToken();
            if (token.tag() == .colon) {
                self.advanceToken();

                const res = try self.parseSingleOrIndentedBodyStmts();

                const for_stmt = try self.pushNode(.forRangeStmt, start);
                self.nodes.items[for_stmt].head = .{
                    .forRangeStmt = .{
                        .range_clause = range_clause,
                        .body_head = res.first,
                        .eachClause = NullId,
                    },
                };
                return for_stmt;
            } else if (token.tag() == .capture) {
                self.advanceToken();

                token = self.peekToken();
                const ident = (try self.parseExpr(.{})) orelse {
                    return self.reportParseError("Expected ident.", &.{});
                };
                if (self.nodes.items[ident].node_t != .ident) {
                    return self.reportParseErrorAt("Expected ident.", &.{}, token.pos());
                }
                token = self.peekToken();
                if (token.tag() != .colon) {
                    return self.reportParseError("Expected :.", &.{});
                }
                self.advanceToken();

                const res = try self.parseSingleOrIndentedBodyStmts();

                const for_stmt = try self.pushNode(.forRangeStmt, start);
                self.nodes.items[for_stmt].head = .{
                    .forRangeStmt = .{
                        .range_clause = range_clause,
                        .body_head = res.first,
                        .eachClause = ident,
                    },
                };
                return for_stmt;
            } else {
                return self.reportParseError("Expected :.", &.{});
            }
        } else if (token.tag() == .capture) {
            self.advanceToken();
            token = self.peekToken();
            var eachClause: NodeId = undefined;
            if (token.tag() == .left_bracket) {
                eachClause = try self.parseSeqDestructure();
            } else {
                eachClause = (try self.parseExpr(.{})) orelse {
                    return self.reportParseError("Expected each clause.", &.{});
                };
            }

            // Optional count var.
            var count: NodeId = cy.NullId;
            if (self.peekToken().tag() == .comma) {
                self.advanceToken();
                count = (try self.parseExpr(.{})) orelse {
                    return self.reportParseError("Expected count declaration.", &.{});
                };
            }

            if (self.peekToken().tag() == .colon) {
                self.advanceToken();
            } else {
                return self.reportParseError("Expected :.", &.{});
            }

            const res = try self.parseSingleOrIndentedBodyStmts();

            const forIterHeader = try self.pushNode(.forIterHeader, start);
            self.nodes.items[forIterHeader].head = .{ .forIterHeader = .{
                .iterable = expr_id,
                .eachClause = eachClause,
                .count = count,
            }};

            const forStmt = try self.pushNode(.forIterStmt, start);
            self.nodes.items[forStmt].head = .{
                .forIterStmt = .{
                    .header = forIterHeader,
                    .bodyHead = res.first,
                },
            };
            return forStmt;
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
        const res = try self.parseIndentedBodyStatements();
        _ = self.popBlock();
        
        const id = try self.pushNode(.label_decl, start);
        self.nodes.items[id].head = .{
            .left_right = .{
                .left = name,
                .right = res.first,
            },
        };
        return id;
    }

    fn parseCaseBlock(self: *Parser) !?NodeId {
        const start = self.next_pos;
        var token = self.peekToken();
        var firstCond: NodeId = undefined;
        var isElse: bool = false;
        var numConds: u32 = 0;
        var bodyExpr: bool = false;
        if (token.tag() == .case_k) {
            self.advanceToken();
            firstCond = (try self.parseTightTermExpr()) orelse {
                return self.reportParseError("Expected case condition.", &.{});
            };
            numConds += 1;

            var lastCond = firstCond;
            while (true) {
                token = self.peekToken();
                if (token.tag() == .colon) {
                    self.advanceToken();
                    break;
                } else if (token.tag() == .equal_greater) {
                    self.advanceToken();
                    bodyExpr = true;
                    break;
                } else if (token.tag() == .comma) {
                    self.advanceToken();
                    self.consumeWhitespaceTokens();
                    const cond = (try self.parseTightTermExpr()) orelse {
                        return self.reportParseError("Expected case condition.", &.{});
                    };
                    self.nodes.items[lastCond].next = cond;
                    lastCond = cond;
                    numConds += 1;
                } else {
                    return self.reportParseError("Expected comma or colon.", &.{});
                }
            }
        } else if (token.tag() == .else_k) {
            self.advanceToken();
            isElse = true;
            firstCond = cy.NullId;

            if (self.peekToken().tag() == .colon) {
                self.advanceToken();
            } else if (self.peekToken().tag() == .equal_greater) {
                self.advanceToken();
                bodyExpr = true;
            } else {
                return self.reportParseError("Expected colon or `=>`.", &.{});
            }
        } else return null;

        // Parse body.
        var bodyHead: cy.NodeId = undefined;
        if (bodyExpr) {
            bodyHead = (try self.parseExpr(.{})) orelse {
                return self.reportParseError("Expected expression.", &.{});
            };
        } else {
            const res = try self.parseSingleOrIndentedBodyStmts();
            bodyHead = res.first;
        }

        const case = try self.pushNode(.caseBlock, start);
        self.nodes.items[case].head = .{
            .caseBlock = .{
                .condHead = firstCond,
                .bodyHead = bodyHead,
                .numConds = @intCast(numConds),
                .isElseCase = isElse,
                .bodyIsExpr = bodyExpr,
            },
        };
        return case;
    }

    fn parseStatement(self: *Parser) anyerror!NodeId {
        var token = self.peekToken();
        switch (token.tag()) {
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
                _ = start;
                self.advanceToken();
                token = self.peekToken();

                if (token.tag() == .ident) {
                    const identName = self.src[token.pos()..token.data.end_pos];

                    const modifier = try self.pushNode(.annotation, self.next_pos);
                    const atype = annotations.get(identName) orelse .custom;
                    self.nodes.items[modifier].head = .{
                        .annotation = .{
                            .type = atype,
                        }
                    };
                    self.advanceToken();
                    self.consumeWhitespaceTokens();

                    if (self.peekToken().tag() == .func_k) {
                        return try self.parseFuncDecl(modifier);
                    } else if (self.peekToken().tag() == .var_k) {
                        return try self.parseVarDecl(modifier, true);
                    } else if (self.peekToken().tag() == .my_k) {
                        return try self.parseVarDecl(modifier, false);
                    } else if (self.peekToken().tag() == .type_k) {
                        return try self.parseTypeDecl(modifier);
                    } else {
                        return self.reportParseError("Expected declaration statement.", &.{});
                    }

                } else {
                    return self.reportParseError("Expected ident after @.", &.{});
                }
            },
            .pound => {
                const start = self.next_pos;
                self.advanceToken();
                token = self.peekToken();

                if (token.tag() == .ident) {
                    const ident = try self.pushIdentNode(self.next_pos);
                    self.advanceToken();

                    if (self.peekToken().tag() != .left_paren) {
                        return self.reportParseError("Expected ( after ident.", &.{});
                    }

                    const callExpr = try self.parseCallExpression(ident);
                    try self.consumeNewLineOrEnd();

                    const stmt = try self.pushNode(.comptimeStmt, start);
                    self.nodes.items[stmt].head = .{
                        .comptimeStmt = .{
                            .expr = callExpr,
                        },
                    };
                    return stmt;
                } else {
                    return self.reportParseError("Expected ident after #.", &.{});
                }
            },
            .type_k => {
                return try self.parseTypeDecl(cy.NullId);
            },
            .func_k => {
                return try self.parseFuncDecl(cy.NullId);
            },
            .if_k => {
                return try self.parseIfStatement();
            },
            .try_k => {
                if (self.peekTokenAhead(1).tag() == .colon) {
                    return try self.parseTryStmt();
                }
            },
            .switch_k => {
                return try self.parseSwitchStatement();
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
                return try self.parseReturnStatement();
            },
            .var_k => {
                return try self.parseVarDecl(cy.NullId, true);
            },
            .my_k => {
                return try self.parseVarDecl(cy.NullId, false);
            },
            else => {},
        }
        if (try self.parseExprOrAssignStatement()) |id| {
            return id;
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
            self.last_err_pos = @intCast(self.src.len);
        } else {
            self.last_err_pos = self.tokens.items[tokenPos].pos();
        }
        return error.ParseError;
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
        return self.reportParseError("Expected end of line or file. Got {}.", &.{v(tag)});
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

    fn parseSeqDestructure(self: *Parser) !NodeId {
        const start = self.next_pos;
        // Assume first token is left bracket.
        self.advanceToken();

        var lastEntry: NodeId = undefined;
        var firstEntry: NodeId = NullId;
        var numArgs: u32 = 0;
        outer: {
            self.consumeWhitespaceTokens();
            var token = self.peekToken();

            if (token.tag() == .right_bracket) {
                // Empty.
                return self.reportParseError("Expected at least one identifier.", &.{});
            } else {
                firstEntry = (try self.parseExpr(.{})) orelse {
                    return self.reportParseError("Expected array item.", &.{});
                };
                if (self.nodes.items[firstEntry].node_t != .ident) {
                    return self.reportParseError("Expected ident.", &.{});
                }
                lastEntry = firstEntry;
                numArgs += 1;
            }

            while (true) {
                self.consumeWhitespaceTokens();
                token = self.peekToken();
                if (token.tag() == .comma) {
                    self.advanceToken();
                    if (self.peekToken().tag() == .new_line) {
                        self.advanceToken();
                        self.consumeWhitespaceTokens();
                    }
                } else if (token.tag() == .right_bracket) {
                    break :outer;
                }

                token = self.peekToken();
                if (token.tag() == .right_bracket) {
                    break :outer;
                } else {
                    const ident = (try self.parseExpr(.{})) orelse {
                        return self.reportParseError("Expected array item.", &.{});
                    };
                    if (self.nodes.items[ident].node_t != .ident) {
                        return self.reportParseError("Expected ident.", &.{});
                    }
                    self.nodes.items[lastEntry].next = ident;
                    lastEntry = ident;
                    numArgs += 1;
                }
            }
        }

        const seqDestr = try self.pushNode(.seqDestructure, start);
        self.nodes.items[seqDestr].head = .{
            .seqDestructure = .{
                .head = firstEntry,
                .numArgs = @intCast(numArgs),
            },
        };

        // Parse closing bracket.
        const token = self.peekToken();
        if (token.tag() == .right_bracket) {
            self.advanceToken();
            return seqDestr;
        } else return self.reportParseError("Expected closing bracket.", &.{});
    }

    fn parseBracketLiteral(self: *Parser) !NodeId {
        const start = self.next_pos;
        // Assume first token is left bracket.
        self.advanceToken();

        // Check for empty map literal.
        if (self.peekToken().tag() == .colon) {
            self.advanceToken();
            if (self.peekToken().tag() != .right_bracket) {
                return self.reportParseError("Expected closing bracket.", &.{});
            }

            self.advanceToken();
            const record = try self.pushNode(.recordLiteral, start);
            self.nodes.items[record].head = .{ .recordLiteral = .{
                .argHead = cy.NullId,
                .numArgs = 0,
            }};
            return record;
        } else if (self.peekToken().tag() == .right_bracket) {
            self.advanceToken();
            const array = try self.pushNode(.arrayLiteral, start);
            self.nodes.items[array].head = .{ .arrayLiteral = .{
                .argHead = cy.NullId,
                .numArgs = 0,
            }};
            return array;
        }

        // Assume there is at least one argument.

        // If `typeName` is set, then this is a object initializer.
        var typeName: NodeId = cy.NullId;

        self.consumeWhitespaceTokens();

        const res = try self.parseEmptyTypeInitOrBracketArg();
        if (res.isEmptyTypeInit) {
            // Empty object init. Can assume arg is the type name.
            const dataLit = try self.pushNode(.recordLiteral, start);
            self.nodes.items[dataLit].head = .{ .recordLiteral = .{
                .argHead = cy.NullId,
                .numArgs = 0,
            }};

            const initN = try self.pushNode(.objectInit, start);
            self.nodes.items[initN].head = .{
                .objectInit = .{
                    .name = res.res,
                    .initializer = dataLit,
                },
            };

            return initN;
        }

        var arg = res.res;
        var isRecordArg = res.isRecordArg;

        self.consumeWhitespaceTokens();
        var token = self.peekToken();
        if (token.tag() == .right_bracket) {
            // One arg literal.
            self.advanceToken();
            if (isRecordArg) {
                const record = try self.pushNode(.recordLiteral, start);
                self.nodes.items[record].head = .{ .recordLiteral = .{
                    .argHead = arg,
                    .numArgs = 1,
                }};
                return record;
            } else {
                const array = try self.pushNode(.arrayLiteral, start);
                self.nodes.items[array].head = .{ .arrayLiteral = .{
                    .argHead = arg,
                    .numArgs = 1,
                }};
                return array;
            }
        } else if (token.tag() == .comma) {
            // Continue.
        } else {
            if (!isRecordArg) {
                // Assume object initializer. `arg` becomes typename. Parse arg again.
                typeName = arg;
                arg = try self.parseBracketArg(&isRecordArg) orelse return error.Unexpected;
            } else {
                return self.reportParseError("Expected comma or closing bracket.", &.{});
            }
        }

        const first = arg;
        var last: NodeId = first;
        var numArgs: u32 = 1;
        var isRecord: bool = isRecordArg;

        while (true) {
            self.consumeWhitespaceTokens();
            token = self.peekToken();
            if (token.tag() == .comma) {
                self.advanceToken();
                if (self.peekToken().tag() == .new_line) {
                    self.advanceToken();
                    self.consumeWhitespaceTokens();
                }
            } else if (token.tag() == .right_bracket) {
                break;
            } else {
                return self.reportParseErrorAt("Expected comma or closing bracket.", &.{}, self.next_pos);
            }

            if (try self.parseBracketArg(&isRecordArg)) |entry| {
                // Check that arg kind is the same.
                if (isRecord != isRecordArg) {
                    const argStart = self.nodes.items[entry].start_token;
                    if (isRecord) {
                        return self.reportParseErrorAt("Expected key/value pair.", &.{}, argStart);
                    } else {
                        return self.reportParseErrorAt("Expected data element.", &.{}, argStart);
                    }
                }
                self.nodes.items[last].next = entry;
                last = entry;
                numArgs += 1;
            } else {
                break;
            }
        }

        // Parse closing bracket.
        if (self.peekToken().tag() != .right_bracket) {
            return self.reportParseError("Expected closing bracket.", &.{});
        }
        self.advanceToken();


        if (typeName == cy.NullId) {
            if (isRecord) {
                const record = try self.pushNode(.recordLiteral, start);
                self.nodes.items[record].head = .{ .recordLiteral = .{
                    .argHead = first,
                    .numArgs = @intCast(numArgs),
                }};
                return record;
            } else {
                const array = try self.pushNode(.arrayLiteral, start);
                self.nodes.items[array].head = .{ .arrayLiteral = .{
                    .argHead = first,
                    .numArgs = @intCast(numArgs),
                }};
                return array;
            }
        } else {
            if (!isRecord) {
                return self.reportParseError("Expected map literal for object initializer.", &.{});
            }

            const record = try self.pushNode(.recordLiteral, start);
            self.nodes.items[record].head = .{ .recordLiteral = .{
                .argHead = first,
                .numArgs = @intCast(numArgs),
            }};

            const initN = try self.pushNode(.objectInit, start);
            self.nodes.items[initN].head = .{
                .objectInit = .{
                    .name = typeName,
                    .initializer = record,
                },
            };
            return initN;
        }
    }

    const EmptyTypeInitOrBracketArg = struct {
        res: cy.NodeId,
        isEmptyTypeInit: bool,
        isRecordArg: bool,
    };

    fn parseEmptyTypeInitOrBracketArg(self: *Parser) !EmptyTypeInitOrBracketArg {
        const start = self.next_pos;

        const arg = (try self.parseExpr(.{ .parseShorthandCallExpr = false })) orelse {
            return self.reportParseError("Expected data argument.", &.{});
        };

        self.consumeWhitespaceTokens();

        if (self.peekToken().tag() != .colon) {
            return EmptyTypeInitOrBracketArg{
                .res = arg,
                .isEmptyTypeInit = false,
                .isRecordArg = false,
            };
        }
        self.advanceToken();

        self.consumeWhitespaceTokens();
        if (self.peekToken().tag() == .right_bracket) {
            self.advanceToken();
            return EmptyTypeInitOrBracketArg{
                .res = arg,
                .isEmptyTypeInit = true,
                .isRecordArg = true,
            };
        }

        // Parse key value pair.
        switch (self.nodes.items[arg].node_t) {
            .ident,
            .string,
            .number => {},
            else => {
                return self.reportParseError("Expected map key.", &.{});
            }
        }

        const val = (try self.parseExpr(.{})) orelse {
            return self.reportParseError("Expected map value.", &.{});
        };
        const pair = try self.pushNode(.keyValue, start);
        self.nodes.items[pair].head = .{
            .keyValue = .{
                .left = arg,
                .right = val,
            }
        };
        return EmptyTypeInitOrBracketArg{
            .res = pair,
            .isEmptyTypeInit = false,
            .isRecordArg = true,
        };
    }

    fn parseBracketArg(self: *Parser, outIsPair: *bool) !?NodeId {
        const start = self.next_pos;

        if (self.peekToken().tag() == .right_bracket) {
            return null;
        }

        const arg = (try self.parseTightTermExpr()) orelse {
            return self.reportParseError("Expected data argument.", &.{});
        };

        if (self.peekToken().tag() != .colon) {
            outIsPair.* = false;
            return arg;
        }
        self.advanceToken();

        // Parse key value pair.
        switch (self.nodes.items[arg].node_t) {
            .ident,
            .string,
            .number => {},
            else => {
                return self.reportParseError("Expected map key.", &.{});
            }
        }

        const val = (try self.parseExpr(.{})) orelse {
            return self.reportParseError("Expected map value.", &.{});
        };
        const pair = try self.pushNode(.keyValue, start);
        self.nodes.items[pair].head = .{
            .keyValue = .{
                .left = arg,
                .right = val,
            }
        };
        outIsPair.* = true;
        return pair;
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
        const callExpr = try self.pushNode(.callExpr, expr_start);

        var has_named_arg = false;
        var numArgs: u32 = 0;
        var first: NodeId = NullId;
        inner: {
            first = (try self.parseCallArg()) orelse {
                break :inner;
            };
            numArgs += 1;
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
                numArgs += 1;
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
            self.nodes.items[callExpr].head = .{
                .callExpr = .{
                    .callee = left_id,
                    .arg_head = first,
                    .has_named_arg = has_named_arg,
                    .numArgs = @intCast(numArgs),
                },
            };
            return callExpr;
        } else return self.reportParseError("Expected closing parenthesis.", &.{});
    }

    /// Assumes first arg exists.
    fn parseNoParenCallExpression(self: *Parser, left_id: NodeId) !NodeId {
        const expr_start = self.nodes.items[left_id].start_token;
        const callExpr = try self.pushNode(.callExpr, expr_start);

        const firstArg = (try self.parseTightTermExpr()) orelse {
            return self.reportParseError("Expected call arg.", &.{});
        };
        var numArgs: u32 = 1;
        var last_arg_id = firstArg;

        while (true) {
            const token = self.peekToken();
            switch (token.tag()) {
                .new_line => break,
                .none => break,
                else => {
                    const arg = (try self.parseTightTermExpr()) orelse {
                        return self.reportParseError("Expected call arg.", &.{});
                    };
                    self.nodes.items[last_arg_id].next = arg;
                    last_arg_id = arg;
                    numArgs += 1;
                },
            }
        }

        self.nodes.items[callExpr].head = .{
            .callExpr = .{
                .callee = left_id,
                .arg_head = firstArg,
                .has_named_arg = false,
                .numArgs = @intCast(numArgs),
            },
        };
        return callExpr;
    }

    /// Parses the right expression of a BinaryExpression.
    fn parseRightExpression(self: *Parser, left_op: cy.ast.BinaryExprOp) anyerror!NodeId {
        var start = self.next_pos;
        var token = self.peekToken();

        switch (token.tag()) {
            .none => {
                return self.reportParseError("Expected right operand.", &.{});
            },
            .indent,
            .new_line => {
                self.advanceToken();
                self.consumeWhitespaceTokens();
                start = self.next_pos;
                token = self.peekToken();
                if (token.tag() == .none) {
                    return self.reportParseError("Expected right operand.", &.{});
                }
            },
            else => {},
        }

        const expr_id = try self.parseTermExpr();

        // Check if next token is an operator with higher precedence.
        token = self.peekToken();

        var rightOp: cy.ast.BinaryExprOp = undefined;
        switch (token.tag()) {
            .operator => rightOp = toBinExprOp(token.data.operator_t),
            .and_k => rightOp = .and_op,
            .or_k => rightOp = .or_op,
            else => return expr_id,
        }

        const op_prec = getBinOpPrecedence(left_op);
        const right_op_prec = getBinOpPrecedence(rightOp);
        if (right_op_prec > op_prec) {
            // Continue parsing right.
            _ = self.consumeToken();
            start = self.next_pos;
            const right_id = try self.parseRightExpression(rightOp);

            const binExpr = try self.pushNode(.binExpr, start);
            self.nodes.items[binExpr].head = .{
                .binExpr = .{
                    .left = expr_id,
                    .right = right_id,
                    .op = rightOp,
                },
            };

            // Before returning the expr, perform left recursion if the op prec greater than the starting op.
            // eg. a + b * c * d
            //         ^ parseRightExpression starts here
            // Returns ((b * c) * d).
            // eg. a < b * c - d
            //         ^ parseRightExpression starts here
            // Returns ((b * c) - d).
            var left = binExpr;
            while (true) {
                token = self.peekToken();

                var rightOp2: cy.ast.BinaryExprOp = undefined;
                switch (token.tag()) {
                    .operator => rightOp2 = toBinExprOp(token.data.operator_t),
                    .and_k => rightOp2 = .and_op,
                    .or_k => rightOp2 = .or_op,
                    else => return left,
                }
                const right2_op_prec = getBinOpPrecedence(rightOp2);
                if (right2_op_prec > op_prec) {
                    self.advanceToken();
                    const rightExpr = try self.parseRightExpression(rightOp);
                    const newBinExpr = try self.pushNode(.binExpr, start);
                    self.nodes.items[newBinExpr].head = .{
                        .binExpr = .{
                            .left = left,
                            .right = rightExpr,
                            .op = rightOp2,
                        },
                    };
                    left = newBinExpr;
                    continue;
                } else {
                    return left;
                }
            }
        }
        return expr_id;
    }

    fn isVarDeclaredFromScope(self: *Parser, name: []const u8) bool {
        var i = self.blockStack.items.len;
        while (i > 0) {
            i -= 1;
            if (self.blockStack.items[i].vars.contains(name)) {
                return true;
            }
        }
        return false;
    }

    fn parseCondExpr(self: *Parser, cond: NodeId, start: u32) !NodeId {
        // Assume `?`.
        self.advanceToken();

        const res = try self.pushNode(.condExpr, start);

        const body = (try self.parseExpr(.{})) orelse {
            return self.reportParseError("Expected conditional true expression.", &.{});
        };
        self.nodes.items[res].head = .{
            .condExpr = .{
                .cond = cond,
                .bodyExpr = body,
                .elseExpr = NullId,
            },
        };

        const token = self.peekToken();
        if (token.tag() == .else_k) {
            self.advanceToken();

            const elseExpr = (try self.parseExpr(.{})) orelse {
                return self.reportParseError("Expected else body.", &.{});
            };
            self.nodes.items[res].head.condExpr.elseExpr = elseExpr;
        }
        return res;
    }

    /// A string template begins and ends with .templateString token.
    /// Inside the template, two template expressions can be adjacent to each other.
    fn parseStringTemplate(self: *Parser) !NodeId {
        const start = self.next_pos;

        const id = try self.pushNode(.stringTemplate, start);

        var firstString: NodeId = undefined;
        var token = self.peekToken();
        if (token.tag() == .templateString) {
            firstString = try self.pushNode(.string, start);
        } else return self.reportParseError("Expected template string or expression.", &.{});

        var lastWasStringPart = true;
        var lastString = firstString;
        var firstExpr: NodeId = cy.NullId;
        var lastExpr: NodeId = cy.NullId;

        self.advanceToken();
        token = self.peekToken();

        var numExprs: u32 = 0;
        while (true) {
            const tag = token.tag();
            if (tag == .templateString) {
                if (lastWasStringPart) {
                    // End of this template.
                    break;
                }
                const str = try self.pushNode(.string, self.next_pos);
                self.nodes.items[lastString].next = str;
                lastString = str;
                lastWasStringPart = true;
            } else if (tag == .templateExprStart) {
                self.advanceToken();
                const expr = (try self.parseExpr(.{})) orelse {
                    return self.reportParseError("Expected expression.", &.{});
                };
                token = self.peekToken();
                if (token.tag() != .right_paren) {
                    return self.reportParseError("Expected right paren.", &.{});
                }
                if (firstExpr == cy.NullId) {
                    firstExpr = expr;
                } else {
                    self.nodes.items[lastExpr].next = expr;
                }
                lastExpr = expr;
                lastWasStringPart = false;
                numExprs += 1;
            } else {
                break;
            }
            self.advanceToken();
            token = self.peekToken();
        }

        self.nodes.items[id].head = .{
            .stringTemplate = .{
                .strHead = firstString,
                .exprHead = firstExpr,
                .numExprs = @intCast(numExprs),
            },
        };
        return id;
    }

    /// An expression term doesn't contain a binary expression at the top.
    fn parseTermExpr(self: *Parser) anyerror!NodeId {
        const start = self.next_pos;
        var token = self.peekToken();
        switch (token.tag()) {
            // .await_k => {
            //     // Await expression.
            //     const expr_id = try self.pushNode(.await_expr, start);
            //     self.advanceToken();
            //     const term_id = try self.parseTermExpr();
            //     self.nodes.items[expr_id].head = .{
            //         .child_head = term_id,
            //     };
            //     return expr_id;
            // },
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
            .throw_k => {
                self.advanceToken();
                const child = try self.parseTermExpr();
                const expr = try self.pushNode(.throwExpr, start);
                self.nodes.items[expr].head = .{
                    .child_head = child,
                };
                return expr;
            },
            .try_k => {
                self.advanceToken();
                const tryExpr = try self.pushNode(.tryExpr, start);
                const expr = try self.parseTermExpr();

                token = self.peekToken();
                var catchExpr: cy.NodeId = cy.NullId;
                if (token.tag() == .catch_k) {
                    self.advanceToken();
                    catchExpr = try self.parseTermExpr();
                }

                self.nodes.items[tryExpr].head = .{
                    .tryExpr = .{
                        .expr = expr,
                        .catchExpr = catchExpr,
                    },
                };
                return tryExpr;
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

                if (self.peekToken().tag() != .left_paren) {
                    return self.reportParseError("Expected ( after coinit.", &.{});
                }
                self.advanceToken();

                const callee = (try self.parseCallArg()) orelse {
                    return self.reportParseError("Expected entry function callee.", &.{});
                };

                var numArgs: u32 = 0;
                var first: NodeId = NullId;
                if (self.peekToken().tag() == .comma) {
                    self.advanceToken();
                    inner: {
                        first = (try self.parseCallArg()) orelse {
                            break :inner;
                        };
                        numArgs += 1;
                        var last = first;
                        while (true) {
                            self.consumeWhitespaceTokens();

                            if (self.peekToken().tag() != .comma) {
                                break;
                            }
                            self.advanceToken();
                            const arg = (try self.parseCallArg()) orelse {
                                break;
                            };
                            numArgs += 1;
                            self.nodes.items[last].next = arg;
                            last = arg;
                        }
                    }
                }

                self.consumeWhitespaceTokens();
                token = self.peekToken();
                if (token.tag() != .right_paren) {
                    return self.reportParseError("Expected closing `)`.", &.{});
                }
                self.advanceToken();

                const callExpr = try self.pushNode(.callExpr, start);
                self.nodes.items[callExpr].head = .{
                    .callExpr = .{
                        .callee = callee,
                        .arg_head = first,
                        .has_named_arg = false,
                        .numArgs = @intCast(numArgs),
                    },
                };

                const coinit = try self.pushNode(.coinit, start);
                self.nodes.items[coinit].head = .{
                    .child_head = callExpr,
                };
                return coinit;
            },
            else => {
                return (try self.parseTightTermExpr()) orelse {
                    return self.reportParseError("Expected term expr. Got: {}.", &.{v(self.peekToken().tag())});
                };
            },
        }
    }

    /// A tight term expr also doesn't include various top expressions
    /// that are separated by whitespace. eg. coinit <expr>
    fn parseTightTermExpr(self: *Parser) anyerror!?NodeId {
        var start = self.next_pos;
        var token = self.peekToken();
        var left_id = switch (token.tag()) {
            .ident => b: {
                self.advanceToken();
                const id = try self.pushIdentNode(start);

                const name_token = self.tokens.items[start];
                const name = self.src[name_token.pos()..name_token.data.end_pos];
                if (!self.isVarDeclaredFromScope(name)) {
                    try self.deps.put(self.alloc, name, id);
                }

                break :b id;
            },
            .type_k => {
                self.advanceToken();
                const id = try self.pushIdentNode(start);
                return id;
            },
            .error_k => b: {
                self.advanceToken();
                token = self.peekToken();
                if (token.tag() == .dot) {
                    // Error symbol literal.
                    self.advanceToken();
                    token = self.peekToken();
                    if (token.tag() == .ident) {
                        const symbol = try self.pushIdentNode(self.next_pos);
                        self.advanceToken();
                        const id = try self.pushNode(.errorSymLit, start);
                        self.nodes.items[id].head = .{
                            .errorSymLit = .{
                                .symbol = symbol,
                            },
                        };
                        break :b id;
                    } else {
                        return self.reportParseError("Expected symbol identifier.", &.{});
                    }
                } else {
                    // Becomes an ident.
                    const id = try self.pushIdentNode(start);
                    break :b id;
                }
            },
            .dot => {
                self.advanceToken();
                token = self.peekToken();
                if (token.tag() == .ident or token.tag() == .error_k) {
                    const sym = try self.pushNode(.symbolLit, self.next_pos);
                    self.advanceToken();
                    return sym;
                } else {
                    return self.reportParseError("Expected symbol identifier.", &.{});
                }
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
            .float => b: {
                self.advanceToken();
                break :b try self.pushNode(.float, start);
            },
            .nonDecInt => b: {
                self.advanceToken();
                break :b try self.pushNode(.nonDecInt, start);
            },
            .string => b: {
                self.advanceToken();
                break :b try self.pushNode(.string, start);
            },
            .templateString => b: {
                break :b try self.parseStringTemplate();
            },
            .pound => b: {
                self.advanceToken();
                token = self.peekToken();
                if (token.tag() == .ident) {
                    const ident = try self.pushIdentNode(self.next_pos);
                    self.advanceToken();
                    const expr = try self.pushNode(.comptimeExpr, start);
                    self.nodes.items[expr].head = .{
                        .comptimeExpr = .{
                            .child = ident,
                        },
                    };
                    break :b expr;
                } else {
                    return self.reportParseError("Expected identifier.", &.{});
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
                        return try self.parseNoParamLambdaFunc();
                    } else {
                        return self.reportParseError("Unexpected paren.", &.{});
                    }
                };
                token = self.peekToken();
                if (token.tag() == .right_paren) {
                    _ = self.consumeToken();

                    token = self.peekToken();
                    if (self.nodes.items[expr_id].node_t == .ident and token.tag() == .equal_greater) {
                        return try self.parseLambdaFuncWithParam(expr_id);
                    }

                    const group = try self.pushNode(.group, start);
                    self.nodes.items[group].head = .{
                        .child_head = expr_id,
                    };
                    break :b group;
                } else if (token.tag() == .comma) {
                    self.next_pos = start;
                    return try self.parseLambdaFunction();
                } else {
                    return self.reportParseError("Expected right parenthesis.", &.{});
                }
            },
            .left_bracket => b: {
                const lit = try self.parseBracketLiteral();
                break :b lit;
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
            else => {
                return null;
            }
        };

        while (true) {
            const next = self.peekToken();
            switch (next.tag()) {
                .dot => {
                    // Access expr.
                    self.advanceToken();
                    switch (self.peekToken().tag()) {
                        .ident,
                        .type_k => {
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
                        },
                        else => {
                            return self.reportParseError("Expected ident", &.{});
                        }
                    }
                },
                .left_bracket => {
                    // index expr, slice expr.
                    self.advanceToken();
                    if (self.peekToken().tag() == .dot_dot) {
                        // Slice expr, start index omitted.
                        self.advanceToken();
                        const rightRange = (try self.parseExpr(.{})) orelse {
                            return self.reportParseError("Expected expression.", &.{});
                        };

                        if (self.peekToken().tag() != .right_bracket) {
                            return self.reportParseError("Expected right bracket.", &.{});
                        }

                        self.advanceToken();
                        const expr = try self.pushNode(.sliceExpr, start);
                        self.nodes.items[expr].head = .{
                            .sliceExpr = .{
                                .arr = left_id,
                                .left = cy.NullId,
                                .right = rightRange,
                            },
                        };
                        left_id = expr;
                        start = self.next_pos;
                        continue;
                    }
                    const index = (try self.parseExpr(.{})) orelse {
                        return self.reportParseError("Expected index.", &.{});
                    };

                    if (self.peekToken().tag() == .right_bracket) {
                        // Index expr.
                        self.advanceToken();
                        const expr = try self.pushNode(.indexExpr, start);
                        self.nodes.items[expr].head = .{
                            .indexExpr = .{
                                .left = left_id,
                                .right = index,
                            },
                        };
                        left_id = expr;
                        start = self.next_pos;
                    } else if (self.peekToken().tag() == .dot_dot) {
                        // Slice expr.
                        self.advanceToken();
                        if (self.peekToken().tag() == .right_bracket) {
                            // End index omitted.
                            self.advanceToken();
                            const expr = try self.pushNode(.sliceExpr, start);
                            self.nodes.items[expr].head = .{
                                .sliceExpr = .{
                                    .arr = left_id,
                                    .left = index,
                                    .right = cy.NullId,
                                },
                            };
                            left_id = expr;
                            start = self.next_pos;
                        } else {
                            const right = (try self.parseExpr(.{})) orelse {
                                return self.reportParseError("Expected end index.", &.{});
                            };
                            if (self.peekToken().tag() != .right_bracket) {
                                return self.reportParseError("Expected right bracket.", &.{});
                            }
                            self.advanceToken();
                            const expr = try self.pushNode(.sliceExpr, start);
                            self.nodes.items[expr].head = .{
                                .sliceExpr = .{
                                    .arr = left_id,
                                    .left = index,
                                    .right = right,
                                },
                            };
                            left_id = expr;
                            start = self.next_pos;
                        }
                    } else {
                        return self.reportParseError("Expected right bracket.", &.{});                            
                    }
                },
                .left_paren => {
                    const call_id = try self.parseCallExpression(left_id);
                    left_id = call_id;
                },
                .dot_dot,
                .right_bracket,
                .right_paren,
                .right_brace,
                .else_k,
                .catch_k,
                .comma,
                .colon,
                .is_k,
                .equal,
                .operator,
                .or_k,
                .and_k,
                .as_k,
                .capture,
                .string,
                .number,
                .float,
                .if_k,
                .ident,
                .pound,
                .templateString,
                .equal_greater,
                .new_line,
                .none => break,
                else => break,
            }
        }
        return left_id;
    }

    fn returnLeftAssignExpr(self: *Parser, leftId: NodeId, outIsAssignStmt: *bool) !NodeId {
        switch (self.nodes.items[leftId].node_t) {
            .accessExpr,
            .indexExpr,
            .ident => {
                outIsAssignStmt.* = true;
                return leftId;
            },
            else => {
                return self.reportParseError("Expected variable to left of assignment operator.", &.{});
            },
        }
    }

    fn parseBinExpr(self: *Parser, left: NodeId, op: cy.ast.BinaryExprOp) !NodeId {
        const opStart = self.next_pos;
        // Assumes current token is the operator.
        self.advanceToken();

        const right = try self.parseRightExpression(op);
        const expr = try self.pushNode(.binExpr, opStart);
        self.nodes.items[expr].head = .{
            .binExpr = .{
                .left = left,
                .right = right,
                .op = op,
            },
        };
        return expr;
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
            .indent,
            .new_line => {
                self.advanceToken();
                self.consumeWhitespaceTokens();
                start = self.next_pos;
                token = self.peekToken();
                if (token.tag() == .none) {
                    return null;
                }
            },
            else => {},
        }
        left_id = try self.parseTermExpr();

        while (true) {
            const next = self.peekToken();
            switch (next.tag()) {
                .equal_greater => {
                    const left = self.nodes.items[left_id];
                    if (left.node_t == .ident) {
                        // Lambda.
                        return try self.parseLambdaFuncWithParam(left_id);
                    } else {
                        return self.reportParseError("Unexpected `=>` token", &.{});
                    }
                },
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
                            if (self.peekTokenAhead(1).tag() == .equal) {
                                if (opts.returnLeftAssignExpr) {
                                    return try self.returnLeftAssignExpr(left_id, opts.outIsAssignStmt);
                                } else {
                                    break;
                                }
                            }
                        },
                        else => {},
                    }
                    const bin_op = toBinExprOp(op_t);
                    left_id = try self.parseBinExpr(left_id, bin_op);
                },
                .as_k => {
                    const opStart = self.next_pos;
                    self.advanceToken();

                    const typeSpecHead = (try self.parseOptNamePath()) orelse {
                        return self.reportParseError("Expected type specifier.", &.{});
                    };
                    const expr = try self.pushNode(.castExpr, opStart);
                    self.nodes.items[expr].head = .{
                        .castExpr = .{
                            .expr = left_id,
                            .typeSpecHead = typeSpecHead,
                        },
                    };
                    left_id = expr;
                },
                .and_k => {
                    left_id = try self.parseBinExpr(left_id, .and_op);
                },
                .or_k => {
                    left_id = try self.parseBinExpr(left_id, .or_op);
                },
                .is_k => {
                    self.advanceToken();
                    token = self.peekToken();
                    var binOp = cy.ast.BinaryExprOp.equal_equal;
                    if (token.tag() == .not_k) {
                        binOp = cy.ast.BinaryExprOp.bang_equal;
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
                .question => {
                    left_id = try self.parseCondExpr(left_id, start);
                },
                .right_bracket,
                .right_paren,
                .right_brace,
                .else_k,
                .comma,
                .colon,
                .minusDotDot,
                .dot_dot,
                .capture,
                .new_line,
                .none => break,
                else => {
                    if (!opts.parseShorthandCallExpr) {
                        return left_id;
                    }
                    // Attempt to parse as no paren call expr.
                    const left = self.nodes.items[left_id];
                    switch (left.node_t) {
                        .accessExpr,
                        .ident => {
                            return try self.parseNoParenCallExpression(left_id);
                        },
                        else => {
                            return left_id;
                        }
                    }
                }
            }
        }
        return left_id;
    }

    fn parseVarDecl(self: *Parser, modifierHead: cy.NodeId, typed: bool) !cy.NodeId {
        const start = self.next_pos;
        self.advanceToken();

        // Var name.
        const name = (try self.parseOptNamePath()) orelse {
            return self.reportParseError("Expected local name identifier.", &.{});
        };
        const isStatic = self.nodes.items[name].next != cy.NullId;

        var typeSpecHead: cy.NodeId = cy.NullId;
        if (typed) {
            typeSpecHead = (try self.parseOptNamePath()) orelse cy.NullId;
        }

        const varSpec = try self.pushNode(.varSpec, start);
        self.nodes.items[varSpec].head = .{
            .varSpec = .{
                .name = name,
                .typeSpecHead = typeSpecHead,
                .modifierHead = modifierHead,
            },
        };

        var decl: cy.NodeId = undefined;
        if (isStatic) {
            decl = try self.pushNode(.staticDecl, start);
        } else {
            decl = try self.pushNode(.localDecl, start);
        }

        var right: NodeId = cy.NullId;
        inner: {
            var token = self.peekToken();
            if (token.tag() == .new_line or token.tag() == .none) {
                break :inner;
            }

            if (self.peekToken().tag() != .equal) {
                return self.reportParseError("Expected `=` after variable name.", &.{});
            }
            self.advanceToken();

            // Continue parsing right expr.
            switch (self.peekToken().tag()) {
                .func_k => {
                    right = try self.parseMultilineLambdaFunction();
                },
                .switch_k => {
                    right = try self.parseStatement();
                },
                else => {
                    right = (try self.parseExpr(.{})) orelse {
                        return self.reportParseError("Expected right expression for assignment statement.", &.{});
                    };
                },
            }
            self.nodes.items[right].hasParentAssignStmt = true;
        }

        if (isStatic) {
            self.nodes.items[decl].head = .{
                .staticDecl = .{
                    .varSpec = varSpec,
                    .right = right,
                    .typed = typed,
                },
            };
            try self.staticDecls.append(self.alloc, .{
                .declT = .variable,
                .nodeId = decl,
                .data = undefined,
            });
        } else {
            self.nodes.items[decl].head = .{
                .localDecl = .{
                    .varSpec = varSpec,
                    .right = right,
                    .typed = typed,
                },
            };
        }
        return decl;
    }

    /// Assumes next token is the return token.
    fn parseReturnStatement(self: *Parser) !NodeId {
        const start = self.next_pos;
        self.advanceToken();
        const token = self.peekToken();
        switch (token.tag()) {
            .new_line,
            .none => {
                return try self.pushNode(.return_stmt, start);
            },
            .func_k => {
                const lambda = try self.parseMultilineLambdaFunction();
                const id = try self.pushNode(.return_expr_stmt, start);
                self.nodes.items[id].head = .{
                    .child_head = lambda,
                };
                return id;
            },
            else => {
                const expr = try self.parseExpr(.{}) orelse {
                    return self.reportParseError("Expected expression.", &.{});
                };
                try self.consumeNewLineOrEnd();

                const id = try self.pushNode(.return_expr_stmt, start);
                self.nodes.items[id].head = .{
                    .child_head = expr,
                };
                return id;
            }
        }
    }

    fn parseExprOrAssignStatement(self: *Parser) !?NodeId {
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

            // Right can be an expr or stmt.
            var right: NodeId = undefined;
            var rightIsStmt = false;
            switch (assignTag) {
                .equal => {
                    assignStmt = try self.pushNode(.assign_stmt, start);
                    switch (self.peekToken().tag()) {
                        .func_k => {
                            right = try self.parseMultilineLambdaFunction();
                        },
                        .switch_k => {
                            right = try self.parseStatement();
                            rightIsStmt = true;
                        },
                        else => {
                            right = (try self.parseExpr(.{})) orelse {
                                return self.reportParseError("Expected right expression for assignment statement.", &.{});
                            };
                        }
                    }
                    self.nodes.items[assignStmt].head = .{
                        .left_right = .{
                            .left = expr_id,
                            .right = right,
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
                            right = (try self.parseExpr(.{})) orelse {
                                return self.reportParseError("Expected right expression for assignment statement.", &.{});
                            };
                            assignStmt = try self.pushNode(.opAssignStmt, start);
                            self.nodes.items[assignStmt].head = .{
                                .opAssignStmt = .{
                                    .left = expr_id,
                                    .right = right,
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
                const name = self.src[name_token.pos()..name_token.data.end_pos];
                const block = &self.blockStack.items[self.blockStack.items.len-1];
                if (self.deps.get(name)) |node_id| {
                    if (node_id == expr_id) {
                        // Remove dependency now that it's recognized as assign statement.
                        _ = self.deps.remove(name);
                    }
                }
                try block.vars.put(self.alloc, name, {});
            }

            if (self.nodes.items[right].node_t != .lambda_multi) {
                token = self.peekToken();
                if (!rightIsStmt) {
                    try self.consumeNewLineOrEnd();
                }
                return assignStmt;
            } else {
                return assignStmt;
            }
        } else {
            const start = self.nodes.items[expr_id].start_token;
            const id = try self.pushNode(.exprStmt, start);
            self.nodes.items[id].head = .{
                .exprStmt = .{
                    .child = expr_id,
                },
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

    pub fn pushNode(self: *Parser, node_t: cy.NodeType, start: u32) !NodeId {
        const id = self.nodes.items.len;
        try self.nodes.append(self.alloc, .{
            .node_t = node_t,
            .start_token = start,
            .next = NullId,
            .head = undefined,
        });
        return @intCast(id);
    }

    fn pushIdentNode(self: *Parser, start: u32) !NodeId {
        const id = try self.pushNode(.ident, start);
        self.nodes.items[id].head = .{
            .ident = .{},
        };
        return id;
    }

    inline fn pushIdentToken(self: *Parser, start_pos: u32, end_pos: u32) !void {
        try self.tokens.append(self.alloc, Token.init(.ident, start_pos, .{ .end_pos = end_pos }));
    }

    inline fn pushNonDecimalIntegerToken(self: *Parser, start_pos: u32, end_pos: u32) !void {
        try self.tokens.append(self.alloc, Token.init(.nonDecInt, start_pos, .{ .end_pos = end_pos }));
    }

    inline fn pushNumberToken(self: *Parser, start_pos: u32, end_pos: u32) !void {
        try self.tokens.append(self.alloc, Token.init(.number, start_pos, .{ .end_pos = end_pos }));
    }

    inline fn pushFloatToken(self: *Parser, start_pos: u32, end_pos: u32) !void {
        try self.tokens.append(self.alloc, Token.init(.float, start_pos, .{ .end_pos = end_pos }));
    }

    inline fn pushTemplateStringToken(self: *Parser, start_pos: u32, end_pos: u32) !void {
        try self.tokens.append(self.alloc, Token.init(.templateString, start_pos, .{ .end_pos = end_pos }));
    }

    inline fn pushStringToken(self: *Parser, start_pos: u32, end_pos: u32) !void {
        try self.tokens.append(self.alloc, Token.init(.string, start_pos, .{ .end_pos = end_pos }));
    }

    inline fn pushOpToken(self: *Parser, operator_t: OperatorType, start_pos: u32) !void {
        try self.tokens.append(self.alloc, Token.init(.operator, start_pos, .{
            .operator_t = operator_t,
        }));
    }

    inline fn pushIndentToken(self: *Parser, count: u32, start_pos: u32, spaces: bool) void {
        self.tokens.append(self.alloc, Token.init(.indent, start_pos, .{
            .indent = if (spaces) count else count | 0x80000000,
        })) catch fatal();
    }

    inline fn pushToken(self: *Parser, token_t: TokenType, start_pos: u32) !void {
        try self.tokens.append(self.alloc, Token.init(token_t, start_pos, .{ .end_pos = NullId }));
    }

    inline fn pushKeywordToken(self: *Parser, token_t: TokenType, startPos: u32, endPos: u32) !void {
        try self.tokens.append(self.alloc, Token.init(token_t, startPos, .{ .end_pos = endPos }));
    }

    /// When n=0, this is equivalent to peekToken.
    inline fn peekTokenAhead(self: Parser, n: u32) Token {
        if (self.next_pos + n < self.tokens.items.len) {
            return self.tokens.items[self.next_pos + n];
        } else {
            return Token.init(.none, self.next_pos, .{
                .end_pos = NullId,
            });
        }
    }

    inline fn peekToken(self: Parser) Token {
        if (!self.isAtEndToken()) {
            return self.tokens.items[self.next_pos];
        } else {
            return Token.init(.none, self.next_pos, .{
                .end_pos = NullId,
            });
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

pub const OperatorType = enum(u8) {
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

pub const TokenType = enum(u8) {
    ident,
    number,
    float,
    nonDecInt,
    string,
    templateString,
    templateExprStart,
    operator,
    capture,
    placeholder,
    at,
    pound,
    question,
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
    minusDotDot,
    logic_op,
    equal,
    new_line,
    indent,
    return_k,
    break_k,
    case_k,
    continue_k,
    if_k,
    else_k,
    for_k,
    while_k,
    // await_k,
    true_k,
    false_k,
    or_k,
    and_k,
    not_k,
    as_k,
    pass_k,
    none_k,
    object_k,
    type_k,
    enum_k,
    error_k,
    func_k,
    is_k,
    coinit_k,
    coyield_k,
    coresume_k,
    import_k,
    try_k,
    catch_k,
    throw_k,
    var_k,
    switch_k,
    my_k,
    // Error token, returned if ignoreErrors = true.
    err,
    /// Used to indicate no token.
    none,
};

pub const Token = extern struct {
    // First 8 bits is the TokenType, last 24 bits is the start pos.
    head: u32,
    data: extern union {
        end_pos: u32,
        operator_t: OperatorType,
        // Num indent spaces.
        indent: u32,
    },

    pub fn init(ttype: TokenType, startPos: u32, data: std.meta.FieldType(Token, .data)) Token {
        return .{
            .head = (startPos << 8) | @intFromEnum(ttype),
            .data = data,
        };
    }

    pub inline fn tag(self: Token) TokenType {
        return @enumFromInt(self.head & 0xff);
    }

    pub inline fn pos(self: Token) u32 {
        return self.head >> 8;
    }
};

pub const Result = struct {
    inner: ResultView,
    
    pub fn init(alloc: std.mem.Allocator, view: ResultView) !Result {
        const arr = try view.nodes.clone(alloc);
        const nodes = try alloc.create(std.ArrayListUnmanaged(cy.Node));
        nodes.* = arr;

        const new_src = try alloc.dupe(u8, view.src);

        const deps = try alloc.create(std.StringHashMapUnmanaged(NodeId));
        deps.* = .{};
        var iter = view.deps.iterator();
        while (iter.next()) |entry| {
            const dep = entry.key_ptr.*;
            const offset = @intFromPtr(dep.ptr) - @intFromPtr(view.src.ptr);
            try deps.put(alloc, new_src[offset..offset+dep.len], entry.value_ptr.*);
        }

        return Result{
            .inner = .{
                .has_error = view.has_error,
                .err_msg = try alloc.dupe(u8, view.err_msg),
                .root_id = view.root_id,
                .nodes = nodes,
                .src = new_src,
                .tokens = try alloc.dupe(Token, view.tokens),
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

pub fn getNodeString(p: *const Parser, nodeId: NodeId) []const u8 {
    const node = p.nodes.items[nodeId];
    const token = p.tokens.items[node.start_token];
    return p.src[token.pos()..token.data.end_pos];
}

/// Result data is not owned.
pub const ResultView = struct {
    root_id: NodeId,
    err_msg: []const u8,
    has_error: bool,
    isTokenError: bool,

    /// ArrayList is returned so resulting ast can be modified.
    nodes: *std.ArrayListUnmanaged(cy.Node),
    tokens: []const Token,
    src: []const u8,

    name: []const u8,
    deps: *std.StringHashMapUnmanaged(NodeId),

    pub fn getFirstNodeString(self: ResultView, nodeId: NodeId) []const u8 {
        const node = self.nodes.items[nodeId];
        const token = self.tokens[node.start_token];
        return self.src[token.pos()..token.data.end_pos];
    }

    pub fn getTokenString(self: ResultView, token_id: u32) []const u8 {
        // Assumes token with end_pos.
        const token = self.tokens[token_id];
        return self.src[token.pos()..token.data.end_pos];
    }

    pub fn dupe(self: ResultView, alloc: std.mem.Allocator) !Result {
        return try Result.init(alloc, self);
    }

    pub fn pushNode(self: ResultView, alloc: std.mem.Allocator, node_t: cy.NodeType, start: TokenId) NodeId {
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

fn toBinExprOp(op: OperatorType) cy.ast.BinaryExprOp {
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

pub fn getBinOpPrecedence(op: cy.ast.BinaryExprOp) u8 {
    switch (op) {
        .bitwiseLeftShift,
        .bitwiseRightShift => return 9,

        .bitwiseAnd => return 8,

        .bitwiseXor,
        .bitwiseOr => return 7,

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

        .cast => return 3,

        .greater,
        .greater_equal,
        .less,
        .less_equal,
        .bang_equal,
        .equal_equal => {
            return 2;
        },

        .and_op => return 1,

        .or_op => return 0,

        else => return 0,
    }
}

pub fn getLastStmt(nodes: []const cy.Node, head: NodeId, out_prev: *NodeId) NodeId {
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

pub fn pushNodeToList(alloc: std.mem.Allocator, nodes: *std.ArrayListUnmanaged(cy.Node), node_t: cy.NodeType, start: u32) NodeId {
    const id = nodes.items.len;
    nodes.append(alloc, .{
        .node_t = node_t,
        .start_token = start,
        .next = NullId,
        .head = undefined,
    }) catch fatal();
    return @intCast(id);
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

    /// For string interpolation, open parens can accumulate so the end of a template expression can be determined.
    openParens: u8 = 0,

    /// For string interpolation, if true the delim is a double quote otherwise it's a backtick.
    stringDelim: StringDelim = .single,
    hadTemplateExpr: u1 = 0,
};

pub const TokenizeStateTag = enum {
    start,
    token,
    templateString,
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
                return p.src.len == p.next_pos;
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
                return p.src[p.next_pos];
            }
        }

        inline fn getSubStrFrom(p: *const Parser, start: u32) []const u8 {
            if (Config.user) {
                return p.user.getSubStrFromDelta(p.user.ctx, p.next_pos - start);
            } else {
                return p.src[start..p.next_pos];
            }
        }

        inline fn peekCharAhead(p: *const Parser, steps: u32) ?u8 {
            if (Config.user) {
                return p.user.peekCharAhead(p.user.ctx, steps);
            } else {
                if (p.next_pos < p.src.len - steps) {
                    return p.src[p.next_pos + steps];
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
                    if (state.stateT == .templateExprToken) {
                        var next = state;
                        next.openParens += 1;
                        return next;
                    }
                },
                ')' => {
                    try p.pushToken(.right_paren, start);
                    if (state.stateT == .templateExprToken) {
                        var next = state;
                        if (state.openParens == 0) {
                            next.stateT = .templateString;
                            next.openParens = 0;
                            return next;
                        } else {
                            next.openParens -= 1;
                            return next;
                        }
                    }
                },
                '{' => {
                    try p.pushToken(.left_brace, start);
                },
                '}' => {
                    try p.pushToken(.right_brace, start);
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
                                if (p.parseComments) {
                                    try p.comments.append(p.alloc, cy.IndexSlice(u32).init(start, p.next_pos));
                                }
                                // Don't consume new line or the current indentation could augment with the next line.
                                return tokenizeOne(p, state);
                            }
                            advanceChar(p);
                        }
                        if (p.parseComments) {
                            try p.comments.append(p.alloc, cy.IndexSlice(u32).init(start, p.next_pos));
                        }
                        return .{ .stateT = .end };
                    } else if (peekChar(p) == '>') {
                        advanceChar(p);
                        try p.pushToken(.capture, start);
                    } else if (peekChar(p) == '.' and peekCharAhead(p, 1) == '.') {
                        advanceChar(p);
                        advanceChar(p);
                        try p.pushToken(.minusDotDot, start);
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
                '_' => {
                    try p.pushToken(.placeholder, start);
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
                '#' => try p.pushToken(.pound, start),
                '?' => try p.pushToken(.question, start),
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
                        if (isAtEndChar(p)) {
                            break;
                        }
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
                        if (isAtEndChar(p)) {
                            break;
                        }
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

            if (p.src.len > 2 and p.src[0] == '#' and p.src[1] == '!') {
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
                    '$' => {
                        const ch2 = peekCharAhead(p, 1) orelse 0;
                        if (ch2 == '(') {
                            try p.pushTemplateStringToken(start, p.next_pos);
                            try p.pushToken(.templateExprStart, p.next_pos);
                            advanceChar(p);
                            advanceChar(p);
                            var next = state;
                            next.stateT = .templateExprToken;
                            next.openParens = 0;
                            next.hadTemplateExpr = 1;
                            return next;
                        } else {
                            advanceChar(p);
                        }
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

        fn tokenizeKeywordOrIdent(p: *Parser, start: u32) !void {
            // Consume alpha.
            while (true) {
                if (isAtEndChar(p)) {
                    if (keywords.get(getSubStrFrom(p, start))) |token_t| {
                        try p.pushKeywordToken(token_t, start, p.next_pos);
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
                        try p.pushKeywordToken(token_t, start, p.next_pos);
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
                    try p.pushKeywordToken(token_t, start, p.next_pos);
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
            if ((ch >= '0' and ch <= '9') or ch == 'e' or ch == '.') {
                // Common path.
                consumeDigits(p);
                if (isAtEndChar(p)) {
                    try p.pushNumberToken(start, p.next_pos);
                    return;
                }

                // Check for decimal notation.
                var isFloat = false;
                ch = peekChar(p);
                const ch2 = peekCharAhead(p, 1) orelse 0;
                if (ch == '.' and ch2 != '.') {
                    // Differentiate decimal from range operator.
                    advanceChar(p);
                    consumeDigits(p);
                    if (isAtEndChar(p)) {
                        try p.pushFloatToken(start, p.next_pos);
                        return;
                    }
                    ch = peekChar(p);
                    isFloat = true;
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
                    isFloat = true;
                }
                if (isFloat) {
                    try p.pushFloatToken(start, p.next_pos);
                } else {
                    try p.pushNumberToken(start, p.next_pos);
                }
                return;
            }

            if (p.src[p.next_pos-1] == '0') {
                // Less common integer notation.
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
                } else if (ch == 'u') {
                    // UTF-8 codepoint literal (rune).
                    advanceChar(p);
                    if (isAtEndChar(p)) {
                        return p.reportTokenError("Expected UTF-8 rune.", &.{});
                    }
                    ch = peekChar(p);
                    if (ch != '\'') {
                        return p.reportTokenError("Expected single quote.", &.{});
                    }
                    advanceChar(p);
                    while (true) {
                        if (isAtEndChar(p)) {
                            return p.reportTokenError("Expected UTF-8 rune.", &.{});
                        }
                        ch = peekChar(p);
                        if (ch == '\\') {
                            advanceChar(p);
                            if (isAtEndChar(p)) {
                                return p.reportTokenError("Expected single quote or backslash.", &.{});
                            }
                            advanceChar(p);
                        } else {
                            advanceChar(p);
                            if (ch == '\'') {
                                break;
                            }
                        }
                    }
                    try p.pushNonDecimalIntegerToken(start, p.next_pos);
                    return;
                } else {
                    if (std.ascii.isAlphabetic(ch)) {
                        const char: []const u8 = &[_]u8{ ch };
                        return p.reportTokenError("Unsupported integer notation: {}", &.{v(char)});
                    }
                }
            }

            // Push single digit number.
            try p.pushNumberToken(start, p.next_pos);
            return;
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
    parseShorthandCallExpr: bool = true,
};

const StaticDeclType = enum {
    variable,
    typeAlias,
    func,
    funcInit,
    import,
    object,
    enumT,
};

pub const StaticDecl = struct {
    declT: StaticDeclType,
    nodeId: cy.NodeId,
    data: union {
        func: *cy.Func,
        sym: *cy.Sym,
    },
};

test "parser internals." {
    try t.eq(@sizeOf(Token), 8);
    try t.eq(@alignOf(Token), 4);
    try t.eq(@sizeOf(TokenizeState), 4);

    try t.eq(std.enums.values(TokenType).len, 63);
    try t.eq(keywords.kvs.len, 32);
}

fn isRecedingIndent(p: *Parser, prevIndent: u32, curIndent: u32, indent: u32) !bool {
    if (indent ^ curIndent < 0x80000000) {
        return indent <= prevIndent;
    } else {
        if (indent == 0) {
            return true;
        } else {
            if (curIndent & 0x80000000 == 0x80000000) {
                return p.reportParseError("Expected tabs for indentation.", &.{});
            } else {
                return p.reportParseError("Expected spaces for indentation.", &.{});
            }
        }
    }
}

const FirstLastStmt = struct {
    first: NodeId,
    last: NodeId,
};