const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const t = stdx.testing;
const fatal = cy.fatal;
const fmt = @import("fmt.zig");
const v = fmt.v;
const cy = @import("cyber.zig");
const c = @import("capi.zig");
const ast = cy.ast;
const Token = cy.tokenizer.Token;

const TokenId = u32;
const log = cy.log.scoped(.parser);
const IndexSlice = cy.IndexSlice(u32);

const dumpParseErrorStackTrace = !cy.isFreestanding and builtin.mode == .Debug and !cy.isWasm and true;

const attributes = std.ComptimeStringMap(cy.ast.AttributeType, .{
    .{ "host", .host },
});

const Block = struct {
    vars: std.StringHashMapUnmanaged(void),

    fn deinit(self: *Block, alloc: std.mem.Allocator) void {
        self.vars.deinit(alloc);
    }
};

const ParseOptions = struct {
    parseComments: bool = false,
};

/// Parses source code into AST.
pub const Parser = struct {
    alloc: std.mem.Allocator,

    /// Context vars.
    next_pos: u32,
    savePos: u32,

    ast: ast.Ast,
    tokens: []const Token,

    node_stack: std.ArrayListUnmanaged(*ast.Node),
    blockStack: std.ArrayListUnmanaged(Block),
    cur_indent: u32,

    /// Use the parser pass to record static declarations.
    staticDecls: std.ArrayListUnmanaged(*ast.Node),

    // TODO: This should be implemented by user callbacks.
    /// @name arg.
    name: []const u8,
    /// Variable dependencies.
    deps: std.StringHashMapUnmanaged(*ast.Node),

    reportFn: *const fn(*anyopaque, format: []const u8, args: []const cy.fmt.FmtValue, pos: u32) anyerror,
    tokenizerReportFn: *const fn(*anyopaque, format: []const u8, args: []const cy.fmt.FmtValue, pos: u32) anyerror!void,
    ctx: *anyopaque,

    has_error: bool,

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

    pub fn init(self: *Parser, alloc: std.mem.Allocator) !void {
        self.* = .{
            .alloc = alloc,
            .ast = undefined,
            .next_pos = undefined,
            .savePos = undefined,
            .tokens = undefined,
            .node_stack = .{},
            .blockStack = .{},
            .cur_indent = 0,
            .name = "",
            .deps = .{},
            .user = undefined,
            .staticDecls = .{},
            .reportFn = defaultReportFn,
            .tokenizerReportFn = cy.tokenizer.defaultReportFn,
            .ctx = undefined,
            .has_error = false,
        };
        try self.ast.init(alloc, "");
    }

    pub fn deinit(self: *Parser) void {
        self.ast.deinit(self.alloc);
        for (self.blockStack.items) |*block| {
            block.deinit(self.alloc);
        }
        self.blockStack.deinit(self.alloc);
        self.deps.deinit(self.alloc);
        self.staticDecls.deinit(self.alloc);
        self.node_stack.deinit(self.alloc);
    }

    pub fn parseNoErr(self: *Parser, src: []const u8, opts: ParseOptions) !ResultView {
        const res = try self.parse(src, opts);
        if (res.has_error) {
            return error.ParseError;
        }
        return res;
    }

    pub fn parse(self: *Parser, src: []const u8, opts: ParseOptions) !ResultView {
        self.ast.src = src;
        self.name = "";
        self.deps.clearRetainingCapacity();
        self.has_error = false;

        var tokenizer = cy.Tokenizer.init(self.alloc, src);
        defer tokenizer.deinit();

        tokenizer.parseComments = opts.parseComments;
        tokenizer.reportFn = self.tokenizerReportFn;
        tokenizer.ctx = self.ctx;
        try tokenizer.tokens.ensureTotalCapacityPrecise(self.alloc, 511);
        tokenizer.tokenize() catch |err| {
            log.tracev("tokenize error: {}", .{err});
            if (dumpParseErrorStackTrace and !c.silent()) {
                std.debug.dumpStackTrace(@errorReturnTrace().?.*);
            }
            if (err == error.TokenError) {
                return ResultView{
                    .has_error = true,
                    .root = null,
                    .ast = self.ast.view(),
                    .name = self.name,
                    .deps = &self.deps,
                };
            } else {
                return err;
            }
        };
        self.ast.comments = tokenizer.consumeComments();
        self.tokens = tokenizer.tokens.items;

        self.ast.root = self.parseRoot() catch |err| {
            if (dumpParseErrorStackTrace and !c.silent()) {
                std.debug.dumpStackTrace(@errorReturnTrace().?.*);
            }
            if (err == error.ParseError) {
                return ResultView{
                    .has_error = true,
                    .root = null,
                    .ast = self.ast.view(),
                    .name = self.name,
                    .deps = &self.deps,
                };
            } else {
                return err;
            }
        };
        return ResultView{
            .has_error = self.has_error or tokenizer.has_error,
            .root = self.ast.root,
            .ast = self.ast.view(),
            .name = self.name,
            .deps = &self.deps,
        };
    }

    fn parseRoot(self: *Parser) !*ast.Root {
        self.next_pos = 0;
        try self.ast.clearNodes();
        self.blockStack.clearRetainingCapacity();
        self.cur_indent = 0;

        const indent = (try self.consumeFirstIndent()) orelse {
            return self.ast.newNode(.root, .{
                .stmts = &.{},
            });
        };
        if (indent != 0) {
            return self.reportError("Unexpected indentation.", &.{});
        }

        try self.pushBlock();
        const stmts = try self.parseBodyStatements(0, .{ .allow_decls = true });

        // Mark last expression stmt.
        const last = stmts[stmts.len-1];
        if (last.type() == .exprStmt) {
            last.cast(.exprStmt).isLastRootStmt = true;
        }

        _ = self.popBlock();

        try self.consumeNewLineOrEnd();
        if (try self.consumeFirstIndent()) |_| {
            return self.reportError("Unexpected indentation.", &.{});
        }

        return self.ast.newNode(.root, .{
            .stmts = stmts,
        });
    }

    /// Returns `true` for a valid indent preceding a statement.
    fn consumeNextLineIndent(self: *Parser, req_indent: u32) !bool {
        // Must consume at least one line.
        if (self.peek().tag() != .new_line) {
            return false;
        }
        self.advance();
        while (true) {
            const token = self.peek();
            switch (token.tag()) {
                .indent => {
                    const res = token.data.indent;
                    self.advance();
                    switch (self.peek().tag()) {
                        .indent => {
                            // If another indent token is encountered, it would be a different type.
                            return self.reportError("Can not mix tabs and spaces for indentation.", &.{});
                        },
                        .new_line => {
                            self.advance();
                            continue;
                        },
                        .null => {
                            return false;
                        },
                        else => {
                            if (res == req_indent) {
                                return true;
                            } else {
                                if (res ^ req_indent < 0x80000000) {
                                    // Same type but different indent.
                                    return false;
                                } else {
                                    if (res & 0x80000000 == 0x80000000) {
                                        return self.reportError("Expected tabs for indentation.", &.{});
                                    } else {
                                        return self.reportError("Expected spaces for indentation.", &.{});
                                    }
                                }
                            }
                        },
                    }
                },
                .new_line => {
                    self.advance();
                },
                .null => {
                    return false;
                },
                else => {
                    return req_indent == 0;
                },
            }
        }
    }

    /// Returns number of spaces that precedes a statement.
    fn consumeFirstIndent(self: *Parser) !?u32 {
        while (true) {
            const token = self.peek();
            switch (token.tag()) {
                .indent => {
                    const res = token.data.indent;
                    self.advance();
                    switch (self.peek().tag()) {
                        .indent => {
                            // If another indent token is encountered, it would be a different type.
                            return self.reportError("Can not mix tabs and spaces for indentation.", &.{});
                        },
                        .new_line => {
                            self.advance();
                            continue;
                        },
                        .null => {
                            return null;
                        },
                        else => {
                            return res;
                        },
                    }
                },
                .new_line => {
                    self.advance();
                },
                .null => {
                    return null;
                },
                else => {
                    return 0;
                },
            }
        }
    }

    fn pushNode(self: *Parser, n: *ast.Node) !void {
        try self.node_stack.append(self.alloc, n);
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

    fn parseSingleOrIndentedBodyStmts(self: *Parser) ![]*ast.Node {
        var token = self.peek();
        if (token.tag() != .new_line) {
            // Parse single statement only.
            const stmt = try self.parseStatement(.{});
            return self.ast.dupeNodes(&.{stmt});
        } else {
            self.advance();
            return self.parseIndentedBodyStatements(.{});
        }
    }

    /// Indent is determined by the first body statement.
    fn parseIndentedBodyStatements(self: *Parser, config: ParseStmtConfig) ![]*ast.Node {
        const reqIndent = try self.parseFirstChildIndent(self.cur_indent);
        return self.parseBodyStatements(reqIndent, config);
    }

    // Assumes the first indent is already consumed.
    fn parseBodyStatements(self: *Parser, reqIndent: u32, config: ParseStmtConfig) ![]*ast.Node {
        const prevIndent = self.cur_indent;
        self.cur_indent = reqIndent;
        defer self.cur_indent = prevIndent;

        const stmt_start = self.node_stack.items.len;
        defer self.node_stack.items.len = stmt_start;

        var stmt = try self.parseStatement(config);
        try self.pushNode(stmt);

        // Parse body statements until indentation goes back to at least the previous indent.
        while (true) {
            const start = self.next_pos;
            if (!try self.consumeNextLineIndent(reqIndent)) {
                self.next_pos = start;
                break;
            }
            stmt = try self.parseStatement(config);
            try self.pushNode(stmt);
        }
        return self.ast.dupeNodes(self.node_stack.items[stmt_start..]);
    }

    /// Parses the first child indent and returns the indent size.
    fn parseFirstChildIndent(self: *Parser, fromIndent: u32) !u32 {
        const indent = (try self.consumeFirstIndent()) orelse {
            return self.reportError("Block requires an indented child statement. Use the `pass` statement as a placeholder.", &.{});
        };
        if ((fromIndent ^ indent < 0x80000000) or fromIndent == 0) {
            // Either same indent style or indenting from root.
            if (indent > fromIndent) {
                return indent;
            } else {
                return self.reportError("Block requires an indented child statement. Use the `pass` statement as a placeholder.", &.{});
            }
        } else {
            if (fromIndent & 0x80000000 == 0x80000000) {
                return self.reportError("Expected tabs for indentation.", &.{});
            } else {
                return self.reportError("Expected spaces for indentation.", &.{});
            }
        }
    }

    fn parseInferLambda(self: *Parser, params: []const *ast.FuncParam) !*ast.LambdaExpr {
        const start = self.next_pos;
        // Assumes first token is `=>`.
        self.advance();
        
        // Parse body expr.
        try self.pushBlock();
        const expr = (try self.parseExpr(.{})) orelse {
            return self.reportError("Expected lambda body expression.", &.{});
        };
        _ = self.popBlock();

        return self.ast.newNode(.lambda_expr, .{
            .sig_t = .infer,
            .stmts = @as([*]*ast.Node, @ptrCast(@alignCast(expr)))[0..1],
            .params = params,
            .pos = self.tokenSrcPos(start),
            .ret = null,
        });
    }

    fn parseInferLambdaBlock(self: *Parser, params: []const *ast.FuncParam, ret: ?*ast.Node) !*ast.LambdaExpr {
        const start = self.next_pos;
        // Assumes first token is `:`.
        self.advance();
        
        try self.pushBlock();
        const stmts = try self.parseSingleOrIndentedBodyStmts();
        _ = self.popBlock();

        return self.ast.newNode(.lambda_multi, .{
            .sig_t = .infer,
            .stmts = stmts,
            .params = params,
            .pos = self.tokenSrcPos(start),
            .ret = ret,
        });
    }

    fn parseFuncUnionType(self: *Parser) !*ast.Node {
        const start = self.next_pos;

        // Assume first token is `func`.
        self.advance();

        const params = try self.parseParenAndFuncParams();
        const ret = try self.parseFuncReturn();

        return self.ast.newNodeErase(.func_type, .{
            .params = params,
            .pos = self.tokenSrcPos(start),
            .ret = ret,
            .is_union = true,
        });
    }

    fn parseLetLambda(self: *Parser) !*ast.Node {
        const start = self.next_pos;

        // Assume first token is `let`.
        self.advance();

        const params = try self.parseParenAndFuncParams();

        if (self.peek().tag() == .equal_right_angle) {
            self.advance();

            // Parse body expr.
            try self.pushBlock();
            const expr = (try self.parseExpr(.{})) orelse {
                return self.reportError("Expected lambda body expression.", &.{});
            };
            _ = self.popBlock();

            return self.ast.newNodeErase(.lambda_expr, .{
                .params = params,
                .sig_t = .let,
                .stmts = @as([*]*ast.Node, @ptrCast(@alignCast(expr)))[0..1],
                .pos = self.tokenSrcPos(start),
                .ret = null,
            });
        }

        if (self.peek().tag() != .colon) {
            return self.reportError("Expected lambda body.", &.{});
        }
        self.advance();

        try self.pushBlock();
        const stmts = try self.parseSingleOrIndentedBodyStmts();
        _ = self.popBlock();

        const lambda = try self.ast.newNodeErase(.lambda_multi, .{
            .params = params,
            .sig_t = .let,
            .stmts = stmts,
            .pos = self.tokenSrcPos(start),
            .ret = null,
        });
        @as(*ast.Node, @ptrCast(lambda)).setBlockExpr(true);
        return lambda;
    }

    fn parseFuncLambdaOrType(self: *Parser) !*ast.Node {
        const start = self.next_pos;

        // Assume first token is `func`.
        self.advance();

        const params = try self.parseParenAndFuncParams();
        const ret = try self.parseFuncReturn();

        if (self.peek().tag() == .equal_right_angle) {
            self.advance();

            // Parse body expr.
            try self.pushBlock();
            const expr = (try self.parseExpr(.{})) orelse {
                return self.reportError("Expected lambda body expression.", &.{});
            };
            _ = self.popBlock();

            return self.ast.newNodeErase(.lambda_expr, .{
                .params = params,
                .sig_t = .func,
                .stmts = @as([*]*ast.Node, @ptrCast(@alignCast(expr)))[0..1],
                .pos = self.tokenSrcPos(start),
                .ret = ret,
            });
        }

        if (self.peek().tag() == .colon) {
            self.advance();
        } else {
            // Parse as function type.
            return self.ast.newNodeErase(.func_type, .{
                .params = params,
                .pos = self.tokenSrcPos(start),
                .ret = ret,
                .is_union = false,
            });
        }

        try self.pushBlock();
        const stmts = try self.parseSingleOrIndentedBodyStmts();
        _ = self.popBlock();

        const lambda = try self.ast.newNodeErase(.lambda_multi, .{
            .params = params,
            .sig_t = .func,
            .stmts = stmts,
            .pos = self.tokenSrcPos(start),
            .ret = ret,
        });
        @as(*ast.Node, @ptrCast(lambda)).setBlockExpr(true);
        return lambda;
    }

    fn parseParenAndFuncParams(self: *Parser) ![]*ast.FuncParam {
        const token = self.peek();
        if (token.tag() != .left_paren) {
            return self.reportError("Expected open parenthesis.", &.{});
        }
        self.advance();
        return self.parseFuncParams(&.{}, false);
    }

    fn genDynFuncParam(self: *Parser, ident: *ast.Node) !*ast.FuncParam {
        return self.ast.newNode(.func_param, .{
            .name_type = ident,
            .type = null,
            .ct_param = false,
        });
    }

    fn parseLetTableFields(self: *Parser) ![]*ast.Node {
        self.consumeWhitespaceTokens();
        if (self.peek().tag() == .right_brace) {
            self.advance();
            return &.{};
        }
        if (self.peek().tag() != .ident) {
            return self.reportError("Expected field identifier.", &.{});
        }

        var start = self.next_pos;
        var field = try self.newSpanNode(.ident, start);
        self.advance();

        const field_start = self.node_stack.items.len;
        defer self.node_stack.items.len = field_start;
        try self.pushNode(@ptrCast(field));

        while (true) {
            self.consumeWhitespaceTokens();
            if (self.peek().tag() == .comma) {
                self.advance();
                self.consumeWhitespaceTokens();
            } else if(self.peek().tag() == .right_brace) {
                self.advance();
                break;
            } else {
                return self.reportError("Expected `,` or `}`.", &.{});
            }
            if (self.peek().tag() == .right_brace) {
                self.advance();
                break;
            }

            start = self.next_pos;
            if (self.peek().tag() != .ident) {
                return self.reportError("Expected field identifier.", &.{});
            }

            field = try self.newSpanNode(.ident, start);
            self.advance();
            try self.pushNode(@ptrCast(field));
        }
        return self.ast.dupeNodes(self.node_stack.items[field_start..]);
    }

    fn parseFuncParam(self: *Parser) anyerror!*ast.FuncParam {
        var ct_param = false;
        var name_type: *ast.Node = undefined;
        if (self.peek().tag() == .pound) {
            self.advance();
            if (self.peek().tag() != .ident) {
                return self.reportError("Expected param name.", &.{});
            }
            name_type = @ptrCast(try self.newSpanNode(.ident, self.next_pos));
            self.advance();
            ct_param = true;
        } else {
            const next = self.peek();
            if (next.tag() == .ident) {
                // Check for space between ident and next token otherwise it could be a type.
                const next2 = self.peekAhead(1);
                if (next2.pos() >= next.data.end_pos + 1) {
                    name_type = @ptrCast(try self.newSpanNode(.ident, self.next_pos));
                    self.advance();
                } else {
                    name_type = (try self.parseTermExpr(.{})) orelse {
                        return self.reportError("Expected param.", &.{});
                    };
                }
            } else {
                name_type = (try self.parseTermExpr(.{})) orelse {
                    return self.reportError("Expected param.", &.{});
                };
            }
        }

        const type_spec = try self.parseOptTypeSpec(false);
        return self.ast.newNode(.func_param, .{
            .name_type = name_type,
            .type = type_spec,
            .ct_param = ct_param,
        });
    }

    /// Assumes token at first param ident or right paren.
    /// Let sema check whether param types are required since it depends on the context.
    fn parseFuncParams(self: *Parser, pre_params: []const *ast.FuncParam, comptime template: bool) ![]*ast.FuncParam {
        const CloseDelim: cy.TokenType = if (template) .right_bracket else .right_paren;

        const param_start = self.node_stack.items.len;
        defer self.node_stack.items.len = param_start;

        if (pre_params.len > 0) {
            try self.node_stack.appendSlice(self.alloc, @ptrCast(pre_params));
        }

        if (self.peek().tag() == CloseDelim) {
            self.advance();
            return &.{};
        }

        // Parse params.
        var param = try self.parseFuncParam();
        try self.pushNode(@ptrCast(param));

        while (true) {
            switch (self.peek().tag()) {
                .comma => {
                    self.advance();
                },
                CloseDelim => {
                    self.advance();
                    break;
                },
                else => return self.reportError("Expected `,`.", &.{}),
            }
            param = try self.parseFuncParam();
            try self.pushNode(@ptrCast(param));
        }
        const params = self.node_stack.items[param_start..];
        return @ptrCast(try self.ast.dupeNodes(params));
    }

    fn parseFuncReturn(self: *Parser) !?*ast.Node {
        return self.parseOptTypeSpec(false);
    }

    fn parseOptName(self: *Parser) !?*ast.Node {
        const start = self.next_pos;
        var token = self.peek();
        switch (token.tag()) {
            .void_k,
            .struct_k,
            .enum_k,
            .type_k,
            .error_k,
            .symbol_k,
            .none_k,
            .Func_k,
            .ident => {
                self.advance();
                return @ptrCast(try self.newSpanNode(.ident, start));
            },
            .raw_string => {
                self.advance();
                return @ptrCast(try self.newSpanNode(.raw_string_lit, start));
            },
            .string => {
                self.advance();
                return @ptrCast(try self.newSpanNode(.stringLit, start));
            },
            else => return null,
        }
    }

    fn parseOptNamePath(self: *Parser) !?*ast.Node {
        var name = (try self.parseOptName()) orelse {
            return null;
        };

        if (self.peek().tag() != .dot) {
            return name;
        }

        const name_start = self.node_stack.items.len;
        defer self.node_stack.items.len = name_start;
        try self.pushNode(name);
        
        while (self.peek().tag() == .dot) {
            self.advance();
            name = (try self.parseOptName()) orelse {
                return self.reportError("Expected name.", &.{});
            };
            try self.pushNode(name);
        }
        return self.ast.newNodeErase(.name_path, .{
            .path = try self.ast.dupeNodes(self.node_stack.items[name_start..]),
        });
    }

    fn parseEnumMember(self: *Parser) !*ast.EnumMember {
        const start = self.next_pos;
        if (self.peek().tag() != .case_k) {
            return self.reportError("Expected case keyword.", &.{});
        }
        self.advance();

        const name = (try self.parseOptName()) orelse {
            return self.reportError("Expected member identifier.", &.{});
        };

        var typeSpec: ?*ast.Node = null;
        const token = self.peek();
        if (token.tag() != .new_line and token.tag() != .null) {
            // Choice member.
            if (try self.parseOptTypeSpec(true)) |res| {
                typeSpec = res;
            }
        }
        return self.ast.newNode(.enumMember, .{
            .name = name,
            .typeSpec = typeSpec,
            .pos = self.tokenSrcPos(start),
        });
    }

    fn parseObjectField(self: *Parser) !?*ast.Field {
        var hidden = false;
        if (self.peek().tag() == .minus) {
            hidden = true;
            self.advance();
        }

        const name = (try self.parseOptName()) orelse {
            return null;
        };

        const typeSpec = try self.parseOptTypeSpec(true) orelse {
            return self.reportError("Expected field type specifier.", &.{});
        };

        return self.ast.newNode(.objectField, .{
            .name = name,
            .typeSpec = typeSpec,
            .hidden = hidden,
        });
    }

    const TypeDeclConfig = struct {
        attrs: []*ast.Attribute,
        hidden: bool,
        allow_decl: bool,
    };

    fn parseTemplate(self: *Parser) !*ast.Node {
        // Assumes starting with '['.
        const tag = self.peekAhead(2).tag();
        if (tag == .right_bracket or tag == .comma) {
            // Might need to parse template child declaration in the future.
            // const args = try self.parseArrayLiteral2();
            return error.TODO;
        } else {
            self.advance();
            const params = try self.parseFuncParams(&.{}, true);
            return self.ast.newNodeErase(.template, .{
                .params = params,
                .child_decl = undefined,
            });
        }
    }

    fn parseTypeDecl(self: *Parser, config: TypeDeclConfig) !*ast.Node {
        if (!config.allow_decl) {
            return self.reportError("`type` declarations are not allowed here.", &.{});
        }

        const start = self.next_pos;
        // Assumes first token is the `type` keyword.
        self.advance();

        // Parse name.
        const name = (try self.parseOptName()) orelse {
            return self.reportError("Expected type name identifier.", &.{});
        };

        var opt_template: ?*ast.Node = null;
        if (self.peek().tag() == .left_bracket) {
            opt_template = try self.parseTemplate();
            try self.staticDecls.append(self.alloc, @ptrCast(opt_template));
            self.consumeWhitespaceTokens();
        }

        var decl: *ast.Node = undefined;
        switch (self.peek().tag()) {
            .enum_k => {
                decl = @ptrCast(try self.parseEnumDecl(start, name, config));
            },
            .struct_k => {
                decl = @ptrCast(try self.parseStructDecl(start, name, false, config));
            },
            .cstruct_k => {
                decl = @ptrCast(try self.parseStructDecl(start, name, true, config));
            },
            .trait_k => {
                decl = @ptrCast(try self.parseTraitDecl(start, name, config));
            },
            // `object` is optional.
            .object_k,
            .new_line,
            .colon => {
                decl = @ptrCast(try self.parseObjectDecl(start, name, config));
            },
            .minus_right_angle => {
                decl = @ptrCast(try self.parseTypeAliasDecl(start, name, config));
            },
            .underscore => {
                decl = @ptrCast(try self.parseCustomTypeDecl(start, name, config));
            },
            else => {
                decl = @ptrCast(try self.parseDistinctTypeDecl(start, name, config));
            }
        }

        if (opt_template) |template| {
            template.cast(.template).child_decl = decl;
            return template;
        } else {
            try self.staticDecls.append(self.alloc, @ptrCast(decl));
            return decl;
        }
    }

    fn parseOptTypeSpec(self: *Parser, allowUnnamedType: bool) anyerror!?*ast.Node {
        const token = self.peek();
        switch (token.tag()) {
            .object_k => {
                if (allowUnnamedType) {
                    const decl = try self.parseObjectDecl(token.pos(), null, .{
                        .hidden = false,
                        .attrs = &.{},
                        .allow_decl = true,
                    });
                    try self.staticDecls.append(self.alloc, @ptrCast(decl));
                    return @ptrCast(decl);
                } else {
                    return self.reportError("Unnamed type is not allowed in this context.", &.{});
                }
            },
            .left_paren,
            .left_bracket,
            .star,
            .question,
            .ampersand,
            .pound,
            .void_k,
            .type_k,
            .symbol_k,
            .func_k,
            .Func_k,
            .error_k,
            .ident => {
                return try self.parseTermExpr(.{});
            },
            else => {
                return null;
            },
        }
    }

    fn parseCustomTypeDecl(self: *Parser, start: TokenId, name: *ast.Node, config: TypeDeclConfig) !*ast.CustomDecl {
        // Assumes `_`.
        self.advance();

        var funcs: []*ast.FuncDecl = &.{};
        if (self.peek().tag() == .colon) {
            self.advance();
            const req_indent = try self.parseFirstChildIndent(self.cur_indent);
            const prev_indent = self.pushIndent(req_indent);
            defer self.cur_indent = prev_indent;

            funcs = try self.parseTypeFuncs(req_indent);
        }
        return self.ast.newNode(.custom_decl, .{
            .name = name,
            .attrs = config.attrs,
            .hidden = config.hidden,
            .funcs = funcs,
            .pos = self.tokenSrcPos(start),
        });
    }

    fn parseDistinctTypeDecl(self: *Parser, start: TokenId, name: *ast.Node, config: TypeDeclConfig) !*ast.DistinctDecl {
        const target = (try self.parseOptTypeSpec(false)) orelse {
            return self.reportError("Expected type specifier.", &.{});
        };

        var funcs: []*ast.FuncDecl = &.{};
        if (self.peek().tag() == .colon) {
            self.advance();
            const req_indent = try self.parseFirstChildIndent(self.cur_indent);
            const prev_indent = self.pushIndent(req_indent);
            defer self.cur_indent = prev_indent;

            funcs = try self.parseTypeFuncs(req_indent);
        }
        return self.ast.newNode(.distinct_decl, .{
            .name = name,
            .target = target,
            .attrs = config.attrs,
            .hidden = config.hidden,
            .funcs = funcs,
            .pos = self.tokenSrcPos(start),
        });
    }

    /// Assumes current token is `=`.
    fn parseTypeAliasDecl(self: *Parser, start: TokenId, name: *ast.Node, config: TypeDeclConfig) !*ast.TypeAliasDecl {
        self.advance();
        const typeSpec = (try self.parseOptTypeSpec(false)) orelse {
            return self.reportError("Expected type specifier.", &.{});
        };
        return self.ast.newNode(.typeAliasDecl, .{
            .name = name,
            .typeSpec = typeSpec,
            .hidden = config.hidden,
            .pos = self.tokenSrcPos(start),
        });
    }

    fn parseEnumDecl(self: *Parser, start: TokenId, name: *ast.Node, config: TypeDeclConfig) !*ast.EnumDecl {
        // Assumes first token is the `enum` keyword.
        self.advance();

        var token = self.peek();
        if (token.tag() == .colon) {
            self.advance();
        } else {
            return self.reportError("Expected colon.", &.{});
        }

        const req_indent = try self.parseFirstChildIndent(self.cur_indent);
        const prev_indent = self.pushIndent(req_indent);
        defer self.cur_indent = prev_indent;

        var member = try self.parseEnumMember();

        const member_start = self.node_stack.items.len;
        defer self.node_stack.items.len = member_start;
        try self.pushNode(@ptrCast(member));

        var isChoiceType = false;

        while (true) {
            const start2 = self.next_pos;
            if (!try self.consumeNextLineIndent(req_indent)) {
                self.next_pos = start2;
                break;
            }
            member = try self.parseEnumMember();
            if (!isChoiceType) {
                if (member.typeSpec != null) {
                    isChoiceType = true;
                }
            }
            try self.pushNode(@ptrCast(member));
        }
        const members: []*ast.EnumMember = @ptrCast(try self.ast.dupeNodes(self.node_stack.items[member_start..]));
        return self.ast.newNode(.enumDecl, .{
            .name = name,
            .members = members,
            .isChoiceType = isChoiceType,
            .hidden = config.hidden,
            .pos = self.tokenSrcPos(start),
        });
    }

    fn newObjectDecl(self: *Parser, start: TokenId, node_t: ast.NodeType, opt_name: ?*ast.Node,
        config: TypeDeclConfig, impl_withs: []*ast.ImplWith, fields: []*ast.Field, funcs: []*ast.FuncDecl,
        is_tuple: bool) !*ast.ObjectDecl {

        const n = try self.ast.newNodeErase(.objectDecl, .{
            .name = opt_name,
            .pos = self.tokenSrcPos(start),
            .impl_withs = impl_withs,
            .fields = fields,
            .attrs = config.attrs,
            .unnamed = opt_name == null,
            .funcs = funcs,
            .is_tuple = is_tuple,
        });
        n.setType(node_t);
        return @ptrCast(@alignCast(n));
    }

    fn parseImplWiths(self: *Parser, req_indent: u32) ![]*ast.ImplWith {
        if (self.peek().tag() != .with_k) {
            return &.{};
        }

        const with_start = self.node_stack.items.len;
        defer self.node_stack.items.len = with_start;

        while (true) {
            if (self.peek().tag() != .with_k) {
                break;
            }
            const start = self.next_pos;
            self.advance();

            const trait = (try self.parseExpr(.{})) orelse {
                return self.reportError("Expected trait type.", &.{});
            };
            const with = try self.ast.newNode(.impl_with, .{
                .trait = trait,
                .pos = self.tokenSrcPos(start),
            });
            try self.pushNode(@ptrCast(with));

            if (!try self.consumeNextLineIndent(req_indent)) {
                self.next_pos = start;
                break;
            }
        }

        return @ptrCast(try self.ast.dupeNodes(self.node_stack.items[with_start..]));
    }

    fn parseTupleFields(self: *Parser) ![]*ast.Field {
        var field = (try self.parseObjectField()) orelse return &.{};

        const field_start = self.node_stack.items.len;
        defer self.node_stack.items.len = field_start;
        try self.pushNode(@ptrCast(field));

        while (true) {
            if (self.peek().tag() == .right_paren) {
                self.advance();
                break;
            }
            if (self.peek().tag() != .comma) {
                return self.reportError("Expected comma or right parenthesis.", &.{});
            }
            self.advance();
            field = (try self.parseObjectField()) orelse return error.Unexpected;
            try self.pushNode(@ptrCast(field));
        }
        return @ptrCast(try self.ast.dupeNodes(self.node_stack.items[field_start..]));
    }

    fn parseTypeFields(self: *Parser, req_indent: u32, has_more_members: *bool) ![]*ast.Field {
        var field = (try self.parseObjectField()) orelse {
            has_more_members.* = true;
            return &.{};
        };

        const field_start = self.node_stack.items.len;
        defer self.node_stack.items.len = field_start;
        try self.pushNode(@ptrCast(field));

        while (true) {
            const start = self.next_pos;
            if (!try self.consumeNextLineIndent(req_indent)) {
                self.next_pos = start;
                has_more_members.* = false;
                break;
            }
            field = (try self.parseObjectField()) orelse {
                has_more_members.* = true;
                break;
            };
            try self.pushNode(@ptrCast(field));
        }
        return @ptrCast(try self.ast.dupeNodes(self.node_stack.items[field_start..]));
    }

    fn parseTypeFuncs(self: *Parser, req_indent: u32) ![]*ast.FuncDecl {
        if (self.isAtEnd()) {
            return &.{};
        }
        var func = try self.parseStatement(.{ .allow_decls = true });
        if (func.type() != .funcDecl) {
            return self.reportErrorAtSrc("Expected function.", &.{}, func.pos());
        }

        const func_start = self.node_stack.items.len;
        defer self.node_stack.items.len = func_start;
        try self.pushNode(func);

        while (true) {
            const start = self.next_pos;
            if (!try self.consumeNextLineIndent(req_indent)) {
                self.next_pos = start;
                break;
            }
            func = try self.parseStatement(.{ .allow_decls = true });
            if (func.type() != .funcDecl) {
                return self.reportErrorAtSrc("Expected function.", &.{}, func.pos());
            }
            try self.pushNode(func);
        }
        return @ptrCast(try self.ast.dupeNodes(self.node_stack.items[func_start..]));
    }

    fn parseTraitDecl(self: *Parser, start: TokenId, name: *ast.Node, config: TypeDeclConfig) anyerror!*ast.TraitDecl {
        var token = self.peek();
        if (token.tag() != .trait_k) {
            return self.reportErrorAt("Expected `trait` keyword.", &.{}, self.next_pos);
        }
        self.advance();

        token = self.peek();
        if (token.tag() == .colon) {
            self.advance();
        } else {
            // Only declaration. No members.
            return self.ast.newNode(.trait_decl, .{
                .name = name,
                .pos = self.tokenSrcPos(start),
                .attrs = config.attrs,
                .funcs = &.{},
            });
        }

        const req_indent = try self.parseFirstChildIndent(self.cur_indent);
        const prev_indent = self.pushIndent(req_indent);
        defer self.cur_indent = prev_indent;
        const funcs = try self.parseTypeFuncs(req_indent);

        return self.ast.newNode(.trait_decl, .{
            .name = name,
            .pos = self.tokenSrcPos(start),
            .attrs = config.attrs,
            .funcs = funcs,
        });
    }

    fn parseStructDecl(self: *Parser, start: TokenId, name: ?*ast.Node, cstruct: bool, config: TypeDeclConfig) anyerror!*ast.ObjectDecl {
        const ntype: ast.NodeType = if (cstruct) .cstruct_decl else .structDecl;
        var token = self.peek();
        // Assumes `struct` or `cstruct` keyword.
        self.advance();

        token = self.peek();
        if (token.tag() == .colon) {
            self.advance();
        } else if (token.tag() == .left_paren) {
            self.advance();
            const fields = try self.parseTupleFields();
            if (token.tag() != .colon) {
                try self.consumeNewLineOrEnd();
                return self.newObjectDecl(start, ntype, name, config, &.{}, fields, &.{}, true);
            }

            const req_indent = try self.parseFirstChildIndent(self.cur_indent);
            const prev_indent = self.pushIndent(req_indent);
            defer self.cur_indent = prev_indent;

            const funcs = try self.parseTypeFuncs(req_indent);
            return self.newObjectDecl(start, ntype, name, config, &.{}, fields, funcs, true);
        } else {
            // Only declaration. No members.
            return self.newObjectDecl(start, ntype, name, config, &.{}, &.{}, &.{}, false);
        }

        const req_indent = try self.parseFirstChildIndent(self.cur_indent);
        const prev_indent = self.pushIndent(req_indent);
        defer self.cur_indent = prev_indent;

        // Check for impl `with`.
        const impl_withs = try self.parseImplWiths(req_indent);

        var has_more_members: bool = undefined;
        const fields = try self.parseTypeFields(req_indent, &has_more_members);
        if (!has_more_members) {
            return self.newObjectDecl(start, ntype, name, config, impl_withs, fields, &.{}, false);
        }
        const funcs = try self.parseTypeFuncs(req_indent);
        return self.newObjectDecl(start, ntype, name, config, impl_withs, fields, funcs, false);
    }

    fn parseObjectDecl(self: *Parser, start: TokenId, name: ?*ast.Node, config: TypeDeclConfig) anyerror!*ast.ObjectDecl {
        var token = self.peek();
        // Optional `object` keyword.
        if (token.tag() == .object_k) {
            self.advance();
        }

        token = self.peek();
        if (token.tag() == .colon) {
            self.advance();
        } else {
            // Only declaration. No members.
            return self.newObjectDecl(start, .objectDecl, name, config, &.{}, &.{}, &.{}, false);
        }

        const req_indent = try self.parseFirstChildIndent(self.cur_indent);
        const prev_indent = self.pushIndent(req_indent);
        defer self.cur_indent = prev_indent;

        // Check for impl `with`.
        const impl_withs = try self.parseImplWiths(req_indent);

        var has_more_members: bool = undefined;
        const fields = try self.parseTypeFields(req_indent, &has_more_members);
        if (!has_more_members) {
            return self.newObjectDecl(start, .objectDecl, name, config, impl_withs, fields, &.{}, false);
        }
        const funcs = try self.parseTypeFuncs(req_indent);
        return self.newObjectDecl(start, .objectDecl, name, config, impl_withs, fields, funcs, false);
    }

    const FuncDeclConfig = struct {
        attrs: []*ast.Attribute,
        hidden: bool,
        allow_decl: bool,
    };

    fn parseFuncDecl(self: *Parser, config: FuncDeclConfig) !*ast.Node {
        if (!config.allow_decl) {
            return self.reportError("`func` declarations are not allowed here.", &.{});
        }
        const start = self.next_pos;
        // Assumes first token is the `func` keyword.
        self.advance();

        // Parse function name.
        const name = (try self.parseOptNamePath()) orelse {
            return self.reportError("Expected function name identifier.", &.{});
        };

        var opt_template: ?*ast.TemplateDecl = null;
        var params: []*ast.FuncParam = undefined;
        if (self.peek().tag() == .left_bracket) {
            self.advance();
            params = try self.parseFuncParams(&.{}, true);
            opt_template = try self.ast.newNode(.template, .{
                .params = params,
                .child_decl = undefined,
            });
            try self.staticDecls.append(self.alloc, @ptrCast(opt_template));
            self.consumeWhitespaceTokens();
        } else {
            params = try self.parseParenAndFuncParams();
        }
        const ret = try self.parseFuncReturn();

        // const nameStr = self.ast.nodeString(name);
        // const block = &self.blockStack.items[self.blockStack.items.len-1];
        // try block.vars.put(self.alloc, nameStr, {});

        var token = self.peek();
        var decl: *ast.FuncDecl = undefined;
        if (token.tag() == .colon) {
            self.advance();

            try self.pushBlock();
            const stmts = try self.parseSingleOrIndentedBodyStmts();
            _ = self.popBlock();

            decl = try self.ast.newNode(.funcDecl, .{
                .name = name,
                .attrs = config.attrs,
                .params = params,
                .ret = ret,
                .stmts = stmts,
                .sig_t = .func,
                .hidden = config.hidden,
                .pos = self.tokenSrcPos(start),
            });
        } else {
            // Just a declaration, no body.
            decl = try self.ast.newNode(.funcDecl, .{
                .attrs = config.attrs,
                .ret = ret,
                .name = name,
                .params = params,
                .stmts = &.{},
                .sig_t = .func,
                .hidden = config.hidden,
                .pos = self.tokenSrcPos(start),
            });
        }

        if (opt_template) |template| {
            template.child_decl = @ptrCast(decl);
        } else {
            if (self.cur_indent == 0) {
                try self.staticDecls.append(self.alloc, @ptrCast(decl));
            }
        }
        return @ptrCast(decl);
    }

    fn parseElseStmts(self: *Parser) ![]*ast.ElseBlock {
        var else_stmt: ?*ast.ElseBlock = (try self.parseElseStmt()) orelse {
            return &.{};
        };
        const start = self.node_stack.items.len;
        defer self.node_stack.items.len = start;

        while (else_stmt != null) {
            try self.pushNode(@ptrCast(else_stmt.?));
            else_stmt = try self.parseElseStmt();
        }

        return @ptrCast(try self.ast.dupeNodes(self.node_stack.items[start..]));
    }

    fn parseElseStmt(self: *Parser) anyerror!?*ast.ElseBlock {
        const save = self.next_pos;
        const indent = try self.consumeFirstIndent();
        if (indent != self.cur_indent) {
            self.next_pos = save;
            return null;
        }

        var token = self.peek();
        if (token.tag() != .else_k) {
            self.next_pos = save;
            return null;
        }

        const start = self.next_pos;
        self.advance();

        token = self.peek();
        if (token.tag() == .colon) {
            // else block.
            self.advance();

            const stmts = try self.parseSingleOrIndentedBodyStmts();
            return self.ast.newNode(.else_block, .{
                .cond = null,
                .stmts = stmts,
                .pos = self.tokenSrcPos(start),
            });
        } else {
            // else if block.
            const cond = (try self.parseExpr(.{})) orelse {
                return self.reportError("Expected else if condition.", &.{});
            };
            token = self.peek();
            if (token.tag() == .colon) {
                self.advance();
                const stmts = try self.parseSingleOrIndentedBodyStmts();
                return self.ast.newNode(.else_block, .{
                    .cond = cond,
                    .stmts = stmts,
                    .pos = self.tokenSrcPos(start),
                });
            } else {
                return self.reportError("Expected colon after else if condition.", &.{});
            }
        }
    }

    fn parseSwitch(self: *Parser, isStmt: bool) !*ast.SwitchBlock {
        const start = self.next_pos;
        // Assumes first token is the `switch` keyword.
        self.advance();

        const expr = (try self.parseExpr(.{})) orelse {
            return self.reportError("Expected switch expression.", &.{});
        };

        var caseIndent = self.cur_indent;
        var isBlock = false;
        if (self.peek().tag() == .colon) {
            isBlock = true;
            self.advance();
            caseIndent = try self.parseFirstChildIndent(self.cur_indent);
        } else {
            self.consumeWhitespaceTokens();
        }

        var case = (try self.parseCaseBlock()) orelse {
            return self.reportError("Expected case or else block.", &.{});
        };

        const case_start = self.node_stack.items.len;
        defer self.node_stack.items.len = case_start;
        try self.pushNode(@ptrCast(case));

        // Parse body statements until no more case blocks indentation recedes.
        while (true) {
            const save = self.next_pos;
            if (!try self.consumeNextLineIndent(caseIndent)) {
                self.next_pos = save;
                break;
            }
            case = (try self.parseCaseBlock()) orelse {
                if (isBlock) {
                    return self.reportError("Expected case or else block.", &.{});
                }
                // Restore so that next statement outside switch can be parsed.
                self.next_pos = save;
                break;
            };
            try self.pushNode(@ptrCast(case));
        }

        const cases: []*ast.CaseBlock = @ptrCast(try self.ast.dupeNodes(self.node_stack.items[case_start..]));
        const switch_n = try self.ast.newNode(.switchStmt, .{
            .expr = expr,
            .cases = cases,
            .pos = self.tokenSrcPos(start),
        });
        if (!isStmt) {
            @as(*ast.Node, @ptrCast(switch_n)).setType(.switchExpr);
        }
        return switch_n;
    }

    fn parseTryStmt(self: *Parser) !*ast.TryStmt {
        const start = self.next_pos;
        // Assumes first tokens are `try` and `:`.
        self.advance();
        self.advance();

        const tryStmts = try self.parseSingleOrIndentedBodyStmts();

        const indent = try self.consumeFirstIndent();
        if (indent != self.cur_indent) {
            return self.reportError("Expected catch block.", &.{});
        }

        var token = self.peek();
        if (token.tag() != .catch_k) {
            return self.reportError("Expected catch block.", &.{});
        }
        const catch_start = self.next_pos;
        self.advance();

        token = self.peek();
        var errorVar: ?*ast.Node = null;
        if (token.tag() == .ident) {
            errorVar = @ptrCast(try self.newSpanNode(.ident, self.next_pos));
            self.advance();
        }

        token = self.peek();
        if (token.tag() != .colon) {
            return self.reportError("Expected colon.", &.{});
        }
        self.advance();

        const catch_stmts = try self.parseSingleOrIndentedBodyStmts();
        const catchStmt = try self.ast.newNode(.catchStmt, .{
            .errorVar = errorVar,
            .stmts = catch_stmts,
            .pos = self.tokenSrcPos(catch_start),
        });

        return self.ast.newNode(.tryStmt, .{
            .stmts = tryStmts,
            .catchStmt = catchStmt,
            .pos = self.tokenSrcPos(start),
        });
    }

    fn parseIfStatement(self: *Parser) !*ast.Node {
        const start = self.next_pos;
        // Assumes first token is the `if` keyword.
        self.advance();

        const cond = (try self.parseExpr(.{})) orelse {
            return self.reportError("Expected if condition.", &.{});
        };

        var token = self.peek();
        if (token.tag() == .colon) {
            self.advance();
            const stmts = try self.parseSingleOrIndentedBodyStmts();
            const else_blocks = try self.parseElseStmts();

            return self.ast.newNodeErase(.if_stmt, .{
                .cond = cond,
                .stmts = stmts,
                .else_blocks = else_blocks,
                .pos = self.tokenSrcPos(start),
            });
        } else if (token.tag() == .minus_right_angle) {
            self.advance();

            const unwrap = (try self.parseExpr(.{})) orelse {
                return self.reportError("Expected unwrap variable.", &.{});
            };

            if (self.peek().tag() != .colon) {
                return self.reportError("Expected colon.", &.{});
            }
            self.advance();

            const stmts = try self.parseSingleOrIndentedBodyStmts();
            const else_blocks = try self.parseElseStmts();
            return self.ast.newNodeErase(.if_unwrap_stmt, .{
                .opt = cond,
                .unwrap = unwrap,
                .stmts = stmts,
                .else_blocks = else_blocks,
                .pos = self.tokenSrcPos(start),
            });
        } else {
            return self.reportError("Expected colon after if condition.", &.{});
        }
    }

    fn parseUseStmt(self: *Parser) !*ast.Node {
        const start = self.next_pos;
        // Assumes first token is the `use` keyword.
        self.advance();

        var name: *ast.Node = undefined;
        var spec: ?*ast.Node = null;
        if (self.peek().tag() == .ident) {
            name = @ptrCast(try self.newSpanNode(.ident, self.next_pos));
            self.advance();

            switch (self.peek().tag()) {
                .raw_string => {
                    spec = @ptrCast(try self.newSpanNode(.raw_string_lit, self.next_pos));
                    self.advance();
                },
                .new_line => {
                },
                .null => {},
                .minus_right_angle => {
                    self.advance();
                    const target = (try self.parseExpr(.{})) orelse {
                        return self.reportError("Expected condition expression.", &.{});
                    };
                    const alias = try self.ast.newNode(.use_alias, .{
                        .name = name,
                        .target = target,
                        .pos = self.tokenSrcPos(start),
                    });
                    try self.staticDecls.append(self.alloc, @ptrCast(alias));
                    return @ptrCast(alias);
                },
                else => {
                    return self.reportError("Expected a module specifier.", &.{});
                },
            }
        } else if (self.peek().tag() == .star) {
            name = try self.ast.newNodeErase(.all, .{ .pos = self.tokenSrcPos(self.next_pos) });
            self.advance();

            if (self.peek().tag() != .raw_string) {
                return self.reportError("Expected a module specifier.", &.{});
            }
            spec = @ptrCast(try self.newSpanNode(.raw_string_lit, self.next_pos));
            self.advance();
        } else {
            return self.reportError("Expected import clause.", &.{});
        }

        const import = try self.ast.newNode(.import_stmt, .{
            .name = name,
            .spec = spec,
            .pos = self.tokenSrcPos(start),
        });
        try self.staticDecls.append(self.alloc, @ptrCast(import));
        return @ptrCast(import);
    }

    fn parseWhileStatement(self: *Parser) !*ast.Node {
        const start = self.next_pos;
        // Assumes first token is the `while` keyword.
        self.advance();

        var token = self.peek();
        if (token.tag() == .colon) {
            self.advance();

            // Infinite loop.
            const stmts = try self.parseSingleOrIndentedBodyStmts();
            return self.ast.newNodeErase(.whileInfStmt, .{
                .stmts = stmts,
                .pos = self.tokenSrcPos(start),
            });
        }

        // Parse next token as expression.
        const expr = (try self.parseExpr(.{})) orelse {
            return self.reportError("Expected condition expression.", &.{});
        };

        token = self.peek();
        if (token.tag() == .colon) {
            self.advance();
            const stmts = try self.parseSingleOrIndentedBodyStmts();
            return self.ast.newNodeErase(.whileCondStmt, .{
                .cond = expr,
                .stmts = stmts,
                .pos = self.tokenSrcPos(start),
            });
        } else if (token.tag() == .minus_right_angle) {
            self.advance();
            token = self.peek();
            const ident = (try self.parseExpr(.{})) orelse {
                return self.reportError("Expected ident.", &.{});
            };
            if (ident.type() != .ident) {
                return self.reportError("Expected ident.", &.{});
            }
            token = self.peek();
            if (token.tag() != .colon) {
                return self.reportError("Expected :.", &.{});
            }
            self.advance();
            const stmts = try self.parseSingleOrIndentedBodyStmts();
            return self.ast.newNodeErase(.whileOptStmt, .{
                .opt = expr,
                .capture = ident,
                .stmts = stmts,
                .pos = self.tokenSrcPos(start),
            });
        } else {
            return self.reportError("Expected :.", &.{});
        }
    }

    fn parseForStatement(self: *Parser) !*ast.Node {
        const start = self.next_pos;
        // Assumes first token is the `for` keyword.
        self.advance();

        var token = self.peek();
        // Parse next token as expression.
        const expr_pos = self.next_pos;
        _ = expr_pos;
        const expr = (try self.parseExpr(.{})) orelse {
            return self.reportError("Expected condition expression.", &.{});
        };

        if (expr.type() == .range) {
            const range = expr.cast(.range);
            if (range.start == null) {
                return self.reportError("Expected left range expression.", &.{});
            }
            if (range.end == null) {
                return self.reportError("Expected right range expression.", &.{});
            }

            token = self.peek();
            if (token.tag() == .colon) {
                self.advance();

                const stmts = try self.parseSingleOrIndentedBodyStmts();
                return self.ast.newNodeErase(.forRangeStmt, .{
                    .stmts = stmts,
                    .start = range.start.?,
                    .end = range.end.?,
                    .increment = range.inc,
                    .each = null,
                    .pos = self.tokenSrcPos(start),
                });
            } else if (token.tag() == .minus_right_angle) {
                self.advance();

                token = self.peek();
                const ident = (try self.parseExpr(.{})) orelse {
                    return self.reportError("Expected ident.", &.{});
                };
                if (ident.type() != .ident) {
                    return self.reportErrorAt("Expected ident.", &.{}, token.pos());
                }
                token = self.peek();
                if (token.tag() != .colon) {
                    return self.reportError("Expected :.", &.{});
                }
                self.advance();

                const stmts = try self.parseSingleOrIndentedBodyStmts();
                return self.ast.newNodeErase(.forRangeStmt, .{
                    .stmts = stmts,
                    .start = range.start.?,
                    .end = range.end.?,
                    .increment = range.inc,
                    .each = ident,
                    .pos = self.tokenSrcPos(start),
                });
            } else {
                return self.reportError("Expected :.", &.{});
            }
        }

        token = self.peek();
        if (token.tag() == .colon) {
            self.advance();
            const stmts = try self.parseSingleOrIndentedBodyStmts();
            return self.ast.newNodeErase(.forIterStmt, .{
                .iterable = expr,
                .each = null,
                .count = null,
                .stmts = stmts,
                .pos = self.tokenSrcPos(start),
            });
        } else if (token.tag() == .minus_right_angle) {
            self.advance();
            token = self.peek();
            var eachClause: *ast.Node = undefined;
            if (token.tag() == .left_brace) {
                eachClause = @ptrCast(try self.parseSeqDestructure());
            } else {
                eachClause = (try self.parseExpr(.{})) orelse {
                    return self.reportError("Expected each clause.", &.{});
                };
            }

            // Optional count var.
            var count: ?*ast.Node = null;
            if (self.peek().tag() == .comma) {
                self.advance();
                count = (try self.parseExpr(.{})) orelse {
                    return self.reportError("Expected count declaration.", &.{});
                };
            }

            if (self.peek().tag() == .colon) {
                self.advance();
            } else {
                return self.reportError("Expected :.", &.{});
            }

            const stmts = try self.parseSingleOrIndentedBodyStmts();
            return self.ast.newNodeErase(.forIterStmt, .{
                .iterable = expr,
                .each = eachClause,
                .count = count,
                .stmts = stmts,
                .pos = self.tokenSrcPos(start),
            });
        } else {
            return self.reportError("Expected :.", &.{});
        }
    }

    // fn parseBlock(self: *Parser) !*ast.Node {
    //     const start = self.next_pos;
    //     // Assumes first token is the ident.
    //     const name = try self.pushSpanNode(.ident, start);
    //     self.advance();
    //     // Assumes second token is colon.
    //     self.advance();

    //     // Parse body.
    //     try self.pushBlock();
    //     const res = try self.parseIndentedBodyStatements();
    //     _ = self.popBlock();
        
    //     const id = try self.pushNode(.label_decl, start);
    //     self.nodes.items[id].head = .{
    //         .left_right = .{
    //             .left = name,
    //             .right = res.first,
    //         },
    //     };
    //     return id;
    // }

    fn parseCaseBlock(self: *Parser) !?*ast.CaseBlock {
        const start = self.next_pos;
        var token = self.peek();
        var isElse: bool = false;
        var bodyExpr: bool = false;
        var capture: ?*ast.Node = null;

        const case_start = self.node_stack.items.len;
        defer self.node_stack.items.len = case_start;

        if (token.tag() == .case_k) {
            self.advance();
            var cond = (try self.parseTermExpr2Opt(.{})) orelse {
                return self.reportError("Expected case condition.", &.{});
            };
            try self.pushNode(cond);

            while (true) {
                token = self.peek();
                if (token.tag() == .colon) {
                    self.advance();
                    break;
                } else if (token.tag() == .equal_right_angle) {
                    self.advance();
                    bodyExpr = true;
                    break;
                } else if (token.tag() == .comma) {
                    self.advance();
                    self.consumeWhitespaceTokens();
                    cond = (try self.parseTermExpr2Opt(.{})) orelse {
                        return self.reportError("Expected case condition.", &.{});
                    };
                    try self.pushNode(cond);
                } else if (token.tag() == .minus_right_angle) {
                    self.advance();

                    // Parse next token as expression.
                    capture = try self.parseTermExpr2(.{});

                    token = self.peek();
                    if (token.tag() == .colon) {
                        self.advance();
                        break;
                    } else if (token.tag() == .equal_right_angle) {
                        self.advance();
                        bodyExpr = true;
                        break;
                    } else {
                        return self.reportError("Expected comma or colon.", &.{});
                    }
                } else {
                    return self.reportError("Expected comma or colon.", &.{});
                }
            }
        } else if (token.tag() == .else_k) {
            self.advance();
            isElse = true;

            if (self.peek().tag() == .colon) {
                self.advance();
            } else if (self.peek().tag() == .equal_right_angle) {
                self.advance();
                bodyExpr = true;
            } else {
                return self.reportError("Expected colon or `=>`.", &.{});
            }
        } else return null;

        // Parse body.
        var stmts: []*ast.Node = undefined;
        if (bodyExpr) {
            const expr = (try self.parseExpr(.{})) orelse {
                return self.reportError("Expected expression.", &.{});
            };
            stmts = @as([*]*ast.Node, @ptrCast(@alignCast(expr)))[0..1];
        } else {
            stmts = try self.parseSingleOrIndentedBodyStmts();
        }

        const conds = try self.ast.dupeNodes(self.node_stack.items[case_start..]);
        return self.ast.newNode(.caseBlock, .{
            .capture = capture,
            .conds = conds,
            .stmts = stmts,
            .bodyIsExpr = bodyExpr,
            .pos = self.tokenSrcPos(start),
        });
    }

    const ParseStmtConfig = struct {
        allow_decls: bool = false,
    };

    /// Does not consume line end.
    fn parseStatement(self: *Parser, config: ParseStmtConfig) anyerror!*ast.Node {
        const start = self.next_pos;
        switch (self.peek().tag()) {
            .ident => {
                const token2 = self.peekAhead(1);
                if (token2.tag() == .colon) {
                    // return try self.parseBlock();
                    return self.reportError("Unsupported block statement.", &.{});
                } else {
                    if (try self.parseExprOrAssignStatement()) |id| {
                        return id;
                    }
                }
            },
            .at => {
                return self.parseAtDecl(config.allow_decls);
            },
            .pound => {
                self.advance();
                if (self.peek().tag() != .ident) {
                    return self.reportError("Unsupported compile-time statement.", &.{});
                }
                const ident = try self.newSpanNode(.ident, self.next_pos);
                self.advance();

                if (self.peek().tag() != .left_paren) {
                    return self.reportError("Expected ( after ident.", &.{});
                }

                const call: *ast.Node = @ptrCast(try self.parseCallExpression(@ptrCast(ident), false));
                return self.ast.newNodeErase(.comptimeStmt, .{
                    .expr = call,
                    .pos = self.tokenSrcPos(start),
                });
            },
            .minus => {
                const next = self.peekAhead(1).tag();
                switch (next) {
                    .type_k => {
                        self.advance();
                        return try self.parseTypeDecl(.{
                            .attrs = &.{},
                            .hidden = true,
                            .allow_decl = config.allow_decls,
                        });
                    },
                    .func_k => {
                        self.advance();
                        return self.parseFuncDecl(.{
                            .attrs = &.{},
                            .hidden = true,
                            .allow_decl = config.allow_decls,
                        });
                    },
                    .let_k => {
                        self.advance();
                        return self.parseLetDecl(&.{}, true, config.allow_decls);
                    },
                    .var_k => {
                        self.advance();
                        return self.parseVarDecl(.{ .attrs = &.{}, .typed = true, .hidden = true, .allow_static = config.allow_decls });
                    },
                    else => {},
                }
            },
            .type_k => {
                return try self.parseTypeDecl(.{
                    .attrs = &.{},
                    .hidden = false,
                    .allow_decl = config.allow_decls,
                });
            },
            .func_k => {
                return self.parseFuncDecl(.{ .attrs = &.{}, .hidden = false, .allow_decl = config.allow_decls});
            },
            .if_k => {
                return try self.parseIfStatement();
            },
            .try_k => {
                if (self.peekAhead(1).tag() == .colon) {
                    return @ptrCast(try self.parseTryStmt());
                }
            },
            .switch_k => {
                return @ptrCast(try self.parseSwitch(true));
            },
            .for_k => {
                return try self.parseForStatement();
            },
            .while_k => {
                return try self.parseWhileStatement();
            },
            .use_k => {
                if (!config.allow_decls) {
                    return self.reportError("`use` declarations are not allowed here.", &.{});
                }
                return self.parseUseStmt();
            },
            .pass_k => {
                self.advance();
                return self.ast.newNodeErase(.passStmt, .{
                    .pos = self.tokenSrcPos(start),
                });
            },
            .continue_k => {
                self.advance();
                return self.ast.newNodeErase(.continueStmt, .{
                    .pos = self.tokenSrcPos(start),
                });
            },
            .break_k => {
                self.advance();
                return self.ast.newNodeErase(.breakStmt, .{
                    .pos = self.tokenSrcPos(start),
                });
            },
            .return_k => {
                return try self.parseReturnStatement();
            },
            .var_k => {
                return try self.parseVarDecl(.{ .attrs = &.{}, .typed = true, .hidden = false, .allow_static = config.allow_decls });
            },
            .context_k => {
                return @ptrCast(try self.parseContextDecl());
            },
            .let_k => {
                return try self.parseLetDecl(&.{}, false, config.allow_decls);
            },
            else => {},
        }
        if (try self.parseExprOrAssignStatement()) |id| {
            return id;
        }
        const token = self.peek();
        return self.reportErrorAtSrc("Unknown token: {}", &.{v(token.tag())}, token.pos());
    }

    fn reportError(self: *Parser, format: []const u8, args: []const fmt.FmtValue) anyerror {
        return self.reportErrorAt(format, args, self.next_pos);
    }

    fn reportErrorAt(self: *Parser, format: []const u8, args: []const fmt.FmtValue, tokenPos: u32) anyerror {
        var srcPos: u32 = undefined;
        if (tokenPos >= self.tokens.len) {
            srcPos = @intCast(self.ast.src.len);
        } else {
            srcPos = self.tokens[tokenPos].pos();
        }
        return self.reportErrorAtSrc(format, args, srcPos);
    }

    fn reportErrorAtSrc(self: *Parser, format: []const u8, args: []const fmt.FmtValue, srcPos: u32) anyerror {
        self.has_error = true;
        return self.reportFn(self.ctx, format, args, srcPos);
    }

    fn consumeNewLineOrEnd(self: *Parser) !void {
        const tag = self.peek().tag();
        if (tag == .new_line) {
            self.advance();
            return;
        }
        if (tag == .null) {
            return;
        }
        return self.reportError("Expected end of line or file. Got {}.", &.{v(tag)});
    }

    fn consumeWhitespaceTokens(self: *Parser) void {
        var token = self.peek();
        while (token.tag() != .null) {
            switch (token.tag()) {
                .new_line,
                .indent => {
                    self.advance();
                    token = self.peek();
                    continue;
                },
                else => return,
            }
        }
    }

    /// Assumes at `@` token.
    fn parseAttr(self: *Parser) !*ast.Attribute {
        self.advance();
        const token = self.peek();
        if (token.tag() != .ident) {
            return self.reportError("Expected ident after `@`.", &.{});
        }
        const start = self.next_pos;
        const name = self.ast.src[token.pos()..token.data.end_pos];
        const attr_t = attributes.get(name) orelse {
            return self.reportError("Unknown attribute.", &.{});
        };
        self.advance();

        // Check for arguments.
        var value: ?*ast.Node = null;
        if (self.peek().tag() == .equal) {
            self.advance();
            value = (try self.parseExpr(.{})) orelse {
                return self.reportError("Expected assignment value.", &.{});
            };
        }
        return self.ast.newNode(.attribute, .{
            .type = attr_t,
            .value = value,
            .pos = self.tokenSrcPos(start),
        });
    }

    fn parseAtDecl(self: *Parser, allow_decls: bool) !*ast.Node {
        const attr: *ast.Node = @ptrCast(try self.parseAttr());
        self.consumeWhitespaceTokens();

        var hidden = false;
        if (self.peek().tag() == .minus) {
            hidden = true;
            self.advance();
        }

        const attrs: []*ast.Attribute = @ptrCast(try self.ast.dupeNodes(&.{attr}));
        if (self.peek().tag() == .func_k) {
            return self.parseFuncDecl(.{ .hidden = hidden, .attrs = attrs, .allow_decl = allow_decls });
        } else if (self.peek().tag() == .var_k) {
            return try self.parseVarDecl(.{ .hidden = hidden, .attrs = attrs, .typed = true, .allow_static = allow_decls });
        } else if (self.peek().tag() == .let_k) {
            return try self.parseVarDecl(.{ .hidden = hidden, .attrs = attrs, .typed = false, .allow_static = allow_decls });
        } else if (self.peek().tag() == .type_k) {
            return try self.parseTypeDecl(.{
                .attrs = attrs,
                .hidden = hidden,
                .allow_decl = allow_decls,
            });
        } else {
            return self.reportError("Expected declaration statement.", &.{});
        }
    }

    fn parseSeqDestructure(self: *Parser) !*ast.SeqDestructure {
        const start = self.next_pos;
        // Assume first token is left bracket.
        self.advance();

        const decl_start = self.node_stack.items.len;
        defer self.node_stack.items.len = decl_start;

        outer: {
            self.consumeWhitespaceTokens();
            var token = self.peek();

            if (token.tag() == .right_brace) {
                // Empty.
                return self.reportError("Expected at least one identifier.", &.{});
            } else {
                const decl = (try self.parseExpr(.{})) orelse {
                    return self.reportError("Expected array item.", &.{});
                };
                if (decl.type() != .ident) {
                    return self.reportError("Expected ident.", &.{});
                }
                try self.pushNode(decl);
            }

            while (true) {
                self.consumeWhitespaceTokens();
                token = self.peek();
                if (token.tag() == .comma) {
                    self.advance();
                    if (self.peek().tag() == .new_line) {
                        self.advance();
                        self.consumeWhitespaceTokens();
                    }
                } else if (token.tag() == .right_brace) {
                    break :outer;
                }

                token = self.peek();
                if (token.tag() == .right_brace) {
                    break :outer;
                } else {
                    const decl = (try self.parseExpr(.{})) orelse {
                        return self.reportError("Expected array item.", &.{});
                    };
                    if (decl.type() != .ident) {
                        return self.reportError("Expected ident.", &.{});
                    }
                    try self.pushNode(decl);
                }
            }
        }

        // Parse closing bracket.
        const token = self.peek();
        if (token.tag() == .right_brace) {
            self.advance();

            const args = try self.ast.dupeNodes(self.node_stack.items[decl_start..]);
            return self.ast.newNode(.seqDestructure, .{
                .args = args,
                .pos = self.tokenSrcPos(start),
            });
        } else return self.reportError("Expected closing bracket.", &.{});
    }

    fn parseArrayLiteral(self: *Parser) !*ast.ArrayLit {
        const start = self.next_pos;
        const args = try self.parseArrayLiteral2();
        return self.ast.newNode(.array_lit, .{
            .args = args,
            .pos = self.tokenSrcPos(start),
        });
    }

    fn parseArrayLiteral2(self: *Parser) ![]*ast.Node {
        const start = self.next_pos;
        _ = start;
        // Assume first token is left bracket.
        self.advance();
        self.consumeWhitespaceTokens();

        if (self.peek().tag() == .right_bracket) {
            self.advance();
            return &.{};
        }

        var arg = (try self.parseExpr(.{})) orelse {
            return self.reportError("Expected element expression.", &.{});
        };

        const arg_start = self.node_stack.items.len;
        defer self.node_stack.items.len = arg_start;
        try self.pushNode(arg);

        while (true) {
            self.consumeWhitespaceTokens();
            if (self.peek().tag() == .comma) {
                self.advance();
                self.consumeWhitespaceTokens();
            } else {
                break;
            }
            if (self.peek().tag() == .right_bracket) {
                break;
            }

            arg = (try self.parseExpr(.{})) orelse {
                return self.reportError("Expected element expression.", &.{});
            };
            try self.pushNode(arg);
        }

        if (self.peek().tag() != .right_bracket) {
            return self.reportErrorAt("Expected closing bracket.", &.{}, self.next_pos);
        }
        self.advance();

        return self.ast.dupeNodes(self.node_stack.items[arg_start..]);
    }

    fn parseInitLiteral(self: *Parser) !*ast.InitLit {
        const start = self.next_pos;
        // Assume first token is left brace.
        self.advance();
        self.consumeWhitespaceTokens();

        if (self.peek().tag() == .right_brace) {
            self.advance();
            return self.ast.newNode(.init_lit, .{
                .args = &.{},
                .pos = self.tokenSrcPos(start),
                .array_like = false,
            });
        } else if (self.peek().tag() == .underscore) {
            self.advance();
            if (self.peek().tag() != .right_brace) {
                return self.reportErrorAt("Expected closing brace.", &.{}, self.next_pos);
            }
            self.advance();
            return self.ast.newNode(.init_lit, .{
                .args = &.{},
                .pos = self.tokenSrcPos(start),
                .array_like = true,
            });
        }

        var array_like = true;
        var is_pair = false;
        var entry = try self.parseInitEntry(&is_pair);
        if (is_pair) {
            array_like = false;
        }
        const entry_start = self.node_stack.items.len;
        defer self.node_stack.items.len = entry_start;
        try self.pushNode(@ptrCast(entry));

        while (true) {
            self.consumeWhitespaceTokens();
            if (self.peek().tag() == .comma) {
                self.advance();
                self.consumeWhitespaceTokens();
            } else {
                break;
            }
            if (self.peek().tag() == .right_brace) {
                break;
            }

            entry = try self.parseInitEntry(&is_pair);
            if (is_pair) {
                array_like = false;
            }
            try self.pushNode(@ptrCast(entry));
        }

        if (self.peek().tag() != .right_brace) {
            return self.reportErrorAt("Expected closing brace.", &.{}, self.next_pos);
        }
        self.advance();

        const args: []*ast.Node = try self.ast.dupeNodes(self.node_stack.items[entry_start..]);
        return self.ast.newNode(.init_lit, .{
            .args = args,
            .pos = self.tokenSrcPos(start),
            .array_like = array_like,
        });
    }

    fn parseInitEntry(self: *Parser, is_pair: *bool) !*ast.Node {
        const arg = (try self.parseTermExpr2Opt(.{})) orelse {
            return self.reportError("Expected arg.", &.{});
        };

        if (self.peek().tag() != .equal) {
            is_pair.* = false;
            return arg;
        }
        self.advance();

        if (!isRecordKeyNodeType(arg.type())) {
            return self.reportError("Expected record key.", &.{});
        }

        const val = (try self.parseExpr(.{})) orelse {
            return self.reportError("Expected record value.", &.{});
        };
        is_pair.* = true;
        return self.ast.newNodeErase(.keyValue, .{
            .key = arg,
            .value = val,
        });
    }

    fn parseCallArg(self: *Parser) !?*ast.Node {
        self.consumeWhitespaceTokens();
        const token = self.peek();
        if (token.tag() == .ident) {
            if (self.peekAhead(1).tag() == .colon) {
                // Named arg.
                _ = self.consume();
                _ = self.consume();
                const arg = (try self.parseExpr(.{})) orelse {
                    return self.reportError("Expected arg expression.", &.{});
                };
                return self.ast.newNodeErase(.namedArg, .{
                    .name_pos = token.pos(),
                    .name_len = token.data.end_pos - token.pos(),
                    .arg = arg,
                });
            } 
        }
        return self.parseExpr(.{});
    }

    fn parseCallArgs(self: *Parser, hasNamedArg: *bool, allow_block_expr: bool) ![]*ast.Node {
        // Assume first token is left paren.
        self.advance();

        const start = self.node_stack.items.len;
        defer self.node_stack.items.len = start;

        var has_named_arg = false;
        inner: {
            var arg = (try self.parseCallArg()) orelse {
                break :inner;
            };
            try self.pushNode(arg);
            if (arg.type() == .namedArg) {
                has_named_arg = true;
            }
            while (true) {
                const token = self.peek();
                if (token.tag() != .comma and token.tag() != .new_line) {
                    break;
                }
                self.advance();
                arg = (try self.parseCallArg()) orelse {
                    break;
                };
                try self.pushNode(arg);
                if (arg.type() == .namedArg) {
                    has_named_arg = true;
                }
            }
        }

        hasNamedArg.* = has_named_arg;

        self.consumeWhitespaceTokens();
        if (self.peek().tag() != .right_paren) {
            return self.reportError("Expected closing parenthesis.", &.{});
        }
        self.advance();

        if (allow_block_expr) {
            // Parse call block syntax.
            const block_start = self.next_pos;
            if (self.peek().tag() == .equal_right_angle) {
                self.advance();
                const params = try self.parseParenAndFuncParams();
                if (self.peek().tag() != .colon) {
                    return self.reportError("Expected colon.", &.{});
                }
                self.advance();

                const child_indent = try self.parseFirstChildIndent(self.cur_indent);
                const prev_indent = self.pushIndent(child_indent);
                defer self.cur_indent = prev_indent;

                const stmt_start = self.node_stack.items.len;

                while (true) {
                    const stmt = try self.parseStatement(.{});
                    try self.pushNode(stmt);

                    const parse_indent_start = self.next_pos;
                    if (!try self.consumeNextLineIndent(child_indent)) {
                        self.next_pos = parse_indent_start;
                        break;
                    }
                }

                const stmts = try self.ast.dupeNodes(self.node_stack.items[stmt_start..]);
                self.node_stack.items.len = stmt_start;

                const lambda = try self.ast.newNodeErase(.lambda_multi, .{
                    .sig_t = .infer,
                    .stmts = stmts,
                    .params = params,
                    .pos = self.tokenSrcPos(block_start),
                    .ret = null,
                });
                try self.pushNode(lambda);
            }
        }
        return self.ast.dupeNodes(self.node_stack.items[start..]);
    }

    fn parseCallExpression(self: *Parser, callee: *ast.Node, allow_block_expr: bool) !*ast.CallExpr {
        var hasNamedArg: bool = undefined;
        const args = try self.parseCallArgs(&hasNamedArg, allow_block_expr);
        return self.ast.newNode(.callExpr, .{
            .callee = callee,
            .args = args,
            .hasNamedArg = hasNamedArg,
        });
    }

    /// Assumes first arg exists.
    fn parseNoParenCallExpression(self: *Parser, callee: *ast.Node) !*ast.CallExpr {
        var arg = (try self.parseExpr(.{})) orelse {
            return self.reportError("Expected call arg.", &.{});
        };

        const arg_start = self.node_stack.items.len;
        defer self.node_stack.items.len = arg_start;
        try self.pushNode(arg);

        while (true) {
            const token = self.peek();
            switch (token.tag()) {
                .right_bracket,
                .right_paren,
                .new_line,
                .null => break,
                .comma => {
                    self.advance();
                    arg = (try self.parseExpr(.{})) orelse {
                        return self.reportError("Expected call arg.", &.{});
                    };
                    try self.pushNode(arg);
                },
                else => {
                    return self.reportError("Expected comma.", &.{});
                },
            }
        }
        return self.ast.newNode(.callExpr, .{
            .callee = callee,
            .args = try self.ast.dupeNodes(self.node_stack.items[arg_start..]),
            .hasNamedArg = false,
        });
    }

    /// Parses the right expression of a BinaryExpression.
    fn parseRightExpr(self: *Parser, left_op: cy.ast.BinaryExprOp, allow_block_expr: bool) anyerror!*ast.Node {
        if (self.peek().tag() == .new_line) {
            self.advance();
            self.consumeWhitespaceTokens();
        }
        const right = (try self.parseTermExpr(.{ .allow_block_expr = allow_block_expr })) orelse {
            return self.reportError("Expected right operand.", &.{});
        };
        if (right.isBlockExpr()) {
            return right;
        }
        return self.parseRightExpr2(left_op, right, allow_block_expr);
    }

    fn parseRightExpr2(self: *Parser, left_op: cy.ast.BinaryExprOp, right_id: *ast.Node, allow_block_expr: bool) anyerror!*ast.Node {
        // Check if next token is an operator with higher precedence.
        var token = self.peek();

        const rightOp = toBinExprOp(token.tag()) orelse {
            return right_id;
        };

        const op_prec = getBinOpPrecedence(left_op);
        const right_op_prec = getBinOpPrecedence(rightOp);
        if (right_op_prec > op_prec) {
            // Continue parsing right.
            _ = self.consume();
            const start = self.next_pos;
            const next_right = try self.parseRightExpr(rightOp, allow_block_expr);

            const binExpr = try self.ast.newNode(.binExpr, .{
                .left = right_id,
                .right = next_right,
                .op = rightOp,
                .op_pos = self.tokenSrcPos(start),
            });

            // Before returning the expr, perform left recursion if the op prec greater than the starting op.
            // eg. a + b * c * d
            //         ^ parseRightExpr starts here
            // Returns ((b * c) * d).
            // eg. a < b * c - d
            //         ^ parseRightExpr starts here
            // Returns ((b * c) - d).
            var left: *ast.Node = @ptrCast(binExpr);
            while (true) {
                token = self.peek();

                const rightOp2 = toBinExprOp(token.tag()) orelse {
                    return left;
                };
                const right2_op_prec = getBinOpPrecedence(rightOp2);
                if (right2_op_prec > op_prec) {
                    self.advance();
                    const rightExpr = try self.parseRightExpr(rightOp, allow_block_expr);
                    const newBinExpr = try self.ast.newNode(.binExpr, .{
                        .left = left,
                        .right = rightExpr,
                        .op = rightOp2,
                        .op_pos = self.tokenSrcPos(start),
                    });
                    left = @ptrCast(newBinExpr);
                    continue;
                } else {
                    return left;
                }
            }
        }
        return right_id;
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

    // Assume at `if` token.
    fn parseIfExpr(self: *Parser, start: u32) !*ast.Node {
        self.advance();
        if (self.peek().tag() != .left_paren) {
            return self.reportError("Expected `(` for `if` expression.", &.{});
        }
        self.advance();
        const cond = (try self.parseExpr(.{})) orelse {
            return self.reportError("Expected condition for `if` expression.", &.{});
        };
        if (self.peek().tag() != .right_paren) {
            return self.reportError("Expected `)` for `if` expression.", &.{});
        }
        self.advance();

        const body = (try self.parseExpr(.{})) orelse {
            return self.reportError("Expected conditional true expression.", &.{});
        };

        const token = self.peek();
        if (token.tag() != .else_k) {
            return self.reportError("Expected else body.", &.{});
        }
        self.advance();

        const elseExpr = (try self.parseExpr(.{})) orelse {
            return self.reportError("Expected else body.", &.{});
        };

        return self.ast.newNodeErase(.if_expr, .{
            .cond = cond,
            .body = body,
            .pos = self.tokenSrcPos(start),
            .else_expr = elseExpr,
        });
    }

    /// A string template begins and ends with .templateString token.
    /// Inside the template, two template expressions can be adjacent to each other.
    fn parseStringTemplate(self: *Parser) !*ast.StringTemplate {
        const start = self.next_pos;

        if (self.peek().tag() != .templateString) {
            return self.reportError("Expected template string.", &.{});
        }

        var part: *ast.Node = @ptrCast(try self.newSpanNode(.stringLit, start));
        self.advance();

        var lastWasStringPart = true;
        const part_start = self.node_stack.items.len;
        defer self.node_stack.items.len = part_start;
        try self.pushNode(@ptrCast(part));

        while (true) {
            const tag = self.peek().tag();
            if (tag == .templateString) {
                if (lastWasStringPart) {
                    // End of this template.
                    break;
                }
                part = @ptrCast(try self.newSpanNode(.stringLit, self.next_pos));
                lastWasStringPart = true;
            } else if (tag == .templateExprStart) {
                self.advance();
                part = (try self.parseExpr(.{})) orelse {
                    return self.reportError("Expected expression.", &.{});
                };
                if (self.peek().tag() != .right_paren) {
                    return self.reportError("Expected right paren.", &.{});
                }
                lastWasStringPart = false;
            } else {
                break;
            }
            try self.pushNode(@ptrCast(part));
            self.advance();
        }

        return self.ast.newNode(.stringTemplate, .{
            .parts = try self.ast.dupeNodes(self.node_stack.items[part_start..]),
        });
    }

    /// Assumes at `coinit` keyword.
    fn parseCoinitExpr(self: *Parser) !*ast.Node {
        const start = self.next_pos;
        self.advance();

        if (self.peek().tag() != .left_paren) {
            return self.reportError("Expected ( after coinit.", &.{});
        }
        self.advance();

        const callee = (try self.parseCallArg()) orelse {
            return self.reportError("Expected entry function callee.", &.{});
        };

        const arg_start = self.node_stack.items.len;
        defer self.node_stack.items.len = arg_start;
        if (self.peek().tag() == .comma) {
            self.advance();
            inner: {
                var arg = (try self.parseCallArg()) orelse {
                    break :inner;
                };
                try self.pushNode(arg);
                while (true) {
                    self.consumeWhitespaceTokens();

                    if (self.peek().tag() != .comma) {
                        break;
                    }
                    self.advance();
                    arg = (try self.parseCallArg()) orelse {
                        break;
                    };
                    try self.pushNode(arg);
                }
            }
        }

        self.consumeWhitespaceTokens();
        if (self.peek().tag() != .right_paren) {
            return self.reportError("Expected closing `)`.", &.{});
        }
        self.advance();

        const call = try self.ast.newNode(.callExpr, .{
            .callee = callee,
            .args = try self.ast.dupeNodes(self.node_stack.items[arg_start..]),
            .hasNamedArg = false,
        });
        return self.ast.newNodeErase(.coinit, .{
            .child = call,
            .pos = self.tokenSrcPos(start),
        });
    }

    fn parseTermExprWithLeft(self: *Parser, left_: *ast.Node, config: ParseTermConfig) !*ast.Node {
        var left = left_;
        while (true) {
            const next = self.peek();
            switch (next.tag()) {
                .dot => {
                    self.advance();

                    // Access expr.
                    const right = (try self.parseOptName()) orelse {
                        return self.reportError("Expected ident", &.{});
                    };
                    left = try self.ast.newNodeErase(.accessExpr, .{
                        .left = left,
                        .right = right,
                    });
                },
                .dot_question => {
                    self.advance();
                    left = try self.ast.newNodeErase(.unwrap, .{
                        .opt = left,
                    });
                },
                .dot_star => {
                    self.advance();
                    left = try self.ast.newNodeErase(.deref, .{
                        .left = left,
                    });
                },
                .left_bracket => {
                    const args = try self.parseArrayLiteral2();
                    left = try self.ast.newNodeErase(.array_expr, .{
                        .left = left,
                        .args = args,
                    });
                },
                .left_brace => {
                    if (!config.parse_record_expr) break;
                    const init_n = try self.parseInitLiteral();
                    left = try self.ast.newNodeErase(.init_expr, .{
                        .left = left,
                        .init = init_n,
                    });
                },
                .left_paren => {
                    left = @ptrCast(try self.parseCallExpression(left, config.allow_block_expr));
                },
                .minus_double_dot,
                .dot_dot,
                .right_bracket,
                .right_paren,
                .right_brace,
                .else_k,
                .catch_k,
                .comma,
                .colon,
                .equal,   
                .plus,
                .minus,
                .star,
                .slash,
                .ampersand,
                .vert_bar,
                .double_vert_bar,
                .double_left_angle,
                .double_right_angle,
                .caret,
                .left_angle,
                .left_angle_equal,
                .right_angle,
                .right_angle_equal,
                .percent,
                .equal_equal,
                .bang_equal,
                .and_k,
                .or_k,
                .as_k,
                .minus_right_angle,
                .raw_string,
                .string,
                .bin,
                .oct,
                .hex,
                .dec,
                .float,
                .if_k,
                .templateString,
                .equal_right_angle,
                .new_line,
                .null => break,
                else => break,
            }
        }
        return left;
    }

    fn parseTermExpr2(self: *Parser, config: ParseTermConfig) anyerror!*ast.Node {
        return (try self.parseTermExpr2Opt(config)) orelse {
            return self.reportError("Expected term expr. Got: {}.", &.{v(self.peek().tag())});
        };
    }

    /// An expression term doesn't contain a unary/binary expression at the top.
    fn parseTermExpr2Opt(self: *Parser, config: ParseTermConfig) anyerror!?*ast.Node {
        const left = (try self.parseTermExprLeft()) orelse return null;
        return try self.parseTermExprWithLeft(left, config);
    }

    fn parseTermExprLeft(self: *Parser) !?*ast.Node {
        const start = self.next_pos;
        var token = self.peek();
        switch (token.tag()) {
            .ident => {
                self.advance();
                const n = try self.newSpanNode(.ident, start);
                const name = self.ast.nodeString(@ptrCast(n));
                if (!self.isVarDeclaredFromScope(name)) {
                    try self.deps.put(self.alloc, name, @ptrCast(n));
                }
                return @ptrCast(n);
            },
            .type_k => {
                self.advance();
                return @ptrCast(try self.newSpanNode(.ident, start));
            },
            .struct_k => {
                self.advance();
                return @ptrCast(try self.newSpanNode(.ident, start));
            },
            .error_k => {
                self.advance();
                token = self.peek();
                if (token.tag() == .dot) {
                    // Error symbol literal.
                    self.advance();
                    token = self.peek();
                    if (token.tag() == .ident) {
                        const lit = try self.newSpanNode(.error_lit, self.next_pos);
                        self.advance();
                        return @ptrCast(lit);
                    } else {
                        return self.reportError("Expected symbol identifier.", &.{});
                    }
                } else {
                    // Becomes an ident.
                    return @ptrCast(try self.newSpanNode(.ident, start));
                }
            },
            .star => {
                self.advance();
                const elem = (try self.parseTermExpr2Opt(.{ .parse_record_expr = false })) orelse {
                    return self.reportError("Expected right child.", &.{});
                };
                return try self.ast.newNodeErase(.ptr, .{
                    .elem = elem,
                    .pos = self.tokenSrcPos(start),
                });
            },
            .ampersand => {
                self.advance();
                const elem = (try self.parseTermExpr2Opt(.{ .parse_record_expr = true })) orelse {
                    return self.reportError("Expected right child.", &.{});
                };
                return try self.ast.newNodeErase(.ref, .{
                    .elem = elem,
                    .pos = self.tokenSrcPos(start),
                });
            },
            .question => {
                self.advance();
                const param = (try self.parseTermExpr2Opt(.{ .parse_record_expr = false })) orelse {
                    return self.reportError("Expected option child type.", &.{});
                };
                return try self.ast.newNodeErase(.expandOpt, .{
                    .param = param,
                    .pos = self.tokenSrcPos(start),
                });
            },
            .dot => {
                self.advance();
                if (self.peek().tag() == .left_brace) {
                    const init_n = try self.parseInitLiteral();
                    return try self.ast.newNodeErase(.dot_init_lit, .{
                        .init = init_n,
                        .pos = self.tokenSrcPos(start),
                    });
                } else {
                    const name = (try self.parseOptName()) orelse {
                        return self.reportError("Expected symbol identifier.", &.{});
                    };
                    name.setType(.dot_lit);
                    return name;
                }
            },
            .symbol_k => {
                self.advance();
                token = self.peek();
                if (token.tag() == .dot) {
                    self.advance();
                    token = self.peek();
                    const name = (try self.parseOptName()) orelse {
                        return self.reportError("Expected symbol identifier.", &.{});
                    };
                    name.setType(.symbol_lit);
                    return name;
                } else {
                    // Becomes an ident.
                    return @ptrCast(try self.newSpanNode(.ident, start));
                }
            },
            .true_k => {
                self.advance();
                return try self.ast.newNodeErase(.trueLit, .{ .pos = self.tokenSrcPos(start) });
            },
            .false_k => {
                self.advance();
                return try self.ast.newNodeErase(.falseLit, .{ .pos = self.tokenSrcPos(start) });
            },
            .none_k => {
                self.advance();
                return try self.ast.newNodeErase(.noneLit, .{ .pos = self.tokenSrcPos(start) });
            },
            .void_k => {
                self.advance();
                return try self.ast.newNodeErase(.void, .{ .pos = self.tokenSrcPos(start) });
            },
            .dec => {
                self.advance();
                return @ptrCast(try self.newSpanNode(.decLit, start));
            },
            .float => {
                self.advance();
                return @ptrCast(try self.newSpanNode(.floatLit, start));
            },
            .bin => {
                self.advance();
                return @ptrCast(try self.newSpanNode(.binLit, start));
            },
            .oct => {
                self.advance();
                return @ptrCast(try self.newSpanNode(.octLit, start));
            },
            .hex => {
                self.advance();
                return @ptrCast(try self.newSpanNode(.hexLit, start));
            },
            .rune => {
                self.advance();
                return @ptrCast(try self.newSpanNode(.runeLit, start));
            },
            .raw_string => {
                self.advance();
                return @ptrCast(try self.newSpanNode(.raw_string_lit, start));
            },
            .string => {
                self.advance();
                return @ptrCast(try self.newSpanNode(.stringLit, start));
            },
            .templateString => {
                return @ptrCast(try self.parseStringTemplate());
            },
            .pound => {
                return @ptrCast(try self.parseComptimeExpr());
            },
            .left_paren => {
                self.advance();
                token = self.peek();

                const expr = (try self.parseExpr(.{})) orelse {
                    token = self.peek();
                    if (token.tag() == .right_paren) {
                        self.advance();
                    } else {
                        return self.reportError("Expected expression.", &.{});
                    }
                    // Assume empty args for lambda.
                    token = self.peek();
                    if (token.tag() == .equal_right_angle) {
                        return @ptrCast(try self.parseInferLambda(&.{}));
                    } else {
                        return self.reportError("Unexpected paren.", &.{});
                    }
                };
                const tag = self.peek().tag();
                if (tag == .right_paren) {
                    self.advance();

                    if (expr.type() == .ident and self.peek().tag() == .equal_right_angle) {
                        const param: *ast.Node = @ptrCast(try self.genDynFuncParam(expr));
                        const params = try self.ast.dupeNodes(&.{param});
                        return @ptrCast(try self.parseInferLambda(@ptrCast(params)));
                    }
                    return try self.ast.newNodeErase(.group, .{
                        .child = expr,
                        .pos = self.tokenSrcPos(start),
                    });
                } else if (tag == .comma) {
                    self.advance();
                    const param = try self.genDynFuncParam(expr);
                    const params = try self.parseFuncParams(&.{param}, false);
                    return @ptrCast(try self.parseInferLambda(params));
                } else {
                    return self.reportError("Expected right parenthesis.", &.{});
                }
            },
            .left_bracket => {
                const next_tag = self.peekAhead(1).tag();
                if (next_tag == .star) {
                    self.advance();
                    self.advance();
                    if (self.peek().tag() != .right_bracket) {
                        return self.reportError("Expected right bracket.", &.{});
                    }
                    self.advance();
                    const elem = (try self.parseTermExpr2Opt(.{ .parse_record_expr = false })) orelse {
                        return self.reportError("Expected pointer slice child type.", &.{});
                    };
                    return try self.ast.newNodeErase(.ptr_slice, .{
                        .elem = elem,
                        .pos = self.tokenSrcPos(start),
                    });
                } else if (next_tag == .right_bracket) {
                    self.advance();
                    self.advance();
                    const elem = (try self.parseTermExpr2Opt(.{ .parse_record_expr = false })) orelse {
                        return self.reportError("Expected reference slice child type.", &.{});
                    };
                    return try self.ast.newNodeErase(.ref_slice, .{
                        .elem = elem,
                        .pos = self.tokenSrcPos(start),
                    });
                } else {
                    // Array type.
                    self.advance();
                    const size = (try self.parseTermExpr2Opt(.{ .parse_record_expr = false })) orelse {
                        return self.reportError("Expected array type size.", &.{});
                    };
                    if (self.peek().tag() != .right_bracket) {
                        return self.reportError("Expected right bracket.", &.{});
                    }
                    self.advance();
                    const elem = (try self.parseTermExpr2Opt(.{ .parse_record_expr = false })) orelse {
                        return self.reportError("Expected array elem type.", &.{});
                    };
                    return try self.ast.newNodeErase(.array_type, .{
                        .size = size,
                        .elem = elem,
                        .pos = self.tokenSrcPos(start),
                    });
                }
            },
            .left_brace => {
                return @ptrCast(try self.parseInitLiteral());
            },
            .minus => {
                self.advance();
                const child = try self.parseTermExpr2(.{});
                return self.ast.newNodeErase(.unary_expr, .{
                    .child = child,
                    .op = .minus,
                });
            },
            .tilde => {
                self.advance();
                const child = try self.parseTermExpr2(.{});
                return self.ast.newNodeErase(.unary_expr, .{
                    .child = child,
                    .op = .bitwiseNot,
                });
            },
            .bang => {
                self.advance();
                const child = try self.parseTermExpr2(.{});
                return self.ast.newNodeErase(.unary_expr, .{
                    .child = child,
                    .op = .not,
                });
            },
            else => {
                return null;
            }
        }
    }

    fn parseComptimeExpr(self: *Parser) !*ast.ComptimeExpr {
        // Assumes current token is `#`.
        self.advance();

        const child = (try self.parseExpr(.{})) orelse {
            return self.reportError("Expected expression.", &.{});
        };

        return self.ast.newNode(.comptimeExpr, .{
            .child = child,
        });
    }

    /// An error can be returned during the expr parsing.
    /// If null is returned instead, no token begins an expression
    /// and the caller can assume next_pos did not change. Instead of reporting
    /// a generic error message, it delegates that to the caller.
    fn parseExpr(self: *Parser, config: ParseExprConfig) anyerror!?*ast.Node {
        if (self.peek().tag() == .new_line) {
            self.advance();
            self.consumeWhitespaceTokens();
        }
        const start = self.next_pos;
        const left = (try self.parseTermExpr(.{ .allow_block_expr = config.allow_block_expr })) orelse return null;
        if (left.isBlockExpr()) {
            return left;
        }
        return try self.parseExprWithLeft(start, left, config);
    }

    fn parseTermExpr(self: *Parser, config: ParseTermConfig) !?*ast.Node {
        const start = self.next_pos;
        switch (self.peek().tag()) {
            .null => return null,
            .right_paren => return null,
            .right_bracket => return null,
            .await_k => {
                self.advance();
                const child = (try self.parseExpr(.{})) orelse {
                    return self.reportError("Expected `await` expression.", &.{});
                };
                return self.ast.newNodeErase(.await_expr, .{
                    .child = child,
                    .pos = self.tokenSrcPos(start),
                });
            },
            .not_k => {
                self.advance();
                const child = (try self.parseExpr(.{})) orelse {
                    return self.reportError("Expected `not` expression.", &.{});
                };
                return self.ast.newNodeErase(.unary_expr, .{
                    .child = child,
                    .op = .not,
                });
            },
            .throw_k => {
                self.advance();
                const child = (try self.parseExpr(.{})) orelse {
                    return self.reportError("Expected `throw` expression.", &.{});
                };
                return self.ast.newNodeErase(.throwExpr, .{
                    .child = child,
                    .pos = self.tokenSrcPos(start),
                });
            },
            .if_k => {
                return try self.parseIfExpr(start);
            },
            .coresume_k => {
                self.advance();
                const fiberExpr = (try self.parseExpr(.{})) orelse {
                    return self.reportError("Expected `coresume` expression.", &.{});
                };
                return self.ast.newNodeErase(.coresume, .{
                    .child = fiberExpr,
                    .pos = self.tokenSrcPos(start),
                });
            },
            .coyield_k => {
                self.advance();
                return self.ast.newNodeErase(.coyield, .{
                    .pos = self.tokenSrcPos(start),
                });
            },
            .try_k => {
                self.advance();
                const expr = (try self.parseExpr(.{})) orelse {
                    return self.reportError("Expected `try` expression.", &.{});
                };

                var catchExpr: ?*ast.Node = null;
                if (self.peek().tag() == .catch_k) {
                    self.advance();
                    catchExpr = (try self.parseExpr(.{})) orelse {
                        return self.reportError("Expected `catch` expression.", &.{});
                    };
                }
                return self.ast.newNodeErase(.tryExpr, .{
                    .expr = expr,
                    .catchExpr = catchExpr,
                    .pos = self.tokenSrcPos(start),
                });
            },
            .coinit_k => {
                return try self.parseCoinitExpr();
            },
            .minus_double_dot => {
                // Start omitted.
                self.advance();
                const end = (try self.parseExpr(.{})) orelse {
                    return self.reportError("Expected range end.", &.{});
                };
                return self.ast.newNodeErase(.range, .{
                    .start = null,
                    .end = end,
                    .inc = false,
                    .pos = self.tokenSrcPos(start),
                });
            },
            .dot_dot => {
                // Start omitted.
                self.advance();
                const opt_end = try self.parseExpr(.{});

                return self.ast.newNodeErase(.range, .{
                    .start = null,
                    .end = opt_end,
                    .inc = true,
                    .pos = self.tokenSrcPos(start),
                });
            },
            .let_k => {
                return @ptrCast(try self.parseLetLambda());
            },
            .func_k => {
                return @ptrCast(try self.parseFuncLambdaOrType());
            },
            .Func_k => {
                return @ptrCast(try self.parseFuncUnionType());
            },
            .left_paren => {
                self.advance();
                const expr = (try self.parseExpr(.{})) orelse {
                    if (self.peek().tag() == .right_paren) {
                        self.advance();
                    } else {
                        return self.reportError("Expected expression.", &.{});
                    }
                    // Assume empty args for lambda.
                    if (self.peek().tag() == .equal_right_angle) {
                        return @ptrCast(try self.parseInferLambda(&.{}));
                    } else if (self.peek().tag() == .colon) {
                        return @ptrCast(try self.parseInferLambdaBlock(&.{}, null));
                    } else {
                        return self.reportError("Unexpected paren.", &.{});
                    }
                };
                if (self.peek().tag() == .right_paren) {
                    self.advance();
                    if (expr.type() == .ident) {
                        const param: *ast.Node = @ptrCast(try self.genDynFuncParam(expr));
                        if (self.peek().tag() == .equal_right_angle) {
                            const params = try self.ast.dupeNodes(&.{param});
                            return @ptrCast(try self.parseInferLambda(@ptrCast(params)));
                        } else if (self.peek().tag() == .colon) {
                            const params = try self.ast.dupeNodes(&.{param});
                            return @ptrCast(try self.parseInferLambdaBlock(@ptrCast(params), null));
                        }
                    }

                    const group = try self.ast.newNodeErase(.group, .{
                        .child = expr,
                        .pos = self.tokenSrcPos(start),
                    });
                    const term = try self.parseTermExprWithLeft(group, .{});
                    return self.parseExprWithLeft(start, term, .{});
                } else if (self.peek().tag() == .comma) {
                    self.advance();
                    const param = try self.genDynFuncParam(expr);
                    const params = try self.parseFuncParams(&.{ param }, false);
                    if (self.peek().tag() == .equal_right_angle) {
                        return @ptrCast(try self.parseInferLambda(params));
                    } else if (self.peek().tag() == .colon) {
                        return @ptrCast(try self.parseInferLambdaBlock(params, null));
                    } else {
                        return self.reportError("Expected `=>` or `:`.", &.{});
                    }
                } else {
                    return self.reportError("Expected right parenthesis.", &.{});
                }
            },
            .switch_k => {
                return @ptrCast(try self.parseSwitch(false));
            },
            else => {}
        }
        return try self.parseTermExpr2(config);
    }

    fn parseExprWithLeft(self: *Parser, start: u32, left_: *ast.Node, config: ParseExprConfig) !*ast.Node {
        var left = left_;
        while (true) {
            const next = self.peek();
            switch (next.tag()) {
                .equal_right_angle => {
                    if (left.type() == .ident) {
                        const param: *ast.Node = @ptrCast(try self.genDynFuncParam(left));
                        const params = try self.ast.dupeNodes(&.{param});
                        return @ptrCast(try self.parseInferLambda(@ptrCast(params)));
                    } else {
                        return self.reportError("Unexpected `=>` token", &.{});
                    }
                },
                .equal => {
                    break;
                },
                .plus,
                .minus,
                .star,
                .slash => {
                    if (self.peekAhead(1).tag() == .equal) {
                        break;
                    }
                    const bin_op = toBinExprOp(next.tag()).?;
                    const op_start = self.next_pos;
                    self.advance();
                    const right = try self.parseRightExpr(bin_op, config.allow_block_expr);
                    left = try self.ast.newNodeErase(.binExpr, .{
                        .left = left,
                        .right = right,
                        .op = bin_op,
                        .op_pos = self.tokenSrcPos(op_start),
                    });
                },
                .ampersand,
                .vert_bar,
                .double_vert_bar,
                .double_left_angle,
                .double_right_angle,
                .caret,
                .left_angle,
                .left_angle_equal,
                .right_angle,
                .percent,
                .equal_equal,
                .bang_equal,
                .and_k,
                .or_k,
                .right_angle_equal => {
                    const bin_op = toBinExprOp(next.tag()).?;
                    const op_start = self.next_pos;
                    self.advance();
                    const right = try self.parseRightExpr(bin_op, config.allow_block_expr);
                    left = try self.ast.newNodeErase(.binExpr, .{
                        .left = left,
                        .right = right,
                        .op = bin_op,
                        .op_pos = self.tokenSrcPos(op_start),
                    });
                },
                .minus_double_dot => {
                    self.advance();
                    const end: ?*ast.Node = if (try self.parseTermExpr2Opt(.{})) |right| b: {
                        break :b try self.parseRightExpr2(.reverse_range, right, config.allow_block_expr);
                    } else null;
                    left = try self.ast.newNodeErase(.range, .{
                        .start = left,
                        .end = end,
                        .inc = false,
                        .pos = self.tokenSrcPos(start),
                    });
                },
                .dot_dot => {
                    self.advance();
                    const end: ?*ast.Node = if (try self.parseTermExpr2Opt(.{})) |right| b: {
                        break :b try self.parseRightExpr2(.range, right, config.allow_block_expr);
                    } else null;
                    left = try self.ast.newNodeErase(.range, .{
                        .start = left,
                        .end = end,
                        .inc = true,
                        .pos = self.tokenSrcPos(start),
                    });
                },
                .as_k => {
                    const opStart = self.next_pos;
                    _ = opStart;
                    self.advance();

                    const typeSpec = (try self.parseOptTypeSpec(false)) orelse {
                        return self.reportError("Expected type specifier.", &.{});
                    };
                    left = try self.ast.newNodeErase(.castExpr, .{
                        .expr = left,
                        .typeSpec = typeSpec,
                    });
                },
                .question => {
                    self.advance();
                    if (self.peek().tag() != .else_k) {
                        return self.reportError("Expected `else`.", &.{});
                    }
                    self.advance();
                    const default = (try self.parseExpr(.{})) orelse {
                        return self.reportError("Expected default expression.", &.{});
                    };
                    left = try self.ast.newNodeErase(.unwrap_or, .{
                        .opt = left,
                        .default = default,
                    });
                },
                .right_bracket,
                .right_paren,
                .right_brace,
                .else_k,
                .comma,
                .colon,
                .minus_right_angle,
                .new_line,
                .null => break,
                else => {
                    if (!config.parseShorthandCallExpr) {
                        return left;
                    }
                    // Attempt to parse as no paren call expr.
                    switch (left.type()) {
                        .accessExpr,
                        .ident => {
                            return @ptrCast(try self.parseNoParenCallExpression(left));
                        },
                        else => {
                            return left;
                        }
                    }
                }
            }
        }
        return left;
    }

    fn pushIndent(self: *Parser, indent: u32) u32 {
        defer self.cur_indent = indent;
        return self.cur_indent;
    }

    fn parseLetDecl(self: *Parser, attrs: []*ast.Attribute, hidden: bool, allow_static: bool) !*ast.Node {
        const start = self.next_pos;
        self.advance();

        const root = self.peek().tag() == .dot;
        if (root) {
            self.advance();
        }

        const name = (try self.parseOptNamePath()) orelse {
            return self.reportError("Expected local name identifier.", &.{});
        };

        if (self.peek().tag() == .left_paren) {
            self.advance();
            
            // Parse as untyped function.
            const params = try self.parseFuncParams(&.{}, false);
            if (self.peek().tag() != .colon) {
                return self.reportError("Expected colon.", &.{});
            }
            self.advance();

            try self.pushBlock();
            const stmts = try self.parseSingleOrIndentedBodyStmts();
            _ = self.popBlock();

            const func = try self.ast.newNode(.funcDecl, .{
                .ret = null,
                .name = name,
                .params = params,
                .attrs = attrs,
                .stmts = stmts,
                .sig_t = .let,
                .hidden = hidden,
                .pos = self.tokenSrcPos(start),
            });

            if (self.cur_indent == 0) {
                try self.staticDecls.append(self.alloc, @ptrCast(func));
            }
            return @ptrCast(func);
        } else if (self.peek().tag() == .left_brace) {
            self.advance();

            // Parse as custom table declaration.
            const fields = try self.parseLetTableFields();
            if (self.peek().tag() == .new_line) {
                const decl = try self.ast.newNode(.table_decl, .{
                    .name = name,
                    .attrs = attrs,
                    .fields = fields,
                    .funcs = &.{},
                    .pos = self.tokenSrcPos(start),
                });
                try self.staticDecls.append(self.alloc, @ptrCast(decl));
                return @ptrCast(decl);
            }

            if (self.peek().tag() != .colon) {
                return self.reportError("Expected `:`.", &.{});
            }
            self.advance();
            
            const req_indent = try self.parseFirstChildIndent(self.cur_indent);
            const prev_indent = self.pushIndent(req_indent);
            defer self.cur_indent = prev_indent;
            const funcs = try self.parseTypeFuncs(req_indent);
            const decl = try self.ast.newNode(.table_decl, .{
                .name = name,
                .attrs = attrs,
                .fields = fields,
                .funcs = funcs,
                .pos = self.tokenSrcPos(start),
            });
            try self.staticDecls.append(self.alloc, @ptrCast(decl));
            return @ptrCast(decl);
        }

        // Parse as dynamic var decl.
        const has_name_path = name.type() == .name_path;
        const is_static = has_name_path or root;
        return self.parseVarDecl2(start, name, null, is_static, root, .{
            .attrs = attrs,
            .typed = false,
            .hidden = hidden,
            .allow_static = allow_static,
        });
    }

    fn parseContextDecl(self: *Parser) !*ast.ContextDecl {
        const start = self.next_pos;
        // Assume `context` keyword.
        self.advance();

        // Var name.
        const name = (try self.parseOptName()) orelse {
            return self.reportError("Expected context name.", &.{});
        };

        if (self.peek().tag() == .equal) {
            return error.TODO;
        }

        const type_spec = (try self.parseOptTypeSpec(false)) orelse {
            return self.reportError("Expected context variable type.", &.{});
        };
        const decl = try self.ast.newNode(.context_decl, .{
            .name = name,
            .type = type_spec,
            .right = null,
            .pos = self.tokenSrcPos(start),
        });
        try self.staticDecls.append(self.alloc, @ptrCast(decl));
        return decl;
    }

    const VarDeclConfig = struct {
        attrs: []*ast.Attribute,
        hidden: bool,
        typed: bool,
        allow_static: bool,
    };

    fn parseVarDecl(self: *Parser, config: VarDeclConfig) !*ast.Node {
        const start = self.next_pos;
        self.advance();

        const root = self.peek().tag() == .dot;
        if (root) {
            self.advance();
        }

        // Var name.
        const name = (try self.parseOptNamePath()) orelse {
            return self.reportError("Expected local name identifier.", &.{});
        };
        const hasNamePath = name.type() == .name_path;
        const isStatic = hasNamePath or root;

        if (!isStatic and config.hidden) {
            return self.reportError("Local variable does not allow the hidden visibility modifier.", &.{});
        }

        var typeSpec: ?*ast.Node = null;
        if (config.typed) {
            typeSpec = try self.parseOptTypeSpec(false);
        }

        return self.parseVarDecl2(start, name, typeSpec, isStatic, root, config);
    }

    fn parseVarDecl2(self: *cy.Parser, start: u32, name: *ast.Node, type_spec: ?*ast.Node, is_static: bool, root: bool, config: VarDeclConfig) !*ast.Node {
        if (is_static and !config.allow_static) {
            return self.reportError("Static variable declarations are not allowed here.", &.{});
        }

        if (!is_static) {
            if (config.attrs.len > 0) {
                return self.reportErrorAt("Attributes are not allowed for local var declarations.", &.{}, start);
            }
        }

        var right: ?*ast.Node = null;
        inner: {
            var token = self.peek();
            if (token.tag() == .new_line or token.tag() == .null) {
                break :inner;
            }

            if (self.peek().tag() != .equal) {
                return self.reportError("Expected `=` after variable name.", &.{});
            }
            self.advance();

            // Continue parsing right expr.
            right = try self.parseExpr(.{ .allow_block_expr = true });
        }

        if (is_static) {
            const decl = try self.ast.newNodeErase(.staticDecl, .{
                .name = name,
                .typeSpec = type_spec,
                .attrs = config.attrs,
                .right = right,
                .typed = config.typed,
                .root = root,
                .hidden = config.hidden,
                .pos = self.tokenSrcPos(start),
            });
            try self.staticDecls.append(self.alloc, decl);
            return decl;
        } else {
            if (right == null) {
                return self.reportError("Expected right value.", &.{});
            }
            return self.ast.newNodeErase(.localDecl, .{
                .name = name,
                .typeSpec = type_spec,
                .right = right.?,
                .typed = config.typed,
                .pos = self.tokenSrcPos(start),
            });
        }
    }

    /// Assumes next token is the return token.
    fn parseReturnStatement(self: *Parser) !*ast.Node {
        const start = self.next_pos;
        self.advance();
        const token = self.peek();
        switch (token.tag()) {
            .new_line,
            .null => {
                return self.ast.newNodeErase(.returnStmt, .{ .pos = self.tokenSrcPos(start) });
            },
            else => {
                const right = (try self.parseExpr(.{})).?;
                return self.ast.newNodeErase(.returnExprStmt, .{
                    .child = right,
                    .pos = self.tokenSrcPos(start),
                });
            },
        }
    }

    fn parseExprOrAssignStatement(self: *Parser) !?*ast.Node {
        const expr = (try self.parseExpr(.{
            .allow_block_expr = true,
        })) orelse {
            return null;
        };

        var is_assign_stmt = false;
        const tag = self.peek().tag();
        const op_start = self.next_pos;
        switch (tag) {
            .equal => {
                self.advance();
                is_assign_stmt = true;
            },
            .plus,
            .minus,
            .star,
            .slash => {
                // +=, -=, etc.
                self.advance();
                self.advance();
                is_assign_stmt = true;
            },
            else => {},
        }

        if (!is_assign_stmt) {
            return self.ast.newNodeErase(.exprStmt, .{
                .child = expr,
            });
        }

        switch (expr.type()) {
            .accessExpr,
            .deref,
            .array_expr,
            .ident => {},
            else => {
                return self.reportError("Unsupported assignment left expression: {}", &.{v(expr.type())});
            },
        }

        // Right can be an expr or stmt.
        const right = (try self.parseExpr(.{ .allow_block_expr = true })) orelse {
            return self.reportError("Expected right expression for assignment statement.", &.{});
        };

        if (tag == .equal) {
            return self.ast.newNodeErase(.assignStmt, .{
                .left = expr,
                .right = right,
            });
        } else {
            return self.ast.newNodeErase(.opAssignStmt, .{
                .left = expr,
                .right = right,
                .op = toBinExprOp(tag).?,
                .assign_pos = self.tokenSrcPos(op_start),
            });
        }

        // if (expr.type() == .ident) {
        //     const name = self.ast.nodeString(expr);
        //     const block = &self.blockStack.items[self.blockStack.items.len-1];
        //     if (self.deps.get(name)) |node| {
        //         if (node == expr) {
        //             // Remove dependency now that it's recognized as assign statement.
        //             _ = self.deps.remove(name);
        //         }
        //     }
        //     try block.vars.put(self.alloc, name, {});
        // }
    }

    fn tokenSrcPos(self: *Parser, idx: u32) u32 {
        return self.tokens[idx].pos();
    }

    fn newSpanNode(self: *Parser, comptime node_t: ast.NodeType, start: u32) !*ast.Span {
        const token = self.tokens[start];
        return self.ast.newSpanNode(node_t, token.pos(), token.data.end_pos);
    }

    /// When n=0, this is equivalent to peek.
    inline fn peekAhead(self: Parser, n: u32) Token {
        if (self.next_pos + n < self.tokens.len) {
            return self.tokens[self.next_pos + n];
        } else {
            return Token.init(.null, self.next_pos, .{
                .end_pos = 0,
            });
        }
    }

    inline fn peek(self: Parser) Token {
        if (!self.isAtEnd()) {
            return self.tokens[self.next_pos];
        } else {
            return Token.init(.null, @intCast(self.ast.src.len), .{
                .end_pos = 0,
            });
        }
    }

    inline fn advance(self: *Parser) void {
        self.next_pos += 1;
    }

    inline fn isAtEnd(self: Parser) bool {
        return self.tokens.len == self.next_pos;
    }

    inline fn consume(self: *Parser) Token {
        const token = self.tokens[self.next_pos];
        self.next_pos += 1;
        return token;
    }
};

pub const Result = struct {
    inner: ResultView,
    
    pub fn init(alloc: std.mem.Allocator, view: ResultView) !Result {
        const arr = try view.nodes.clone(alloc);
        const nodes = try alloc.create(std.ArrayListUnmanaged(ast.Node));
        nodes.* = arr;

        const new_src = try alloc.dupe(u8, view.src);

        const deps = try alloc.create(std.StringHashMapUnmanaged(*ast.Node));
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
                .src = new_src,
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
    root: ?*ast.Root,
    has_error: bool,

    ast: cy.ast.AstView,

    name: []const u8,
    deps: *std.StringHashMapUnmanaged(*ast.Node),

    pub fn dupe(self: ResultView, alloc: std.mem.Allocator) !Result {
        return try Result.init(alloc, self);
    }
};

fn toBinExprOp(op: cy.tokenizer.TokenType) ?cy.ast.BinaryExprOp {
    return switch (op) {
        .plus => .plus,
        .minus => .minus,
        .star => .star,
        .caret => .caret,
        .slash => .slash,
        .percent => .percent,
        .ampersand => .bitwiseAnd,
        .vert_bar => .bitwiseOr,
        .double_vert_bar => .bitwiseXor,
        .double_left_angle => .bitwiseLeftShift,
        .double_right_angle => .bitwiseRightShift,
        .bang_equal => .bang_equal,
        .left_angle => .less,
        .left_angle_equal => .less_equal,
        .right_angle => .greater,
        .right_angle_equal => .greater_equal,
        .equal_equal => .equal_equal,
        .and_k => .and_op,
        .or_k => .or_op,
        .null,
        .as_k, .at, .await_k,
        .bang, .bin, .break_k,
        .minus_right_angle, .case_k, .catch_k, .coinit_k, .colon, .comma, .context_k, .continue_k, .coresume_k, .coyield_k, .cstruct_k,
        .dec, .dot, .dot_question, .dot_dot, .dot_star,
        .else_k, .enum_k, .err, .error_k, .equal, .equal_right_angle,
        .false_k, .float, .for_k, .func_k, .Func_k,
        .hex, .ident, .if_k, .mod_k, .indent,
        .left_brace, .left_bracket, .left_paren, .let_k,
        .minus_double_dot, .new_line, .none_k, .not_k, .object_k, .oct, .pass_k, .underscore, .pound, .question,
        .return_k, .right_brace, .right_bracket, .right_paren, .rune, .raw_string,
        .string, .struct_k, .switch_k, .symbol_k, .templateExprStart, .templateString,
        .throw_k, .tilde, .trait_k, .true_k, .try_k, .type_k, .use_k, .var_k, .void_k, .while_k, .with_k => null,
    };
}

pub fn getBinOpPrecedence(op: cy.ast.BinaryExprOp) u8 {
    switch (op) {
        .bitwiseLeftShift,
        .bitwiseRightShift => return 10,

        .bitwiseAnd => return 9,

        .bitwiseXor,
        .bitwiseOr => return 8,

        .caret => return 7,

        .slash,
        .percent,
        .star => {
            return 6;
        },

        .minus,
        .plus => {
            return 5;
        },

        .cast => return 4,

        .greater,
        .greater_equal,
        .less,
        .less_equal,
        .bang_equal,
        .equal_equal => {
            return 3;
        },

        .and_op => return 2,

        .or_op => return 1,

        .range,
        .reverse_range => return 0,

        else => return 0,
    }
}

test "Parse dependency variables" {
    // var parser: Parser = undefined;
    // try parser.init(t.alloc);
    // defer parser.deinit();

    // var res = try parser.parseNoErr(
    //     \\foo
    // , .{});
    // try t.eq(res.deps.size, 1);
    // try t.eq(res.deps.contains("foo"), true);

    // // Assign statement.
    // res = try parser.parseNoErr(
    //     \\foo = 123
    //     \\foo
    // , .{});
    // try t.eq(res.deps.size, 0);

    // // Function call.
    // res = try parser.parseNoErr(
    //     \\foo()
    // , .{});
    // try t.eq(res.deps.size, 1);
    // try t.eq(res.deps.contains("foo"), true);

    // // Function call after declaration.
    // res = try parser.parseNoErr(
    //     \\func foo():
    //     \\  pass
    //     \\foo()
    // , .{});
    // try t.eq(res.deps.size, 0);
}

pub fn logSrcPos(src: []const u8, start: u32, len: u32) void {
    if (start + len > src.len) {
        log.tracev("{s}", .{ src[start..] });
    } else {
        log.tracev("{s}", .{ src[start..start+len] });
    }
}

const ParseExprConfig = struct {
    parseShorthandCallExpr: bool = true,
    allow_block_expr: bool = false,
};

const ParseTermConfig = struct {
    parse_record_expr: bool = true,
    allow_block_expr: bool = false,
};

fn isRecordKeyNodeType(node_t: ast.NodeType) bool {
    switch (node_t) {
        .ident,
        .raw_string_lit,
        .decLit,
        .binLit,
        .octLit,
        .hexLit => {
            return true;
        },
        else => {
            return false;
        }
    }
}

fn defaultReportFn(ctx: *anyopaque, format: []const u8, args: []const cy.fmt.FmtValue, pos: u32) anyerror {
    _ = ctx;
    _ = format;
    _ = args;
    _ = pos;
    return error.ParseError;
}