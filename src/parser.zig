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

const attributes = std.StaticStringMap(cy.ast.AttributeType).initComptime(.{
    .{ "bind", .bind },
    .{ "extern", .extern_ },
    .{ "reserve", .reserve },
    .{ "call", .call },
    .{ "generator", .generator },
    .{ "cond", .cond },
    .{ "unsafe", .unsafe },
    .{ "consteval", .consteval },
});

const special_strings = std.StaticStringMap(cy.ast.SpecialStringKind).initComptime(.{
    .{ "c", .c },
    .{ "a", .ascii_cp },
    .{ "u", .unicode_cp },
});

const Block = struct {
    vars: std.StringHashMapUnmanaged(void),

    fn deinit(self: *Block, alloc: std.mem.Allocator) void {
        self.vars.deinit(alloc);
    }
};

const ParseOptions = struct {
    parseComments: bool = false,

    /// Used by libcyber's `findType`.
    parse_func_types: bool = false,
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

    has_top_ct_stmt: bool,

    // TODO: This should be implemented by user callbacks.
    /// @name arg.
    name: []const u8,
    /// Variable dependencies.
    deps: std.StringHashMapUnmanaged(*ast.Node),

    reportFn: *const fn (*Parser, format: []const u8, args: []const cy.fmt.FmtValue, pos: u32) anyerror,
    tokenizerReportFn: *const fn (*anyopaque, format: []const u8, args: []const cy.fmt.FmtValue, pos: u32) anyerror!void,
    ctx: *anyopaque,

    has_error: bool,

    parse_func_types: bool,

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
            .reportFn = defaultReportFn,
            .tokenizerReportFn = cy.tokenizer.defaultReportFn,
            .ctx = undefined,
            .has_error = false,
            .parse_func_types = false,
            .has_top_ct_stmt = false,
        };
        try self.ast.init(alloc, "", 0);
    }

    pub fn deinit(self: *Parser) void {
        self.ast.deinit(self.alloc);
        for (self.blockStack.items) |*block| {
            block.deinit(self.alloc);
        }
        self.blockStack.deinit(self.alloc);
        self.deps.deinit(self.alloc);
        self.node_stack.deinit(self.alloc);
    }

    pub fn parseNoErr(self: *Parser, src: []const u8, opts: ParseOptions) !ResultView {
        const res = try self.parse(src, opts);
        if (res.has_error) {
            return error.ParseError;
        }
        return res;
    }

    pub fn parse(self: *Parser, src: []const u8, src_id: u32, opts: ParseOptions) !ResultView {
        self.ast.src = src;
        self.ast.src_id = src_id;
        self.name = "";
        self.deps.clearRetainingCapacity();
        self.has_error = false;
        self.parse_func_types = opts.parse_func_types;

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
                .stmts = .{
                    .ptr = undefined,
                    .len = 0,
                },
                .src = self.ast.src_id,
            });
        };
        if (indent != 0) {
            return self.reportError("Unexpected indentation.", &.{});
        }

        try self.pushBlock();
        const stmts = try self.parseBodyStatements(0, .{ .allow_decls = true });
        _ = self.popBlock();

        try self.consumeNewLineOrEnd();
        if (try self.consumeFirstIndent()) |_| {
            return self.reportError("Unexpected indentation.", &.{});
        }

        return self.ast.newNode(.root, .{
            .stmts = .{
                .ptr = stmts.ptr,
                .len = stmts.len,
            },
            .src = self.ast.src_id,
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
        var block = self.blockStack.pop().?;
        block.deinit(self.alloc);
        return block;
    }

    fn parseSingleOrIndentedBodyStmts(self: *Parser) ![]*ast.Node {
        var token = self.peek();
        if (token.tag() != .new_line) {
            // Parse single statement only.
            const stmt = try self.parseStatement(.{ .allow_block = false });
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

    fn newLambdaCont(self: *Parser, start: u32, end: u32, params: []const *ast.FuncParam) !*ast.LambdaMulti {
        return self.ast.newNode(.lambda_cont, .{
            .sig_t = .infer,
            .params = params,
            .stmts = &.{},
            .src = self.ast.src_id,
            .pos = self.tokenPos(start),
            .sig_end = self.tokenPos(end) + 1,
            .ret = null,
        });
    }

    fn parseInferLambda(self: *Parser, start: u32, params: []const *ast.FuncParam) !*ast.Node {
        const end = self.next_pos;

        if (self.peek().tag() != .vert_bar) {
            return self.reportError("Expected `|`.", &.{});
        }
        self.advance();

        // Parse body expr.
        try self.pushBlock();
        const expr = (try self.parseExpr(.{})) orelse {
            return @ptrCast(try self.newLambdaCont(start, end, params));
        };
        _ = self.popBlock();

        return self.ast.newNodeErase(.lambda_expr, .{
            .sig_t = .infer,
            .expr = expr,
            .params = params,
            .src = self.ast.src_id,
            .pos = self.tokenPos(start),
            .ret = null,
        });
    }

    fn parseFuncType(self: *Parser, call_attr: ?*ast.Attribute) !*ast.Node {
        const start = self.next_pos;

        // Assume first token is `fn`.
        self.advance();

        const params = try self.parseParenAndFuncParams();
        const ret = try self.parseFuncReturn();

        // Parse as function type.
        return self.ast.newNodeErase(.fn_type, .{
            .params = .{ .ptr = params.ptr, .len = params.len },
            .src = self.ast.src_id,
            .pos = self.tokenPos(start),
            .ret = if (ret) |unwrap| unwrap.ret else null,
            .extern_ = call_attr,
        });
    }

    fn parseFuncSymType(self: *Parser) !*ast.Node {
        const start = self.next_pos;

        // Assume first token is `fnsym`.
        self.advance();

        const params = try self.parseParenAndFuncParams();
        const ret = try self.parseFuncReturn();

        // Parse as function type.
        return self.ast.newNodeErase(.fnsym_type, .{
            .params = .{ .ptr = params.ptr, .len = params.len },
            .src = self.ast.src_id,
            .pos = self.tokenPos(start),
            .ret = if (ret) |unwrap| unwrap.ret else null,
        });
    }

    fn parseFuncLambda(self: *Parser, params: []*ast.FuncParam) !*ast.Node {
        const start = self.next_pos;
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
                .expr = expr,
                .src = self.ast.src_id,
                .pos = self.tokenPos(start),
                .ret = if (ret) |unwrap| unwrap.ret else null,
            });
        }

        if (self.peek().tag() != .colon) {
            return self.reportError("Expected colon.", &.{});
        }
        const sig_end = self.next_pos;
        self.advance();

        try self.pushBlock();
        const stmts = try self.parseSingleOrIndentedBodyStmts();
        _ = self.popBlock();

        const lambda = try self.ast.newNodeErase(.lambda_multi, .{
            .params = params,
            .sig_t = .func,
            .stmts = stmts,
            .src = self.ast.src_id,
            .pos = self.tokenPos(start),
            .ret = if (ret) |unwrap| unwrap.ret else null,
            .sig_end = self.tokenPos(sig_end) + 1,
        });
        return lambda;
    }

    fn parseFuncParam(self: *Parser) anyerror!*ast.FuncParam {
        const start = self.next_pos;
        const next = self.peek();

        var name_type: *ast.Node = undefined;
        var template_param = false;
        var scope_param = false;
        var sink_param = false;
        var const_param = false;

        if (next.tag() == .percent) {
            self.advance();
            if (self.peek().tag() != .ident) {
                return self.reportError("Expected identifier.", &.{});
            }
            name_type = @ptrCast(try self.newIdentNode(self.next_pos));
            self.advance();
            template_param = true;
        } else if (next.tag() == .sink_k) {
            self.advance();
            sink_param = true;
            name_type = (try self.parseTermExpr(.{})) orelse {
                return self.reportError("Expected param.", &.{});
            };
        } else if (next.tag() == .scope_k) {
            self.advance();
            scope_param = true;
            name_type = (try self.parseTermExpr(.{})) orelse {
                return self.reportError("Expected param.", &.{});
            };
        } else if (next.tag() == .const_k) {
            self.advance();
            const_param = true;
            name_type = (try self.parseTermExpr(.{})) orelse {
                return self.reportError("Expected param.", &.{});
            };
        } else if (next.tag() == .ident and self.ast.src[self.tokenEnd(self.next_pos)] == ' ') {
            name_type = @ptrCast(try self.newIdentNode(self.next_pos));
            self.advance();
        } else {
            name_type = (try self.parseTermExpr(.{})) orelse {
                return self.reportError("Expected param.", &.{});
            };
        }

        if (self.peek().tag() == .comma or self.peek().tag() == .right_paren) {
            return self.ast.newNode(.func_param, .{
                .name_type = name_type,
                .type = null,
                .template_param = template_param,
                .scope_param = scope_param,
                .sink_param = sink_param,
                .const_param = const_param,
                .pos = self.tokenPos(start),
            });
        }

        if (self.peek().pos() <= self.tokenEnd(start)) {
            return self.reportError("Expected type specifier.", &.{});
        }

        const type_spec = (try self.parseOptTypeSpec()) orelse {
            return self.reportError("Expected type specifier.", &.{});
        };
        self.consumeWhitespaceTokens();
        return self.ast.newNode(.func_param, .{
            .name_type = name_type,
            .type = type_spec,
            .template_param = template_param,
            .scope_param = scope_param,
            .sink_param = sink_param,
            .const_param = const_param,
            .pos = self.tokenPos(start),
        });
    }

    fn parseTemplateParams(self: *Parser) ![]*ast.FuncParam {
        const param_start = self.node_stack.items.len;
        defer self.node_stack.items.len = param_start;

        if (self.peek().tag() == .right_bracket) {
            return self.reportError("Expected template parameter.", &.{});
        }

        // Parse params.
        var param = try self.parseFuncParam();
        try self.pushNode(@ptrCast(param));

        while (true) {
            switch (self.peek().tag()) {
                .comma => {
                    self.advance();
                },
                .right_bracket => {
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

    fn parseParenAndFuncParams(self: *Parser) ![]*ast.FuncParam {
        const token = self.peek();
        if (token.tag() != .left_paren) {
            return self.reportError("Expected open parenthesis.", &.{});
        }
        self.advance();
        return self.parseFuncParams(&.{});
    }

    fn parseLambdaParam(self: *Parser) anyerror!*ast.FuncParam {
        const start = self.next_pos;

        const name_type = (try self.parseTermExpr(.{})) orelse {
            return self.reportError("Expected param.", &.{});
        };

        self.consumeWhitespaceTokens();
        return self.ast.newNode(.func_param, .{
            .name_type = name_type,
            .type = null,
            .template_param = false,
            .scope_param = false,
            .sink_param = false,
            .const_param = false,
            .pos = self.tokenPos(start),
        });
    }

    fn parseLambdaParams(self: *Parser) ![]*ast.FuncParam {
        if (self.peek().tag() == .underscore) {
            self.advance();
            return &.{};
        }

        const param_start = self.node_stack.items.len;
        defer self.node_stack.items.len = param_start;

        // Parse params.
        self.consumeWhitespaceTokens();
        var param = try self.parseLambdaParam();
        try self.pushNode(@ptrCast(param));

        while (true) {
            switch (self.peek().tag()) {
                .comma => {
                    self.advance();
                    self.consumeWhitespaceTokens();
                    if (self.peek().tag() == .vert_bar) {
                        break;
                    }
                },
                .vert_bar => {
                    break;
                },
                else => return self.reportError("Expected `,`.", &.{}),
            }
            param = try self.parseLambdaParam();
            try self.pushNode(@ptrCast(param));
        }
        const params = self.node_stack.items[param_start..];
        return @ptrCast(try self.ast.dupeNodes(params));
    }

    /// Assumes token at first param ident or right paren.
    /// Let sema check whether param types are required since it depends on the context.
    fn parseFuncParams(self: *Parser, pre_params: []const *ast.FuncParam) ![]*ast.FuncParam {
        const param_start = self.node_stack.items.len;
        defer self.node_stack.items.len = param_start;

        if (pre_params.len > 0) {
            try self.node_stack.appendSlice(self.alloc, @ptrCast(pre_params));
        }

        if (self.peek().tag() == .right_paren) {
            self.advance();
            const params = self.node_stack.items[param_start..];
            return @ptrCast(try self.ast.dupeNodes(params));
        }

        // Parse params.
        self.consumeWhitespaceTokens();
        var param = try self.parseFuncParam();
        try self.pushNode(@ptrCast(param));

        while (true) {
            switch (self.peek().tag()) {
                .comma => {
                    self.advance();
                    self.consumeWhitespaceTokens();
                    if (self.peek().tag() == .right_paren) {
                        self.advance();
                        break;
                    }
                },
                .right_paren => {
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

    const FuncReturn = struct {
        ret: *ast.Node,
        scope_ret: bool,
    };

    fn parseFuncReturn(self: *Parser) !?FuncReturn {
        var scope_ret = false;
        if (self.peek().tag() != .minus_right_angle) {
            return null;
        } else if (self.peek().tag() == .colon) {
            return null;
        }
        self.advance();
        if (self.peek().tag() == .scope_k) {
            scope_ret = true;
            self.advance();
        }
        const ret = (try self.parseOptTypeSpec()) orelse {
            return self.reportError("Expected return type.", &.{});
        };
        return .{
            .ret = ret,
            .scope_ret = scope_ret,
        };
    }

    fn parseOptNameOrParent(self: *Parser) !?*ast.Node {
        var name = try self.parseOptName() orelse return null;

        // Look for parent template syntax.
        if (self.peek().tag() == .left_bracket) {
            if (self.peekAhead(1).tag() == .right_bracket) {
                // Make sure it doesn't match an adjacent array type.
                if (name.end() == self.tokenPos(self.next_pos)) {
                    self.advance();
                    self.advance();
                    name = try self.ast.newNodeErase(.generic_expand, .{
                        .left = name,
                        .end = self.tokenPos(self.next_pos-1) + 1,
                    });
                }
            }
        }
        return name;
    }

    fn parseOptName(self: *Parser) !?*ast.Node {
        const start = self.next_pos;
        var token = self.peek();
        switch (token.tag()) {
            .with_k,
            .scope_k,
            .void_k,
            .struct_k,
            .global_k,
            .cstruct_k,
            .cunion_k,
            .trait_k,
            .enum_k,
            .type_k,
            .error_k,
            .none_k,
            .ident => {
                self.advance();
                return @ptrCast(try self.newIdentNode(start));
            },
            .at_ident => {
                self.advance();
                return @ptrCast(try self.newLitNode(.at_lit, start));
            },
            .raw_string => {
                self.advance();
                return @ptrCast(try self.newLitNode(.raw_string_lit, start));
            },
            .pound => {
                return try self.parseComptimeExpr();
            },
            else => {},
        }
        return null;
    }

    fn parseOptNamePath(self: *Parser) !?*ast.Node {
        var name = (try self.parseOptName()) orelse {
            return null;
        };

        if (self.peek().tag() != .dot) {
            return name;
        }

        while (self.peek().tag() == .dot) {
            self.advance();
            const right = (try self.parseOptName()) orelse {
                return self.reportError("Expected name.", &.{});
            };
            name = try self.ast.newNodeErase(.accessExpr, .{
                .left = name,
                .right = right,
            });
        }
        return name;
    }

    /// A string template begins and ends with .stringt/stringt_multi token.
    /// Inside the template there are stringt_part and stringt_expr.
    fn parseStringTemplate(self: *Parser, template_kind: cy.tokenizer.TokenType) !*ast.StringTemplate {
        const start = self.next_pos;
        self.advance();

        const part_start = self.node_stack.items.len;
        defer self.node_stack.items.len = part_start;

        var expect_expr = false;
        while (true) {
            const tag = self.peek().tag();
            if (tag == template_kind) {
                // End of this template.
                self.advance();
                break;
            } else if (tag == .stringt_expr) {
                if (!expect_expr) {
                    return self.reportError("Expected string template part.", &.{});
                }
                const expr_start = self.next_pos;
                self.advance();
                const child = (try self.parseExpr(.{})) orelse {
                    return self.reportError("Expected expression.", &.{});
                };
                if (self.peek().tag() != .right_brace) {
                    return self.reportError("Expected right brace.", &.{});
                }
                self.advance();
                const expr = try self.ast.newNode(.stringt_expr, .{
                    .child = child,
                    .pos = self.tokenPos(expr_start),
                });
                try self.pushNode(@ptrCast(expr));
                expect_expr = false;
            } else if (tag == .stringt_part) {
                if (expect_expr) {
                    return self.reportError("Expected string template expression.", &.{});
                }
                const part = try self.newLitNode(.stringt_part, self.next_pos);
                self.advance();
                try self.pushNode(@ptrCast(part));
                expect_expr = true;
            } else {
                return self.reportError("Expected string template part.", &.{});
            }
        }

        if (template_kind == .stringt) {
            return self.ast.newNode(.stringt, .{
                .parts = try self.ast.dupeNodes(self.node_stack.items[part_start..]),
                .pos = self.tokenPos(start),
            });
        } else {
            return self.ast.newNode(.stringt_multi, .{
                .parts = try self.ast.dupeNodes(self.node_stack.items[part_start..]),
                .pos = self.tokenPos(start),
            });
        }
    }

    fn parseUnionCase(self: *Parser) !*ast.UnionCase {
        const start = self.next_pos;
        if (self.peek().tag() != .case_k) {
            return self.reportError("Expected case keyword.", &.{});
        }
        self.advance();

        const name = (try self.parseOptName()) orelse {
            return self.reportError("Expected case name.", &.{});
        };
        const payload_t = (try self.parseOptTypeSpec()) orelse {
            return self.reportError("Expected case payload type.", &.{});
        };
        return self.ast.newNode(.union_case, .{
            .name = name,
            .payload_t = payload_t,
            .pos = self.tokenPos(start),
        });
    }

    fn parseEnumMember(self: *Parser) !*ast.Node {
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
            if (try self.parseOptTypeSpec()) |res| {
                typeSpec = res;
            }
        }
        return self.ast.newNodeErase(.enumMember, .{
            .name = name,
            .typeSpec = typeSpec,
            .pos = self.tokenPos(start),
        });
    }

    fn parseStructField(self: *Parser) !?*ast.Field {
        var hidden = false;
        if (self.peek().tag() == .minus) {
            hidden = true;
            self.advance();
        }

        const name = (try self.parseOptName()) orelse {
            return null;
        };

        // Check for 'use' after field name (type embedding)
        const is_embedded = if (self.peek().tag() == .use_k) blk: {
            self.advance();
            break :blk true;
        } else false;

        const typeSpec = try self.parseOptTypeSpec();

        // Validate embedded fields must have a type specifier
        if (is_embedded and typeSpec == null) {
            return self.reportError("Embedded field must have a type specifier.", &.{});
        }

        var init_expr: ?*ast.Node = null;
        if (self.peek().tag() == .equal) {
            self.advance();
            init_expr = try self.parseExpr(.{}) orelse {
                return self.reportError("Expected default initializer.", &.{});
            };
        }

        return self.ast.newNode(.struct_field, .{
            .name = name,
            .typeSpec = typeSpec,
            .init = init_expr,
            .hidden = hidden,
            .embedded = is_embedded,
        });
    }

    const TypeDeclConfig = struct {
        attrs: []*ast.Attribute,
        hidden: bool,
        allow_decl: bool,
    };

    fn parseTemplate(self: *Parser) !*ast.Node {
        // Assumes starting with '['.
        const tag = self.peekAhead(1).tag();
        if (tag == .right_bracket or tag == .comma) {
            // Might need to parse template child declaration in the future.
            // const args = try self.parseArrayLiteral2();
            return error.TODO;
        } else {
            self.advance();
            const params = try self.parseTemplateParams();
            return self.ast.newNodeErase(.template, .{
                .params = params,
                .child_decl = undefined,
            });
        }
    }

    fn parseTypeDecl(self: *Parser, start: u32, config: TypeDeclConfig) !*ast.Node {
        if (!config.allow_decl) {
            return self.reportError("`type` declarations are not allowed here.", &.{});
        }

        // Assumes first token is the `type` keyword.
        self.advance();

        // Parse name.
        var name = (try self.parseOptName()) orelse {
            return self.reportError("Expected type name.", &.{});
        };

        var parent: ?*ast.Node = null;
        if (self.peek().tag() == .colon2) {
            // Allow parent for type aliases only.
            self.advance();
            parent = name;
            name = (try self.parseOptName()) orelse {
                return self.reportError("Expected type name.", &.{});
            };
        }

        var opt_template: ?*ast.Node = null;
        if (self.peek().tag() == .left_bracket) {
            opt_template = try self.parseTemplate();
        }

        var decl: *ast.Node = undefined;
        switch (self.peek().tag()) {
            .enum_k => {
                decl = @ptrCast(try self.parseEnumDecl(start, name, config));
            },
            .cstruct_k => {
                decl = @ptrCast(try self.parseStructDecl(start, name, true, config));
            },
            .cunion_k => {
                decl = @ptrCast(try self.parseCUnionDecl(start, name, config));
            },
            .trait_k => {
                decl = @ptrCast(try self.parseTraitDecl(start, name, config));
            },
            .equal => {
                self.advance();

                var target: *ast.Node = undefined;

                if (self.peek().tag() == .fn_k) {
                    target = try self.parseFuncType(null);
                } else if (self.peek().tag() == .fnsym_k) {
                    target = try self.parseFuncSymType();
                } else if (self.peek().tag() == .pound) {
                    self.advance();
                    if (self.peek().tag() != .left_bracket) {
                        return self.reportError("Expected `[`.", &.{});
                    }
                    self.advance();
                    const attr = (try self.parseAttr()).?;
                    if (attr.type != .extern_) {
                        return self.reportError("Only the call convention attribute `@extern` is recognized in a function type.", &.{});
                    }
                    if (self.peek().tag() != .right_bracket) {
                        return self.reportError("Expected `]`.", &.{});
                    }
                    self.advance();
                    if (self.peek().tag() == .fn_k) {
                        target = try self.parseFuncType(attr);
                    } else {
                        return self.reportError("Expected `fn` type.", &.{});
                    }
                } else {
                    target = (try self.parseExpr(.{ .first_expr = true })) orelse {
                        return self.reportError("Expected target type.", &.{});
                    };
                }
                decl = try self.ast.newNodeErase(.type_alias_decl, .{
                    .name = name,
                    .parent = parent,
                    .target = target,
                    .hidden = config.hidden,
                    .attrs = .{ .ptr = config.attrs.ptr, .len = config.attrs.len },
                    .pos = self.tokenPos(start),
                });
            },
            .const_k => {
                decl = @ptrCast(try self.parseTypeConstDecl(start, name, config));
            },
            // `struct` is optional.
            .left_paren,
            .struct_k,
            .new_line,
            .colon => {
                decl = @ptrCast(try self.parseStructDecl(start, name, false, config));
            },
            .underscore => {
                decl = @ptrCast(try self.parseCustomTypeDecl(start, name, config));
            },
            else => {
                return self.reportError("Unexpected type definition.", &.{});
            }
        }

        if (decl.type() != .type_alias_decl and parent != null) {
            return self.reportErrorAt("Nested types are not allowed.", &.{}, start);
        }

        if (opt_template) |template| {
            template.cast(.template).child_decl = decl;
            return template;
        } else {
            return decl;
        }
    }

    fn parseOptTypeSpec(self: *Parser) anyerror!?*ast.Node {
        const token = self.peek();
        switch (token.tag()) {
            .left_paren,
            .space_left_bracket,
            .left_bracket,
            .star,
            .question,
            .ampersand,
            .double_ampersand,
            .bang,
            .caret,

            // `#extern fn()`
            .pound,

            .void_k,
            .type_k,
            .error_k,
            .percent,
            .ident => {
                return try self.parseTermExpr(.{});
            },
            else => {
                return null;
            },
        }
    }

    // Allows `switch`.
    fn parseOptTypeSpec2(self: *Parser) anyerror!?*ast.Node {
        const token = self.peek();
        switch (token.tag()) {
            // `@extern fn()`
            .at,
            .left_paren,
            .space_left_bracket,
            .left_bracket,
            .star,
            .question,
            .ampersand,
            .bang,
            .caret,
            .pound,
            .void_k,
            .type_k,
            .error_k,
            .percent,
            .switch_k,
            .ident => {
                return try self.parseTermExpr(.{ .first_expr = true });
            },
            else => {
                return null;
            },
        }
    }

    fn parseTypeConstDecl(self: *Parser, start: TokenId, name: *ast.Node, config: TypeDeclConfig) !*ast.TypeConstDecl {
        // Assumes `const`.
        self.advance();

        if (self.peek().tag() != .colon) {
            return self.reportError("Expected `:`.", &.{});
        }
        self.advance();

        const stmts = try self.parseSingleOrIndentedBodyStmts();

        return self.ast.newNode(.type_const_decl, .{
            .name = name,
            .attrs = .{ .ptr = config.attrs.ptr, .len = config.attrs.len },
            .hidden = config.hidden,
            .stmts = .{ .ptr = stmts.ptr, .len = stmts.len },
            .pos = self.tokenPos(start),
        });
    }

    fn parseCustomTypeDecl(self: *Parser, start: TokenId, name: *ast.Node, config: TypeDeclConfig) !*ast.CustomTypeDecl {
        // Assumes `_`.
        self.advance();

        if (self.peek().tag() == .colon) {
            self.advance();
            const req_indent = try self.parseFirstChildIndent(self.cur_indent);
            const prev_indent = self.pushIndent(req_indent);
            defer self.cur_indent = prev_indent;
        }
        return self.ast.newNode(.custom_type_decl, .{
            .name = name,
            .attrs = .{ .ptr = config.attrs.ptr, .len = config.attrs.len },
            .hidden = config.hidden,
            .pos = self.tokenPos(start),
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

        var isChoiceType = member.type() == .enumMember and member.cast(.enumMember).typeSpec != null;

        while (true) {
            const start2 = self.next_pos;
            if (!try self.consumeNextLineIndent(req_indent)) {
                self.next_pos = start2;
                break;
            }
            member = try self.parseEnumMember();
            if (!isChoiceType) {
                if (member.type() == .enumMember and member.cast(.enumMember).typeSpec != null) {
                    isChoiceType = true;
                }
            }
            try self.pushNode(@ptrCast(member));
        }
        const members: []*ast.Node = try self.ast.dupeNodes(self.node_stack.items[member_start..]);
        return self.ast.newNode(.enumDecl, .{
            .name = name,
            .members = .{ .ptr = @ptrCast(members.ptr), .len = members.len },
            .isChoiceType = isChoiceType,
            .hidden = config.hidden,
            .pos = self.tokenPos(start),
        });
    }

    fn newCUnionDecl(self: *Parser, start: TokenId, name: *ast.Node,
        config: TypeDeclConfig, impls: []*ast.ImplDecl, cases: []*ast.UnionCase) !*ast.CUnionDecl {
        return self.ast.newNode(.cunion_decl, .{
            .name = name,
            .pos = self.tokenPos(start),
            .impls = .{ .ptr = impls.ptr, .len = impls.len },
            .cases = .{ .ptr = cases.ptr, .len = cases.len },
            .attrs = .{ .ptr = config.attrs.ptr, .len = config.attrs.len },
        });
    }

    fn newStructDecl(self: *Parser, start: TokenId, node_t: ast.NodeType, name: *ast.Node,
        config: TypeDeclConfig, impls: []*ast.ImplDecl, fields: []*ast.Field, num_embedded_fields: usize,
        is_tuple: bool) !*ast.StructDecl {

        const n = try self.ast.newNodeErase(.struct_decl, .{
            .name = name,
            .pos = self.tokenPos(start),
            .impls = .{ .ptr = impls.ptr, .len = impls.len },
            .fields = .{ .ptr = fields.ptr, .len = fields.len },
            .attrs = .{ .ptr = config.attrs.ptr, .len = config.attrs.len },
            .num_embedded_fields = @intCast(num_embedded_fields),
            .is_tuple = is_tuple,
        });
        n.setType(node_t);
        return @ptrCast(@alignCast(n));
    }

    fn parseImplDecls(self: *Parser, req_indent: u32) ![]*ast.ImplDecl {
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
            const with = try self.ast.newNode(.impl_decl, .{
                .trait = trait,
                .pos = self.tokenPos(start),
            });
            try self.pushNode(@ptrCast(with));

            const save = self.next_pos;
            if (!try self.consumeNextLineIndent(req_indent)) {
                self.next_pos = save;
                break;
            }
        }

        return @ptrCast(try self.ast.dupeNodes(self.node_stack.items[with_start..]));
    }

    fn parseTupleFields(self: *Parser) ![]*ast.Field {
        self.consumeWhitespaceTokens();
        var field = (try self.parseStructField()) orelse return &.{};

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
            self.consumeWhitespaceTokens();
            field = (try self.parseStructField()) orelse return error.Unexpected;
            try self.pushNode(@ptrCast(field));
        }
        return @ptrCast(try self.ast.dupeNodes(self.node_stack.items[field_start..]));
    }

    fn parseTypeFields(self: *Parser, req_indent: u32, out_num_embedded_fields: *usize) ![]*ast.Field {
        var num_embedded_fields: usize = 0;
        var field = (try self.parseStructField()) orelse {
            out_num_embedded_fields.* = 0;
            return &.{};
        };
        if (field.embedded) {
            num_embedded_fields += 1;
        }

        const field_start = self.node_stack.items.len;
        defer self.node_stack.items.len = field_start;
        try self.pushNode(@ptrCast(field));

        while (true) {
            const start = self.next_pos;
            if (!try self.consumeNextLineIndent(req_indent)) {
                self.next_pos = start;
                break;
            }
            field = (try self.parseStructField()) orelse {
                break;
            };
            if (field.embedded) {
                num_embedded_fields += 1;
            }
            try self.pushNode(@ptrCast(field));
        }
        out_num_embedded_fields.* = num_embedded_fields;
        return @ptrCast(try self.ast.dupeNodes(self.node_stack.items[field_start..]));
    }

    fn parseTraitFuncs(self: *Parser, req_indent: u32) ![]*ast.FuncDecl {
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
                .pos = self.tokenPos(start),
                .attrs = .{ .ptr = config.attrs.ptr, .len = config.attrs.len },
                .funcs = .{ .ptr = undefined, .len = 0 },
            });
        }

        const req_indent = try self.parseFirstChildIndent(self.cur_indent);
        const prev_indent = self.pushIndent(req_indent);
        defer self.cur_indent = prev_indent;
        const funcs = try self.parseTraitFuncs(req_indent);

        return self.ast.newNode(.trait_decl, .{
            .name = name,
            .pos = self.tokenPos(start),
            .attrs = .{ .ptr = config.attrs.ptr, .len = config.attrs.len },
            .funcs = .{ .ptr = funcs.ptr, .len = funcs.len },
        });
    }

    fn parseCUnionDecl(self: *Parser, start: TokenId, name: *ast.Node, config: TypeDeclConfig) anyerror!*ast.CUnionDecl {
        // Assume `cunion` keyword.
        self.advance();

        const token = self.peek();
        if (token.tag() == .colon) {
            self.advance();
        } else {
            // Only declaration. No members.
            return self.newCUnionDecl(start, name, config, &.{}, &.{});
        }

        const req_indent = try self.parseFirstChildIndent(self.cur_indent);
        const prev_indent = self.pushIndent(req_indent);
        defer self.cur_indent = prev_indent;

        const impls = try self.parseImplDecls(req_indent);

        var case = try self.parseUnionCase();
        const case_start = self.node_stack.items.len;
        defer self.node_stack.items.len = case_start;
        try self.pushNode(@ptrCast(case));

        while (true) {
            const start2 = self.next_pos;
            if (!try self.consumeNextLineIndent(req_indent)) {
                self.next_pos = start2;
                break;
            }
            case = try self.parseUnionCase();
            try self.pushNode(@ptrCast(case));
        }
        const cases: []*ast.UnionCase = @ptrCast(try self.ast.dupeNodes(self.node_stack.items[case_start..]));
        return self.newCUnionDecl(start, name, config, impls, cases);
    }

    fn parseStructDecl(self: *Parser, start: TokenId, name: *ast.Node, cstruct: bool, config: TypeDeclConfig) anyerror!*ast.StructDecl {
        const ntype: ast.NodeType = if (cstruct) .cstruct_decl else .struct_decl;
        var token = self.peek();
        // Optional `struct` keyword.
        if (token.tag() == .struct_k or token.tag() == .cstruct_k) {
            self.advance();
        }

        token = self.peek();
        if (token.tag() == .colon) {
            self.advance();
        } else if (token.tag() == .left_paren) {
            self.advance();
            const fields = try self.parseTupleFields();
            if (self.peek().tag() != .colon) {
                return self.newStructDecl(start, ntype, name, config, &.{}, fields, 0, true);
            }

            self.advance();
            const req_indent = try self.parseFirstChildIndent(self.cur_indent);
            const prev_indent = self.pushIndent(req_indent);
            defer self.cur_indent = prev_indent;

            return self.newStructDecl(start, ntype, name, config, &.{}, fields, 0, true);
        } else {
            // Only declaration. No members.
            return self.newStructDecl(start, ntype, name, config, &.{}, &.{}, 0, false);
        }

        const req_indent = try self.parseFirstChildIndent(self.cur_indent);
        const prev_indent = self.pushIndent(req_indent);
        defer self.cur_indent = prev_indent;

        const impls = try self.parseImplDecls(req_indent);
        var num_embedded_fields: usize = 0;
        const fields = try self.parseTypeFields(req_indent, &num_embedded_fields);
        return self.newStructDecl(start, ntype, name, config, impls, fields, num_embedded_fields, false);
    }

    pub fn parse_with_decl(self: *Parser, start: u32, attrs: []*ast.Attribute) !*ast.Node {
        if (self.peek().tag() != .left_bracket and self.peek().tag() != .space_left_bracket) {
            return self.reportError("Expected `[`.", &.{});
        }
        self.advance();

        const params = try self.parseTemplateParams();
        const with = try self.ast.newNode(.with, .{
            .params = .{ .ptr = params.ptr, .len = params.len },
            .src = self.ast.src_id,
            .pos = self.tokenPos(start),
            .end = self.tokenPos(self.next_pos-1) + 1,
        });

        if (self.peek().tag() != .new_line) {
            return self.reportError("Expected new line.", &.{});
        }
        self.consumeWhitespaceTokens();

        switch (self.peek().tag()) {
            .fn_k => {
                return self.parseFuncDecl(start, .{ .with = with, .attrs = attrs, .hidden = false, .allow_decl = true});
            },
            else => {
                return self.reportError("Unsupported `with` declaration.", &.{});
            },
        }
    }

    const FuncDeclConfig = struct {
        attrs: []*ast.Attribute,
        with: ?*ast.With = null,
        hidden: bool,
        allow_decl: bool,
    }; 

    fn parseFuncDecl(self: *Parser, start: u32, config: FuncDeclConfig) !*ast.Node {
        if (!config.allow_decl) {
            return self.reportError("`fn` declarations are not allowed here.", &.{});
        }
        // Assumes first token is the `func` keyword.
        self.advance();

        if (self.peek().tag() == .left_paren) {
            // Method.
            self.advance();

            const param_start = self.next_pos;
            var scope_param = false;
            var sink_param = false;
            if (self.peek().tag() == .scope_k) {
                scope_param = true;
                self.advance();
            } else if (self.peek().tag() == .sink_k) {
                sink_param = true;
                self.advance();
            }

            const self_t = (try self.parseOptTypeSpec()) orelse {
                return self.reportError("Expected receiver type.", &.{});
            };

            const self_str = "self";
            const self_ident = try self.ast.newNodeErase(.ident, .{
                .src = cy.NullId,
                .pos = cy.NullId,
                .name = .{
                    .ptr = self_str.ptr,
                    .len = self_str.len,
                },
            });

            const self_param = try self.ast.newNode(.func_param, .{
                .name_type = self_ident,
                .type = self_t,
                .template_param = false,
                .scope_param = scope_param,
                .sink_param = sink_param,
                .const_param = false,
                .pos = self.tokenPos(param_start),
            });

            if (self.peek().tag() != .right_paren) {
                return self.reportError("Expected `)`.", &.{});
            }
            self.advance();

            const name = (try self.parseOptName()) orelse {
                return self.reportError("Expected method name.", &.{});
            };

            // TODO: Handle template params.

            if (self.peek().tag() != .left_paren) {
                return self.reportError("Expected `(`.", &.{});
            }
            self.advance();

            const params = try self.parseFuncParams(&.{ self_param });
            const ret = try self.parseFuncReturn();

            var stmts: []*ast.Node = undefined;
            if (self.peek().tag() == .colon) {
                self.advance();
                try self.pushBlock();
                stmts = try self.parseSingleOrIndentedBodyStmts();
                _ = self.popBlock();
            } else {
                // Just a declaration, no body.
                stmts = &.{};
            }

            var decl = try self.ast.newNode(.funcDecl, .{
                .name = name,
                .parent = null,
                .attrs = .{ .ptr = config.attrs.ptr, .len = config.attrs.len },
                .with = config.with,
                .params = .{ .ptr = params.ptr, .len = params.len },
                .ret = null,
                .scope_ret = false,
                .stmts = .{ .ptr = stmts.ptr, .len = stmts.len },
                .sig_t = .method,
                .hidden = config.hidden,
                .pos = self.tokenPos(start),
            });
            if (ret) |unwrap| {
                decl.ret = unwrap.ret;
                decl.scope_ret = unwrap.scope_ret;
            }
            return @ptrCast(decl);
        }

        var name = (try self.parseOptNameOrParent()) orelse {
            return self.reportError("Expected function name.", &.{});
        };

        var parent: ?*ast.Node = null;
        if (self.peek().tag() == .colon2) {
            self.advance();
            parent = name;
            name = (try self.parseOptName()) orelse {
                return self.reportError("Expected function name.", &.{});
            };
        }

        var opt_template: ?*ast.TemplateDecl = null;
        if (self.peek().tag() == .left_bracket) {
            self.advance();
            const params = try self.parseTemplateParams();
            opt_template = try self.ast.newNode(.template, .{
                .params = params,
                .child_decl = undefined,
            });
            self.consumeWhitespaceTokens();
        }

        if (self.peek().tag() != .left_paren) {
            return self.reportError("Expected function parameter list.", &.{});
        }
        self.advance();

        const params = try self.parseFuncParams(&.{});
        const ret = try self.parseFuncReturn();

        // const nameStr = self.ast.nodeString(name);
        // const block = &self.blockStack.items[self.blockStack.items.len-1];
        // try block.vars.put(self.alloc, nameStr, {});

        var stmts: []*ast.Node = undefined;
        if (self.peek().tag() == .colon) {
            self.advance();
            try self.pushBlock();
            stmts = try self.parseSingleOrIndentedBodyStmts();
            _ = self.popBlock();
        } else {
            // Just a declaration, no body.
            stmts = &.{};
        }
        var decl = try self.ast.newNode(.funcDecl, .{
            .name = name,
            .parent = parent,
            .attrs = .{ .ptr = config.attrs.ptr, .len = config.attrs.len },
            .with = config.with,
            .params = .{ .ptr = params.ptr, .len = params.len },
            .ret = null,
            .scope_ret = false,
            .stmts = .{ .ptr = stmts.ptr, .len = stmts.len },
            .sig_t = .func,
            .hidden = config.hidden,
            .pos = self.tokenPos(start),
        });
        if (ret) |unwrap| {
            decl.ret = unwrap.ret;
            decl.scope_ret = unwrap.scope_ret;
        }

        if (opt_template) |template| {
            template.child_decl = @ptrCast(decl);
            return @ptrCast(template);
        } else {
            return @ptrCast(decl);
        }
    }

    fn parseElseStmts(self: *Parser) ![]*ast.Node {
        var else_stmt: ?*ast.Node = (try self.parseElseStmt()) orelse {
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

    fn parseElseStmt(self: *Parser) anyerror!?*ast.Node {
        const save = self.next_pos;
        const indent = try self.consumeFirstIndent();
        if (indent != self.cur_indent) {
            self.next_pos = save;
            return null;
        }

        var token = self.peek();
        var ct = false;
        if (token.tag() == .pound) {
            ct = true;
            self.advance();
            token = self.peek();
        }
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
            var res = try self.ast.newNodeErase(.else_block, .{
                .stmts = stmts,
                .src = self.ast.src_id,
                .pos = self.tokenPos(start),
            });
            if (ct) {
                res = try self.ast.newNodeErase(.ct_stmt, .{
                    .child = res,
                    .pos = self.tokenPos(start),
                });
            }
            return res;
        } else {
            // else if block.
            const cond = (try self.parseExpr(.{})) orelse {
                return self.reportError("Expected else if condition.", &.{});
            };
            token = self.peek();
            if (token.tag() == .colon) {
                self.advance();
                const stmts = try self.parseSingleOrIndentedBodyStmts();
                var res = try self.ast.newNodeErase(.elseif_block, .{
                    .cond = cond,
                    .stmts = stmts,
                    .pos = self.tokenPos(start),
                });
                if (ct) {
                    res = try self.ast.newNodeErase(.ct_stmt, .{
                        .child = res,
                        .pos = self.tokenPos(start),
                    });
                }
                return res;
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

        if (self.peek().tag() != .colon) {
            return self.reportError("Expected `:`.", &.{});
        }
        self.advance();
        const caseIndent = try self.parseFirstChildIndent(self.cur_indent);

        var case = (try self.parse_switch_case_stmt()) orelse {
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
            case = (try self.parse_switch_case_stmt()) orelse {
                return self.reportError("Expected case or else block.", &.{});
            };
            try self.pushNode(@ptrCast(case));
        }

        const cases: []*ast.CaseStmt = @ptrCast(try self.ast.dupeNodes(self.node_stack.items[case_start..]));
        const switch_n = try self.ast.newNode(.switch_stmt, .{
            .expr = expr,
            .cases = .{ .ptr = @ptrCast(cases.ptr), .len = cases.len },
            .pos = self.tokenPos(start),
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
        if (token.tag() != .else_k) {
            return self.reportError("Expected catch block.", &.{});
        }
        const catch_start = self.next_pos;
        self.advance();

        token = self.peek();
        var errorVar: ?*ast.Node = null;
        if (token.tag() == .ident) {
            errorVar = @ptrCast(try self.newIdentNode(self.next_pos));
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
            .src = self.ast.src_id,
            .pos = self.tokenPos(catch_start),
        });

        return self.ast.newNode(.tryStmt, .{
            .stmts = tryStmts,
            .catchStmt = catchStmt,
            .pos = self.tokenPos(start),
        });
    }

    fn parse_begin_block(self: *Parser) !*ast.Node {
        const start = self.next_pos;
        // Assumes first token is the `begin` keyword.
        self.advance();

        if (self.peek().tag() != .colon) {
            return self.reportError("Expected colon after if condition.", &.{});
        }
        self.advance();
        const stmts = try self.parseSingleOrIndentedBodyStmts();
        return self.ast.newNodeErase(.begin_stmt, .{
            .stmts = stmts,
            .pos = self.tokenPos(start),
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
                .pos = self.tokenPos(start),
            });
        } else if (token.tag() == .vert_bar) {
            self.advance();

            const unwrap = (try self.parseExpr(.{})) orelse {
                return self.reportError("Expected unwrap variable.", &.{});
            };

            if (self.peek().tag() != .vert_bar) {
                return self.reportError("Expected vertical bar.", &.{});
            }
            self.advance();

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
                .pos = self.tokenPos(start),
            });
        } else {
            return self.reportError("Expected colon after if condition.", &.{});
        }
    }

    const AliasDeclConfig = struct {
        hidden: bool,
        attrs: []*ast.Attribute,
    };

    fn parseUseStmt(self: *Parser, config: AliasDeclConfig) !*ast.Node {
    
        const start = self.next_pos;
        // Assumes first token is the `use` keyword.
        self.advance();

        var name: *ast.Node = undefined;
        var spec: ?*ast.Node = null;
        if (self.peek().tag() == .ident) {
            name = (try self.parseOptNamePath()) orelse {
                return self.reportError("Expected name identifier.", &.{});
            };

            var opt_template: ?*ast.Node = null;
            if (self.peek().tag() == .left_bracket) {
                opt_template = try self.parseTemplate();
                self.consumeWhitespaceTokens();
            }

            switch (self.peek().tag()) {
                .sq_string => {
                    spec = @ptrCast(try self.newLitNode(.sq_string_lit, self.next_pos));
                    self.advance();
                },
                .new_line => {},
                .null => {},
                .equal => {
                    self.advance();

                    const target = (try self.parseExpr(.{})) orelse {
                        return self.reportError("Expected target expression.", &.{});
                    };
                    const decl = try self.ast.newNodeErase(.use_alias, .{
                        .name = name,
                        .target = target,
                        .hidden = config.hidden,
                        .attrs = config.attrs,
                        .pos = self.tokenPos(start),
                    });

                    if (opt_template) |template| {
                        template.cast(.template).child_decl = decl;
                        return template;
                    } else {
                        return decl;
                    }
                },
                else => {
                    return self.reportError("Expected a module specifier.", &.{});
                },
            }
        } else if (self.peek().tag() == .star) {
            name = try self.newTokenNode(.all, self.next_pos);
            self.advance();

            if (self.peek().tag() != .sq_string) {
                return self.reportError("Expected a module specifier.", &.{});
            }
            spec = @ptrCast(try self.newLitNode(.sq_string_lit, self.next_pos));
            self.advance();
        } else {
            return self.reportError("Expected import clause.", &.{});
        }

        const import = try self.ast.newNode(.import_stmt, .{
            .attrs = .{ .ptr = config.attrs.ptr, .len = config.attrs.len },
            .name = name,
            .spec = spec,
            .pos = self.tokenPos(start),
        });
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
                .src = self.ast.src_id,
                .pos = self.tokenPos(start),
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
                .pos = self.tokenPos(start),
            });
        } else if (token.tag() == .vert_bar) {
            self.advance();
            token = self.peek();
            const ident = (try self.parseExpr(.{})) orelse {
                return self.reportError("Expected ident.", &.{});
            };
            if (ident.type() != .ident) {
                return self.reportError("Expected ident.", &.{});
            }

            if (self.peek().tag() != .vert_bar) {
                return self.reportError("Expected vertical bar.", &.{});
            }
            self.advance();

            token = self.peek();
            if (token.tag() != .colon) {
                return self.reportError("Expected :.", &.{});
            }
            self.advance();
            const stmts = try self.parseSingleOrIndentedBodyStmts();
            return self.ast.newNodeErase(.while_unwrap_stmt, .{
                .opt = expr,
                .capture = ident,
                .stmts = stmts,
                .pos = self.tokenPos(start),
            });
        } else {
            return self.reportError("Expected :.", &.{});
        }
    }

    fn parseForCase(self: *Parser) !*ast.CaseStmt {
        const start = self.next_pos;

        // Assume at `for`.
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
            if (range.end == null) {
                return self.reportError("Expected right range expression.", &.{});
            }

            token = self.peek();
            var is_body_expr: bool = undefined;
            var each: ?*ast.Node = null;
            if (token.tag() == .colon) {
                self.advance();
                is_body_expr = false;
            } else if (token.tag() == .vert_bar) {
                self.advance();

                token = self.peek();
                each = (try self.parseExpr(.{})) orelse {
                    return self.reportError("Expected ident.", &.{});
                };
                if (each.?.type() != .ident) {
                    return self.reportErrorAt("Expected ident.", &.{}, token.pos());
                }

                if (self.peek().tag() != .vert_bar) {
                    return self.reportError("Expected vertical bar.", &.{});
                }
                self.advance();

                token = self.peek();
                if (token.tag() == .colon) {
                    self.advance();
                    is_body_expr = false;
                } else if (token.tag() == .equal_right_angle) {
                    self.advance();
                    is_body_expr = true;
                } else {
                    return self.reportError("Expected :.", &.{});
                }
            } else if (token.tag() == .equal_right_angle) {
                self.advance();
                is_body_expr = true;
            } else {
                return self.reportError("Expected :.", &.{});
            }

            // Parse body.
            var stmts: []*ast.Node = undefined;
            if (is_body_expr) {
                const body_expr = (try self.parseExpr(.{})) orelse {
                    return self.reportError("Expected expression.", &.{});
                };
                stmts = @as([*]*ast.Node, @ptrCast(@alignCast(body_expr)))[0..1];
            } else {
                stmts = try self.parseSingleOrIndentedBodyStmts();
            }
            return error.Unsupported;
            // return self.ast.newNode(.ct_for_range_case, .{
            //     .stmts = stmts,
            //     .start = range.start.?,
            //     .end = range.end.?,
            //     .increment = range.inc,
            //     .each = each,
            //     .body_expr = is_body_expr,
            //     .pos = self.tokenPos(start),
            // });
        }

        token = self.peek();
        var each: ?*ast.Node = null;
        var count: ?*ast.Node = null;
        var is_body_expr: bool = undefined;
        if (token.tag() == .colon) {
            self.advance();
            is_body_expr = false;
        } else if (token.tag() == .vert_bar) {
            self.advance();
            token = self.peek();
            if (token.tag() == .left_brace) {
                each = @ptrCast(try self.parseSeqDestructure());
            } else {
                each = (try self.parseExpr(.{})) orelse {
                    return self.reportError("Expected each clause.", &.{});
                };
            }

            // Optional count var.
            if (self.peek().tag() == .comma) {
                self.advance();
                count = each;
                each = (try self.parseExpr(.{})) orelse {
                    return self.reportError("Expected count declaration.", &.{});
                };
            }

            if (self.peek().tag() != .vert_bar) {
                return self.reportError("Expected vertical bar.", &.{});
            }
            self.advance();

            if (self.peek().tag() == .colon) {
                self.advance();
                is_body_expr = false;
            } else if (self.peek().tag() == .right_angle_equal) {
                self.advance();
                is_body_expr = true;
            } else {
                return self.reportError("Expected :.", &.{});
            }
        } else if (token.tag() == .right_angle_equal) {
            self.advance();
            is_body_expr = true;
        } else {
            return self.reportError("Expected :.", &.{});
        }

        var res = try self.ast.newNode(.case_stmt, .{
            .kind = .case_for,
            .data = .{
                .case_for = .{
                    .iter = expr,
                    .capture = each,
                    // .count = count,
                },
            },
            .body_data = undefined,
            .body_kind = undefined,
            .pos = self.tokenPos(start),
        });
        if (is_body_expr) {
            const body_expr = (try self.parseExpr(.{})) orelse {
                return self.reportError("Expected expression.", &.{});
            };
            res.body_data = .{ .expr = body_expr };
            res.body_kind = .expr;
        } else {
            const stmts = try self.parseSingleOrIndentedBodyStmts();
            res.body_data = .{ .block = .{ .ptr=stmts.ptr, .len=stmts.len }};
            res.body_kind = .block;
        }
        return res;
    }

    fn parseForStatement(self: *Parser, comptime ct: bool) !*ast.Node {
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
            if (range.end == null) {
                return self.reportError("Expected right range expression.", &.{});
            }

            token = self.peek();
            var each: ?*ast.Node = undefined;
            if (token.tag() == .colon) {
                self.advance();
                each = null;
            } else if (token.tag() == .vert_bar) {
                self.advance();

                token = self.peek();
                const ident = (try self.parseExpr(.{})) orelse {
                    return self.reportError("Expected ident.", &.{});
                };
                if (ident.type() != .ident) {
                    return self.reportErrorAt("Expected ident.", &.{}, token.pos());
                }
                each = ident;

                if (self.peek().tag() != .vert_bar) {
                    return self.reportError("Expected vertical bar.", &.{});
                }
                self.advance();

                token = self.peek();
                if (token.tag() != .colon) {
                    return self.reportError("Expected :.", &.{});
                }
                self.advance();
            } else {
                return self.reportError("Expected :.", &.{});
            }

            const range_start = range.start orelse {
                return self.reportError("Expected range start.", &.{});
            };

            const stmts = try self.parseSingleOrIndentedBodyStmts();
            var res = try self.ast.newNodeErase(.for_range_stmt, .{
                .stmts = stmts,
                .start = range_start,
                .end = range.end.?,
                .end_inclusive = range.end_inclusive,
                .increment = range.inc,
                .each = each,
                .pos = self.tokenPos(start),
            });
            if (ct) {
                res = try self.ast.newNodeErase(.ct_stmt, .{
                    .child = res,
                    .pos = self.tokenPos(start),
                });
            }
            return res;
        }

        token = self.peek();
        var each: ?*ast.Node = null;
        var count: ?*ast.Node = null;
        if (token.tag() == .colon) {
            self.advance();
        } else if (token.tag() == .vert_bar) {
            self.advance();
            token = self.peek();
            if (token.tag() == .left_brace) {
                each = @ptrCast(try self.parseSeqDestructure());
            } else {
                each = (try self.parseExpr(.{})) orelse {
                    return self.reportError("Expected each clause.", &.{});
                };
            }

            // Optional count var.
            if (self.peek().tag() == .comma) {
                self.advance();
                count = each;
                each = (try self.parseExpr(.{})) orelse {
                    return self.reportError("Expected count declaration.", &.{});
                };
            }

            if (self.peek().tag() != .vert_bar) {
                return self.reportError("Expected vertical bar.", &.{});
            }
            self.advance();

            if (self.peek().tag() == .colon) {
                self.advance();
            } else {
                return self.reportError("Expected :.", &.{});
            }
        } else {
            return self.reportError("Expected :.", &.{});
        }

        const stmts = try self.parseSingleOrIndentedBodyStmts();
        var res = try self.ast.newNodeErase(.for_iter_stmt, .{
            .iterable = expr,
            .each = each,
            .count = count,
            .stmts = stmts,
            .pos = self.tokenPos(start),
        }); 
        if (ct) {
            res = try self.ast.newNodeErase(.ct_stmt, .{
                .child = res,
                .pos = self.tokenPos(start),
            });
        }
        return res;
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

    fn parse_case_stmt(self: *Parser, ct: bool) !*ast.Node {
        const start = self.next_pos;
        self.advance();

        var bodyExpr: bool = false;
        var capture: ?*ast.Node = null;
        const case_start = self.node_stack.items.len;
        defer self.node_stack.items.len = case_start;

        var cond = (try self.parseExpr(.{ .parse_infer_lambda = false })) orelse {
            return self.reportError("Expected case condition.", &.{});
        };
        try self.pushNode(cond);

        while (true) {
            var token = self.peek();
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
                cond = (try self.parseExpr(.{ .parse_infer_lambda = false })) orelse {
                    return self.reportError("Expected case condition.", &.{});
                };
                try self.pushNode(cond);
            } else if (token.tag() == .vert_bar) {
                self.advance();

                // Parse next token as expression.
                capture = try self.parseTightTerm(.{});

                if (self.peek().tag() != .vert_bar) {
                    return self.reportError("Expected vertical bar.", &.{});
                }
                self.advance();

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
            } else if (token.tag() == .new_line) {
                // Fallthrough case.
                const conds = try self.ast.dupeNodes(self.node_stack.items[case_start..]);
                var res = try self.ast.newNodeErase(.case_stmt, .{
                    .kind = .case,
                    .data = .{ .case = .{
                        .conds = .{ .ptr = conds.ptr, .len = conds.len },
                        .capture = capture,
                    }},
                    .body_data = undefined,
                    .body_kind = .fallthrough,
                    .pos = self.tokenPos(start),
                });
                if (ct) {
                    res = try self.ast.newNodeErase(.ct_stmt, .{
                        .child = res,
                        .pos = self.tokenPos(start),
                    });
                }
                return res;
            } else {
                return self.reportError("Expected comma or colon.", &.{});
            }
        }

        const conds = try self.ast.dupeNodes(self.node_stack.items[case_start..]);
        var res = try self.ast.newNodeErase(.case_stmt, .{
            .kind = .case,
            .data = .{ .case = .{
                .conds = .{ .ptr = conds.ptr, .len = conds.len },
                .capture = capture,
            }},
            .body_data = undefined,
            .body_kind = undefined,
            .pos = self.tokenPos(start),
        });

        // Parse body.
        if (bodyExpr) {
            const expr = (try self.parseExpr(.{})) orelse {
                return self.reportError("Expected expression.", &.{});
            };
            const case_stmt = res.cast(.case_stmt);
            case_stmt.body_data = .{ .expr = expr };
            case_stmt.body_kind = .expr;
        } else {
            const stmts = try self.parseSingleOrIndentedBodyStmts();
            const case_stmt = res.cast(.case_stmt);
            case_stmt.body_data = .{ .block = .{ .ptr = stmts.ptr, .len = stmts.len } };
            case_stmt.body_kind = .block;
        }

        if (ct) {
            res = try self.ast.newNodeErase(.ct_stmt, .{
                .child = @ptrCast(res),
                .pos = self.tokenPos(start),
            });
        }
        return res;
    }

    fn parse_case_else(self: *Parser, ct: bool) !*ast.Node {
        const start = self.next_pos;
        self.advance();
        var bodyExpr: bool = false;
        if (self.peek().tag() == .colon) {
            self.advance();
        } else if (self.peek().tag() == .equal_right_angle) {
            self.advance();
            bodyExpr = true;
        } else {
            return self.reportError("Expected colon or `=>`.", &.{});
        }

        var res = try self.ast.newNode(.case_stmt, .{
            .kind = .else_,
            .data = .{ .else_ = .{ .src = self.ast.src_id }},
            .body_data = undefined,
            .body_kind = undefined,
            .pos = self.tokenPos(start),
        });
        // Parse body.
        if (bodyExpr) {
            const expr = (try self.parseExpr(.{})) orelse {
                return self.reportError("Expected expression.", &.{});
            };
            res.body_data = .{ .expr = expr };
            res.body_kind = .expr;
        } else {
            const stmts = try self.parseSingleOrIndentedBodyStmts();
            res.body_data = .{ .block = .{ .ptr = stmts.ptr, .len = stmts.len } };
            res.body_kind = .block;
        }

        if (ct) {
            const ct_stmt = try self.ast.newNodeErase(.ct_stmt, .{
                .child = @ptrCast(res),
                .pos = self.tokenPos(start),
            });
            return ct_stmt;
        } else {
            return @ptrCast(res);
        }
    }

    fn parse_switch_case_stmt(self: *Parser) !?*ast.Node {
        var token = self.peek();

        var ct = false;
        if (token.tag() == .pound) {
            ct = true;
            self.advance();
            token = self.peek();
        }

        if (token.tag() == .case_k) {
            return self.parse_case_stmt(ct);
        } else if (token.tag() == .else_k) {
            return self.parse_case_else(ct);
        } else if (token.tag() == .for_k and ct) {
            return self.parseForStatement(true);
        } else {
            return null;
        }
    }

    const ParseStmtConfig = struct {
        allow_decls: bool = false,
        allow_block: bool = true,
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
                    if (try self.parseExprOrAssignStatement(config.allow_block)) |id| {
                        return id;
                    }
                }
            },
            .pound => {
                self.advance();
                const token = self.peek();
                if (token.tag() == .left_bracket) {
                    return self.parseAttrsDecl(start, config.allow_decls);
                } else if (token.tag() != .left_brace) {
                    const stmt = try self.parseStatement(.{});

                    if (self.blockStack.items.len == 1) {
                        self.has_top_ct_stmt = true;
                    }

                    return self.ast.newNodeErase(.ct_stmt, .{
                        .child = stmt,
                        .pos = self.tokenPos(start),
                    });
                } else {
                    return self.parseStatement(.{});
                }
            },
            .minus => {
                const next = self.peekAhead(1).tag();
                switch (next) {
                    .type_k => {
                        self.advance();
                        return try self.parseTypeDecl(start, .{
                            .attrs = &.{},
                            .hidden = true,
                            .allow_decl = config.allow_decls,
                        });
                    },
                    .fn_k => {
                        self.advance();
                        return self.parseFuncDecl(start, .{
                            .attrs = &.{},
                            .hidden = true,
                            .allow_decl = config.allow_decls,
                        });
                    },
                    .const_k => {
                        self.advance();
                        return self.parseConstDecl(start, .{ .attrs = &.{}, .hidden = true, .allow_static = config.allow_decls });
                    },
                    .global_k => {
                        self.advance();
                        return self.parseGlobalDecl(start, .{ .attrs = &.{}, .hidden = true, .allow_static = config.allow_decls });
                    },
                    .use_k => {
                        self.advance();
                        return self.parseUseStmt(.{ .attrs = &.{}, .hidden = true });
                    },
                    else => {},
                }
            },
            .type_k => {
                return try self.parseTypeDecl(start, .{
                    .attrs = &.{},
                    .hidden = false,
                    .allow_decl = config.allow_decls,
                });
            },
            .with_k => {
                self.advance();
                return self.parse_with_decl(start, &.{});
            },
            .fn_k => {
                return self.parseFuncDecl(start, .{ .attrs = &.{}, .hidden = false, .allow_decl = config.allow_decls});
            },
            .if_k => {
                return try self.parseIfStatement();
            },
            .begin_k => {
                return try self.parse_begin_block();
            },
            .try_k => {
                if (self.peekAhead(1).tag() == .colon) {
                    return @ptrCast(try self.parseTryStmt());
                }
            },
            .case_k => {
                // Parse child case statement in #for case.
                return self.parse_case_stmt(false);
            },
            .switch_k => {
                return @ptrCast(try self.parseSwitch(true));
            },
            .for_k => {
                return try self.parseForStatement(false);
            },
            .while_k => {
                return try self.parseWhileStatement();
            },
            .use_k => {
                return self.parseUseStmt(.{ .attrs = &.{}, .hidden = false });
            },
            .pass_k => {
                self.advance();
                return self.newTokenNode(.passStmt, start);
            },
            .continue_k => {
                self.advance();
                return self.newTokenNode(.continueStmt, start);
            },
            .break_k => {
                self.advance();
                return self.newTokenNode(.breakStmt, start);
            },
            .yield_k => {
                self.advance();
                const child = try self.parseExpr(.{});
                return self.ast.newNodeErase(.yield_stmt, .{
                    .child = child,
                    .src = self.ast.src_id,
                    .pos = self.tokenPos(start),
                });
            },
            .return_k => {
                return try self.parseReturnStatement();
            },
            .const_k => {
                return try self.parseConstDecl(start, .{ .attrs = &.{}, .hidden = false, .allow_static = config.allow_decls });
            },
            .global_k => {
                return try self.parseGlobalDecl(start, .{ .attrs = &.{}, .hidden = false, .allow_static = config.allow_decls });
            },
            .var_k => {
                return try self.parseVarDecl();
            },
            else => {},
        }
        if (try self.parseExprOrAssignStatement(config.allow_block)) |id| {
            return id;
        }
        const token = self.peek();
        return self.reportErrorAtSrc("Unknown token: {}", &.{v(token.tag())}, token.pos());
    }

    fn reportError(self: *Parser, format: []const u8, args: []const fmt.FmtValue) anyerror {
        return self.reportErrorAt(format, args, self.next_pos);
    }

    fn reportErrorAt(self: *Parser, format: []const u8, args: []const fmt.FmtValue, token_pos: u32) anyerror {
        var srcPos: u32 = undefined;
        if (token_pos >= self.tokens.len) {
            srcPos = @intCast(self.ast.src.len);
        } else {
            srcPos = self.tokens[token_pos].pos();
        }
        return self.reportErrorAtSrc(format, args, srcPos);
    }

    fn reportErrorAtSrc(self: *Parser, format: []const u8, args: []const fmt.FmtValue, srcPos: u32) anyerror {
        self.has_error = true;
        return self.reportFn(self, format, args, srcPos);
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

    fn consumeNewLineWhitespace(self: *Parser) void {
        if (self.peek().tag() == .new_line) {
            self.next_pos += 1;
            self.consumeWhitespaceTokens();
        }
    }

    fn consumeWhitespaceTokens(self: *Parser) void {
        var token = self.peek();
        while (token.tag() != .null) {
            switch (token.tag()) {
                .new_line, .indent => {
                    self.advance();
                    token = self.peek();
                    continue;
                },
                else => return,
            }
        }
    }

    /// Assume inside `#[]`.
    fn parseAttr(self: *Parser) !?*ast.Attribute {
        const start = self.next_pos;
        const arg = (try self.parseExpr(.{})) orelse {
            return null;
        };
        if (arg.type() != .ident) {
            return self.reportError("Expected attribute name.", &.{});
        }
        const name = arg.name();
        const attr_t = attributes.get(name) orelse {
            return self.reportError("Unknown attribute.", &.{});
        };

        var val: ?*ast.Node = null;
        if (self.peek().tag() == .equal) {
            self.advance();
            val = (try self.parseExpr(.{})) orelse {
                return self.reportError("Expected attribute value.", &.{});
            };
        }
        if (self.peek().tag() == .comma) {
            self.advance();
        }
        return self.ast.newNode(.attribute, .{
            .type = attr_t,
            .value = val,
            .src = self.ast.src_id,
            .pos = self.tokenPos(start),
        });
    }

    fn parseAttrs(self: *Parser) ![]*ast.Attribute {
        // Assume at first arg.
        const attr_start = self.node_stack.items.len;
        defer self.node_stack.items.len = attr_start;
        while (try self.parseAttr()) |attr| {
            try self.pushNode(@ptrCast(attr));
        } 
        if (self.peek().tag() != .right_bracket) {
            return self.reportError("Expected `]`.", &.{});
        }
        self.advance();
        return @ptrCast(try self.ast.dupeNodes(self.node_stack.items[attr_start..]));
    }

    fn parseAttrsDecl(self: *Parser, start: u32, allow_decls: bool) !*ast.Node {
        self.advance();
        const attrs = try self.parseAttrs();
        self.consumeWhitespaceTokens();

        var hidden = false;
        if (self.peek().tag() == .minus) {
            hidden = true;
            self.advance();
        }

        var ct = false;
        if (self.peek().tag() == .pound) {
            ct = true;
            self.advance();
        }
        if (self.peek().tag() == .fn_k) {
            return self.parseFuncDecl(start, .{ .hidden = hidden, .attrs = attrs, .allow_decl = allow_decls });
        } else if (self.peek().tag() == .global_k) {
            return try self.parseGlobalDecl(start, .{ .hidden = hidden, .attrs = attrs, .allow_static = allow_decls });
        } else if (self.peek().tag() == .use_k) {
            return try self.parseUseStmt(.{ .attrs = attrs, .hidden = hidden });
        } else if (self.peek().tag() == .type_k) {
            return try self.parseTypeDecl(start, .{
                .attrs = attrs,
                .hidden = hidden,
                .allow_decl = allow_decls,
            });
        } else if (self.peek().tag() == .with_k) {
            self.advance();
            return self.parse_with_decl(start, attrs);
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
            const end = self.next_pos;
            self.advance();

            const args = try self.ast.dupeNodes(self.node_stack.items[decl_start..]);
            return self.ast.newNode(.seqDestructure, .{
                .args = args,
                .pos = self.tokenPos(start),
                .end = self.tokenPos(end) + 1,
            });
        } else return self.reportError("Expected closing bracket.", &.{});
    }

    fn parseBracketArgs(self: *Parser) ![]*ast.Node {
        const start = self.next_pos;
        _ = start;
        // Assume left bracket is already consumed.
        self.consumeWhitespaceTokens();

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

    fn parseInitLit(self: *Parser) !*ast.InitLit {
        const start = self.next_pos;
        // Assume first token is left brace.
        self.advance();
        self.consumeWhitespaceTokens();

        if (self.peek().tag() == .right_brace) {
            const end = self.next_pos;
            self.advance();
            return self.ast.newNode(.init_lit, .{
                .args = .{ .ptr = undefined, .len = 0 },
                .src = self.ast.src_id,
                .pos = self.tokenPos(start),
                .end = self.tokenPos(end) + 1,
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
        const end = self.next_pos;
        self.advance();

        const args: []*ast.Node = try self.ast.dupeNodes(self.node_stack.items[entry_start..]);
        return self.ast.newNode(.init_lit, .{
            .args = .{ .ptr = args.ptr, .len = args.len },
            .src = self.ast.src_id,
            .pos = self.tokenPos(start),
            .end = self.tokenPos(end) + 1,
            .array_like = array_like,
        });
    }

    fn parseInitEntry(self: *Parser, is_pair: *bool) !*ast.Node {
        const arg = (try self.parseExpr(.{})) orelse {
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
                    .name_len = token.data.end - token.pos(),
                    .arg = arg,
                });
            }
        }
        return self.parseExpr(.{});
    }

    fn parseCallArgs(self: *Parser, hasNamedArg: *bool) ![]*ast.Node {
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
                if (arg.type() == .lambda_multi) {
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

        return self.ast.dupeNodes(self.node_stack.items[start..]);
    }

    fn parseCallExpression(self: *Parser, callee: *ast.Node) !*ast.CallExpr {
        var hasNamedArg: bool = undefined;
        const args = try self.parseCallArgs(&hasNamedArg);
        return self.ast.newNode(.callExpr, .{
            .callee = callee,
            .args = .{ .ptr = args.ptr, .len = args.len },
            .hasNamedArg = hasNamedArg,
            .end = self.tokenPos(self.next_pos-1) + 1,
        });
    }

    /// Parses the right expression of a BinaryExpression.
    fn parseRightExpr(self: *Parser, left_op: cy.ast.BinaryExprOp) anyerror!*ast.Node {
        if (self.peek().tag() == .new_line) {
            self.advance();
            self.consumeWhitespaceTokens();
        }
        const right = (try self.parseTermExpr(.{})) orelse {
            return self.reportError("Expected right operand.", &.{});
        };
        if (right.type() == .lambda_multi) {
            return right;
        }
        return self.parseRightExpr2(left_op, right);
    }

    fn parseRightExpr2(self: *Parser, left_op: cy.ast.BinaryExprOp, right_id: *ast.Node) anyerror!*ast.Node {
        // Check if next token is an operator with higher precedence.
        var token = self.peek();

        const rightOp = toBinExprOp(token.tag()) orelse {
            return right_id;
        };

        const op_prec = getBinOpPrecedence(left_op);
        const right_op_prec = getBinOpPrecedence(rightOp);
        if (right_op_prec > op_prec) {
            // Continue parsing right.
            const start = self.next_pos;
            _ = self.consume();
            const next_right = try self.parseRightExpr(rightOp);

            const binExpr = try self.ast.newNode(.binExpr, .{
                .left = right_id,
                .right = next_right,
                .op = rightOp,
                .op_pos = self.tokenPos(start),
            });

            // Before returning the expr, perform left recursion if the op prec greater than the starting op.
            // e.g. a + b * c * d
            //         ^ parseRightExpr starts here
            // Returns ((b * c) * d).
            // e.g. a < b * c - d
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
                    const rightExpr = try self.parseRightExpr(rightOp);
                    const newBinExpr = try self.ast.newNode(.binExpr, .{
                        .left = left,
                        .right = rightExpr,
                        .op = rightOp2,
                        .op_pos = self.tokenPos(start),
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
            return self.reportError("Expected `(`.", &.{});
        }
        self.advance();

        const cond = (try self.parseExpr(.{})) orelse {
            return self.reportError("Expected condition for `if` expression.", &.{});
        };

        if (self.peek().tag() != .right_paren) {
            return self.reportError("Expected `)`.", &.{});
        }
        self.advance();

        const body = (try self.parseExpr(.{})) orelse {
            return self.reportError("Expected conditional true expression.", &.{});
        };

        self.consumeNewLineWhitespace();
        if (self.peek().tag() != .else_k) {
            return self.reportError("Expected `else`.", &.{});
        }
        self.advance();

        const elseExpr = (try self.parseExpr(.{})) orelse {
            return self.reportError("Expected else body.", &.{});
        };

        return self.ast.newNodeErase(.if_expr, .{
            .cond = cond,
            .body = body,
            .pos = self.tokenPos(start),
            .else_expr = elseExpr,
        });
    }

    fn parseTightTermWithLeft(self: *Parser, left_: *ast.Node, config: ParseTermConfig) !*ast.Node {
        var left = left_;
        while (true) {
            const next = self.peek();
            switch (next.tag()) {
                .dot => {
                    self.advance();
                    // Access expr.
                    const right = (try self.parseOptName()) orelse {
                        return self.reportError("Expected ident.", &.{});
                    };
                    left = try self.ast.newNodeErase(.accessExpr, .{
                        .left = left,
                        .right = right,
                    });
                },
                .bang => {
                    if (self.peekAhead(1).tag() == .else_k) {
                        break;
                    }
                    const end = self.next_pos;
                    self.advance();
                    left = try self.ast.newNodeErase(.unwrap_res, .{
                        .res = left,
                        .end = self.tokenPos(end) + 1,
                    });
                },
                .dot_bang => {
                    self.advance();
                    const right = (try self.parseOptName()) orelse {
                        return self.reportError("Expected ident.", &.{});
                    };
                    left = try self.ast.newNodeErase(.unwrap_choice, .{
                        .left = left,
                        .right = right,
                    });
                },
                .dot_question => {
                    const end = self.next_pos;
                    self.advance();
                    left = try self.ast.newNodeErase(.unwrap, .{
                        .opt = left,
                        .end = self.tokenPos(end) + 2,
                    });
                },
                .dot_star => {
                    const end = self.next_pos;
                    self.advance();
                    left = try self.ast.newNodeErase(.deref, .{
                        .left = left,
                        .end = self.tokenPos(end) + 2,
                    });
                },
                .left_bracket => {
                    self.advance();
                    if (self.peek().tag() == .right_bracket) {
                        self.advance();
                        left = try self.ast.newNodeErase(.generic_expand, .{
                            .left = left,
                            .end = self.tokenPos(self.next_pos-1) + 1,
                        });
                    } else {
                        const args = try self.parseBracketArgs();
                        left = try self.ast.newNodeErase(.index_expr, .{
                            .left = left,
                            .args = .{ .ptr = args.ptr, .len = args.len },
                            .end = self.tokenPos(self.next_pos-1) + 1,
                        });
                    }
                },
                .left_brace => {
                    if (!config.parse_record_expr) break;
                    const lit = try self.parseInitLit();
                    left = try self.ast.newNodeErase(.init_expr, .{
                        .left = left,
                        .lit = lit,
                    });
                },
                .left_paren => {
                    if (!config.parse_call_expr) {
                        break;
                    }
                    left = @ptrCast(try self.parseCallExpression(left));
                },
                .sq_string => {
                    if (left.type() == .ident) {
                        if (special_strings.get(left.name())) |kind| {
                            const lit = try self.newLitNode(.sq_string_lit, self.next_pos);
                            self.advance();
                            left = try self.ast.newNodeErase(.special_string_lit, .{
                                .kind = kind,
                                .lit = lit,
                                .pos = left.pos(),
                            });
                            continue;
                        }
                    }
                    break;
                },
                .sq_string_multi => {
                    if (left.type() == .ident) {
                        if (special_strings.get(left.name())) |kind| {
                            const lit = try self.newLitNode(.sq_string_multi_lit, self.next_pos);
                            self.advance();
                            left = try self.ast.newNodeErase(.special_string_lit, .{
                                .kind = kind,
                                .lit = lit,
                                .pos = left.pos(),
                            });
                            continue;
                        }
                    }
                    break;
                },
                .string => {
                    if (left.type() == .ident) {
                        if (special_strings.get(left.name())) |kind| {
                            const lit = try self.newLitNode(.string_lit, self.next_pos);
                            self.advance();
                            left = try self.ast.newNodeErase(.special_string_lit, .{
                                .kind = kind,
                                .lit = lit,
                                .pos = left.pos(),
                            });
                            continue;
                        }
                    }
                    break;
                },
                .string_multi => {
                    if (left.type() == .ident) {
                        if (special_strings.get(left.name())) |kind| {
                            const lit = try self.newLitNode(.string_multi_lit, self.next_pos);
                            self.advance();
                            left = try self.ast.newNodeErase(.special_string_lit, .{
                                .kind = kind,
                                .lit = lit,
                                .pos = left.pos(),
                            });
                            continue;
                        }
                    }
                    break;
                },
                .dot_dot_rangle,
                .dot_dot_rangle_equal,
                .dot2,
                .right_bracket,
                .right_paren,
                .right_brace,
                .else_k,
                .comma,
                .colon,
                .equal,   
                .plus,
                .minus,
                .star,
                .slash,
                .ampersand,
                .vert_bar,
                .double_ampersand,
                .double_vert_bar,
                .double_left_angle,
                .double_right_angle,
                .double_star,
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
                .minus_right_angle,
                .bin,
                .oct,
                .hex,
                .dec,
                .float,
                .if_k,
                .stringt,
                .stringt_multi,
                .equal_right_angle,
                .new_line,
                .null => break,
                else => break,
            }
        }
        return left;
    }

    fn parseTightTerm(self: *Parser, config: ParseTermConfig) anyerror!*ast.Node {
        return (try self.parseTightTermOpt(config)) orelse {
            return self.reportError("Expected term expr. Got: {}.", &.{v(self.peek().tag())});
        };
    }

    /// An expression term doesn't contain a unary/binary expression at the top.
    fn parseTightTermOpt(self: *Parser, config: ParseTermConfig) anyerror!?*ast.Node {
        const left = (try self.parseTightTermLeft(config)) orelse return null;
        return try self.parseTightTermWithLeft(left, config);
    }

    fn parseTightTermLeft(self: *Parser, config: ParseTermConfig) !?*ast.Node {
        const start = self.next_pos;
        var token = self.peek();
        switch (token.tag()) {
            .ident => {
                self.advance();
                const n = try self.newIdentNode(start);
                const name = n.cast(.ident).name.slice();
                if (!self.isVarDeclaredFromScope(name)) {
                    try self.deps.put(self.alloc, name, @ptrCast(n));
                }
                return @ptrCast(n);
            },
            .dot => {
                self.advance();
                if (try self.parseOptName()) |name| {
                    return @ptrCast(try self.ast.newLitNode(.dot_lit, self.tokenPos(start), name.end()));
                } else {
                    return try self.newTokenNode(.dot, start);
                }
            },
            .at_ident => {
                self.advance();
                return @ptrCast(try self.newLitNode(.at_lit, start));
            },
            .type_k => {
                self.advance();
                return @ptrCast(try self.newIdentNode(start));
            },
            .struct_k => {
                self.advance();
                return @ptrCast(try self.newIdentNode(start));
            },
            .error_k => {
                self.advance();
                token = self.peek();
                if (token.tag() == .dot) {
                    // Error symbol literal.
                    self.advance();
                    token = self.peek();
                    if (token.tag() == .ident) {
                        const name = try self.newIdentNode(self.next_pos);
                        self.advance();
                        return try self.ast.newNodeErase(.error_lit, .{
                            .name = name,
                            .pos = self.tokenPos(start),
                        });
                    } else {
                        return self.reportError("Expected symbol identifier.", &.{});
                    }
                } else {
                    // Becomes an ident.
                    return @ptrCast(try self.newIdentNode(start));
                }
            },
            .double_ampersand => {
                self.advance();
                const child = (try self.parseTermExpr(config)) orelse {
                    return self.reportError("Expected right child.", &.{});
                };
                return try self.ast.newNodeErase(.ex_borrow, .{
                    .child = child,
                    .pos = self.tokenPos(start),
                });
            },
            .ampersand => {
                self.advance();
                const child = (try self.parseTermExpr(config)) orelse {
                    return self.reportError("Expected right child.", &.{});
                };
                return try self.ast.newNodeErase(.borrow, .{
                    .child = child,
                    .pos = self.tokenPos(start),
                });
            },
            .star => {
                self.advance();
                const child = (try self.parseTightTermOpt(config)) orelse {
                    return self.reportError("Expected right child.", &.{});
                };
                return try self.ast.newNodeErase(.ptr, .{
                    .child = child,
                    .pos = self.tokenPos(start),
                });
            },
            .caret => {
                self.advance();
                const child = (try self.parseTermExpr(config)) orelse {
                    return self.reportError("Expected right child.", &.{});
                };
                return try self.ast.newNodeErase(.ref, .{
                    .child = child,
                    .pos = self.tokenPos(start),
                });
            },
            .underscore => {
                self.advance();
                return try self.newTokenNode(.void_lit, start);
            },
            .true_k => {
                self.advance();
                return try self.newTokenNode(.trueLit, start);
            },
            .false_k => {
                self.advance();
                return try self.newTokenNode(.falseLit, start);
            },
            .none_k => {
                self.advance();
                return try self.newTokenNode(.noneLit, start);
            },
            .void_k => {
                self.advance();
                return @ptrCast(try self.newIdentNode(start));
            },
            .undef_k => {
                self.advance();
                return try self.newTokenNode(.undef_lit, start);
            },
            .dec => {
                self.advance();
                return @ptrCast(try self.newLitNode(.decLit, start));
            },
            .dec_u => {
                self.advance();
                return @ptrCast(try self.newLitNode(.dec_u, start));
            },
            .float => {
                self.advance();
                return @ptrCast(try self.newLitNode(.floatLit, start));
            },
            .bin => {
                self.advance();
                return @ptrCast(try self.newLitNode(.binLit, start));
            },
            .oct => {
                self.advance();
                return @ptrCast(try self.newLitNode(.octLit, start));
            },
            .hex => {
                self.advance();
                return @ptrCast(try self.newLitNode(.hexLit, start));
            },
            .raw_string => {
                self.advance();
                return @ptrCast(try self.newLitNode(.raw_string_lit, start));
            },
            .raw_string_multi => {
                self.advance();
                return @ptrCast(try self.newLitNode(.raw_string_multi_lit, start));
            },
            .sq_string => {
                self.advance();
                return @ptrCast(try self.newLitNode(.sq_string_lit, start));
            },
            .sq_string_multi => {
                self.advance();
                return @ptrCast(try self.newLitNode(.sq_string_multi_lit, start));
            },
            .string => {
                self.advance();
                return @ptrCast(try self.newLitNode(.string_lit, start));
            },
            .string_multi => {
                self.advance();
                return @ptrCast(try self.newLitNode(.string_multi_lit, start));
            },
            .stringt => {
                return @ptrCast(try self.parseStringTemplate(.stringt));
            },
            .stringt_multi => {
                return @ptrCast(try self.parseStringTemplate(.stringt_multi));
            },
            .pound => {
                if (self.peekAhead(1).tag() == .left_bracket) {
                    self.advance();
                    self.advance();
                    const attrs = try self.parseAttrs();
                    if (self.peek().tag() != .fn_k) {
                        return self.reportError("Expected `fn`.", &.{});
                    }
                    return self.parseFuncType(attrs[0]);
                }
                return @ptrCast(try self.parseComptimeExpr());
            },
            .left_paren => {
                self.advance();

                const expr = (try self.parseExpr(.{})) orelse {
                    return self.reportError("Expected expression.", &.{});
                };

                const end = self.next_pos;
                if (self.peek().tag() != .right_paren) {
                    return self.reportError("Expected `)`.", &.{});
                }
                self.advance();

                return try self.ast.newNodeErase(.group, .{
                    .child = expr,
                    .pos = self.tokenPos(start),
                    .end = self.tokenPos(end) + 1,
                });
            },
            .vert_bar => {
                self.advance();
                const params = try self.parseLambdaParams();
                return @ptrCast(try self.parseInferLambda(start, params));
            },
            .left_brace => {
                return @ptrCast(try self.parseInitLit());
            },
            .space_left_bracket,
            .left_bracket => {
                self.advance();

                if (self.peek().tag() == .underscore) {
                    // generic_vector_type
                    self.advance();

                    if (self.peek().tag() != .right_bracket) {
                        return self.reportError("Expected `]`.", &.{});
                    }
                    self.advance();

                    const elem_t = (try self.parseTightTermOpt(.{ .parse_record_expr = false })) orelse {
                        return self.reportError("Expected `Vector` element type.", &.{});
                    };
                    return self.ast.newNodeErase(.generic_vector_type, .{
                        .elem_t = elem_t,
                        .pos = self.tokenPos(start),
                    });
                } else if (self.peek().tag() == .ampersand) {
                    // span
                    self.advance();
                    if (self.peek().tag() != .right_bracket) {
                        return self.reportError("Expected `]`.", &.{});
                    }
                    self.advance();
                    const elem_t = (try self.parseTightTermOpt(.{ .parse_record_expr = false, .parse_call_expr = false })) orelse {
                        return self.reportError("Expected `Span` element type.", &.{});
                    };
                    return self.ast.newNodeErase(.span_type, .{
                        .child = elem_t,
                        .pos = self.tokenPos(start),
                    });
                } else if (self.peek().tag() == .right_bracket) {
                    // slice 
                    self.advance();

                    const elem_t = (try self.parseTightTermOpt(.{ .parse_record_expr = false, .parse_call_expr = false })) orelse {
                        return self.reportError("Expected `Slice` element type.", &.{});
                    };
                    return self.ast.newNodeErase(.slice_type, .{
                        .child = elem_t,
                        .pos = self.tokenPos(start),
                    });
                } else if (self.peek().tag() == .dot2) {
                    // partial_vector_type
                    self.advance();
                    const size = (try self.parseExpr(.{})) orelse {
                        return self.reportError("Expected `PartialVector` size.", &.{});
                    };
                    if (self.peek().tag() != .right_bracket) {
                        return self.reportError("Expected `]`.", &.{});
                    }
                    self.advance();
                    const elem_t = (try self.parseTightTermOpt(.{ .parse_record_expr = false, .parse_call_expr = false })) orelse {
                        return self.reportError("Expected `PartialVector` element type.", &.{});
                    };
                    return self.ast.newNodeErase(.partial_vector_type, .{
                        .child = elem_t,
                        .n = size,
                        .pos = self.tokenPos(start),
                    });
                } else {
                    // vector_type
                    const size = (try self.parseExpr(.{})) orelse {
                        return self.reportError("Expected `Vector` size.", &.{});
                    };
                    if (self.peek().tag() != .right_bracket) {
                        return self.reportError("Expected `]`.", &.{});
                    }
                    self.advance();
                    const elem_t = (try self.parseTightTermOpt(.{ .parse_record_expr = false, .parse_call_expr = false })) orelse {
                        return self.reportError("Expected `Vector` element type.", &.{});
                    };
                    return self.ast.newNodeErase(.vector_type, .{
                        .child = elem_t,
                        .n = size,
                        .pos = self.tokenPos(start),
                    });
                }
            },
            .minus => {
                self.advance();
                const child = try self.parseTightTerm(.{});
                return self.ast.newNodeErase(.unary_expr, .{
                    .child = child,
                    .op = .minus,
                    .pos = self.tokenPos(start),
                });
            },
            .tilde => {
                self.advance();
                const child = try self.parseTightTerm(.{});
                return self.ast.newNodeErase(.unary_expr, .{
                    .child = child,
                    .op = .bitwiseNot,
                    .pos = self.tokenPos(start),
                });
            },
            .question => {
                self.advance();
                const child = (try self.parseTermExpr(.{ .parse_call_expr = false })) orelse {
                    return self.reportError("Expected `Option` child type.", &.{});
                };
                return self.ast.newNodeErase(.option_type, .{
                    .child = child,
                    .pos = self.tokenPos(start),
                });
            },
            .bang => {
                self.advance();
                const child = try self.parseTightTerm(.{});
                return self.ast.newNodeErase(.unary_expr, .{
                    .child = child,
                    .op = .lnot,
                    .pos = self.tokenPos(start),
                });
            },
            .percent => {
                self.advance();
                if (self.peek().tag() != .ident) {
                    return self.reportError("Expected identifier.", &.{});
                }
                const name = try self.newIdentNode(self.next_pos);
                self.advance();
                return self.ast.newNodeErase(.infer_param, .{
                    .name = name,
                    .pos = self.tokenPos(start),
                });
            },
            else => {
                return null;
            },
        }
    }

    fn parseComptimeExpr(self: *Parser) !*ast.Node {
        const start = self.next_pos;
        // Assumes current token is `#`.
        self.advance();

        if (self.peek().tag() != .left_brace) {
            return self.reportError("Expected left brace.", &.{});
        }
        self.advance();
        const expr = (try self.parseExpr(.{})) orelse {
            return self.reportError("Expected expression.", &.{});
        };
        if (self.peek().tag() != .right_brace) {
            return self.reportError("Expected `}`.", &.{});
        }
        const end = self.next_pos;
        self.advance();
        return self.ast.newNodeErase(.expand_lit, .{
            .child = expr,
            .pos = self.tokenPos(start),
            .end = self.tokenPos(end) + 1,
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
        const left = (try self.parseTermExpr(.{ .first_expr = config.first_expr })) orelse return null;
        if (left.type() == .lambda_multi) {
            return left;
        }
        return try self.parseExprWithLeft(start, left, .{ .parse_infer_lambda = config.parse_infer_lambda });
    }

    /// A term includes the following in order of precendence:
    /// 1. Postfix operators.
    /// 2. Tight prefix operators.
    /// 3. Spaced prefix operators.
    fn parseTermExpr(self: *Parser, config: ParseTermConfig) !?*ast.Node {
        const start = self.next_pos;
        switch (self.peek().tag()) {
            .null => return null,
            .right_paren => return null,
            .right_bracket => return null,
            .as_k => {
                self.advance();
                var target: ?*ast.Node = null;
                if (self.peek().tag() == .left_bracket) {
                    self.advance();
                    target = (try self.parseOptTypeSpec()) orelse {
                        return self.reportError("Expected type specifier.", &.{});
                    };
                    if (self.peek().tag() != .right_bracket) {
                        return self.reportError("Expected `]`.", &.{});
                    }
                    self.advance();
                }
                const expr =(try self.parseTermExpr(.{})) orelse {
                    return self.reportError("Expected `as` expression.", &.{});
                };
                return self.ast.newNodeErase(.as_expr, .{
                    .target = target,
                    .expr = expr,
                    .pos = self.tokenPos(start),
                });
            },
            .move_k => {
                self.advance();
                const expr = (try self.parseExpr(.{})) orelse {
                    return self.reportError("Expected `move` expression.", &.{});
                };
                return self.ast.newNodeErase(.move_expr, .{
                    .expr = expr,
                    .pos = self.tokenPos(start),
                });
            },
            .not_k => {
                self.advance();
                const child = (try self.parseExpr(.{})) orelse {
                    return self.reportError("Expected `not` expression.", &.{});
                };
                return self.ast.newNodeErase(.unary_expr, .{
                    .child = child,
                    .op = .lnot,
                    .pos = self.tokenPos(start),
                });
            },
            .if_k => {
                return self.parseIfExpr(start);
            },
            .dot2 => {
                const op_end = self.next_pos;
                self.advance();
                return self.ast.newNodeErase(.range, .{
                    .start = null,
                    .end = null,
                    .inc = true,
                    .end_inclusive = false,
                    .src = self.ast.src_id,
                    .pos = self.tokenPos(start),
                    .op_end = self.tokenPos(op_end) + 2,
                });
            },
            .fn_k => {
                if (self.parse_func_types) {
                    return self.parseFuncType(null);
                } else {
                    // Parse typed lambda.
                    self.advance();
                    const params = try self.parseParenAndFuncParams();
                    return self.parseFuncLambda(params);
                }
            },
            .fnsym_k => {
                if (self.parse_func_types) {
                    return self.parseFuncSymType();
                }
            },
            .left_paren => {
                self.advance();

                const expr = (try self.parseExpr(.{})) orelse {
                    return self.reportError("Expected expression.", &.{});
                };

                const end = self.next_pos;
                if (self.peek().tag() != .right_paren) {
                    return self.reportError("Expected `)`.", &.{});
                }
                self.advance();

                const group = try self.ast.newNodeErase(.group, .{
                    .child = expr,
                    .pos = self.tokenPos(start),
                    .end = self.tokenPos(end) + 1,
                });
                const term = try self.parseTightTermWithLeft(group, .{});
                return self.parseExprWithLeft(start, term, .{});
            },
            .vert_bar => {
                // Parse infer lambda.
                self.advance();
                const params = try self.parseLambdaParams();
                return @ptrCast(try self.parseInferLambda(start, params));
            },
            .switch_k => {
                if (config.first_expr) {
                    return @ptrCast(try self.parseSwitch(false));
                }
            },
            else => {},
        }
        return try self.parseTightTermOpt(config);
    }

    fn parseExprWithLeft(self: *Parser, start: u32, left_: *ast.Node, config: ParseExprConfig) !*ast.Node {
        _ = config;
        var left = left_;
        while (true) {
            const next = self.peek();
            switch (next.tag()) {
                .colon_equal,
                .equal => {
                    break;
                },
                .double_vert_bar,
                .double_ampersand,
                .double_left_angle,
                .double_right_angle,
                .tilde,
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
                    const right = try self.parseRightExpr(bin_op);
                    left = try self.ast.newNodeErase(.binExpr, .{
                        .left = left,
                        .right = right,
                        .op = bin_op,
                        .op_pos = self.tokenPos(op_start),
                    });
                },
                .double_star,
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
                    const right = try self.parseRightExpr(bin_op);
                    left = try self.ast.newNodeErase(.binExpr, .{
                        .left = left,
                        .right = right,
                        .op = bin_op,
                        .op_pos = self.tokenPos(op_start),
                    });
                },
                .dot_dot_rangle => {
                    const op_end = self.next_pos;
                    self.advance();
                    const end: *ast.Node = if (try self.parseTightTermOpt(.{})) |right| b: {
                        break :b try self.parseRightExpr2(.reverse_range, right);
                    } else {
                        return self.reportError("Expected range end.", &.{});
                    };
                    left = try self.ast.newNodeErase(.range, .{
                        .start = left,
                        .end = end,
                        .inc = false,
                        .end_inclusive = false,
                        .src = self.ast.src_id,
                        .pos = self.tokenPos(start),
                        .op_end = self.tokenPos(op_end) + 3,
                    });
                },
                .dot_dot_rangle_equal => {
                    const op_end = self.next_pos;
                    self.advance();
                    const end: *ast.Node = if (try self.parseTightTermOpt(.{})) |right| b: {
                        break :b try self.parseRightExpr2(.reverse_range, right);
                    } else {
                        return self.reportError("Expected range end.", &.{});
                    };
                    left = try self.ast.newNodeErase(.range, .{
                        .start = left,
                        .end = end,
                        .inc = false,
                        .end_inclusive = true,
                        .src = self.ast.src_id,
                        .pos = self.tokenPos(start),
                        .op_end = self.tokenPos(op_end) + 4,
                    });
                },
                .dot_dot_equal => {
                    const op_end = self.next_pos;
                    self.advance();
                    const end: *ast.Node = if (try self.parseTightTermOpt(.{})) |right| b: {
                        break :b try self.parseRightExpr2(.range, right);
                    } else {
                        return self.reportError("Expected range end.", &.{});
                    };
                    left = try self.ast.newNodeErase(.range, .{
                        .start = left,
                        .end = end,
                        .inc = true,
                        .end_inclusive = true,
                        .src = self.ast.src_id,
                        .pos = self.tokenPos(start),
                        .op_end = self.tokenPos(op_end) + 3,
                    });
                },
                .dot2 => {
                    const op_end = self.next_pos;
                    self.advance();
                    const end: ?*ast.Node = if (try self.parseTermExpr(.{})) |right| b: {
                        break :b try self.parseRightExpr2(.range, right);
                    } else null;
                    left = try self.ast.newNodeErase(.range, .{
                        .start = left,
                        .end = end,
                        .inc = true,
                        .end_inclusive = false,
                        .src = self.ast.src_id,
                        .pos = self.tokenPos(start),
                        .op_end = self.tokenPos(op_end) + 2,
                    });
                },
                .bang => {
                    self.advance();
                    if (self.peek().tag() != .else_k) {
                        return self.reportError("Expected `else`.", &.{});
                    }
                    self.advance();
                    if (self.peek().tag() == .vert_bar) {
                        self.advance();
                        const capture = (try self.parseExpr(.{})) orelse {
                            return self.reportError("Expected error variable.", &.{});
                        };

                        if (self.peek().tag() != .vert_bar) {
                            return self.reportError("Expected vertical bar.", &.{});
                        }
                        self.advance();

                        if (self.peek().tag() != .colon) {
                            return self.reportError("Expected colon.", &.{});
                        }
                        self.advance();
                        try self.pushBlock();
                        const stmts = try self.parseSingleOrIndentedBodyStmts();
                        _ = self.popBlock();
                        return self.ast.newNodeErase(.unwrap_res_or_block, .{
                            .res = left,
                            .capture = capture,
                            .else_stmts = stmts,
                        });
                    } else if (self.peek().tag() == .colon) {
                        self.advance();
                        try self.pushBlock();
                        const stmts = try self.parseSingleOrIndentedBodyStmts();
                        _ = self.popBlock();
                        return self.ast.newNodeErase(.unwrap_res_or_block, .{
                            .res = left,
                            .capture = null,
                            .else_stmts = stmts,
                        });
                    } else {
                        const default = (try self.parseExpr(.{})) orelse {
                            return self.reportError("Expected default expression.", &.{});
                        };
                        left = try self.ast.newNodeErase(.unwrap_res_or, .{
                            .res = left,
                            .default = default,
                        });
                    }
                },
                .question => {
                    self.advance();
                    if (self.peek().tag() != .else_k) {
                        return self.reportError("Expected `else`.", &.{});
                    }
                    self.advance();
                    if (self.peek().tag() == .colon) {
                        self.advance();
                        try self.pushBlock();
                        const stmts = try self.parseSingleOrIndentedBodyStmts();
                        _ = self.popBlock();
                        return self.ast.newNodeErase(.unwrap_or_block, .{
                            .opt = left,
                            .else_stmts = stmts,
                        });
                    } else {
                        const default = (try self.parseExpr(.{})) orelse {
                            return self.reportError("Expected default expression.", &.{});
                        };
                        left = try self.ast.newNodeErase(.unwrap_or, .{
                            .opt = left,
                            .default = default,
                        });
                    }
                },
                .right_bracket, .right_paren, .right_brace, .else_k, .comma, .colon, .minus_right_angle, .new_line, .null => break,
                else => {
                    return left;
                }
            }
        }
        return left;
    }

    fn pushIndent(self: *Parser, indent: u32) u32 {
        defer self.cur_indent = indent;
        return self.cur_indent;
    }

    fn parseConstDecl(self: *Parser, start: u32, config: TheDeclConfig) !*ast.Node {
        if (!config.allow_static) {
            return self.reportError("Static variable declarations are not allowed here.", &.{});
        }

        self.advance();

        // Var name.
        var name = (try self.parseOptNameOrParent()) orelse {
            return self.reportError("Expected constant name.", &.{});
        };

        var parent: ?*ast.Node = null;
        if (self.peek().tag() == .colon2) {
            self.advance();
            parent = name;
            name = (try self.parseOptName()) orelse {
                return self.reportError("Expected constant name.", &.{});
            };
        }
        const type_ = try self.parseOptTypeSpec();

        var right: ?*ast.Node = null;
        inner: {
            var token = self.peek();
            if (token.tag() == .new_line or token.tag() == .null) {
                break :inner;
            }

            if (self.peek().tag() != .equal) {
                return self.reportError("Expected `=` after constant name.", &.{});
            }
            self.advance();

            // Continue parsing right expr.
            right = try self.parseExpr(.{ .first_expr = true });
        }

        const decl = try self.ast.newNodeErase(.const_decl, .{
            .name = name,
            .parent = parent,
            .type = type_,
            .attrs = .{ .ptr = config.attrs.ptr, .len = config.attrs.len },
            .right = right,
            .hidden = config.hidden,
            .pos = self.tokenPos(start),
        });
        return decl;
    }

    fn parseVarDecl(self: *Parser) !*ast.Node {
        const start = self.next_pos;

        // Assumes `var`.
        self.advance();

        const name = try self.parseOptName() orelse {
            return self.reportError("Expected variable name.", &.{});
        };

        const var_t = try self.parseOptTypeSpec() orelse {
            return self.reportError("Expected variable type.", &.{});
        };

        if (self.peek().tag() != .equal) {
            return self.reportError("Expected `=`.", &.{});
        }
        self.advance();

        // Right can be an expr or stmt.
        var right = (try self.parseExpr(.{ .first_expr = true })) orelse {
            return self.reportError("Expected right expression for assignment statement.", &.{});
        };

        if (self.peek().tag() == .colon) {
            self.advance();
            try self.pushBlock();
            const stmts = try self.parseSingleOrIndentedBodyStmts();
            _ = self.popBlock();
            right = try self.ast.newNodeErase(.lambda_cont_expr, .{
                .child = right,
                .stmts = stmts,
            });
        }

        return self.ast.newNodeErase(.var_decl, .{
            .name = name,
            .typeSpec = var_t,
            .right = right,
            .pos = self.tokenPos(start),
        });
    }

    fn parseVarDeclInfer(self: *Parser, start: u32, name: *ast.Node) !*ast.Node {
        // Assumes `:=`.
        self.advance();

        // Right can be an expr or stmt.
        var right = (try self.parseExpr(.{ .first_expr = true })) orelse {
            return self.reportError("Expected right expression for assignment statement.", &.{});
        };

        if (self.peek().tag() == .colon) {
            self.advance();
            try self.pushBlock();
            const stmts = try self.parseSingleOrIndentedBodyStmts();
            _ = self.popBlock();
            right = try self.ast.newNodeErase(.lambda_cont_expr, .{
                .child = right,
                .stmts = stmts,
            });
        }

        return self.ast.newNodeErase(.var_decl, .{
            .name = name,
            .typeSpec = null,
            .right = right,
            .pos = self.tokenPos(start),
        });
    }

    const TheDeclConfig = struct {
        attrs: []*ast.Attribute,
        hidden: bool,
        allow_static: bool,
    };

    fn parseGlobalDecl(self: *Parser, start: u32, config: TheDeclConfig) !*ast.Node {
        self.advance();

        var name = (try self.parseOptNameOrParent()) orelse {
            return self.reportError("Expected global name.", &.{});
        };

        var parent: ?*ast.Node = null;
        if (self.peek().tag() == .colon2) {
            self.advance();
            parent = name;
            name = (try self.parseOptName()) orelse {
                return self.reportError("Expected global name.", &.{});
            };
        }

        const var_t = (try self.parseOptTypeSpec()) orelse {
            return self.reportError("Expected global type.", &.{});
        };
        return self.parseGlobalDecl2(start, parent, name, var_t, config);
    }

    fn parseLambdaContExpr(self: *cy.Parser, child: *ast.Node) !*ast.Node {
        // Assume colon.
        self.advance();

        try self.pushBlock();
        const stmts = try self.parseSingleOrIndentedBodyStmts();
        _ = self.popBlock();
        return self.ast.newNodeErase(.lambda_cont_expr, .{
            .child = child,
            .stmts = stmts,
        });
    }

    fn parseGlobalDecl2(self: *cy.Parser, start: u32, parent: ?*ast.Node, name: *ast.Node, type_spec: *ast.Node, config: TheDeclConfig) !*ast.Node {
        if (!config.allow_static) {
            return self.reportError("Static variable declarations are not allowed here.", &.{});
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
            right = try self.parseExpr(.{});

            if (right != null) {
                if (self.peek().tag() == .colon) {
                    right = try self.parseLambdaContExpr(right.?);
                }
            }
        }

        const decl = try self.ast.newNodeErase(.global_decl, .{
            .name = name,
            .parent = parent,
            .typeSpec = type_spec,
            .attrs = .{
                .ptr = config.attrs.ptr,
                .len = config.attrs.len,
            },
            .right = right,
            .hidden = config.hidden,
            .pos = self.tokenPos(start),
        });
        return decl;
    }

    /// Assumes next token is the return token.
    fn parseReturnStatement(self: *Parser) !*ast.Node {
        const start = self.next_pos;
        self.advance();
        const token = self.peek();
        switch (token.tag()) {
            .new_line,
            .null => {
                return self.newTokenNode(.returnStmt, start);
            },
            else => {
                const right = (try self.parseExpr(.{ .first_expr = true })).?;
                return self.ast.newNodeErase(.returnExprStmt, .{
                    .child = right,
                    .pos = self.tokenPos(start),
                });
            },
        }
    }

    fn parseExprOrAssignStatement(self: *Parser, allow_block: bool) !?*ast.Node {
        const start = self.next_pos;
        var expr = (try self.parseExpr(.{})) orelse {
            return null;
        };

        var is_assign_stmt = false;
        const tag = self.peek().tag();
        const op_start = self.next_pos;
        switch (tag) {
            .colon_equal => {
                // Local decl.
                switch (expr.type()) {
                    .ident => {},
                    else => {
                        return self.reportErrorAtSrc("Expected identifier for variable declaration.", &.{}, expr.pos());
                    },
                }
                return self.parseVarDeclInfer(start, expr);
            },
            .equal => {
                if (expr.type() == .void_lit) {
                    return self.parseVarDeclInfer(start, expr);
                }
                self.advance();
                is_assign_stmt = true;
            },
            .double_left_angle,
            .double_right_angle,
            .double_vert_bar,
            .double_ampersand,
            .plus,
            .tilde,
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
            if (allow_block and self.peek().tag() == .colon) {
                expr = try self.parseLambdaContExpr(expr);
            }
            return self.ast.newNodeErase(.exprStmt, .{
                .child = expr,
            });
        }

        switch (expr.type()) {
            .callExpr,
            .accessExpr,
            .deref,
            .index_expr,
            .ident => {},
            else => {
                return self.reportError("Unsupported assignment left expression: {}", &.{v(expr.type())});
            },
        }

        // Right can be an expr or stmt.
        var right = (try self.parseExpr(.{ .first_expr = true })) orelse {
            return self.reportError("Expected right expression for assignment statement.", &.{});
        };

        if (allow_block and self.peek().tag() == .colon) {
            right = try self.parseLambdaContExpr(right);
        }

        if (tag == .equal) {
            return self.ast.newNodeErase(.assign_stmt, .{
                .left = expr,
                .right = right,
            });
        } else {
            return self.ast.newNodeErase(.op_assign_stmt, .{
                .left = expr,
                .right = right,
                .op = toBinExprOp(tag).?,
                .assign_pos = self.tokenPos(op_start),
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

    fn tokenPos(self: *Parser, idx: u32) u32 {
        return self.tokens[idx].pos();
    }

    fn tokenEnd(self: *Parser, idx: u32) u32 {
        return self.tokens[idx].data.end;
    }

    pub fn newTokenNode(self: *Parser, comptime node_t: ast.NodeType, start: u32) !*ast.Node {
        const token = self.tokens[start];
        return self.ast.newNodeErase(node_t, .{
            .src = self.ast.src_id,
            .pos = token.pos(),
        });
    }

    pub fn newIdentNode(self: *Parser, start: u32) !*ast.Node {
        const token = self.tokens[start];
        const pos = token.pos();
        return self.ast.newNodeErase(.ident, .{
            .src = self.ast.src_id,
            .pos = pos,
            .name = .{
                .ptr = self.ast.src.ptr + pos,
                .len = token.data.end - pos,
            },
        });
    }

    fn newLitNode(self: *Parser, comptime node_t: ast.NodeType, start: u32) !*ast.Literal {
        const token = self.tokens[start];
        const pos = token.pos();
        return self.ast.newLitNode(node_t, pos, token.data.end);
    }

    /// When n=0, this is equivalent to peek.
    inline fn peekAhead(self: Parser, n: u32) Token {
        if (self.next_pos + n < self.tokens.len) {
            return self.tokens[self.next_pos + n];
        } else {
            return Token.init(.null, self.next_pos, 0);
        }
    }

    inline fn peek(self: Parser) Token {
        if (!self.isAtEnd()) {
            return self.tokens[self.next_pos];
        } else {
            return Token.init(.null, @intCast(self.ast.src.len), 0);
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
            try deps.put(alloc, new_src[offset .. offset + dep.len], entry.value_ptr.*);
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
        .tilde => .bitwiseXor,
        .double_star => .pow,
        .slash => .slash,
        .percent => .percent,
        .double_ampersand => .bitwiseAnd,
        .double_vert_bar => .bitwiseOr,
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
        else => null,
    };
}

pub fn getBinOpPrecedence(op: cy.ast.BinaryExprOp) u8 {
    switch (op) {
        .bitwiseLeftShift, .bitwiseRightShift => return 10,

        .bitwiseAnd => return 9,

        .bitwiseXor, .bitwiseOr => return 8,

        .pow => return 7,

        .slash, .percent, .star => {
            return 6;
        },

        .minus, .plus => {
            return 5;
        },

        .cast => return 4,

        .greater, .greater_equal, .less, .less_equal, .bang_equal, .equal_equal => {
            return 3;
        },

        .and_op => return 2,

        .or_op => return 1,

        .range, .reverse_range => return 0,

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
        log.tracev("{s}", .{src[start..]});
    } else {
        log.tracev("{s}", .{src[start .. start + len]});
    }
}

const ParseExprConfig = struct {
    first_expr: bool = false,
    parse_infer_lambda: bool = true,
};

const ParseTermConfig = struct {
    first_expr: bool = false,
    parse_call_expr: bool = true,
    parse_record_expr: bool = true,
    parse_infer_lambda: bool = true,
};

fn isRecordKeyNodeType(node_t: ast.NodeType) bool {
    switch (node_t) {
        .ident,
        .sq_string_lit,
        .decLit,
        .binLit,
        .octLit,
        .hexLit => {
            return true;
        },
        else => {
            return false;
        },
    }
}

fn defaultReportFn(p: *cy.Parser, format: []const u8, args: []const cy.fmt.FmtValue, pos: u32) anyerror {
    _ = p;
    _ = format;
    _ = args;
    _ = pos;
    return error.ParseError;
}
