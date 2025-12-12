const std = @import("std");
const stdx = @import("stdx");
const tt = stdx.testing;
const cy = @import("cyber.zig");
const v = cy.fmt.v;

const keywords = std.StaticStringMap(TokenType).initComptime(.{
    .{ "and", .and_k },
    .{ "as", .as_k },
    .{ "begin", .begin_k },
    .{ "break", .break_k },
    .{ "case", .case_k },
    .{ "const", .const_k },
    .{ "continue", .continue_k },
    .{ "cstruct", .cstruct_k },
    .{ "cunion", .cunion_k },
    .{ "else", .else_k },
    .{ "enum", .enum_k },
    .{ "error", .error_k },
    .{ "false", .false_k },
    .{ "for", .for_k },
    .{ "fn", .fn_k },
    .{ "fnsym", .fnsym_k },
    .{ "global", .global_k },
    .{ "if", .if_k },
    .{ "move", .move_k },
    .{ "none", .none_k },
    .{ "not", .not_k },
    .{ "or", .or_k },
    .{ "pass", .pass_k },
    .{ "return", .return_k },
    .{ "scope", .scope_k },
    .{ "sink", .sink_k },
    .{ "struct", .struct_k },
    .{ "switch", .switch_k },
    .{ "trait", .trait_k },
    .{ "true", .true_k },
    .{ "try", .try_k },
    .{ "type", .type_k },
    .{ "undef", .undef_k },
    .{ "use", .use_k },
    .{ "var", .var_k },
    .{ "void", .void_k },
    .{ "while", .while_k },
    .{ "with", .with_k },
    .{ "yield", .yield_k },
});

pub const TokenType = enum(u8) {
    /// Used to indicate no token.
    null,
    ampersand,
    and_k,
    as_k,
    at_ident,
    bang,
    bang_equal,
    begin_k,
    bin,
    break_k,
    caret,
    case_k,
    colon,
    colon_equal,
    colon2,
    comma,
    const_k,
    continue_k,
    cstruct_k,
    cunion_k,
    dec,
    dec_u,
    dot,
    dot2,
    dot_bang,
    dot_dot_equal,
    dot_dot_rangle,
    dot_dot_rangle_equal,
    dot_star,
    dot_question,
    double_ampersand,
    double_left_angle,
    double_right_angle,
    double_star,
    double_vert_bar,
    else_k,
    enum_k,
    // Error token, returned if ignoreErrors = true.
    err,
    error_k,
    equal,
    equal_equal,
    equal_right_angle,
    false_k,
    float,
    for_k,
    fn_k,
    fnsym_k,
    global_k,
    hex,
    ident,
    if_k,
    indent,
    left_angle,
    left_angle_equal,
    left_brace,
    left_bracket,
    left_paren,
    left_right_angle,
    minus,
    minus_right_angle,
    mod_k,
    move_k,
    new_line,
    none_k,
    not_k,
    oct,
    or_k,
    pass_k,
    percent,
    underscore,
    plus,
    pound,
    question,
    raw_string,
    raw_string_multi,
    return_k,
    right_angle,
    right_angle_equal,
    right_brace,
    right_bracket,
    right_paren,
    scope_k,
    semicolon,
    sink_k,
    slash,
    space_left_bracket,
    sq_string,
    sq_string_multi,
    star,
    string,
    string_multi,
    stringt,
    stringt_multi,
    stringt_part,
    stringt_expr,
    struct_k,
    switch_k,
    tilde,
    trait_k,
    true_k,
    try_k,
    type_k,
    undef_k,
    use_k,
    var_k,
    vert_bar,
    void_k,
    while_k,
    with_k,
    yield_k,
};

pub const Token = extern struct {
    // First 8 bits is the TokenType, last 24 bits is the start pos.
    head: u32,
    data: extern union {
        /// End position.
        end: u32,
        // Num indent spaces.
        indent: u32,
    },

    pub fn init(ttype: TokenType, start_pos: u32, end_pos: u32) Token {
        return .{
            .head = (start_pos << 8) | @intFromEnum(ttype),
            .data = .{ .end = end_pos },
        };
    }

    pub fn initIndent(start_pos: u32, indent: u32) Token {
        return .{
            .head = (start_pos << 8) | @intFromEnum(TokenType.indent),
            .data = .{ .indent = indent },
        };
    }

    pub inline fn tag(self: Token) TokenType {
        return @enumFromInt(self.head & 0xff);
    }

    pub inline fn pos(self: Token) u32 {
        return self.head >> 8;
    }
};

const StringDelim = enum(u2) {
    sq_single,
    sq_triple,
    single,
    triple,
};

pub const TokenizeState = struct {
    stateT: TokenizeStateTag,

    /// For string interpolation, open braces can accumulate so the end of a template can be determined.
    open_braces: u8 = 0,

    /// For string interpolation.
    string_delim: StringDelim = undefined,

    after_whitespace: bool = false,

    has_template_expr: u1 = 0,
};

pub const TokenizeStateTag = enum {
    whitespace,
    token,
    stringt_part,
    stringt_expr,
    end,
};

/// Made generic in case there is a need to use a different src buffer. TODO: substring still needs to be abstracted into user fn.
pub const Tokenizer = struct {
    alloc: std.mem.Allocator,
    src: []const u8,
    tokens: std.ArrayListUnmanaged(Token),
    nextPos: u32,

    /// Whether to parse and accumulate comment tokens in `comments`.
    parseComments: bool,
    comments: std.ArrayListUnmanaged(cy.IndexSlice(u32)),

    /// For syntax highlighting, skip errors.
    ignoreErrors: bool,

    has_error: bool,
    reportFn: *const fn (*anyopaque, format: []const u8, args: []const cy.fmt.FmtValue, pos: u32) anyerror!void,
    ctx: *anyopaque,

    pub fn init(alloc: std.mem.Allocator, src: []const u8) Tokenizer {
        return .{
            .alloc = alloc,
            .src = src,
            .tokens = .{},
            .nextPos = 0,
            .parseComments = false,
            .ignoreErrors = false,
            .comments = .{},
            .reportFn = defaultReportFn,
            .ctx = undefined,
            .has_error = false,
        };
    }

    pub fn deinit(self: *Tokenizer) void {
        self.tokens.deinit(self.alloc);
        self.comments.deinit(self.alloc);
    }

    pub fn consumeComments(self: *Tokenizer) std.ArrayListUnmanaged(cy.IndexSlice(u32)) {
        defer self.comments = .{};
        return self.comments;
    }

    fn isAtEnd(self: *const Tokenizer) bool {
        return self.src.len == self.nextPos;
    }

    fn isNext(self: *const Tokenizer, ch: u8, lookahead: u32) bool {
        if (self.nextPos >= self.src.len - lookahead) {
            return false;
        }
        return self.src[self.nextPos + lookahead] == ch;
    }

    fn consume(self: *Tokenizer) u8 {
        const ch = self.peek();
        self.advance();
        return ch;
    }

    fn peek(self: *const Tokenizer) u8 {
        return self.src[self.nextPos];
    }

    fn getSubStrFrom(self: *const Tokenizer, start: u32) []const u8 {
        return self.src[start..self.nextPos];
    }

    fn peekAhead(self: *const Tokenizer, steps: u32) ?u8 {
        if (self.nextPos < self.src.len - steps) {
            return self.src[self.nextPos + steps];
        } else return null;
    }

    fn advance(self: *Tokenizer) void {
        self.nextPos += 1;
    }

    /// Consumes the next token skipping whitespace and returns the next tokenizer state.
    fn tokenizeOne(t: *Tokenizer, state: TokenizeState) !TokenizeState {
        if (isAtEnd(t)) {
            return .{
                .stateT = .end,
            };
        }

        const start = t.nextPos;
        const ch = consume(t);
        switch (ch) {
            '(' => {
                try t.pushToken(.left_paren, start);
            },
            ')' => {
                try t.pushToken(.right_paren, start);
            },
            '{' => {
                try t.pushToken(.left_brace, start);
                if (state.stateT == .stringt_expr) {
                    var next = state;
                    next.open_braces += 1;
                    return next;
                }
            },
            '}' => {
                try t.pushToken(.right_brace, start);
                if (state.stateT == .stringt_expr) {
                    var next = state;
                    if (state.open_braces == 0) {
                        next.stateT = .stringt_part;
                    } else {
                        next.open_braces -= 1;
                    }
                    return next;
                }
            },
            '[' => {
                if (state.after_whitespace) {
                    try t.pushToken(.space_left_bracket, start);
                } else {
                    try t.pushToken(.left_bracket, start);
                }
            },
            ']' => try t.pushToken(.right_bracket, start),
            ',' => try t.pushToken(.comma, start),
            '.' => {
                switch (peek(t)) {
                    '!' => {
                        advance(t);
                        try t.pushToken(.dot_bang, start);
                    },
                    '.' => {
                        advance(t);
                        if (peek(t) == '>') {
                            advance(t);
                            if (peek(t) == '=') {
                                advance(t);
                                try t.pushToken(.dot_dot_rangle_equal, start);
                            } else {
                                try t.pushToken(.dot_dot_rangle, start);
                            }
                        } else if (peek(t) == '=') {
                            advance(t);
                            try t.pushToken(.dot_dot_equal, start);
                        } else {
                            try t.pushToken(.dot2, start);
                        }
                    },
                    '?' => {
                        advance(t);
                        try t.pushToken(.dot_question, start);
                    },
                    '*' => {
                        advance(t);
                        try t.pushToken(.dot_star, start);
                    },
                    else => {
                        try t.pushToken(.dot, start);
                    },
                }
            },
            ';' => {
                try t.pushToken(.semicolon, start);
            },
            ':' => {
                if (peek(t) == '=') {
                    advance(t);
                    try t.pushToken(.colon_equal, start);
                } else if (peek(t) == ':') {
                    advance(t);
                    try t.pushToken(.colon2, start);
                } else {
                    try t.pushToken(.colon, start);
                }
            },
            '@' => {
                consumeIdent(t);
                try t.pushToken(.at_ident, start);
            },
            '-' => {
                const next = peek(t);
                if (next == '-') {
                    advance(t);
                    // Single line comment. Ignore chars until eol.
                    while (!isAtEnd(t)) {
                        if (peek(t) == '\n') {
                            if (t.parseComments) {
                                try t.comments.append(t.alloc, cy.IndexSlice(u32).init(start, t.nextPos));
                            }
                            // Don't consume new line or the current indentation could augment with the next line.
                            return tokenizeOne(t, state);
                        }
                        advance(t);
                    }
                    if (t.parseComments) {
                        try t.comments.append(t.alloc, cy.IndexSlice(u32).init(start, t.nextPos));
                    }
                    return .{ .stateT = .end };
                } else if (next == '>') {
                    advance(t);
                    try t.pushToken(.minus_right_angle, start);
                } else {
                    try t.pushToken(.minus, start);
                }
            },
            '%' => try t.pushToken(.percent, start),
            '&' => {
                if (peek(t) == '&') {
                    advance(t);
                    try t.pushToken(.double_ampersand, start);
                } else {
                    try t.pushToken(.ampersand, start);
                }
            },
            '|' => {
                if (peek(t) == '|') {
                    advance(t);
                    try t.pushToken(.double_vert_bar, start);
                } else {
                    try t.pushToken(.vert_bar, start);
                }
            },
            '~' => try t.pushToken(.tilde, start),
            '+' => {
                try t.pushToken(.plus, start);
            },
            '_' => {
                const next = peek(t);
                if ((next >= 'A' and next <= 'Z') or (next >= 'a' and next <= 'z') or next == '_') {
                    try tokenizeKeywordOrIdent(t, start);
                } else {
                    try t.pushToken(.underscore, start);
                }
            },
            '^' => {
                try t.pushToken(.caret, start);
            },
            '*' => {
                if (peek(t) == '*') {
                    advance(t);
                    try t.pushToken(.double_star, start);
                } else {
                    try t.pushToken(.star, start);
                }
            },
            '/' => {
                try t.pushToken(.slash, start);
            },
            '!' => {
                if (isNext(t, '=', 0)) {
                    try t.pushToken(.bang_equal, start);
                    advance(t);
                } else {
                    try t.pushToken(.bang, start);
                }
            },
            '=' => {
                if (!isAtEnd(t)) {
                    switch (peek(t)) {
                        '=' => {
                            advance(t);
                            try t.pushToken(.equal_equal, start);
                        },
                        '>' => {
                            advance(t);
                            try t.pushToken(.equal_right_angle, start);
                        },
                        else => {
                            try t.pushToken(.equal, start);
                        },
                    }
                } else {
                    try t.pushToken(.equal, start);
                }
            },
            '<' => {
                const ch2 = peek(t);
                if (ch2 == '=') {
                    try t.pushToken(.left_angle_equal, start);
                    advance(t);
                } else if (ch2 == '<') {
                    try t.pushToken(.double_left_angle, start);
                    advance(t);
                } else if (ch2 == '>') {
                    try t.pushToken(.left_right_angle, start);
                    advance(t);
                } else {
                    try t.pushToken(.left_angle, start);
                }
            },
            '>' => {
                const ch2 = peek(t);
                if (ch2 == '=') {
                    try t.pushToken(.right_angle_equal, start);
                    advance(t);
                } else if (ch2 == '>') {
                    try t.pushToken(.double_right_angle, start);
                    advance(t);
                } else {
                    try t.pushToken(.right_angle, start);
                }
            },
            ' ', '\r', '\t' => {
                // Consume whitespace.
                while (!isAtEnd(t)) {
                    const ch2 = peek(t);
                    switch (ch2) {
                        ' ',
                        '\r',
                        '\t' => advance(t),
                        else => {
                            var next = state;
                            next.after_whitespace = true;
                            return tokenizeOne(t, next);
                        },
                    }
                }
                return .{ .stateT = .end };
            },
            '\n' => {
                try t.pushToken(.new_line, start);
                return .{ .stateT = .whitespace };
            },
            '`' => {
                if (t.isNext('`', 0)) {
                    if (t.isNext('`', 1)) {
                        _ = t.consume();
                        _ = t.consume();
                        try t.consumeRawStringMulti();
                        try t.pushToken(.raw_string_multi, start);
                    } else {
                        _ = t.consume();
                        try t.pushToken(.raw_string, start);
                    }
                } else {
                    try t.consumeRawString();
                    try t.pushToken(.raw_string, start);
                }
            },
            '"' => {
                if (peek(t) == '"') {
                    if (peekAhead(t, 1)) |ch2| {
                        if (ch2 == '"') {
                            _ = consume(t);
                            _ = consume(t);
                            return tokenizeStringOne(t, start, .{
                                .stateT = state.stateT,
                                .string_delim = .triple,
                            });
                        }
                    }
                }
                return tokenizeStringOne(t, start, .{
                    .stateT = state.stateT,
                    .string_delim = .single,
                });
            },
            '\'' => {
                if (peek(t) == '\'') {
                    if (peekAhead(t, 1)) |ch2| {
                        if (ch2 == '\'') {
                            _ = consume(t);
                            _ = consume(t);
                            // try tokenizeMultiLineRawString(t, start);
                            // return state;
                            return tokenizeStringOne(t, start, .{
                                .stateT = state.stateT,
                                .string_delim = .sq_triple,
                            });
                        }
                    }
                }
                // try tokenizeSingleLineRawString(t, start);
                return tokenizeStringOne(t, start, .{
                    .stateT = state.stateT,
                    .string_delim = .sq_single,
                });
            },
            '#' => try t.pushToken(.pound, start),
            '?' => try t.pushToken(.question, start),
            else => {
                switch (ch) {
                    'A'...'Z', 'a'...'z' => {
                        try tokenizeKeywordOrIdent(t, start);
                        return .{ .stateT = .token };
                    },
                    '0'...'9' => {
                        try tokenizeNumber(t, start);
                        return .{ .stateT = .token };
                    },
                    else => {
                        if (t.ignoreErrors) {
                            try t.pushToken(.err, start);
                            return .{ .stateT = .token };
                        } else {
                            try t.reportErrorAt("unknown character: {} ({}) at {}", &.{ cy.fmt.char(ch), v(ch), v(start) }, start);
                        }
                    },
                }
            },
        }
        return .{ .stateT = .token };
    }

    /// Returns true if an indent or new line token was parsed.
    fn tokenizeIndentOne(t: *Tokenizer) !bool {
        if (isAtEnd(t)) {
            return false;
        }
        var ch = peek(t);
        switch (ch) {
            ' ' => {
                const start = t.nextPos;
                advance(t);
                var count: u32 = 1;
                while (true) {
                    if (isAtEnd(t)) {
                        break;
                    }
                    ch = peek(t);
                    if (ch == ' ') {
                        count += 1;
                        advance(t);
                    } else break;
                }
                try t.pushIndentToken(count, start, true);
                return true;
            },
            '\t' => {
                const start = t.nextPos;
                advance(t);
                var count: u32 = 1;
                while (true) {
                    if (isAtEnd(t)) {
                        break;
                    }
                    ch = peek(t);
                    if (ch == '\t') {
                        count += 1;
                        advance(t);
                    } else break;
                }
                try t.pushIndentToken(count, start, false);
                return true;
            },
            '\n' => {
                try t.pushToken(.new_line, t.nextPos);
                advance(t);
                return true;
            },
            else => return false,
        }
    }

    pub fn tokenize(t: *Tokenizer) !void {
        t.tokens.clearRetainingCapacity();
        t.nextPos = 0;

        if (t.src.len >= 3) {
            if (t.src[0] == 0xEF and t.src[1] == 0xBB and t.src[2] == 0xBF) {
                // Skip UTF-8 BOM.
                t.nextPos = 3;
            }
        }

        if (t.src.len >= t.nextPos + 2) {
            if (t.src[t.nextPos] == '#' and t.src[t.nextPos + 1] == '!') {
                // Ignore shebang line.
                while (!isAtEnd(t)) {
                    if (peek(t) == '\n') {
                        advance(t);
                        break;
                    }
                    advance(t);
                }
            }
        }

        var state = TokenizeState{
            .stateT = .whitespace,
        };
        while (true) {
            switch (state.stateT) {
                .whitespace => {
                    while (true) {
                        if (!(try tokenizeIndentOne(t))) {
                            state.stateT = .token;
                            break;
                        }
                    }
                },
                .token => {
                    while (true) {
                        state = try tokenizeOne(t, state);
                        if (state.stateT != .token) {
                            break;
                        }
                    }
                },
                .stringt_part => {
                    state = try tokenizeStringOne(t, t.nextPos, state);
                },
                .stringt_expr => {
                    while (true) {
                        const nextState = try tokenizeOne(t, state);
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
    fn tokenizeStringOne(t: *Tokenizer, start: u32, state: TokenizeState) !TokenizeState {
        const save = t.nextPos;
        while (true) {
            if (isAtEnd(t)) {
                if (t.ignoreErrors) {
                    t.nextPos = save;
                    try t.pushToken(.err, save);
                    return .{ .stateT = .token };
                } else {
                    try t.reportErrorAt("UnterminatedString", &.{}, save);
                }
            }

            switch (t.peek()) {
                '"' => {
                    switch (state.string_delim) {
                        .single => {
                            if (state.has_template_expr == 1) {
                                try t.pushToken(.stringt_part, start);
                                const end = t.nextPos;
                                advance(t);
                                try t.pushToken(.stringt, end);
                            } else {
                                advance(t);
                                try t.pushToken(.string, start);
                            }
                            return .{ .stateT = .token };
                        },
                        .triple => {
                            var ch2 = peekAhead(t, 1) orelse 0;
                            if (ch2 == '"') {
                                ch2 = peekAhead(t, 2) orelse 0;
                                if (ch2 == '"') {
                                    if (state.has_template_expr == 1) {
                                        try t.pushToken(.stringt_part, start);
                                        const end = t.nextPos;
                                        advance(t);
                                        advance(t);
                                        advance(t);
                                        try t.pushToken(.stringt_multi, end);
                                    } else {
                                        advance(t);
                                        advance(t);
                                        advance(t);
                                        try t.pushToken(.string_multi, start);
                                    }
                                    return .{ .stateT = .token };
                                }
                            }
                            advance(t);
                        },
                        else => {
                            advance(t);
                        },
                    }
                },
                '\'' => {
                    switch (state.string_delim) {
                        .sq_single => {
                            if (state.has_template_expr == 1) {
                                try t.pushToken(.stringt_part, start);
                                const end = t.nextPos;
                                advance(t);
                                try t.pushToken(.stringt, end);
                            } else {
                                advance(t);
                                try t.pushToken(.sq_string, start);
                            }
                            return .{ .stateT = .token };
                        },
                        .sq_triple => {
                            var ch2 = peekAhead(t, 1) orelse 0;
                            if (ch2 == '\'') {
                                ch2 = peekAhead(t, 2) orelse 0;
                                if (ch2 == '\'') {
                                    if (state.has_template_expr == 1) {
                                        try t.pushToken(.stringt_part, start);
                                        const end = t.nextPos;
                                        advance(t);
                                        advance(t);
                                        advance(t);
                                        try t.pushToken(.stringt_multi, end);
                                    } else {
                                        advance(t);
                                        advance(t);
                                        advance(t);
                                        try t.pushToken(.sq_string_multi, start);
                                    }
                                    return .{ .stateT = .token };
                                }
                            }
                            advance(t);
                        },
                        else => {
                            advance(t);
                        },
                    }
                },
                '%' => {
                    const ch2 = peekAhead(t, 1) orelse 0;
                    if (ch2 == '{') {
                        var next = state;
                        if (state.has_template_expr == 0) {
                            // Encounter first expression.
                            switch (state.string_delim) {
                                .sq_single,
                                .single => {
                                    try t.pushToken2(.stringt, start, save);
                                },
                                .sq_triple,
                                .triple => {
                                    try t.pushToken2(.stringt_multi, start, save);
                                },
                            }
                            try t.pushToken(.stringt_part, save);
                            next.has_template_expr = 1;
                        } else {
                            try t.pushToken(.stringt_part, start);
                        }
                        const expr_start = t.nextPos;
                        advance(t);
                        advance(t);
                        try t.pushToken(.stringt_expr, expr_start);
                        next.stateT = .stringt_expr;
                        next.open_braces = 0;
                        return next;
                    } else {
                        advance(t);
                    }
                },
                '\\' => {
                    // Escape the next character.
                    advance(t);
                    if (isAtEnd(t)) {
                        if (t.ignoreErrors) {
                            t.nextPos = start;
                            try t.pushToken(.err, start);
                            return .{ .stateT = .token };
                        } else {
                            try t.reportErrorAt("UnterminatedString", &.{}, start);
                        }
                    }
                    advance(t);
                },
                '\n' => {
                    if (state.string_delim == .single or state.string_delim == .sq_single) {
                        if (t.ignoreErrors) {
                            t.nextPos = start;
                            try t.pushToken(.err, start);
                            return .{ .stateT = .token };
                        } else {
                            try t.reportErrorAt("Encountered new line in single line literal.", &.{}, t.nextPos);
                        }
                    }
                    advance(t);
                },
                else => {
                    advance(t);
                }
            }
        }
    } 

    /// Assume first character is consumed already.
    fn consumeIdent(t: *Tokenizer) void {
        while (true) {
            if (isAtEnd(t)) {
                return;
            }
            const ch = peek(t);
            switch (ch) {
                '0'...'9', 'A'...'Z', 'a'...'z', '_' => {
                    advance(t);
                    continue;
                },
                else => return,
            }
        }
    }

    fn consumeRawStringMulti(t: *Tokenizer) !void {
        while (true) {
            if (isAtEnd(t)) {
                try t.reportError("Expected raw string.", &.{});
            }
            switch (peek(t)) {
                '`' => {
                    _ = t.consume();
                    if (t.isNext('`', 0)) {
                        _ = t.consume();
                    } else {
                        continue;
                    }
                    if (t.isNext('`', 0)) {
                        _ = t.consume();
                        return;
                    }
                },
                else => {
                    _ = t.consume();
                },
            }
        }
    }

    fn consumeRawString(t: *Tokenizer) !void {
        while (true) {
            if (isAtEnd(t)) {
                try t.reportError("Expected raw string.", &.{});
            }
            switch (peek(t)) {
                '`' => {
                    _ = t.consume();
                    return;
                },
                '\n' => {
                    try t.reportError("Expected raw string on a single line.", &.{});
                },
                else => {
                    _ = t.consume();
                },
            }
        }
    }

    fn tokenizeKeywordOrIdent(t: *Tokenizer, start: u32) !void {
        consumeIdent(t);
        if (keywords.get(getSubStrFrom(t, start))) |token_t| {
            try t.pushToken(token_t, start);
        } else {
            try t.pushToken(.ident, start);
        }
    }

    fn tokenizeSingleLineRawString(t: *Tokenizer, start: u32) !void {
        const save = t.nextPos;
        while (true) {
            if (isAtEnd(t)) {
                if (t.ignoreErrors) {
                    t.nextPos = save;
                    try t.pushToken(.err, start);
                } else return t.reportErrorAt("UnterminatedString", &.{}, start);
            }
            if (peek(t) == '\'') {
                advance(t);
                try t.pushToken(.sq_string, start);
                return;
            } else if (peek(t) == '\n') {
                return t.reportErrorAt("Encountered new line in single line literal.", &.{}, t.nextPos);
            } else {
                advance(t);
            }
        }
    }

    fn tokenizeMultiLineRawString(t: *Tokenizer, start: u32) !void {
        const save = t.nextPos;
        while (true) {
            if (isAtEnd(t)) {
                if (t.ignoreErrors) {
                    t.nextPos = save;
                    try t.pushToken(.err, start);
                } else return t.reportErrorAt("UnterminatedString", &.{}, start);
            }
            if (peek(t) == '\'') {
                const ch = peekAhead(t, 1) orelse {
                    advance(t);
                    continue;
                };
                const ch2 = peekAhead(t, 2) orelse {
                    advance(t);
                    continue;
                };
                if (ch == '\'' and ch2 == '\'') {
                    advance(t);
                    advance(t);
                    advance(t);
                    try t.pushToken(.sq_string_multi, start);
                    return;
                } else {
                    advance(t);
                    continue;
                }
            } else {
                advance(t);
            }
        }
    }

    fn consumeDigits(t: *Tokenizer) void {
        while (true) {
            if (isAtEnd(t)) {
                return;
            }
            const ch = peek(t);
            if (ch >= '0' and ch <= '9') {
                advance(t);
                continue;
            } else break;
        }
    }

    /// Assumes first digit is consumed.
    fn tokenizeNumber(t: *Tokenizer, start: u32) !void {
        if (isAtEnd(t)) {
            try t.pushToken(.dec, start);
            return;
        }

        var ch = peek(t);
        if ((ch >= '0' and ch <= '9') or ch == '.' or ch == 'e' or ch == 'u') {
            consumeDigits(t);
            if (isAtEnd(t)) {
                try t.pushToken(.dec, start);
                return;
            }
            if (peek(t) == 'u') {
                advance(t);
                try t.pushToken(.dec_u, start);
                return;
            }

            var isFloat = false;
            ch = peek(t);
            if (ch == '.') {
                const next = peekAhead(t, 1) orelse {
                    try t.pushToken(.dec, start);
                    return;
                };
                if (next < '0' or next > '9') {
                    try t.pushToken(.dec, start);
                    return;
                }
                advance(t);
                advance(t);
                consumeDigits(t);
                if (isAtEnd(t)) {
                    try t.pushToken(.float, start);
                    return;
                }
                ch = peek(t);
                isFloat = true;
            }

            if (ch == 'e') {
                advance(t);
                if (isAtEnd(t)) {
                    return t.reportError("Expected number.", &.{});
                }
                ch = peek(t);
                if (ch == '-') {
                    advance(t);
                    if (isAtEnd(t)) {
                        return t.reportError("Expected number.", &.{});
                    }
                    ch = peek(t);
                }
                if (ch < '0' and ch > '9') {
                    return t.reportError("Expected number.", &.{});
                }

                consumeDigits(t);
                isFloat = true;
            }

            if (isFloat) {
                try t.pushToken(.float, start);
            } else {
                try t.pushToken(.dec, start);
            }
            return;
        }

        if (t.src[t.nextPos - 1] == '0') {
            // Less common integer notation.
            if (ch == 'x') {
                // Hex integer.
                advance(t);
                while (true) {
                    if (isAtEnd(t)) {
                        break;
                    }
                    ch = peek(t);
                    if ((ch >= '0' and ch <= '9') or (ch >= 'A' and ch <= 'Z') or (ch >= 'a' and ch <= 'z')) {
                        advance(t);
                        continue;
                    } else break;
                }
                try t.pushToken(.hex, start);
                return;
            } else if (ch == 'o') {
                // Oct integer.
                advance(t);
                while (true) {
                    if (isAtEnd(t)) {
                        break;
                    }
                    ch = peek(t);
                    if (ch >= '0' and ch <= '8') {
                        advance(t);
                        continue;
                    } else break;
                }
                try t.pushToken(.oct, start);
                return;
            } else if (ch == 'b') {
                // Bin integer.
                advance(t);
                while (true) {
                    if (isAtEnd(t)) {
                        break;
                    }
                    ch = peek(t);
                    if (ch == '0' or ch == '1') {
                        advance(t);
                        continue;
                    } else break;
                }
                try t.pushToken(.bin, start);
                return;
            } else if (ch == 'u') {
                advance(t);
                try t.pushToken(.dec_u, start);
                return;
            } else {
                if (std.ascii.isAlphabetic(ch)) {
                    const char: []const u8 = &[_]u8{ch};
                    return t.reportError("Unsupported integer notation: {}", &.{v(char)});
                }
            }
        }

        // Push single digit number.
        try t.pushToken(.dec, start);
        return;
    }

    fn pushIndentToken(self: *Tokenizer, count: u32, start_pos: u32, spaces: bool) !void {
        try self.tokens.append(self.alloc, Token.initIndent(start_pos, if (spaces) count else count | 0x80000000));
    }

    fn pushToken(self: *Tokenizer, token_t: TokenType, start_pos: u32) !void {
        try self.tokens.append(self.alloc, Token.init(token_t, start_pos, self.nextPos));
    }

    fn pushToken2(self: *Tokenizer, token_t: TokenType, start_pos: u32, end_pos: u32) !void {
        try self.tokens.append(self.alloc, Token.init(token_t, start_pos, end_pos));
    }

    fn reportError(self: *Tokenizer, format: []const u8, args: []const cy.fmt.FmtValue) anyerror!void {
        try self.reportErrorAt(format, args, self.nextPos);
    }

    fn reportErrorAt(self: *Tokenizer, format: []const u8, args: []const cy.fmt.FmtValue, pos: u32) anyerror!void {
        self.has_error = true;
        try self.reportFn(self.ctx, format, args, pos);
    }
};

pub fn defaultReportFn(ctx: *anyopaque, format: []const u8, args: []const cy.fmt.FmtValue, pos: u32) anyerror!void {
    _ = ctx;
    _ = format;
    _ = args;
    _ = pos;
    return error.TokenError;
}

test "tokenizer internals." {
    try tt.eq(@sizeOf(Token), 8);
    try tt.eq(@alignOf(Token), 4);
    try tt.eq(@sizeOf(TokenizeState), 5);

    try tt.eq(110, std.enums.values(TokenType).len);
    try tt.eq(39, keywords.kvs.len);
}
