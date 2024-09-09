const std = @import("std");
const stdx = @import("stdx");
const tt = stdx.testing;
const cy = @import("cyber.zig");
const v = cy.fmt.v;

const keywords = std.StaticStringMap(TokenType).initComptime(.{
    .{ "and", .and_k },
    .{ "as", .as_k },
    .{ "await", .await_k },
    .{ "break", .break_k },
    .{ "case", .case_k },
    .{ "catch", .catch_k },
    .{ "coinit", .coinit_k },
    .{ "context", .context_k },
    .{ "continue", .continue_k },
    .{ "coresume", .coresume_k },
    .{ "coyield", .coyield_k },
    .{ "cstruct", .cstruct_k },
    .{ "def", .def_k },
    .{ "dyn", .dyn_k },
    .{ "else", .else_k },
    .{ "enum", .enum_k },
    .{ "error", .error_k },
    .{ "false", .false_k },
    .{ "for", .for_k },
    .{ "func", .func_k },
    .{ "Func", .Func_k },
    .{ "if", .if_k },
    .{ "mod", .mod_k },
    .{ "none", .none_k },
    .{ "not", .not_k },
    .{ "object", .object_k },
    .{ "or", .or_k },
    .{ "pass", .pass_k },
    .{ "return", .return_k },
    .{ "struct", .struct_k },
    .{ "switch", .switch_k },
    .{ "symbol", .symbol_k },
    .{ "throw", .throw_k },
    .{ "trait", .trait_k },
    .{ "true", .true_k },
    .{ "try", .try_k },
    .{ "type", .type_k },
    .{ "use", .use_k },
    .{ "var", .var_k },
    .{ "void", .void_k },
    .{ "while", .while_k },
    .{ "with", .with_k },
});

pub const TokenType = enum(u8) {
    /// Used to indicate no token.
    null,
    ampersand,
    and_k,
    as_k,
    at,
    await_k,
    bang,
    bang_equal,
    bin,
    break_k,
    caret,
    case_k,
    catch_k,
    coinit_k,
    colon,
    comma,
    context_k,
    continue_k,
    coresume_k,
    coyield_k,
    cstruct_k,
    dec,
    def_k,
    dot,
    dot_bang,
    dot_dot,
    dot_star,
    dot_question,
    double_left_angle,
    double_right_angle,
    double_vert_bar,
    dyn_k,
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
    func_k,
    Func_k,
    hex,
    ident,
    if_k,
    indent,
    left_angle,
    left_angle_equal,
    left_brace,
    left_bracket,
    left_paren,
    minus,
    minus_double_dot,
    minus_right_angle,
    mod_k,
    new_line,
    none_k,
    not_k,
    object_k,
    oct,
    or_k,
    pass_k,
    percent,
    underscore,
    plus,
    pound,
    question,
    return_k,
    right_angle,
    right_angle_equal,
    right_brace,
    right_bracket,
    right_paren,
    rune,
    raw_string,
    slash,
    star,
    string,
    struct_k,
    switch_k,
    symbol_k,
    throw_k,
    tilde,
    trait_k,
    true_k,
    try_k,
    type_k,
    use_k,
    var_k,
    vert_bar,
    void_k,
    while_k,
    with_k,
};

pub const Token = extern struct {
    // First 8 bits is the TokenType, last 24 bits is the start pos.
    head: u32,
    data: extern union {
        end_pos: u32,
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

const StringDelim = enum(u2) {
    single,
    triple,
};

pub const TokenizeState = struct {
    stateT: TokenizeStateTag,
};

pub const TokenizeStateTag = enum {
    start,
    token,
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

    fn isNextChar(self: *const Tokenizer, ch: u8) bool {
        if (self.isAtEnd()) {
            return false;
        }
        return self.peek() == ch;
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
        var ch = consume(t);
        switch (ch) {
            '(' => {
                try t.pushToken(.left_paren, start);
            },
            ')' => {
                try t.pushToken(.right_paren, start);
            },
            '{' => {
                try t.pushToken(.left_brace, start);
            },
            '}' => {
                try t.pushToken(.right_brace, start);
            },
            '[' => try t.pushToken(.left_bracket, start),
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
                        try t.pushToken(.dot_dot, start);
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
            ':' => {
                try t.pushToken(.colon, start);
            },
            '@' => try t.pushToken(.at, start),
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
                } else if (next == '.' and peekAhead(t, 1) == '.') {
                    advance(t);
                    advance(t);
                    try t.pushToken(.minus_double_dot, start);
                } else {
                    try t.pushToken(.minus, start);
                }
            },
            '%' => try t.pushToken(.percent, start),
            '&' => try t.pushToken(.ampersand, start),
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
                if ((next >= 'A' and next <= 'Z') or (next >= 'a' and next <= 'z') or next == '$' or next == '_') {
                    try tokenizeKeywordOrIdent(t, start);
                } else {
                    try t.pushToken(.underscore, start);
                }
            },
            '^' => {
                try t.pushToken(.caret, start);
            },
            '*' => {
                try t.pushToken(.star, start);
            },
            '/' => {
                try t.pushToken(.slash, start);
            },
            '!' => {
                if (isNextChar(t, '=')) {
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
                        ' ', '\r', '\t' => advance(t),
                        else => return tokenizeOne(t, state),
                    }
                }
                return .{ .stateT = .end };
            },
            '\n' => {
                try t.pushToken(.new_line, start);
                return .{ .stateT = .start };
            },
            '`' => {
                // UTF-8 codepoint literal (rune).
                if (isAtEnd(t)) {
                    try t.reportError("Expected UTF-8 rune.", &.{});
                }
                while (true) {
                    if (isAtEnd(t)) {
                        try t.reportError("Expected UTF-8 rune.", &.{});
                    }
                    ch = peek(t);
                    if (ch == '\\') {
                        advance(t);
                        if (isAtEnd(t)) {
                            try t.reportError("Expected back tick or backslash.", &.{});
                        }
                        advance(t);
                    } else {
                        advance(t);
                        if (ch == '`') {
                            break;
                        }
                    }
                }
                try t.pushSpanToken(.rune, start + 1, t.nextPos - 1);
            },
            '"' => {
                if (peek(t) == '"') {
                    if (peekAhead(t, 1)) |ch2| {
                        if (ch2 == '"') {
                            _ = consume(t);
                            _ = consume(t);
                            try tokenizeStringMulti(t, t.nextPos);
                            return state;
                        }
                    }
                }
                try tokenizeString(t, t.nextPos);
            },
            '\'' => {
                if (peek(t) == '\'') {
                    if (peekAhead(t, 1)) |ch2| {
                        if (ch2 == '\'') {
                            _ = consume(t);
                            _ = consume(t);
                            try tokenizeMultiLineRawString(t, t.nextPos);
                            return state;
                        }
                    }
                }
                try tokenizeSingleLineRawString(t, t.nextPos);
            },
            '#' => try t.pushToken(.pound, start),
            '?' => try t.pushToken(.question, start),
            else => {
                switch (ch) {
                    'A'...'Z', 'a'...'z', '$' => {
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
            .stateT = .start,
        };
        while (true) {
            switch (state.stateT) {
                .start => {
                    // First parse indent spaces.
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
                .end => {
                    break;
                },
            }
        }
    }

    fn tokenizeString(t: *Tokenizer, start: u32) !void {
        const save = t.nextPos;
        while (true) {
            if (isAtEnd(t)) {
                if (t.ignoreErrors) {
                    t.nextPos = save;
                    try t.pushToken(.err, start);
                } else return t.reportErrorAt("UnterminatedString", &.{}, start);
            }

            switch (t.peek()) {
                '"' => {
                    try t.pushSpanToken(.string, start, t.nextPos);
                    advance(t);
                    return;
                },
                '\\' => {
                    // Escape the next character.
                    _ = consume(t);
                    if (isAtEnd(t)) {
                        if (t.ignoreErrors) {
                            t.nextPos = start;
                            try t.pushToken(.err, start);
                            return;
                        } else {
                            return t.reportErrorAt("UnterminatedString", &.{}, start);
                        }
                    }
                    _ = consume(t);
                },
                '\n' => {
                    if (t.ignoreErrors) {
                        t.nextPos = start;
                        try t.pushToken(.err, start);
                    } else {
                        return t.reportErrorAt("Encountered new line in single line literal.", &.{}, start);
                    }
                },
                else => {
                    advance(t);
                }
            }
        }
    }

    fn tokenizeStringMulti(t: *Tokenizer, start: u32) !void {
        const save = t.nextPos;
        while (true) {
            if (isAtEnd(t)) {
                if (t.ignoreErrors) {
                    t.nextPos = save;
                    try t.pushToken(.err, start);
                } else return t.reportErrorAt("UnterminatedString", &.{}, start);
            }

            switch (t.peek()) {
                '"' => {
                    var ch = peekAhead(t, 1) orelse 0;
                    if (ch == '"') {
                        ch = peekAhead(t, 2) orelse 0;
                        if (ch == '"') {
                            try t.pushSpanToken(.string, start, t.nextPos);
                            _ = consume(t);
                            _ = consume(t);
                            _ = consume(t);
                            return;
                        }
                    }
                    _ = consume(t);
                },
                '\\' => {
                    // Escape the next character.
                    _ = consume(t);
                    if (isAtEnd(t)) {
                        if (t.ignoreErrors) {
                            t.nextPos = start;
                            try t.pushToken(.err, start);
                        } else {
                            return t.reportErrorAt("UnterminatedString", &.{}, start);
                        }
                    }
                    _ = consume(t);
                    continue;
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
                '0'...'9', 'A'...'Z', 'a'...'z', '$', '_' => {
                    advance(t);
                    continue;
                },
                else => return,
            }
        }
    }

    fn tokenizeKeywordOrIdent(t: *Tokenizer, start: u32) !void {
        consumeIdent(t);
        if (keywords.get(getSubStrFrom(t, start))) |token_t| {
            try t.pushSpanToken(token_t, start, t.nextPos);
        } else {
            try t.pushSpanToken(.ident, start, t.nextPos);
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
                try t.pushSpanToken(.raw_string, start, t.nextPos);
                advance(t);
                return;
            } else if (peek(t) == '\n') {
                return t.reportErrorAt("Encountered new line in single line literal.", &.{}, start);
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
                    try t.pushSpanToken(.raw_string, start, t.nextPos);
                    advance(t);
                    advance(t);
                    advance(t);
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
            try t.pushSpanToken(.dec, start, t.nextPos);
            return;
        }

        var ch = peek(t);
        if ((ch >= '0' and ch <= '9') or ch == '.' or ch == 'e') {
            consumeDigits(t);
            if (isAtEnd(t)) {
                try t.pushSpanToken(.dec, start, t.nextPos);
                return;
            }

            var isFloat = false;
            ch = peek(t);
            if (ch == '.') {
                const next = peekAhead(t, 1) orelse {
                    try t.pushSpanToken(.dec, start, t.nextPos);
                    return;
                };
                if (next < '0' or next > '9') {
                    try t.pushSpanToken(.dec, start, t.nextPos);
                    return;
                }
                advance(t);
                advance(t);
                consumeDigits(t);
                if (isAtEnd(t)) {
                    try t.pushSpanToken(.float, start, t.nextPos);
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
                try t.pushSpanToken(.float, start, t.nextPos);
            } else {
                try t.pushSpanToken(.dec, start, t.nextPos);
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
                try t.pushSpanToken(.hex, start, t.nextPos);
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
                try t.pushSpanToken(.oct, start, t.nextPos);
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
                try t.pushSpanToken(.bin, start, t.nextPos);
                return;
            } else {
                if (std.ascii.isAlphabetic(ch)) {
                    const char: []const u8 = &[_]u8{ch};
                    return t.reportError("Unsupported integer notation: {}", &.{v(char)});
                }
            }
        }

        // Push single digit number.
        try t.pushSpanToken(.dec, start, t.nextPos);
        return;
    }

    fn pushIndentToken(self: *Tokenizer, count: u32, start_pos: u32, spaces: bool) !void {
        try self.tokens.append(self.alloc, Token.init(.indent, start_pos, .{
            .indent = if (spaces) count else count | 0x80000000,
        }));
    }

    fn pushToken(self: *Tokenizer, token_t: TokenType, start_pos: u32) !void {
        try self.tokens.append(self.alloc, Token.init(token_t, start_pos, .{ .end_pos = cy.NullId }));
    }

    fn pushSpanToken(self: *Tokenizer, token_t: TokenType, startPos: u32, endPos: u32) !void {
        try self.tokens.append(self.alloc, Token.init(token_t, startPos, .{ .end_pos = endPos }));
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
    try tt.eq(@sizeOf(TokenizeState), 1);

    try tt.eq(std.enums.values(TokenType).len, 95);
    try tt.eq(keywords.kvs.len, 42);
}
