use meta

src := ```
use math

nouns := []str{'World', '世界', 'दुनिया', 'mundo'}
nouns += math.random().fmt()
for nouns |n|:
    print('Hello, %{n}!')
```

tokenizer := Tokenizer(src)
tokenizer.tokenize()!

for tokenizer.tokens |token|:
    eprint('%{meta.enum_name(token.tag())}, %{token.pos()}')

type TokenType enum:
    -- Used to indicate no token.
    case null
    case ampersand
    case and_k
    case as_k
    case at_ident
    case bang
    case bang_equal
    case begin_k
    case bin
    case break_k
    case caret
    case case_k
    case colon
    case colon_equal
    case colon2
    case comma
    case const_k
    case continue_k
    case cstruct_k
    case cunion_k
    case dec
    case dec_u
    case dot
    case dot2
    case dot_bang
    case dot_dot_equal
    case dot_dot_rangle
    case dot_dot_rangle_equal
    case dot_star
    case dot_question
    case double_ampersand
    case double_left_angle
    case double_right_angle
    case double_star
    case double_vert_bar
    case else_k
    case enum_k
    -- Error token, returned if ignore_errors = true.
    case err
    case error_k
    case equal
    case equal_equal
    case equal_right_angle
    case false_k
    case float
    case for_k
    case fn_k
    case fnsym_k
    case global_k
    case hex
    case ident
    case if_k
    case indent
    case left_angle
    case left_angle_equal
    case left_brace
    case left_bracket
    case left_paren
    case left_right_angle
    case minus
    case minus_right_angle
    case mod_k
    case move_k
    case new_line
    case none_k
    case not_k
    case oct
    case or_k
    case pass_k
    case percent
    case underscore
    case plus
    case pound
    case question
    case raw_string
    case raw_string_multi
    case return_k
    case right_angle
    case right_angle_equal
    case right_brace
    case right_bracket
    case right_paren
    case scope_k
    case semicolon
    case sink_k
    case slash
    case space_left_bracket
    case sq_string
    case sq_string_multi
    case star
    case string
    case string_multi
    case stringt
    case stringt_multi
    case stringt_part
    case stringt_expr
    case struct_k
    case switch_k
    case tilde
    case trait_k
    case true_k
    case try_k
    case type_k
    case undef_k
    case use_k
    case var_k
    case vert_bar
    case void_k
    case while_k
    case with_k
    case yield_k

type Token:
    -- First 8 bits is the TokenType, last 24 bits is the start pos.
    head i32

    -- Can hold the end position or the number of indent spaces.
    data i32

fn Token :: @init(token_t TokenType, start_pos i32, end_pos i32) -> Token:
    return {
        head = (start_pos << 8) || i32(@enumToInt(token_t)),
        data = end_pos,
    }

fn Token :: indent(start_pos i32, indent i32) -> Token:
    return {
        head = (start_pos << 8) || i32(@enumToInt(TokenType.indent)),
        data = indent,
    }

fn (&Token) tag() -> TokenType:
    return @intToEnum(TokenType, int(self.head && 0xff))

fn (&Token) pos() -> i32:
    return self.head >> 8

type StringDelim enum:
    case sq_single
    case sq_triple
    case single
    case triple

type TokenizeState:
    tag TokenizeStateTag

    -- For string interpolation, open braces can accumulate so the end of a template can be determined.
    open_braces byte = 0

    -- For string interpolation.
    string_delim StringDelim = .single

    after_whitespace  bool = false

    has_template_expr bool = false

type TokenizeStateTag enum:
    case whitespace
    case token
    case stringt_part
    case stringt_expr
    case end

-- @export
-- fn tokenize():
--     pass

type ReportFn = fn(ctx Ptr[void], msg str, pos int) -> !void

-- Made generic in case there is a need to use a different src buffer. TODO: substring still needs to be abstracted into user fn.
type Tokenizer:
    src      str
    tokens   []Token
    next_pos int
    keywords Map[str, TokenType]

    -- Whether to parse and accumulate comment tokens in `comments`.
    parse_comments bool
    comments []str

    -- For syntax highlighting, skip errors.
    ignore_errors bool

    has_error bool
    report_fn ReportFn
    ctx       Ptr[void]

fn Tokenizer :: @init(src str) -> Tokenizer:
    keywords := Map[str, TokenType]{
        'and'      = .and_k,
        'as'       = .as_k,
        'begin'    = .begin_k,
        'break'    = .break_k,
        'case'     = .case_k,
        'const'    = .const_k,
        'continue' = .continue_k,
        'cstruct'  = .cstruct_k,
        'cunion'   = .cunion_k,
        'else'     = .else_k,
        'enum'     = .enum_k,
        'error'    = .error_k,
        'false'    = .false_k,
        'for'      = .for_k,
        'fn'       = .fn_k,
        'fnsym'    = .fnsym_k,
        'global'   = .global_k,
        'if'       = .if_k,
        'move'     = .move_k,
        'none'     = .none_k,
        'not'      = .not_k,
        'or'       = .or_k,
        'pass'     = .pass_k,
        'return'   = .return_k,
        'scope'    = .scope_k,
        'sink'     = .sink_k,
        'struct'   = .struct_k,
        'switch'   = .switch_k,
        'trait'    = .trait_k,
        'true'     = .true_k,
        'try'      = .try_k,
        'type'     = .type_k,
        'undef'    = .undef_k,
        'use'      = .use_k,
        'var'      = .var_k,
        'void'     = .void_k,
        'while'    = .while_k,
        'with'     = .with_k,
        'yield'    = .yield_k,
    }
    return {
        src = src,
        tokens = {},
        next_pos = 0,
        comments = {},
        keywords = move keywords,
        parse_comments = false,
        ignore_errors = false,
        has_error = false,
        report_fn = defaultReportFn,
        ctx = none
    }

fn (&Tokenizer) isAtEnd() -> bool:
    return self.src.len() == self.next_pos

fn (&Tokenizer) consume() -> byte:
    ch := self.peek()
    self.advance()
    return ch

fn (&Tokenizer) peek() -> byte:
    return self.src[self.next_pos]

fn (&Tokenizer) peekAhead(steps int) -> ?byte:
    if self.next_pos < self.src.len() - steps:
        return self.src[self.next_pos + steps]
    else: return none

fn (&Tokenizer) advance():
    self.next_pos += 1

fn (&Tokenizer) tokenize() -> !void:
    self.tokens = self.tokens.clear()
    self.next_pos = 0

    if self.src.len() >= 3:
        if self.src[0] == 0xEF and self.src[1] == 0xBB and self.src[2] == 0xBF:
            -- Skip UTF-8 BOM.
            self.next_pos = 3

    if self.src.len() >= self.next_pos + 2:
        if self.src[self.next_pos] == '#' and self.src[self.next_pos+1] == '!':
            -- Ignore shebang line.
            while !self.isAtEnd():
                if self.peek() == '\n':
                    self.advance()
                    break
                self.advance()

    state := TokenizeState{
        tag = .whitespace,
    }
    while:
        switch state.tag:
            case .whitespace:
                -- First parse indent spaces.
                while:
                    if !self.tokenizeIndentOne():
                        state.tag = .token
                        break

            case .token:
                while:
                    state = self.tokenizeOne(state)!
                    if state.tag != .token:
                        break

            case .stringt_part:
                state = self.tokenizeStringOne(self.next_pos, state)!

            case .stringt_expr:
                while:
                    next_state := self.tokenizeOne(state)!
                    if next_state.tag != .token:
                        state = next_state
                        break

            case .end:
                break

-- Consumes the next token skipping whitespace and returns the next tokenizer state.
fn (&Tokenizer) tokenizeOne(state TokenizeState) -> !TokenizeState:
    if self.isAtEnd():
        return {tag=.end}

    start := self.next_pos

    ch := self.consume()
    switch ch:
        case '(':
            self.pushToken(.left_paren, start)

        case ')':
            self.pushToken(.right_paren, start)

        case '{':
            self.pushToken(.left_brace, start)

            if state.tag == .stringt_expr:
                next := state
                next.open_braces += 1
                return next

        case '}':
            self.pushToken(.right_brace, start)
            if state.tag == .stringt_expr:
                next := state
                if state.open_braces == 0:
                    next.tag = .stringt_part
                else:
                    next.open_braces -= 1
                return next

        case '[':
            if state.after_whitespace:
                self.pushToken(.space_left_bracket, start)
            else:
                self.pushToken(.left_bracket, start)

        case ']': self.pushToken(.right_bracket, start)

        case ',': self.pushToken(.comma, start)

        case '.':
            switch self.peek():
                case '!':
                    self.advance()
                    self.pushToken(.dot_bang, start)

                case '.':
                    self.advance()
                    if self.peek() == '>':
                        self.advance()
                        if self.peek() == '=':
                            self.advance()
                            self.pushToken(.dot_dot_rangle_equal, start)
                        else:
                            self.pushToken(.dot_dot_rangle, start)
                    else self.peek() == '=':
                        self.advance()
                        self.pushToken(.dot_dot_equal, start)
                    else:
                        self.pushToken(.dot2, start)

                case '?':
                    self.advance()
                    self.pushToken(.dot_question, start)

                case '*':
                    self.advance()
                    self.pushToken(.dot_star, start)

                else:
                    self.pushToken(.dot, start)

        case ';': self.pushToken(.semicolon, start)

        case ':':
            if self.peek() == '=':
                self.pushToken(.colon_equal, start)
            else self.peek() == ':':
                self.pushToken(.colon2, start)
            else:
                self.pushToken(.colon, start)

        case '@':
            self.consumeIdent()
            self.pushToken(.at_ident, start)

        case '-':
            next := self.peek()
            if next == '-':
                self.advance()
                -- Single line comment. Ignore chars until eol.
                while !self.isAtEnd():
                    if self.peek() == '\n':
                        if self.parse_comments:
                            self.comments = self.comments + self.src[start..self.next_pos]

                        -- Don't consume new line or the current indentation could augment with the next line.
                        return self.tokenizeOne(state)
                    self.advance()

                if self.parse_comments:
                    self.comments = self.comments + self.src[start..self.next_pos]
                return {tag=.end}
            else next == '>':
                self.advance()
                self.pushToken(.minus_right_angle, start)
            else:
                self.pushToken(.minus, start)

        case '%': self.pushToken(.percent, start)

        case '&':
            if self.peek() == '&':
                self.advance()
                self.pushToken(.double_ampersand, start)
            else:
                self.pushToken(.ampersand, start)

        case '|':
            if self.peek() == '|':
                self.advance()
                self.pushToken(.double_vert_bar, start)
            else:
                self.pushToken(.vert_bar, start)

        case '~': self.pushToken(.tilde, start)

        case '+': self.pushToken(.plus, start)

        case '_':
            next := self.peek()
            if (next >= 'A' and next <= 'Z') or (next >= 'a' and next <= 'z') or next == '_':
                self.tokenizeKeywordOrIdent(start)
            else:
                self.pushToken(.underscore, start)

        case '^': self.pushToken(.caret, start)

        case '*':
            if self.peek() == '*':
                self.advance()
                self.pushToken(.double_star, start)
            else:
                self.pushToken(.star, start)

        case '/': self.pushToken(.slash, start)

        case '!':
            if self.peek() == '=':
                self.advance()
                self.pushToken(.bang_equal, start)
            else:
                self.pushToken(.bang, start)

        case '=':
            if !self.isAtEnd():
                switch self.peek():
                    case '=':
                        self.advance()
                        self.pushToken(.equal_equal, start)
                    case '>':
                        self.advance()
                        self.pushToken(.equal_right_angle, start)
                    else:
                        self.pushToken(.equal, start)
            else:
                self.pushToken(.equal, start)

        case '<':
            ch2 := self.peek()
            if ch2 == '=':
                self.pushToken(.left_angle_equal, start)
                self.advance()
            else ch2 == '<':
                self.pushToken(.double_left_angle, start)
                self.advance()
            else ch2 == '>':
                self.pushToken(.left_right_angle, start)
                self.advance()
            else:
                self.pushToken(.left_angle, start)

        case '>':
            ch2 := self.peek()
            if ch2 == '=':
                self.pushToken(.right_angle_equal, start)
                self.advance()
            else ch2 == '>':
                self.pushToken(.double_right_angle, start)
                self.advance()
            else:
                self.pushToken(.right_angle, start)

        case ' ', '\r', '\t':
            -- Consume whitespace.
            while !self.isAtEnd():
                ch2 := self.peek()
                switch ch2:
                    case ' ', '\r', '\t': self.advance()
                    else:
                        next := state
                        next.after_whitespace = true
                        return self.tokenizeOne(next)

            return {tag=.end}

        case '\n':
            self.pushToken(.new_line, start)
            return {tag=.whitespace}

        case '`':
            if self.isNext('`', 0):
                if self.isNext('`', 1):
                    self.advance()
                    self.advance()
                    self.consumeRawStringMulti()!
                    self.pushToken(.raw_string_multi, start)
                else:
                    self.advance()
                    self.pushToken(.raw_string, start)
            else:
                self.consumeRawString()!
                self.pushToken(.raw_string, start)

        case '"':
            if self.peek() == '"':
                if self.peekAhead(1) |ch2|:
                    if ch2 == '"':
                        self.advance()
                        self.advance()
                        return self.tokenizeStringOne(start, {
                            tag = state.tag,
                            string_delim = .triple,
                        })
            return self.tokenizeStringOne(start, {
                tag = state.tag,
                string_delim = .single,
            })

        case '\'':
            if self.peek() == '\'':
                if self.peekAhead(1) |ch2|:
                    if ch2 == '\'':
                        self.advance()
                        self.advance()
                        -- self.tokenizeMultiLineRawString(start)!
                        -- return state
                        return self.tokenizeStringOne(start, {
                            tag = state.tag,
                            string_delim = .sq_triple,
                        })
            -- self.tokenizeSingleLineRawString(start)!
            return self.tokenizeStringOne(start, {
                tag = state.tag,
                string_delim = .sq_single,
            })

        case '#': self.pushToken(.pound, start)

        case '?': self.pushToken(.question, start)

        else:
            switch ch:
                case 'A'..'Z', 'a'..'z':
                    self.tokenizeKeywordOrIdent(start)
                    return {tag=.token}

                case '0'..'9':
                    self.tokenizeNumber(start)!
                    return {tag=.token}

                else:
                    if self.ignore_errors:
                        self.pushToken(.err, start)
                        return {tag=.token}
                    else:
                        return self.reportErrorAt('unknown character: %{ch.fmt(.ch)} (%{ch}) at %{start}', start)
    return {tag=.token}

-- Returns true if an indent or new line token was parsed.
fn (&Tokenizer) tokenizeIndentOne() -> bool:
    if self.isAtEnd():
        return false

    ch := self.peek()
    switch ch:
        case ' ':
            start := self.next_pos
            self.advance()
            count := 1
            while:
                if self.isAtEnd():
                    break

                ch = self.peek()
                if ch == ' ':
                    count += 1
                    self.advance()
                else: break
            self.pushIndentToken(count, start, true)
            return true

        case '\t':
            start := self.next_pos
            self.advance()
            count := 1
            while true:
                if self.isAtEnd():
                    break

                ch = self.peek()
                if ch == '\t':
                    count += 1
                    self.advance()
                else: break
            self.pushIndentToken(count, start, false)
            return true

        case '\n':
            self.pushToken(.new_line, self.next_pos)
            self.advance()
            return true

        else: return false

-- Returns the next tokenizer state.
fn (&Tokenizer) tokenizeStringOne(start int, state TokenizeState) -> !TokenizeState:
    save := self.next_pos
    while:
        if self.isAtEnd():
            if self.ignore_errors:
                self.next_pos = save
                self.pushToken(.err, save)
                return {tag=.token}
            else:
                return self.reportErrorAt('UnterminatedString', save)

        switch self.peek():
            case '"':
                switch state.string_delim:
                    case .single:
                        if state.has_template_expr:
                            self.pushToken(.stringt_part, start)
                            end := self.next_pos
                            self.advance()
                            self.pushToken(.stringt, end)
                        else:
                            self.advance()
                            self.pushToken(.string, start)
                        return {tag=.token}

                    case .triple:
                        ch2 := self.peekAhead(1) ?else 0
                        if ch2 == '"':
                            ch2 = self.peekAhead(2) ?else 0
                            if ch2 == '"':
                                if state.has_template_expr:
                                    self.pushToken(.stringt_part, start)
                                    end := self.next_pos
                                    self.advance()
                                    self.advance()
                                    self.advance()
                                    self.pushToken(.stringt_multi, end)
                                else:
                                    self.advance()
                                    self.advance()
                                    self.advance()
                                    self.pushToken(.string_multi, start)
                                return {tag=.token}
                        self.advance()
                    else:
                        self.advance()

            case '\'':
                switch state.string_delim:
                    case .sq_single:
                        if state.has_template_expr:
                            self.pushToken(.stringt_part, start)
                            end := self.next_pos
                            self.advance()
                            self.pushToken(.stringt, end)
                        else:
                            self.advance()
                            self.pushToken(.sq_string, start)
                        return {tag=.token}

                    case .sq_triple:
                        ch2 := self.peekAhead(1) ?else 0
                        if ch2 == '\'':
                            ch2 = self.peekAhead(2) ?else 0
                            if ch2 == '\'':
                                if state.has_template_expr:
                                    self.pushToken(.stringt_part, start)
                                    end := self.next_pos
                                    self.advance()
                                    self.advance()
                                    self.advance()
                                    self.pushToken(.stringt_multi, end)
                                else:
                                    self.advance()
                                    self.advance()
                                    self.advance()
                                    self.pushToken(.sq_string_multi, start)
                                return {tag=.token}
                        self.advance()
                    else:
                        self.advance()

            case '%':
                ch2 := self.peekAhead(1) ?else 0
                if ch2 == '{':
                    next := state
                    if state.has_template_expr:
                        -- Encounter first expression.
                        switch state.string_delim:
                            case .sq_single
                            case .single: self.pushToken2(.stringt, start, save)
                            case .sq_triple
                            case .triple: self.pushToken2(.stringt_multi, start, save)

                        self.pushToken(.stringt_part, save)
                        next.has_template_expr = true

                    else:
                        self.pushToken(.stringt_part, start)

                    expr_start := self.next_pos
                    self.advance()
                    self.advance()
                    self.pushToken(.stringt_expr, expr_start)
                    next.tag = .stringt_expr
                    next.open_braces = 0
                    return next

                else:
                    self.advance()

            case '\\':
                -- Escape the next character.
                self.advance()
                if self.isAtEnd():
                    if self.ignore_errors:
                        self.next_pos = start
                        self.pushToken(.err, start)
                        return {tag=.token}

                    else:
                        return self.reportErrorAt('UnterminatedString', start)

                self.advance()

            case '\n':
                if state.string_delim == .single or state.string_delim == .sq_single:
                    if self.ignore_errors:
                        self.next_pos = start
                        self.pushToken(.err, start)
                        return {tag=.token}
                    else:
                        return self.reportErrorAt('Encountered new line in single line literal.', self.next_pos)
                self.advance()

            else:
                self.advance()

fn (&Tokenizer) consumeDigits():
    while:
        if self.isAtEnd():
            return

        ch := self.peek()
        if ch >= '0' and ch <= '9':
            self.advance()
            continue
        else: break

-- Assume first character is consumed already.
fn (&Tokenizer) consumeIdent():
    while: 
        if self.isAtEnd():
            return
        ch := self.peek()
        switch ch:
            case '0'..'9', 'A'..'Z', 'a'..'z', '_':
                self.advance()
                continue
            else:
                return

fn (&Tokenizer) consumeRawStringMulti() -> !void:
    while:
        if self.isAtEnd():
            return self.reportError('Expected raw string.')
        switch self.peek():
            case '`':
                self.advance()
                if self.isNext('`', 0):
                    self.advance()
                else:
                    continue

                if self.isNext('`', 0):
                    self.advance()
                    return
            else:
                self.advance()

fn (&Tokenizer) consumeRawString() -> !void:
    while:
        if self.isAtEnd():
            return self.reportError('Expected raw string.')
        switch self.peek():
            case '`':
                self.advance()
                return
            case '\n':
                return self.reportError('Expected raw string on a single line.')
            else:
                self.advance()

fn (&Tokenizer) isNext(ch byte, lookahead int) -> bool:
    if self.next_pos >= self.src.len() - lookahead:
        return false
    return self.src[self.next_pos + lookahead] == ch

fn (&Tokenizer) tokenizeKeywordOrIdent(start int):
    self.consumeIdent()
    if self.keywords.get(self.src[start..self.next_pos]) |token_t|:
        self.pushToken(token_t, start)
    else:
        self.pushToken(.ident, start)

fn (&Tokenizer) tokenizeSingleLineRawString(start int) -> !void:
    save := self.next_pos
    while:
        if self.isAtEnd():
            if self.ignore_errors:
                self.next_pos = save
                self.pushToken(.err, start)
            else: return self.reportErrorAt('UnterminatedString', start)

        if self.peek() == '\'':
            self.advance()
            self.pushToken(.sq_string, start)
            return
        else self.peek() == '\n':
            return self.reportErrorAt('Encountered new line in single line literal.', self.next_pos)
        else:
            self.advance()

fn (&Tokenizer) tokenizeMultiLineRawString(start int) -> !void:
    save := self.next_pos
    while:
        if self.isAtEnd():
            if self.ignore_errors:
                self.next_pos = save
                self.pushToken(.err, start)
            else: return self.reportErrorAt('UnterminatedString', start)

        if self.peek() == '\'':
            ch := self.peekAhead(1) ?else:
                self.advance()
                continue

            ch2 := self.peekAhead(2) ?else:
                self.advance()
                continue

            if ch == '\'' and ch2 == '\'':
                self.advance()
                self.advance()
                self.advance()
                self.pushToken(.sq_string_multi, start)
                return
            else:
                self.advance()
                continue
        else:
            self.advance()

-- Assumes first digit is consumed.
fn (&Tokenizer) tokenizeNumber(start int) -> !void:
    if self.isAtEnd():
        self.pushToken(.dec, start)
        return

    ch := self.peek()
    if (ch >= '0' and ch <= '9') or ch == '.' or ch == 'e' or ch == 'u':
        self.consumeDigits()
        if self.isAtEnd():
            self.pushToken(.dec, start)
            return

        if self.peek() == 'u':
            self.advance()
            self.pushToken(.dec_u, start)
            return

        isFloat := false
        ch = self.peek()
        if ch == '.':
            next := self.peekAhead(1) ?else:
                self.pushToken(.dec, start)
                return

            if next < '0' or next > '9':
                self.pushToken(.dec, start)
                return

            self.advance()
            self.advance()
            self.consumeDigits()
            if self.isAtEnd():
                self.pushToken(.float, start)
                return
            ch = self.peek()
            isFloat = true

        if ch == 'e':
            self.advance()
            if self.isAtEnd():
                return self.reportError('Expected number.')
            ch = self.peek()
            if ch == '-':
                self.advance()
                if self.isAtEnd():
                    return self.reportError('Expected number.')
                ch = self.peek()
            if ch < '0' and ch > '9':
                return self.reportError('Expected number.')

            self.consumeDigits()
            isFloat = true

        if isFloat:
            self.pushToken(.float, start)
        else:
            self.pushToken(.dec, start)
        return

    if self.src[self.next_pos-1] == '0':
        -- Less common integer notation.
        if ch == 'x':
            -- Hex integer.
            self.advance()
            while:
                if self.isAtEnd():
                    break
                ch = self.peek()
                if (ch >= '0' and ch <= '9') or (ch >= 'A' and ch <= 'Z') or (ch >= 'a' and ch <= 'z'):
                    self.advance()
                    continue
                else: break
            self.pushToken(.hex, start)
            return
        else ch == 'o':
            -- Oct integer.
            self.advance()
            while:
                if self.isAtEnd():
                    break

                ch = self.peek()
                if ch >= '0' and ch <= '8':
                    self.advance()
                    continue
                else: break
            self.pushToken(.oct, start)
            return
        else ch == 'b':
            -- Bin integer.
            self.advance()
            while:
                if self.isAtEnd():
                    break

                ch = self.peek()
                if ch == '0' or ch == '1':
                    self.advance()
                    continue
                else: break
            self.pushToken(.bin, start)
            return
        else ch == 'u':
            self.advance()
            self.pushToken(.dec_u, start)
            return
        else:
            if str.is_ascii_alpha(ch):
                return self.reportError('Unsupported integer notation: %{ch.fmt()}')

    -- Push single digit number.
    self.pushToken(.dec, start)
    return

fn (&Tokenizer) pushIndentToken(count int, start_pos int, spaces bool):
    self.tokens = self.tokens + Token.indent(i32(start_pos), if (spaces) i32(count) else i32(count) || 0x80000000)

fn (&Tokenizer) pushToken(token_t TokenType, start_pos int):
    self.tokens = self.tokens + Token(token_t, i32(start_pos), i32(self.next_pos))

fn (&Tokenizer) pushToken2(token_t TokenType, start_pos int, end_pos int):
    self.tokens = self.tokens + Token(token_t, i32(start_pos), i32(end_pos))

fn (&Tokenizer) reportError(msg str) -> error:
    return self.reportErrorAt(msg, self.next_pos)

fn (&Tokenizer) reportErrorAt(msg str, pos int) -> error:
    self.has_error = true
    self.report_fn(self.ctx, msg, pos)!
    return error.TokenError

fn defaultReportFn(ctx Ptr[void], msg str, pos int) -> !void:
    pass

--cytest: pass