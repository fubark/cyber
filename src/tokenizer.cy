src := ```
use math

nouns := []str{'World', '世界', 'दुनिया', 'mundo'}
nouns <<= math.random().fmt()
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
    case dollar
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
    return @intToEnum(TokenType, int($head && 0xff))

fn (&Token) pos() -> i32:
    return $head >> 8

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
    return $src.len() == $next_pos

fn (&Tokenizer) consume() -> byte:
    ch := $peek()
    $advance()
    return ch

fn (&Tokenizer) peek() -> byte:
    return $src[$next_pos]

fn (&Tokenizer) peekAhead(steps int) -> ?byte:
    if $next_pos < $src.len() - steps:
        return $src[$next_pos + steps]
    else: return none

fn (&Tokenizer) advance():
    $next_pos += 1

fn (&Tokenizer) tokenize() -> !void:
    $tokens = $tokens.clear()
    $next_pos = 0

    if $src.len() >= 3:
        if $src[0] == 0xEF and $src[1] == 0xBB and $src[2] == 0xBF:
            -- Skip UTF-8 BOM.
            $next_pos = 3

    if $src.len() >= $next_pos + 2:
        if $src[$next_pos] == '#' and $src[$next_pos+1] == '!':
            -- Ignore shebang line.
            while !$isAtEnd():
                if $peek() == '\n':
                    $advance()
                    break
                $advance()

    state := TokenizeState{
        tag = .whitespace,
    }
    while:
        switch state.tag:
            case .whitespace:
                -- First parse indent spaces.
                while:
                    if !$tokenizeIndentOne():
                        state.tag = .token
                        break

            case .token:
                while:
                    state = $tokenizeOne(state)!
                    if state.tag != .token:
                        break

            case .stringt_part:
                state = $tokenizeStringOne($next_pos, state)!

            case .stringt_expr:
                while:
                    next_state := $tokenizeOne(state)!
                    if next_state.tag != .token:
                        state = next_state
                        break

            case .end:
                break

-- Consumes the next token skipping whitespace and returns the next tokenizer state.
fn (&Tokenizer) tokenizeOne(state TokenizeState) -> !TokenizeState:
    if $isAtEnd():
        return {tag=.end}

    start := $next_pos

    ch := $consume()
    switch ch:
        case '(':
            $pushToken(.left_paren, start)

        case ')':
            $pushToken(.right_paren, start)

        case '{':
            $pushToken(.left_brace, start)

            if state.tag == .stringt_expr:
                next := state
                next.open_braces += 1
                return next

        case '}':
            $pushToken(.right_brace, start)
            if state.tag == .stringt_expr:
                next := state
                if state.open_braces == 0:
                    next.tag = .stringt_part
                else:
                    next.open_braces -= 1
                return next

        case '[':
            if state.after_whitespace:
                $pushToken(.space_left_bracket, start)
            else:
                $pushToken(.left_bracket, start)

        case ']': $pushToken(.right_bracket, start)

        case ',': $pushToken(.comma, start)

        case '.':
            switch $peek():
                case '!':
                    $advance()
                    $pushToken(.dot_bang, start)

                case '.':
                    $advance()
                    if $peek() == '>':
                        $advance()
                        if $peek() == '=':
                            $advance()
                            $pushToken(.dot_dot_rangle_equal, start)
                        else:
                            $pushToken(.dot_dot_rangle, start)
                    else $peek() == '=':
                        $advance()
                        $pushToken(.dot_dot_equal, start)
                    else:
                        $pushToken(.dot2, start)

                case '?':
                    $advance()
                    $pushToken(.dot_question, start)

                case '*':
                    $advance()
                    $pushToken(.dot_star, start)

                else:
                    $pushToken(.dot, start)

        case ';': $pushToken(.semicolon, start)

        case ':':
            if $peek() == '=':
                $pushToken(.colon_equal, start)
            else $peek() == ':':
                $pushToken(.colon2, start)
            else:
                $pushToken(.colon, start)

        case '@':
            $consumeIdent()
            $pushToken(.at_ident, start)

        case '$': $pushToken(.dollar, start)

        case '-':
            next := $peek()
            if next == '-':
                $advance()
                -- Single line comment. Ignore chars until eol.
                while !$isAtEnd():
                    if $peek() == '\n':
                        if $parse_comments:
                            $comments = $comments << $src[start..$next_pos]

                        -- Don't consume new line or the current indentation could augment with the next line.
                        return $tokenizeOne(state)
                    $advance()

                if $parse_comments:
                    $comments = $comments << $src[start..$next_pos]
                return {tag=.end}
            else next == '>':
                $advance()
                $pushToken(.minus_right_angle, start)
            else:
                $pushToken(.minus, start)

        case '%': $pushToken(.percent, start)

        case '&':
            if $peek() == '&':
                $advance()
                $pushToken(.double_ampersand, start)
            else:
                $pushToken(.ampersand, start)

        case '|':
            if $peek() == '|':
                $advance()
                $pushToken(.double_vert_bar, start)
            else:
                $pushToken(.vert_bar, start)

        case '~': $pushToken(.tilde, start)

        case '+': $pushToken(.plus, start)

        case '_':
            next := $peek()
            if (next >= 'A' and next <= 'Z') or (next >= 'a' and next <= 'z') or next == '_':
                $tokenizeKeywordOrIdent(start)
            else:
                $pushToken(.underscore, start)

        case '^': $pushToken(.caret, start)

        case '*':
            if $peek() == '*':
                $advance()
                $pushToken(.double_star, start)
            else:
                $pushToken(.star, start)

        case '/': $pushToken(.slash, start)

        case '!':
            if $peek() == '=':
                $advance()
                $pushToken(.bang_equal, start)
            else:
                $pushToken(.bang, start)

        case '=':
            if !$isAtEnd():
                switch $peek():
                    case '=':
                        $advance()
                        $pushToken(.equal_equal, start)
                    case '>':
                        $advance()
                        $pushToken(.equal_right_angle, start)
                    else:
                        $pushToken(.equal, start)
            else:
                $pushToken(.equal, start)

        case '<':
            ch2 := $peek()
            if ch2 == '=':
                $pushToken(.left_angle_equal, start)
                $advance()
            else ch2 == '<':
                $pushToken(.double_left_angle, start)
                $advance()
            else ch2 == '>':
                $pushToken(.left_right_angle, start)
                $advance()
            else:
                $pushToken(.left_angle, start)

        case '>':
            ch2 := $peek()
            if ch2 == '=':
                $pushToken(.right_angle_equal, start)
                $advance()
            else ch2 == '>':
                $pushToken(.double_right_angle, start)
                $advance()
            else:
                $pushToken(.right_angle, start)

        case ' ', '\r', '\t':
            -- Consume whitespace.
            while !$isAtEnd():
                ch2 := $peek()
                switch ch2:
                    case ' ', '\r', '\t': $advance()
                    else:
                        next := state
                        next.after_whitespace = true
                        return $tokenizeOne(next)

            return {tag=.end}

        case '\n':
            $pushToken(.new_line, start)
            return {tag=.whitespace}

        case '`':
            if $isNext('`', 0):
                if $isNext('`', 1):
                    $advance()
                    $advance()
                    $consumeRawStringMulti()!
                    $pushToken(.raw_string_multi, start)
                else:
                    $advance()
                    $pushToken(.raw_string, start)
            else:
                $consumeRawString()!
                $pushToken(.raw_string, start)

        case '"':
            if $peek() == '"':
                if $peekAhead(1) |ch2|:
                    if ch2 == '"':
                        $advance()
                        $advance()
                        return $tokenizeStringOne(start, {
                            tag = state.tag,
                            string_delim = .triple,
                        })
            return $tokenizeStringOne(start, {
                tag = state.tag,
                string_delim = .single,
            })

        case '\'':
            if $peek() == '\'':
                if $peekAhead(1) |ch2|:
                    if ch2 == '\'':
                        $advance()
                        $advance()
                        -- $tokenizeMultiLineRawString(start)!
                        -- return state
                        return $tokenizeStringOne(start, {
                            tag = state.tag,
                            string_delim = .sq_triple,
                        })
            -- $tokenizeSingleLineRawString(start)!
            return $tokenizeStringOne(start, {
                tag = state.tag,
                string_delim = .sq_single,
            })

        case '#': $pushToken(.pound, start)

        case '?': $pushToken(.question, start)

        else:
            switch ch:
                case 'A'..'Z', 'a'..'z':
                    $tokenizeKeywordOrIdent(start)
                    return {tag=.token}

                case '0'..'9':
                    $tokenizeNumber(start)!
                    return {tag=.token}

                else:
                    if $ignore_errors:
                        $pushToken(.err, start)
                        return {tag=.token}
                    else:
                        return $reportErrorAt('unknown character: %{ch.fmt(.ch)} (%{ch}) at %{start}', start)
    return {tag=.token}

-- Returns true if an indent or new line token was parsed.
fn (&Tokenizer) tokenizeIndentOne() -> bool:
    if $isAtEnd():
        return false

    ch := $peek()
    switch ch:
        case ' ':
            start := $next_pos
            $advance()
            count := 1
            while:
                if $isAtEnd():
                    break

                ch = $peek()
                if ch == ' ':
                    count += 1
                    $advance()
                else: break
            $pushIndentToken(count, start, true)
            return true

        case '\t':
            start := $next_pos
            $advance()
            count := 1
            while true:
                if $isAtEnd():
                    break

                ch = $peek()
                if ch == '\t':
                    count += 1
                    $advance()
                else: break
            $pushIndentToken(count, start, false)
            return true

        case '\n':
            $pushToken(.new_line, $next_pos)
            $advance()
            return true

        else: return false

-- Returns the next tokenizer state.
fn (&Tokenizer) tokenizeStringOne(start int, state TokenizeState) -> !TokenizeState:
    save := $next_pos
    while:
        if $isAtEnd():
            if $ignore_errors:
                $next_pos = save
                $pushToken(.err, save)
                return {tag=.token}
            else:
                return $reportErrorAt('UnterminatedString', save)

        switch $peek():
            case '"':
                switch state.string_delim:
                    case .single:
                        if state.has_template_expr:
                            $pushToken(.stringt_part, start)
                            end := $next_pos
                            $advance()
                            $pushToken(.stringt, end)
                        else:
                            $advance()
                            $pushToken(.string, start)
                        return {tag=.token}

                    case .triple:
                        ch2 := $peekAhead(1) ?else 0
                        if ch2 == '"':
                            ch2 = $peekAhead(2) ?else 0
                            if ch2 == '"':
                                if state.has_template_expr:
                                    $pushToken(.stringt_part, start)
                                    end := $next_pos
                                    $advance()
                                    $advance()
                                    $advance()
                                    $pushToken(.stringt_multi, end)
                                else:
                                    $advance()
                                    $advance()
                                    $advance()
                                    $pushToken(.string_multi, start)
                                return {tag=.token}
                        $advance()
                    else:
                        $advance()

            case '\'':
                switch state.string_delim:
                    case .sq_single:
                        if state.has_template_expr:
                            $pushToken(.stringt_part, start)
                            end := $next_pos
                            $advance()
                            $pushToken(.stringt, end)
                        else:
                            $advance()
                            $pushToken(.sq_string, start)
                        return {tag=.token}

                    case .sq_triple:
                        ch2 := $peekAhead(1) ?else 0
                        if ch2 == '\'':
                            ch2 = $peekAhead(2) ?else 0
                            if ch2 == '\'':
                                if state.has_template_expr:
                                    $pushToken(.stringt_part, start)
                                    end := $next_pos
                                    $advance()
                                    $advance()
                                    $advance()
                                    $pushToken(.stringt_multi, end)
                                else:
                                    $advance()
                                    $advance()
                                    $advance()
                                    $pushToken(.sq_string_multi, start)
                                return {tag=.token}
                        $advance()
                    else:
                        $advance()

            case '%':
                ch2 := $peekAhead(1) ?else 0
                if ch2 == '{':
                    next := state
                    if state.has_template_expr:
                        -- Encounter first expression.
                        switch state.string_delim:
                            case .sq_single
                            case .single: $pushToken2(.stringt, start, save)
                            case .sq_triple
                            case .triple: $pushToken2(.stringt_multi, start, save)

                        $pushToken(.stringt_part, save)
                        next.has_template_expr = true

                    else:
                        $pushToken(.stringt_part, start)

                    expr_start := $next_pos
                    $advance()
                    $advance()
                    $pushToken(.stringt_expr, expr_start)
                    next.tag = .stringt_expr
                    next.open_braces = 0
                    return next

                else:
                    $advance()

            case '\\':
                -- Escape the next character.
                $advance()
                if $isAtEnd():
                    if $ignore_errors:
                        $next_pos = start
                        $pushToken(.err, start)
                        return {tag=.token}

                    else:
                        return $reportErrorAt('UnterminatedString', start)

                $advance()

            case '\n':
                if state.string_delim == .single or state.string_delim == .sq_single:
                    if $ignore_errors:
                        $next_pos = start
                        $pushToken(.err, start)
                        return {tag=.token}
                    else:
                        return $reportErrorAt('Encountered new line in single line literal.', $next_pos)
                $advance()

            else:
                $advance()

fn (&Tokenizer) consumeDigits():
    while:
        if $isAtEnd():
            return

        ch := $peek()
        if ch >= '0' and ch <= '9':
            $advance()
            continue
        else: break

-- Assume first character is consumed already.
fn (&Tokenizer) consumeIdent():
    while: 
        if $isAtEnd():
            return
        ch := $peek()
        switch ch:
            case '0'..'9', 'A'..'Z', 'a'..'z', '_':
                $advance()
                continue
            else:
                return

fn (&Tokenizer) consumeRawStringMulti() -> !void:
    while:
        if $isAtEnd():
            return $reportError('Expected raw string.')
        switch $peek():
            case '`':
                $advance()
                if $isNext('`', 0):
                    $advance()
                else:
                    continue

                if $isNext('`', 0):
                    $advance()
                    return
            else:
                $advance()

fn (&Tokenizer) consumeRawString() -> !void:
    while:
        if $isAtEnd():
            return $reportError('Expected raw string.')
        switch $peek():
            case '`':
                $advance()
                return
            case '\n':
                return $reportError('Expected raw string on a single line.')
            else:
                $advance()

fn (&Tokenizer) isNext(ch byte, lookahead int) -> bool:
    if $next_pos >= $src.len() - lookahead:
        return false
    return $src[$next_pos + lookahead] == ch

fn (&Tokenizer) tokenizeKeywordOrIdent(start int):
    $consumeIdent()
    if $keywords.get($src[start..$next_pos]) |token_t|:
        $pushToken(token_t, start)
    else:
        $pushToken(.ident, start)

fn (&Tokenizer) tokenizeSingleLineRawString(start int) -> !void:
    save := $next_pos
    while:
        if $isAtEnd():
            if $ignore_errors:
                $next_pos = save
                $pushToken(.err, start)
            else: return $reportErrorAt('UnterminatedString', start)

        if $peek() == '\'':
            $advance()
            $pushToken(.sq_string, start)
            return
        else $peek() == '\n':
            return $reportErrorAt('Encountered new line in single line literal.', $next_pos)
        else:
            $advance()

fn (&Tokenizer) tokenizeMultiLineRawString(start int) -> !void:
    save := $next_pos
    while:
        if $isAtEnd():
            if $ignore_errors:
                $next_pos = save
                $pushToken(.err, start)
            else: return $reportErrorAt('UnterminatedString', start)

        if $peek() == '\'':
            ch := $peekAhead(1) ?else:
                $advance()
                continue

            ch2 := $peekAhead(2) ?else:
                $advance()
                continue

            if ch == '\'' and ch2 == '\'':
                $advance()
                $advance()
                $advance()
                $pushToken(.sq_string_multi, start)
                return
            else:
                $advance()
                continue
        else:
            $advance()

-- Assumes first digit is consumed.
fn (&Tokenizer) tokenizeNumber(start int) -> !void:
    if $isAtEnd():
        $pushToken(.dec, start)
        return

    ch := $peek()
    if (ch >= '0' and ch <= '9') or ch == '.' or ch == 'e' or ch == 'u':
        $consumeDigits()
        if $isAtEnd():
            $pushToken(.dec, start)
            return

        if $peek() == 'u':
            $advance()
            $pushToken(.dec_u, start)
            return

        isFloat := false
        ch = $peek()
        if ch == '.':
            next := $peekAhead(1) ?else:
                $pushToken(.dec, start)
                return

            if next < '0' or next > '9':
                $pushToken(.dec, start)
                return

            $advance()
            $advance()
            $consumeDigits()
            if $isAtEnd():
                $pushToken(.float, start)
                return
            ch = $peek()
            isFloat = true

        if ch == 'e':
            $advance()
            if $isAtEnd():
                return $reportError('Expected number.')
            ch = $peek()
            if ch == '-':
                $advance()
                if $isAtEnd():
                    return $reportError('Expected number.')
                ch = $peek()
            if ch < '0' and ch > '9':
                return $reportError('Expected number.')

            $consumeDigits()
            isFloat = true

        if isFloat:
            $pushToken(.float, start)
        else:
            $pushToken(.dec, start)
        return

    if $src[$next_pos-1] == '0':
        -- Less common integer notation.
        if ch == 'x':
            -- Hex integer.
            $advance()
            while:
                if $isAtEnd():
                    break
                ch = $peek()
                if (ch >= '0' and ch <= '9') or (ch >= 'A' and ch <= 'Z') or (ch >= 'a' and ch <= 'z'):
                    $advance()
                    continue
                else: break
            $pushToken(.hex, start)
            return
        else ch == 'o':
            -- Oct integer.
            $advance()
            while:
                if $isAtEnd():
                    break

                ch = $peek()
                if ch >= '0' and ch <= '8':
                    $advance()
                    continue
                else: break
            $pushToken(.oct, start)
            return
        else ch == 'b':
            -- Bin integer.
            $advance()
            while:
                if $isAtEnd():
                    break

                ch = $peek()
                if ch == '0' or ch == '1':
                    $advance()
                    continue
                else: break
            $pushToken(.bin, start)
            return
        else ch == 'u':
            $advance()
            $pushToken(.dec_u, start)
            return
        else:
            if str.is_ascii_alpha(ch):
                return $reportError('Unsupported integer notation: %{ch.fmt()}')

    -- Push single digit number.
    $pushToken(.dec, start)
    return

fn (&Tokenizer) pushIndentToken(count int, start_pos int, spaces bool):
    $tokens = $tokens << Token.indent(i32(start_pos), if (spaces) i32(count) else i32(count) || 0x80000000)

fn (&Tokenizer) pushToken(token_t TokenType, start_pos int):
    $tokens = $tokens << Token(token_t, i32(start_pos), i32($next_pos))

fn (&Tokenizer) pushToken2(token_t TokenType, start_pos int, end_pos int):
    $tokens = $tokens << Token(token_t, i32(start_pos), i32(end_pos))

fn (&Tokenizer) reportError(msg str) -> error:
    return $reportErrorAt(msg, $next_pos)

fn (&Tokenizer) reportErrorAt(msg str, pos int) -> error:
    $has_error = true
    $report_fn($ctx, msg, pos)!
    return error.TokenError

fn defaultReportFn(ctx Ptr[void], msg str, pos int) -> !void:
    pass

--cytest: pass