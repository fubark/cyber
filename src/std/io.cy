--| SIMD accelerated.
#[bind] fn indexOfNewLine(buf [&]byte) -> ?int

type Reader trait:
    fn read(bytes [&]byte) -> !int

fn Reader :: @init(s str) -> StrReader:
    return {
        val = s
    }

type Scanner[T Reader]:
    reader &T
    buf []byte
    cur int = 0
    end int = 0

const Scanner :: DefaultBufferSize = 8192

fn Scanner :: @init(scope reader &%T) -> scope Scanner[T]:
    return Scanner(reader, Scanner.DefaultBufferSize)

fn Scanner :: @init(scope reader &%T, buf_size int) -> scope Scanner[T]:
    return {
        reader = reader,
        buf = []byte(buf_size, 0),
        cur = buf_size,
        end = buf_size,
    }

fn (&Scanner[]) read_full() -> !void:
    -- Read until buffer is filled or end of file.
    while self.end < self.buf.len():
        self.end += self.reader.read(self.buf.span()[self.end..]) !else |err|:
            if err == error.EndOfFile:
                break
            return err

type ScannerNextFn = fn([&]byte) -> !?NextRange

type NextRange:
    start int
    end   int
    advance_end int

fn (&Scanner[]) next(next_fn ScannerNextFn) -> !?[]byte:
    if self.cur == self.end and self.end < self.buf.len():
        -- EOF
        return none

    if self.cur == self.end:
        self.cur = 0
        self.end = 0
        self.read_full()!

    if next_fn(self.buf.span()[self.cur..self.end])! |range|:
        slice := self.buf[self.cur+range.start..self.cur+range.end]
        self.cur += range.advance_end
        return slice

    -- Start building the result.
    res := self.buf[self.cur..self.end]
    self.cur = self.end

    while true:
        if self.cur == self.end and self.end < self.buf.len():
            -- EOF
            return none

        if self.cur == self.end:
            self.cur = 0
            self.end = 0
            self.read_full()!

        if next_fn(self.buf.span()[self.cur..self.end])! |range|:
            slice := self.buf[self.cur+range.start..self.cur+range.end]
            self.cur += range.advance_end
            return res + slice

        -- Save partial.
        res += self.buf[self.cur..self.end]
        self.cur = self.end
    return none

--| Returns the next line that ends with `\n`, `\r`, `\r\n`, or until the EOF.
--| The line returned does not include the new line character(s).
fn (&Scanner[]) next_line() -> !?[]byte:
    next := fn(span [&]byte) -> !?NextRange:
        idx := indexOfNewLine(span) ?else: return none
        advance_end := idx
        if span[advance_end] == '\r':
            advance_end += 1
        if span[advance_end] == '\n':
            advance_end += 1
        return NextRange{start=0, end=idx, advance_end=advance_end}
    return self.next(next)

type Writer trait:
    fn write(bytes [&]byte) -> !int

-- TODO: Wrapper for AsSpan types.
type SpanReader

type StrReader:
    with Reader
    val str
    idx int = 0

fn (&StrReader) read(buf [&]byte) -> !int:
    vlen := self.val.len()
    if self.idx == vlen:
        return error.EndOfFile

    rest := as[[&]byte] self.val.as_ptr_span()[self.idx..]
    if rest.length < buf.length:
        buf[0..rest.length].set(rest)
        self.idx = vlen
        return rest.length
    else:
        buf.set(rest[0..buf.len()])
        self.idx += buf.len()
        return buf.len()
