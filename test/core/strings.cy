use t 'test'

-- TODO: All string tests will be separated to strings_ascii and strings_utf8.

var escaped = 'Return the underlying `symbol`.'.replace('`', '\\`')
t.eq(escaped, 'Return the underlying \\`symbol\\`.')

-- Single quotes does not interpret escape characters.
var a = '\n'
t.eq(a.len(), 2)
a[0] == `\\`
a[1] == `n`

-- Indexing an invalid utf8.
var invalid = "\xc3\x28"
t.eq(invalid.len(), 2)
t.eq(invalid.count(), 1)
t.eq(invalid[0], 0xFFFD)
-- var iter = invalid.iterator()
-- t.eq(iter.next(), 0xFFFD)

-- -- decode()
-- t.eq(arr.decode(), 'abc🦊xyz🐶')
-- t.eq(arr.decode().isAscii(), false)
-- t.eq(Array('abc').decode(), 'abc')
-- t.eq(Array('abc').decode().isAscii(), true)
-- t.eq(try Array('').insertByte(0, 255).decode(), error.Unicode)

-- fmt()
t.eq('@'.fmt(.{1}), '1')
t.eq('@, @'.fmt(.{1, 2}), '1, 2')
t.eq('$PH'.fmt('$PH', .{1}), '1')
t.eq('$PH, $PH'.fmt('$PH', .{1, 2}), '1, 2')

-- fmtBytes()
t.eq('z'.fmtBytes(.bin), '01111010')
t.eq('z'.fmtBytes(.oct), '172')
t.eq('z'.fmtBytes(.dec), '122')
t.eq('z'.fmtBytes(.hex), '7a')
var str = 'abc🦊xyz🐶'
t.eq(str.fmtBytes(.bin), '0110000101100010011000111111000010011111101001101000101001111000011110010111101011110000100111111001000010110110')
t.eq(str.fmtBytes(.oct), '141142143360237246212170171172360237220266')
t.eq(str.fmtBytes(.dec), '097098099240159166138120121122240159144182')
t.eq(str.fmtBytes(.hex), '616263f09fa68a78797af09f90b6')

-- getByte()
t.eq(str.getByte(0), 97)
t.eq(str.getByte(3), 240)
t.eq(str.getByte(4), 159)
t.eq(str.getByte(10), 240)
t.eq(str.getByte(13), 182)
t.eq(try str.getByte(-1), error.OutOfBounds)
t.eq(try str.getByte(14), error.OutOfBounds)

-- getInt()
var iarr = ''
iarr = iarr.insertByte(0, 0x5a)
iarr = iarr.insertByte(1, 0xf1)
iarr = iarr.insertByte(2, 0x06)
iarr = iarr.insertByte(3, 0x04)
iarr = iarr.insertByte(4, 0x5e)
iarr = iarr.insertByte(5, 0x23)
iarr = iarr.insertByte(6, 0x8d)
iarr = iarr.insertByte(7, 0xd2)
t.eq(iarr.getInt(0, .big), 6553025548629806546)
t.eq(iarr.getInt(0, .little), -3274922467327020710)

-- getInt32()
iarr = ''
iarr = iarr.insertByte(0, 0x49)
iarr = iarr.insertByte(1, 0x96)
iarr = iarr.insertByte(2, 0x02)
iarr = iarr.insertByte(3, 0xD2)
t.eq(iarr.getInt32(0, .big), 1234567890)
t.eq(iarr.getInt32(0, .little), 3523384905)

-- insertByte()
t.eq(str.insertByte(2, 97), 'abac🦊xyz🐶')

--cytest: pass