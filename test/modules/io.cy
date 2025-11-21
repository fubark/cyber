use test
use io
use os

-- Scanner.nextLine
var file = os.openFile('test/assets/multiline.txt', .read)!
var scanner = Scanner(file)
var lines = []str{}
for scanner.nextLine() -> line:
    lines.append(line)
test.eq(3, lines.len())
test.eq("foo\n", lines[0])
test.eq("abcxyz\n", lines[1])
test.eq('bar', lines[2])

-- Scanner.nextLine with small buffer.
file = os.openFile('test/assets/multiline.txt', .read)!
scanner = Scanner(file, 2)
lines = []str{}
for scanner.nextLine() -> line:
    lines.append(line)
test.eq(3, lines.len())
test.eq("foo\n", lines[0])
test.eq("abcxyz\n", lines[1])
test.eq('bar', lines[2])