use os
use t 'test'

if os.system != 'windows':
    os.setEnv('testfoo', 'testbar')
    t.eq(os.getEnv('testfoo').?, 'testbar')
    os.unsetEnv('testfoo')
    t.assert(os.getEnv('testfoo') == none)

-- access()
try:
    os.access('test/assets/missing.txt', .read)
catch err:
    t.eq(err, error.FileNotFound)
os.access('test/assets/file.txt', .read)

-- args()
-- Not available from static linking.
dyn res = os.args()
t.eq(res.len() > 0, true)

-- createDir()
try:
    os.removeDir('test/assets/tempdir')
catch:
    pass
os.createDir('test/assets/tempdir')
var dir = os.openDir('test/assets/tempdir')
t.eq(dir.stat()['type'], symbol.dir)

-- createFile() new file.
try:
    os.removeFile('test/assets/write.txt')
catch:
    pass
var file = os.createFile('test/assets/write.txt', false)
file.write('foobar')
t.eq(os.readFile('test/assets/write.txt'), 'foobar')

-- createFile() no truncate.
file = os.createFile('test/assets/write.txt', false)
t.eq(os.readFile('test/assets/write.txt'), 'foobar')

-- createFile() truncate.
file = os.createFile('test/assets/write.txt', true)
t.eq(os.readFile('test/assets/write.txt'), '')

-- dirName()
t.assert(os.dirName('.') == none)
t.eq(os.dirName('./foo').?, '.')
t.eq(os.dirName('./foo/bar.txt').?, './foo')
t.eq(os.dirName('./foo/bar').?, './foo')
t.eq(os.dirName('/root').?, '/')
t.eq(os.dirName('/root/bar').?, '/root')
t.eq(os.dirName('/root/bar.txt').?, '/root')

-- openDir()
dir = os.openDir('test')
dyn info = dir.stat()
t.eq(info['type'], symbol.dir)

-- openFile()
file = os.openFile('test/assets/file.txt', .read)
info = file.stat()
t.eq(info['type'], symbol.file)

-- free() / malloc()
var ptr = os.malloc(16)
t.eq(type(ptr), *void)
os.free(ptr)

-- File.read()
file = os.openFile('test/assets/file.txt', .read)
t.eq(file.read(3), 'foo')
t.eq(file.read(10), 'bar')
t.eq(file.read(10), '')
t.eq(file.read(10), '')
t.eq(try file.read(-1), error.InvalidArgument)
t.eq(try file.read(0), error.InvalidArgument)

-- File.readAll()
file = os.openFile('test/assets/file.txt', .read)
t.eq(file.readAll(), 'foobar')
t.eq(file.readAll(), '')

-- File.close()
file = os.openFile('test/assets/file.txt', .read)
file.close()
res = try file.stat()
t.eq(res, error.Closed)

-- File.seek()
file = os.openFile('test/assets/file.txt', .read)
try:
    file.seek(-1)
catch err:
    t.eq(err, error.InvalidArgument)
file.seek(3)
t.eq(file.read(3), 'bar')

-- File.seekFromCur()
file = os.openFile('test/assets/file.txt', .read)
file.seekFromCur(3)
t.eq(file.read(3), 'bar')
file.seekFromCur(-6)
t.eq(file.read(3), 'foo')

-- File.seekFromEnd()
file = os.openFile('test/assets/file.txt', .read)
file.seekFromEnd(-3)
t.eq(file.read(3), 'bar')
try:
    file.seekFromEnd(1)
catch err:
    t.eq(err, error.InvalidArgument)
file.seekFromEnd(-6)
t.eq(file.read(3), 'foo')

-- File.streamLines()
file = os.openFile('test/assets/multiline.txt', .read)
var lines = List[String]{}
for file.streamLines() -> line:
    lines.append(line)
t.eq(lines.len(), 3)
t.eq(lines[0], "foo\n")
t.eq(lines[1], "abcxyz\n")
t.eq(lines[2], 'bar')

-- File.streamLines2()
file = os.openFile('test/assets/multiline.txt', .read)
lines = List[String]{}
for file.streamLines(2) -> line:
    lines.append(line)
t.eq(lines.len(), 3)
t.eq(lines[0], "foo\n")
t.eq(lines[1], "abcxyz\n")
t.eq(lines[2], 'bar')

-- File.write() from create
file = os.createFile('test/assets/write.txt', true)
t.eq(file.write('foobar'), 6)
t.eq(os.readFile('test/assets/write.txt'), 'foobar')

-- File.write() from open
file = os.openFile('test/assets/write.txt', .write)
file.seekFromEnd(0)
t.eq(file.write('abcxyz'), 6)
t.eq(os.readFile('test/assets/write.txt'), 'foobarabcxyz')

-- Dir.iterator()
dir = os.openDir('test/assets/dir', true)
var iter = dir.iterator()
var entries = List[dyn]{}
while iter.next() -> n:
    entries.append(n)
t.eq(entries.len(), 3)
entries.sort((a, b) => a['name'].less(b['name']))
t.eq(entries[0]['name'], 'dir2')
t.eq(entries[0]['type'], symbol.dir)
t.eq(entries[1]['name'], 'file.txt')
t.eq(entries[1]['type'], symbol.file)
t.eq(entries[2]['name'], 'file2.txt')
t.eq(entries[2]['type'], symbol.file)

-- Dir.walk()
dir = os.openDir('test/assets/dir', true)
iter = dir.walk()
entries = .{}
while iter.next() -> n:
    entries.append(n)
t.eq(entries.len(), 4)
entries.sort((a, b) => a['path'].less(b['path']))
t.eq(entries[0]['path'], 'dir2')
if os.system == 'windows':
    t.eq(entries[1]['path'], 'dir2\file.txt')
else:
    t.eq(entries[1]['path'], 'dir2/file.txt')
t.eq(entries[2]['path'], 'file.txt')
t.eq(entries[3]['path'], 'file2.txt')

-- writeFile()
if os.cpu != 'wasm32':
    var s = ''.insertByte(0, 255)
    os.writeFile('test.txt', s)
    var file = os.openFile('test.txt', .read)
    var bytes = file.readAll()
    t.eq(bytes.len(), 1)
    t.eq(bytes.getByte(0), 255)

--cytest: pass