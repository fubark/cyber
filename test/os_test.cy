import os 'os'
import t 'test'

if os.system != 'windows':
    os.setEnv('testfoo', 'testbar')
    t.eq(os.getEnv('testfoo'), 'testbar')
    os.unsetEnv('testfoo')
    t.eq(os.getEnv('testfoo'), none)

-- access()
var res = try os.access('test/assets/missing.txt', .read)
t.eq(res, error.FileNotFound)
res = os.access('test/assets/file.txt', .read)
t.eq(res, true)

-- args()
res = os.args()
t.eq(res.len() > 0, true)

-- createDir()
try os.removeDir('test/assets/tempdir')
t.eq(os.createDir('test/assets/tempdir'), true)
var dir = os.openDir('test/assets/tempdir')
t.eq(dir.stat().type, .dir)

-- createFile() new file.
try os.removeFile('test/assets/write.txt')
var file = os.createFile('test/assets/write.txt', false)
file.write('foobar')
t.eq(os.readFile('test/assets/write.txt'), rawstring('foobar'))
-- createFile() no truncate.
file = os.createFile('test/assets/write.txt', false)
t.eq(os.readFile('test/assets/write.txt'), rawstring('foobar'))
-- createFile() truncate.
file = os.createFile('test/assets/write.txt', true)
t.eq(os.readFile('test/assets/write.txt'), rawstring(''))

-- dirName()
t.eq(os.dirName('.'), none)
t.eq(os.dirName('./foo'), '.')
t.eq(os.dirName('./foo/bar.txt'), './foo')
t.eq(os.dirName('./foo/bar'), './foo')
t.eq(os.dirName('/root'), '/')
t.eq(os.dirName('/root/bar'), '/root')
t.eq(os.dirName('/root/bar.txt'), '/root')

-- openDir()
dir = os.openDir('test')
var info = dir.stat()
t.eq(info.type, .dir)

-- openFile()
file = os.openFile('test/assets/file.txt', .read)
info = file.stat()
t.eq(info.type, .file)

-- free() / malloc()
var ptr = os.malloc(16)
t.eq(typesym(ptr), .pointer)
try os.free(ptr)

-- File.read()
file = os.openFile('test/assets/file.txt', .read)
t.eq(file.read(3), rawstring('foo'))
t.eq(file.read(10), rawstring('bar'))
t.eq(file.read(10), rawstring(''))
t.eq(file.read(10), rawstring(''))
t.eq(try file.read(-1), error.InvalidArgument)
t.eq(try file.read(0), error.InvalidArgument)

-- File.readToEnd()
file = os.openFile('test/assets/file.txt', .read)
t.eq(file.readToEnd(), rawstring('foobar'))
t.eq(file.readToEnd(), rawstring(''))

-- File.close()
file = os.openFile('test/assets/file.txt', .read)
file.close()
res = try file.stat()
t.eq(res, error.Closed)

-- File.seek()
file = os.openFile('test/assets/file.txt', .read)
t.eq(try file.seek(-1), error.InvalidArgument)
file.seek(3)
t.eq(file.read(3), rawstring('bar'))

-- File.seekFromCur()
file = os.openFile('test/assets/file.txt', .read)
file.seekFromCur(3)
t.eq(file.read(3), rawstring('bar'))
file.seekFromCur(-6)
t.eq(file.read(3), rawstring('foo'))

-- File.seekFromEnd()
file = os.openFile('test/assets/file.txt', .read)
file.seekFromEnd(-3)
t.eq(file.read(3), rawstring('bar'))
t.eq(try file.seekFromEnd(1), error.InvalidArgument)
file.seekFromEnd(-6)
t.eq(file.read(3), rawstring('foo'))

-- File.write() from create
file = os.createFile('test/assets/write.txt', true)
t.eq(file.write('foobar'), 6)
t.eq(os.readFile('test/assets/write.txt'), rawstring('foobar'))

-- File.write() from open
file = os.openFile('test/assets/write.txt', .write)
file.seekFromEnd(0)
t.eq(file.write('abcxyz'), 6)
t.eq(os.readFile('test/assets/write.txt'), rawstring('foobarabcxyz'))

-- Dir.iterator()
dir = os.openDir('test/assets/dir', true)
var iter = dir.iterator()
var entries = []
while iter.next() some n:
    entries.append(n)
t.eq(entries.len(), 3)
entries.sort((a, b) => a.name.less(b.name))
t.eq(entries[0].name, rawstring('dir2'))
t.eq(entries[0].type, .dir)
t.eq(entries[1].name, rawstring('file.txt'))
t.eq(entries[1].type, .file)
t.eq(entries[2].name, rawstring('file2.txt'))
t.eq(entries[2].type, .file)

-- Dir.walk()
dir = os.openDir('test/assets/dir', true)
iter = dir.walk()
entries = []
while iter.next() some n:
    entries.append(n)
t.eq(entries.len(), 4)
entries.sort((a, b) => a.path.less(b.path))
t.eq(entries[0].path, rawstring('dir2'))
if os.system == 'windows':
    t.eq(entries[1].path, rawstring('dir2\\file.txt'))
else:
    t.eq(entries[1].path, rawstring('dir2/file.txt'))
t.eq(entries[2].path, rawstring('file.txt'))
t.eq(entries[3].path, rawstring('file2.txt'))

-- writeFile() rawstring
if os.cpu != 'wasm32':
  var s = rawstring('').insertByte(0, 255)
  os.writeFile('test.txt', s)
  var read = os.readFile('test.txt')
  t.eq(read.len(), 1)
  t.eq(read.byteAt(0), 255)