import os
import t 'test'

if os.system != 'windows':
    os.setEnv('testfoo', 'testbar')
    t.eq(os.getEnv('testfoo'), 'testbar')
    os.unsetEnv('testfoo')
    t.eq(os.getEnv('testfoo'), none)

-- access()
my res = try os.access('test/assets/missing.txt', .read)
t.eq(res, error.FileNotFound)
res = os.access('test/assets/file.txt', .read)
t.eq(res, none)

-- args()
res = os.args()
t.eq(res.len() > 0, true)

-- createDir()
os.removeDir('test/assets/tempdir')
t.eq(os.createDir('test/assets/tempdir'), none)
var dir = os.openDir('test/assets/tempdir')
t.eq(dir.stat().type, .dir)

-- createFile() new file.
os.removeFile('test/assets/write.txt')
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
t.eq(os.dirName('.'), none)
t.eq(os.dirName('./foo'), '.')
t.eq(os.dirName('./foo/bar.txt'), './foo')
t.eq(os.dirName('./foo/bar'), './foo')
t.eq(os.dirName('/root'), '/')
t.eq(os.dirName('/root/bar'), '/root')
t.eq(os.dirName('/root/bar.txt'), '/root')

-- openDir()
dir = os.openDir('test')
my info = dir.stat()
t.eq(info.type, .dir)

-- openFile()
file = os.openFile('test/assets/file.txt', .read)
info = file.stat()
t.eq(info.type, .file)

-- free() / malloc()
var ptr = os.malloc(16)
t.eq(typesym(ptr), .pointer)
os.free(ptr)

-- File.read()
file = os.openFile('test/assets/file.txt', .read)
t.eq(file.read(3), array('foo'))
t.eq(file.read(10), array('bar'))
t.eq(file.read(10), array(''))
t.eq(file.read(10), array(''))
t.eq(try file.read(-1), error.InvalidArgument)
t.eq(try file.read(0), error.InvalidArgument)

-- File.readAll()
file = os.openFile('test/assets/file.txt', .read)
t.eq(file.readAll(), array('foobar'))
t.eq(file.readAll(), array(''))

-- File.close()
file = os.openFile('test/assets/file.txt', .read)
file.close()
res = try file.stat()
t.eq(res, error.Closed)

-- File.seek()
file = os.openFile('test/assets/file.txt', .read)
t.eq(try file.seek(-1), error.InvalidArgument)
file.seek(3)
t.eq(file.read(3), array('bar'))

-- File.seekFromCur()
file = os.openFile('test/assets/file.txt', .read)
file.seekFromCur(3)
t.eq(file.read(3), array('bar'))
file.seekFromCur(-6)
t.eq(file.read(3), array('foo'))

-- File.seekFromEnd()
file = os.openFile('test/assets/file.txt', .read)
file.seekFromEnd(-3)
t.eq(file.read(3), array('bar'))
t.eq(try file.seekFromEnd(1), error.InvalidArgument)
file.seekFromEnd(-6)
t.eq(file.read(3), array('foo'))

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
var entries = []
while iter.next() -> n:
    entries.append(n)
t.eq(entries.len(), 3)
entries.sort((a, b) => a.name.less(b.name))
t.eq(entries[0].name, 'dir2')
t.eq(entries[0].type, .dir)
t.eq(entries[1].name, 'file.txt')
t.eq(entries[1].type, .file)
t.eq(entries[2].name, 'file2.txt')
t.eq(entries[2].type, .file)

-- Dir.walk()
dir = os.openDir('test/assets/dir', true)
iter = dir.walk()
entries = []
while iter.next() -> n:
    entries.append(n)
t.eq(entries.len(), 4)
entries.sort((a, b) => a.path.less(b.path))
t.eq(entries[0].path, 'dir2')
if os.system == 'windows':
    t.eq(entries[1].path, 'dir2\\file.txt')
else:
    t.eq(entries[1].path, 'dir2/file.txt')
t.eq(entries[2].path, 'file.txt')
t.eq(entries[3].path, 'file2.txt')

-- writeFile()
if os.cpu != 'wasm32':
    var s = array('').insertByte(0, 255)
    os.writeFile('test.txt', s)
    var file = os.openFile('test.txt', .read)
    var bytes = file.readAll()
    t.eq(bytes.len(), 1)
    t.eq(bytes.getByte(0), 255)