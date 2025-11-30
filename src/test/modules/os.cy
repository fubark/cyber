use os
use meta
use t 'test'

-- Environment variable tests (work on all platforms)
os.set_env('testfoo', 'testbar')!
t.eq(os.get_env('testfoo').?, 'testbar')
os.unset_env('testfoo')!
t.assert(os.get_env('testfoo') == none)

-- accessFile()
t.eq(error.FileNotFound, os.accessFile('src/test/assets/missing.txt', os.AccessExists).unwrapError())
os.accessFile('src/test/assets/file.txt', os.AccessExists)!

-- args()
-- Not available from static linking.
res := os.args()
t.eq(res.len() > 0, true)

-- createDir()
if os.fileExists('src/test/assets/tempdir'):
    os.removeDir('src/test/assets/tempdir')!
os.createDir('src/test/assets/tempdir')!

dir := os.open_dir('src/test/assets/tempdir')!
t.eq(os.FileKind.directory, dir.info()!.kind)

-- createFile() new file.
rm_res := os.removeFile('src/test/assets/write.txt')
file := os.createFile('src/test/assets/write.txt')!
_ = file.write('foobar')!
t.eq('foobar', str(os.read_file('src/test/assets/write.txt')!))

-- createFile() no truncate.
file = os.createFile('src/test/assets/write.txt', false)!
t.eq('foobar', str(os.read_file('src/test/assets/write.txt')!))

-- createFile() truncate.
file = os.createFile('src/test/assets/write.txt', true)!
t.eq('', str(os.read_file('src/test/assets/write.txt')!))

-- dirname()
t.eq(?str(none), os.dirname('.'))
t.eq('.', os.dirname('./foo').?)
t.eq('./foo', os.dirname('./foo/bar.txt').?)
t.eq('./foo', os.dirname('./foo/bar').?)
t.eq('/', os.dirname('/root').?)
t.eq('/root', os.dirname('/root/bar').?)
t.eq('/root', os.dirname('/root/bar.txt').?)

t.eq('/a/b', os.dirname('/a/b/c').?)
t.eq('/a/b', os.dirname('/a/b/c///').?)
t.eq('/', os.dirname('/a').?)
t.eq(?str(none), os.dirname('/'))
t.eq(?str(none), os.dirname('//'))
t.eq(?str(none), os.dirname('///'))
t.eq(?str(none), os.dirname('////'))
t.eq(?str(none), os.dirname(''))
t.eq(?str(none), os.dirname('a'))
t.eq(?str(none), os.dirname('a/'))
t.eq(?str(none), os.dirname('a//'))

-- open_dir()
dir = os.open_dir('src/test')!
t.eq(os.FileKind.directory, dir.info()!.kind)

-- open_file()
file = os.open_file('src/test/assets/file.txt', .read)!
t.eq(os.FileKind.file, file.info()!.kind)

-- free() / malloc()
ptr := os.malloc(16)!
t.eqType(Ptr[void], type.of(ptr))
os.free(ptr)

-- File.read(bytes)
file = os.open_file('src/test/assets/file.txt', .read)!
t.eq('foo', str(file.read(3)!))
t.eq('bar', str(file.read(10)!))
t.eq('', str(file.read(10)!))
-- t.eq(error.InvalidArgument, file.read(-1).unwrapError())
t.eq('', str(file.read(0)!))

-- File.read_all()
file = os.open_file('src/test/assets/file.txt', .read)!
t.eq('foobar', str(file.read_all()!))
t.eq('', str(file.read_all()!))

-- File.close()
file = os.open_file('src/test/assets/file.txt', .read)!
file.close()
t.eq(true, file.closed)

-- File.seek()
file = os.open_file('src/test/assets/file.txt', .read)!
file.seek(3)!
t.eq('bar', str(file.read(3)!))
file.seek(-6)!
t.eq('foo', str(file.read(3)!))

-- File.seekFromEnd()
file = os.open_file('src/test/assets/file.txt', .read)!
file.seekFromEnd(-3)!
t.eq('bar', str(file.read(3)!))
-- t.eq(error.InvalidArgument, file.seekFromEnd(1).unwrapError())
file.seekFromEnd(-6)!
t.eq('foo', str(file.read(3)!))

-- File.seekFromStart()
file = os.open_file('src/test/assets/file.txt', .read)!
t.eq(error.InvalidArgument, file.seekFromStart(-1).unwrapError())
file.seekFromStart(3)!
t.eq('bar', str(file.read(3)!))

-- File.write(str) from create
file = os.createFile('src/test/assets/write.txt', true)!
_ = file.write('foobar')!
t.eq('foobar', str(os.read_file('src/test/assets/write.txt')!))

-- File.write(str) from open
file = os.open_file('src/test/assets/write.txt', .write)!
file.seekFromEnd(0)!
_ = file.write('abcxyz')!
t.eq('foobarabcxyz', str(os.read_file('src/test/assets/write.txt')!))

-- Dir.iterator()
dir = os.open_dir('src/test/assets/dir', true)!
iter := dir.iterator()
entries := Array[os.DirEntry]{}
while iter.next(&dir)! |n|:
    entries << n
t.eq(3, entries.len())
entries.sort(|a, b| a.name.less(b.name))
t.eq('dir2', entries[0].name)
t.eq(os.FileKind.directory, entries[0].kind)
t.eq('file.txt', entries[1].name)
t.eq(os.FileKind.file, entries[1].kind)
t.eq('file2.txt', entries[2].name)
t.eq(os.FileKind.file, entries[2].kind)

-- -- Dir.walk()
-- dir = os.open_dir('src/test/assets/dir', true)!
-- iter = dir.walk()
-- entries = {}
-- while iter.next() -> n:
--     entries.append(n)
-- t.eq(4, entries.len())
-- entries.sort((a, b) => a.path.less(b.path))
-- t.eq('dir2', entries[0].path)
-- if meta.system() == .windows:
--     t.eq('dir2\file.txt', entries[1].path)
-- else:
--     t.eq('dir2/file.txt', entries[1].path)
-- t.eq('file.txt', entries[2].path)
-- t.eq('file2.txt', entries[3].path)

-- writeFile(str)
if meta.cpu() != 'wasm32':
    s := 'hello'
    os.write_file('test.txt', s)!
    file := os.open_file('test.txt', .read)!
    t.eq('hello', str(file.read_all()!))

--cytest: pass