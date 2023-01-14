import os 'os'
import t 'test'

os.setEnv('testfoo', 'testbar')
try t.eq(os.getEnv('testfoo'), 'testbar')
os.unsetEnv('testfoo')
try t.eq(os.getEnv('testfoo'), none)

-- createDir()
os.removeDir('test/dir')
try t.eq(os.createDir('test/dir'), true)
dir = os.openDir('test/dir')
try t.eq(dir.stat().type, #dir)

-- createFile() new file.
os.removeFile('test/write.txt')
file = os.createFile('test/write.txt', false)
file.write('foobar')
try t.eq(readFile('test/write.txt'), 'foobar')
-- createFile() no truncate.
file = os.createFile('test/write.txt', false)
try t.eq(readFile('test/write.txt'), 'foobar')
-- createFile() truncate.
file = os.createFile('test/write.txt', true)
try t.eq(readFile('test/write.txt'), '')

-- openDir()
dir = os.openDir('test')
info = dir.stat()
try t.eq(info.type, #dir)

-- openFile()
file = os.openFile('test/file.txt', #read)
info = file.stat()
try t.eq(info.type, #file)

-- File.read()
file = os.openFile('test/file.txt', #read)
try t.eq(file.read(3), 'foo')
try t.eq(file.read(10), 'bar')
try t.eq(file.read(10), '')
try t.eq(file.read(10), '')
try t.eq(file.read(-1), error(#InvalidArgument))
try t.eq(file.read(0), error(#InvalidArgument))

-- File.readToEnd()
file = os.openFile('test/file.txt', #read)
try t.eq(file.readToEnd(), 'foobar')
try t.eq(file.readToEnd(), '')

-- File.seek()
file = os.openFile('test/file.txt', #read)
try t.eq(file.seek(-1), error(#InvalidArgument))
file.seek(3)
try t.eq(file.read(3), 'bar')

-- File.seekFromCur()
file = os.openFile('test/file.txt', #read)
file.seekFromCur(3)
try t.eq(file.read(3), 'bar')
file.seekFromCur(-6)
try t.eq(file.read(3), 'foo')

-- File.seekFromEnd()
file = os.openFile('test/file.txt', #read)
file.seekFromEnd(-3)
try t.eq(file.read(3), 'bar')
try t.eq(file.seekFromEnd(1), error(#InvalidArgument))
file.seekFromEnd(-6)
try t.eq(file.read(3), 'foo')

-- File.write() from create
file = os.createFile('test/write.txt', true)
try t.eq(file.write('foobar'), 6)
try t.eq(readFile('test/write.txt'), 'foobar')

-- File.write() from open
file = os.openFile('test/write.txt', #write)
file.seekFromEnd(0)
try t.eq(file.write('abcxyz'), 6)
try t.eq(readFile('test/write.txt'), 'foobarabcxyz')