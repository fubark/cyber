use os
use t 'test'

var printCmd = ''
if os.system == 'windows':
    printCmd = 'Write-Output'
else:
    printCmd = 'printf'

-- os.readLine() returns error.EndOfStream
runPipeInput('$(printCmd) "abc"', "
use test
use os
if os.system == 'windows':
  test.eq(os.readLine(), 'abc\\r')
  test.eq(try os.readLine(), error.EndOfStream)
else:
  test.eq(try os.readLine(), error.EndOfStream)
")

-- os.readLine() returns user input before new line.
runPipeInput('$(printCmd) "abc\n"', "
use test
use os
test.eq(os.readLine(), 'abc')
")

-- os.stdin.streamLines()
runPipeInput('$(printCmd) "abc\nfoo\r\nbar"', "
use os
use test
var lines = []
for os.stdin.streamLines() -> line:
  lines.append(line)
test.eq(lines.len(), 3)
test.eq(lines[0].decode(), 'abc\\n')
test.eq(lines[1].decode(), 'foo\\r\\n')
if os.system == 'windows':
  test.eq(lines[2].decode(), 'bar\\r\\n')
else:
  test.eq(lines[2].decode(), 'bar')
")

-- os.stdin.streamLines() with small buffer size to test string building.
runPipeInput('$(printCmd) "abcxyz\nfoobar\r\ndeadbeef"', "
use os
use test
var lines = []
for os.stdin.streamLines(4) -> line:
  lines.append(line)
if os.system == 'windows':
  test.eq(lines.len(), 4)
  test.eq(lines[0].decode(), 'abcxyz\\n')
  test.eq(lines[1].decode(), 'foobar\\r\\n')
  test.eq(lines[2].decode(), 'deadbeef\\r')
  test.eq(lines[3].decode(), '\\n')
else:
  test.eq(lines.len(), 3)
  test.eq(lines[0].decode(), 'abcxyz\\n')
  test.eq(lines[1].decode(), 'foobar\\r\\n')
  test.eq(lines[2].decode(), 'deadbeef')
")

runArgs(['123', 'foobar'], "
use test
use os
var args = os.args()
test.eq(args.len(), 4)
test.eq(args[0], os.exePath())
test.eq(args[1], 'temp.cy')
test.eq(args[2], '123')
test.eq(args[3], 'foobar')
")

-- os.stdout
runExpectOut("
use os
os.stdout.write('foo')
", 'foo')

-- os.stderr
runExpectErr("
use os
os.stderr.write('foo')
", 'foo')

fn runExpectErr(src, expErr):
    os.writeFile('temp.cy', src)
    var cyber = os.exePath()
    var res = os.execCmd([ cyber, 'temp.cy'])
    if res.exited != 0:
        print res.out
        print res.err
    t.eq(res.exited, 0)
    t.eq(res.err, expErr)

fn runExpectOut(src, expOut):
    os.writeFile('temp.cy', src)
    var cyber = os.exePath()
    var res = os.execCmd([ cyber, 'temp.cy'])
    if res.exited != 0:
        print res.out
        print res.err
    t.eq(res.exited, 0)
    t.eq(res.out, expOut)

fn runPipeInput(cmd, src):
    os.writeFile('temp.cy', src)
    var cyber = os.exePath()
    let res = none
    if os.system == 'windows':
      res = os.execCmd([ 'powershell', '-c', '$(cmd) | $(cyber) temp.cy' ])
    else:
      res = os.execCmd([ '/bin/bash', '-c', '$(cmd) | $(cyber) temp.cy' ])
    if res.exited != 0:
        print res.out
        print res.err
    t.eq(res.exited, 0)

fn runArgs(args, src):
    os.writeFile('temp.cy', src)
    var cyber = os.exePath()
    var cmd = [ cyber, 'temp.cy' ]
    cmd.concat(args)
    var res = os.execCmd(cmd)
    if res.exited != 0:
        print res.out
        print res.err
    t.eq(res.exited, 0)