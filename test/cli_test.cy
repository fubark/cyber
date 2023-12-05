import os
import t 'test'

var printCmd = ''
if os.system == 'windows':
    printCmd = 'Write-Output'
else:
    printCmd = 'printf'

-- os.getInput() returns error.EndOfStream
runPipeInput('$(printCmd) "abc"', "
import test
import os
if os.system == 'windows':
  test.eq(os.getInput(), 'abc\\r')
  test.eq(try os.getInput(), error.EndOfStream)
else:
  test.eq(try os.getInput(), error.EndOfStream)
")

-- os.getInput() returns user input before new line.
runPipeInput('$(printCmd) "abc\n"', "
import test
import os
test.eq(os.getInput(), 'abc')
")

-- os.stdin.streamLines()
runPipeInput('$(printCmd) "abc\nfoo\r\nbar"', "
import os
import test
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
import os
import test
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
import test
import os
var args = os.args()
test.eq(args.len(), 4)
test.eq(args[0], os.exePath())
test.eq(args[1], 'temp.cy')
test.eq(args[2], '123')
test.eq(args[3], 'foobar')
")

-- os.stdout
runExpectOut("
import os
os.stdout.write('foo')
", 'foo')

-- os.stderr
runExpectErr("
import os
os.stderr.write('foo')
", 'foo')

func runExpectErr(src, expErr):
    os.writeFile('temp.cy', src)
    var cyber = os.exePath()
    var res = os.execCmd([ cyber, 'temp.cy'])
    if res.exited != 0:
        print res.out
        print res.err
    t.eq(res.exited, 0)
    t.eq(res.err, expErr)

func runExpectOut(src, expOut):
    os.writeFile('temp.cy', src)
    var cyber = os.exePath()
    var res = os.execCmd([ cyber, 'temp.cy'])
    if res.exited != 0:
        print res.out
        print res.err
    t.eq(res.exited, 0)
    t.eq(res.out, expOut)

func runPipeInput(cmd, src):
    os.writeFile('temp.cy', src)
    var cyber = os.exePath()
    my res = none
    if os.system == 'windows':
      res = os.execCmd([ 'powershell', '-c', '$(cmd) | $(cyber) temp.cy' ])
    else:
      res = os.execCmd([ '/bin/bash', '-c', '$(cmd) | $(cyber) temp.cy' ])
    if res.exited != 0:
        print res.out
        print res.err
    t.eq(res.exited, 0)

func runArgs(args, src):
    os.writeFile('temp.cy', src)
    var cyber = os.exePath()
    var cmd = [ cyber, 'temp.cy' ]
    cmd.concat(args)
    var res = os.execCmd(cmd)
    if res.exited != 0:
        print res.out
        print res.err
    t.eq(res.exited, 0)