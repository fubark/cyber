import os 'os'
import t 'test'

if os.system == 'windows':
  printCmd = 'Write-Output'
else:
  printCmd = 'printf'

-- core.getInput() returns error.EndOfStream
runPipeInput('{printCmd} "abc"', "
import t 'test'
import os 'os'
if os.system == 'windows':
  t.eq(getInput().utf8(), 'abc\\r')
  t.eq(try getInput(), error.EndOfStream)
else:
  t.eq(try getInput(), error.EndOfStream)
")

-- core.getInput() returns user input before new line.
runPipeInput('{printCmd} "abc\n"', "
import t 'test'
t.eq(getInput().utf8(), 'abc')
")

-- os.stdin.streamLines()
runPipeInput('{printCmd} "abc\nfoo\r\nbar"', "
import os 'os'
import t 'test'
lines = []
for os.stdin.streamLines() each line:
  lines.append(line)
t.eq(lines.len(), 3)
t.eq(lines[0].utf8(), 'abc\\n')
t.eq(lines[1].utf8(), 'foo\\r\\n')
if os.system == 'windows':
  t.eq(lines[2].utf8(), 'bar\\r\\n')
else:
  t.eq(lines[2].utf8(), 'bar')
")

-- os.stdin.streamLines() with small buffer size to test string building.
runPipeInput('{printCmd} "abcxyz\nfoobar\r\ndeadbeef"', "
import os 'os'
import t 'test'
lines = []
for os.stdin.streamLines(4) each line:
  lines.append(line)
if os.system == 'windows':
  t.eq(lines.len(), 4)
  t.eq(lines[0].utf8(), 'abcxyz\\n')
  t.eq(lines[1].utf8(), 'foobar\\r\\n')
  t.eq(lines[2].utf8(), 'deadbeef\\r')
  t.eq(lines[3].utf8(), '\\n')
else:
  t.eq(lines.len(), 3)
  t.eq(lines[0].utf8(), 'abcxyz\\n')
  t.eq(lines[1].utf8(), 'foobar\\r\\n')
  t.eq(lines[2].utf8(), 'deadbeef')
")

runArgs(['123', 'foobar'], "
import t 'test'
import os 'os'
args = os.args()
t.eq(args.len(), 4)
t.eq(args[0].utf8(), os.exePath())
t.eq(args[1].utf8(), 'temp.cy')
t.eq(args[2].utf8(), '123')
t.eq(args[3].utf8(), 'foobar')
")

-- os.stdout
runExpectOut("
import os 'os'
os.stdout.write('foo')
", 'foo')

-- os.stderr
runExpectErr("
import os 'os'
os.stderr.write('foo')
", 'foo')

func runExpectErr(src, expErr):
    writeFile('temp.cy', src)
    cyber = os.exePath()
    res = execCmd([ cyber, 'temp.cy'])
    if res.exited != 0:
        print res.out
        print res.err
    t.eq(res.exited, 0)
    t.eq(res.err, expErr)

func runExpectOut(src, expOut):
    writeFile('temp.cy', src)
    cyber = os.exePath()
    res = execCmd([ cyber, 'temp.cy'])
    if res.exited != 0:
        print res.out
        print res.err
    t.eq(res.exited, 0)
    t.eq(res.out, expOut)

func runPipeInput(cmd, src):
    writeFile('temp.cy', src)
    cyber = os.exePath()
    if os.system == 'windows':
      res = execCmd([ 'powershell', '-c', '{cmd} | {cyber} temp.cy' ])
    else:
      res = execCmd([ '/bin/bash', '-c', '{cmd} | {cyber} temp.cy' ])
    if res.exited != 0:
        print res.out
        print res.err
    t.eq(res.exited, 0)

func runArgs(args, src):
    writeFile('temp.cy', src)
    cyber = os.exePath()
    cmd = [ cyber, 'temp.cy' ]
    cmd.concat(args as List)
    res = execCmd(cmd)
    if res.exited != 0:
        print res.out
        print res.err
    t.eq(res.exited, 0)