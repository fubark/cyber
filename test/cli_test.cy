import os 'os'
import t 'test'

-- core.getInput() returns error.EndOfStream
runPipeInput('printf "abc"', "
import t 'test'
try t.eq(getInput(), error(#EndOfStream))
")

-- core.getInput() returns user input before new line.
runPipeInput('printf "abc\n"', "
import t 'test'
try t.eq(getInput(), 'abc')
")

-- os.stdin.streamLines()
runPipeInput('printf "abc\nfoo\r\nbar"', "
import os 'os'
import t 'test'
lines = []
for os.stdin.streamLines() each line:
  lines.add(line)
try t.eq(lines.len(), 3)
try t.eq(lines[0], 'abc\\n')
try t.eq(lines[1], 'foo\\r\\n')
try t.eq(lines[2], 'bar')
")

-- os.stdin.streamLines() with small buffer size to test string building.
runPipeInput('printf "abcxyz\nfoobar\r\ndeadbeef"', "
import os 'os'
import t 'test'
lines = []
for os.stdin.streamLines(4) each line:
  lines.add(line)
try t.eq(lines.len(), 3)
try t.eq(lines[0], 'abcxyz\\n')
try t.eq(lines[1], 'foobar\\r\\n')
try t.eq(lines[2], 'deadbeef')
")

runArgs(['123', 'foobar'], "
import t 'test'
import os 'os'
args = os.args()
try t.eq(args.len(), 4)
try t.eq(args[0], os.exePath())
try t.eq(args[1], 'temp.cy')
try t.eq(args[2], '123')
try t.eq(args[3], 'foobar')
")

func runPipeInput(cmd, src):
    writeFile('temp.cy', src)
    cyber = os.exePath()
    res = execCmd([ '/bin/bash', '-c', '{cmd} | {cyber} temp.cy' ])
    if res.exited != 0:
        print res.out
        print res.err
    try t.eq(res.exited, 0)

func runArgs(args, src):
    writeFile('temp.cy', src)
    cyber = os.exePath()
    cmd = [ cyber, 'temp.cy' ]
    cmd.concat(args)
    res = execCmd(cmd)
    if res.exited != 0:
        print res.out
        print res.err
    try t.eq(res.exited, 0)