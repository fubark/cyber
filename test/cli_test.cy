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
for os.stdin.streamLines() as line:
  lines.add(line)
print lines[0]
print lines[1]
try t.eq(lines.len(), 3)
try t.eq(lines[0], 'abc\\n')
try t.eq(lines[1], 'foo\\r\\n')
try t.eq(lines[2], 'bar')
")

func runPipeInput(cmd, src):
    writeFile('temp.cy', src)
    cyber = os.exePath()
    res = execCmd([ '/bin/bash', '-c', '{cmd} | {cyber} temp.cy' ])
    if res.exited != 0:
        print res.out
        print res.err
    try t.eq(res.exited, 0)