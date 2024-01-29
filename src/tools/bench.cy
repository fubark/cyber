#!cyber

-- Runs a command multiple times and relies on the process stdout for the clock time
-- which should be in the format `time: {ms}`.
-- This is useful for benching scripts where only the body of the script is measured.
-- Total wall time, and the difference from script time (load time) are also measured.
-- Finally the command is passed into `/usr/bin/time -v {cmd}` to get the peak memory usage.

import os

var MaxRuns = 50

var cmd = os.args()[2..]

var totalTimes = []
var times = []

for 0..MaxRuns:
    var start = os.now()

    var res = os.execCmd(cmd)
    var out = res.out as String
    var idx = out.find('time:') as int
    if idx == none:
        throw error.NoTimeOutput
    out = out[idx + 'time:'.len()..]
    var endIdx = out.find('\n')
    var msStr = out[0..endIdx].trim(.ends, ' \t')
    var ms = float(msStr)
    times.append(ms)

    var totalTime = (os.now() - start) * 1000
    totalTimes.append(totalTime)

var sum = 0.0
for times -> time:
    sum += time
var scriptTime = sum / float(times.len())

sum = 0.0
for totalTimes -> time:
    sum += time
var totalTime = sum / float(totalTimes.len())

-- Get peak memory usage.
cmd.insert(0, '/usr/bin/time')
cmd.insert(1, '-l')
var res = os.execCmd(cmd)
var out = res.err as String
var idx = out.find('maximum resident set size')
if idx == none:
    throw error.NoMemoryOutput
out = out[0..idx].trim(.right, ' ')
var i = out.len()-1
while out.runeAt(i) != ` `:
    i -= 1
var mem = float(out[i+1..])

print "Total: $(totalTime)ms"
print "Script: $(scriptTime)ms"
print "Load: $(totalTime - scriptTime)ms"
print "Peak Memory: $(mem / 1000000) MB"