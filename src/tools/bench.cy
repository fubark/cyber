#!cyber

-- Runs a command multiple times and relies on the process stdout for the clock time
-- which should be in the format `time: {ms}`.
-- This is useful for benching scripts where only the body of the script is measured.
-- Total wall time, and the difference from script time (load time) are also measured.
-- Finally the command is passed into `/usr/bin/time -v {cmd}` to get the peak memory usage.

use os

max_runs := 50

cmd := os.args()[2..]

total_times := []float{}
times := []float{}

for 0..max_runs:
    start := os.now()!

    res := os.exec(cmd)
    idx := res.out.index('time:') ?else
        panic('NoTimeOutput')
    out := res.out[idx + 'time:'.len()..]
    endIdx := out.index('\n').?
    msStr := out[0..endIdx].trim(' \t')
    ms := float(msStr)
    times += ms

    total_time := (os.now()! - start) * 1000
    total_times += total_time

sum := 0.0
for times |time|:
    sum += time
scriptTime := sum / float(times.len())

sum = 0.0
for total_times |time|:
    sum += time
totalTime := sum / float(total_times.len())

-- Get peak memory usage.
cmd = cmd.insert(0, '/usr/bin/time')
cmd = cmd.insert(1, '-l')
res := os.exec(cmd)
idx := res.err.index('maximum resident set size') ?else
    panic('NoMemoryOutput')
out := res.err[0..idx].trimRight(' ')
i := out.len()-1
while out[i] != ' ':
    i -= 1
mem := float(out[i+1..])

print('Total: %{totalTime}ms')
print('Script: %{scriptTime}ms')
print('Load: %{totalTime - scriptTime}ms')
print('Peak Memory: %{mem / 1000000} MB')