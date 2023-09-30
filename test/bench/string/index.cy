import os

str = 'abcdefghijklmnopqrstuvwxyz123456'.repeat(1000000)
str = str.concat('waldo')

start = os.milliTime()
for 0..50:
    idx = str.index('waldo')
print 'idx: {idx} ms: {os.milliTime() - start}'