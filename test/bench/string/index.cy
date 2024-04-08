use os

var str = 'abcdefghijklmnopqrstuvwxyz123456'.repeat(1000000)
str = str.concat('waldo')

var start = os.now()
let idx = 0
for 0..50:
    idx = str.find('waldo')

print "time: $((os.now() - start) * 1000)"
print idx