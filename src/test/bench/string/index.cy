use os

s := 'abcdefghijklmnopqrstuvwxyz123456'.repeat(1000000)
s += 'waldo'

start := os.now()!
idx := 0
for 0..50:
    idx = s.index('waldo').?

print('time: %{(os.now()! - start) * 1000}')
print(idx)