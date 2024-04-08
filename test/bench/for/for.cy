use os

var start = os.now()

var list = []

for 0..1000000 -> i:
    list.append(i)

var sum = 0
for list -> i:
    sum += i

print("time: $((os.now() - start) * 1000)")
print(sum)