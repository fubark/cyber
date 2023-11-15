var list = []

for 0..1000000 -> i:
    list.append(i)

var sum = 0
for list -> i:
    sum += i

print(sum)