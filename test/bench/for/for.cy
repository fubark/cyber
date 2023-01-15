list = []

for 0..1000000 each i:
    list.append(i)

sum = 0
for list each i:
    sum += i

print(sum)