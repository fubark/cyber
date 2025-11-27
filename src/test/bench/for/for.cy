use os

start := os.now()!

arr := Array[int]{}

for 0..1000000 |i|:
    arr.append(i)

sum := 0
for arr |i|:
    sum += i

print('time: %{(os.now()! - start) * 1000}')
print(sum)