try:
    range = xrange
except NameError:
    pass

list = []
for i in range(0, 1000000):
  list.append(i)

sum = 0
for i in list:
  sum += i
print(sum)