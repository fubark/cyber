var list = []

for (i in 0...1000000) list.add(i)

var sum = 0
for (i in list) sum = sum + i

System.print(sum)