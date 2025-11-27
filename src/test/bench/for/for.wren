var start = System.clock

var list = []

for (i in 0...1000000) list.add(i)

var sum = 0
for (i in list) sum = sum + i

System.print("time: %((System.clock - start) * 1000)")
System.print(sum)