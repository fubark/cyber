var count = 0

var list = []
for (i in 0...100000) {
    var f = Fiber.new {
        count = count + 1
        Fiber.yield()
        count = count + 1
    }
    f.call()
    list.add(f)
}

for (f in list) {
    f.call()
}

System.print(count)
