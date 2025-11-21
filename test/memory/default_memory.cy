use test

type Foo cstruct:
    a int
    b float

test.eq(sizeof(int), 8)
test.eq(sizeof(Foo), 16)

-- Memory.new primitive
var intp = mem.new(int)
intp.* = 123
test.eq(intp.*, 123)
mem.free(intp)

-- Memory.new cstruct
var foop = mem.new(Foo)
foop.a = 123
foop.b = 1.23
test.eq(foop.a, 123)
test.eq(foop.b, 1.23)
mem.free(foop)

-- Memory.alloc primitive
var ints = mem.alloc(int, 2)
test.eq(ints.len(), 2)
ints[0] = 123
ints[1] = 234
test.eq(ints[0], 123)
test.eq(ints[1], 234)
mem.free_(ints)

-- Memory.alloc cstruct
var foos = mem.alloc(Foo, 2)
test.eq(foos.len(), 2)
foos[0] = Foo{a=123, b=1.23}
foos[1] = Foo{a=234, b=2.34}
test.eq(foos[0].a, 123)
test.eq(foos[0].b, 1.23)
test.eq(foos[1].a, 234)
test.eq(foos[1].b, 2.34)
foos[0].a = 234
foos[0].b = 2.34
test.eq(foos[0].a, 234)
test.eq(foos[0].b, 2.34)
mem.free_(foos)

--cytest: pass