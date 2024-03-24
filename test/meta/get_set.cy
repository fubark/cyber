import test

type Foo:
    a    int
    data Map

    func '$get'(name String):
        return data[name]

    func '$set'(name String, value int):
        data[name] = value

-- Initialize with missing field.
var f = Foo{a: 123, b: 234}

-- $set was not called for `a`.
test.eq(f.data.contains('a'), false)
test.eq(f.a, 123)

-- $set was called for `b`
test.eq(f.data.contains('b'), true)
test.eq(f.b, 234)

-- Setting missing field.
f.c = 345
test.eq(f.data.contains('c'), true)
test.eq(f.c, 345)

--cytest: pass