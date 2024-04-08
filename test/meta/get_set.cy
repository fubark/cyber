import test

type Foo:
    a    int
    data Map

    func $get(name String):
        return data[name]

    func $set(name String, value int):
        data[name] = value

var f = Foo{a: 123}

-- $set was not called for `a`.
test.eq(f.data.contains('a'), false)
test.eq(f.a, 123)

-- Setting missing field.
f.b = 345
test.eq(f.data.contains('b'), true)
test.eq(f.b, 345)

--cytest: pass