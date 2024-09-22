use test

type Foo:
    a    int
    data Map

    func $get(self, name string) dyn:
        return self.data[name]

    func $set(self, name string, value int):
        self.data[name] = value

var f = Foo{a=123}

-- $set was not called for `a`.
test.eq(f.data.contains('a'), false)
test.eq(f.a, 123)

-- Setting missing field.
f.b = 345
test.eq(f.data.contains('b'), true)
test.eq(f.b, 345)

--cytest: pass