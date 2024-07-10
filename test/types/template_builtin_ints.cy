use test

type Foo[T type] #int64_t:
    func get(self, a T) T:
        return a

-- Declare explicit type.
var f = bitcast(Foo[String], 123)
test.eq(123, bitcast(int, f))
test.eq(f.get('abc'), 'abc')

-- Different variant.
var f2 = bitcast(Foo[float], 123)
test.eq(123, bitcast(int, f2))
test.eq(f2.get(7.02), 7.02)

--cytest: pass