use test

var .keys = List[string]{}
var .values = List[any]{}

type Foo:
    fn $initPair(self, key string, value any):
        keys.append(key)
        values.append(value)

var f = Foo{a=123, b=234}
test.eq(keys.len(), 2)
test.eq(keys[0], 'a')
test.eq(keys[1], 'b')
test.eq(values.len(), 2)
test.eq(values[0], 123)
test.eq(values[1], 234)

--cytest: pass