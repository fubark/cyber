use test

global keys ArrayStore[str] = {}
global values ArrayStore[int] = {}

type Foo

fn Foo :: @init_record(pairs [&]Pair[str, int]) -> int:
    for 0..pairs.length |i|:
        pair := &pairs[i]
        keys << pair.left
        values << pair.right
    return 0

f := Foo{a=123, b=234}
test.eq(keys.len(), 2)
test.eq(keys[0], 'a')
test.eq(keys[1], 'b')
test.eq(values.len(), 2)
test.eq(values[0], 123)
test.eq(values[1], 234)

--cytest: pass