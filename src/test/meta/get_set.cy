use test

type Foo:
    a    int
    data Map[str, int]

fn (&Foo) @get(name str) -> int:
    return $data[name]

fn (&Foo) @set(name str, value int):
    $data[name] = value

f := Foo{a=123, data={}}

-- $set was not called when initializing `Foo`.
test.eq(false, f.data.contains('a'))
test.eq(123, f.a)

-- Setting missing field.
f.b = 345
test.eq(true, f.data.contains('b'))
test.eq(345, f.b)

--cytest: pass