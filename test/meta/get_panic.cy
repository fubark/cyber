type Foo:
    a    int

fn (&Foo) @get(name str):
    panic('error')

f := Foo{a=123}
f.foo

--cytest: panic
--panic: error
--
--[trace]
--main:5:5 Foo.@get:
--    panic('error')
--    ^
--main:8:3 main:
--f.foo
--  ^
--