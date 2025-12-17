type Foo:
    a    int

fn (&Foo) @get(%name EvalStr):
    panic('error')

f := Foo{a=123}
f.foo

--cytest: panic
--panic: error
--
--[trace]
--@MainPath():5:5 Foo.@get:
--    panic('error')
--    ^
--@MainPath():8:3 main:
--f.foo
--  ^
--