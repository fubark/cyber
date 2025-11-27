type Foo:
    a    int

fn (&Foo) @set(name str, val int):
    panic('error')

f := Foo{a=123}
f.foo = 234

--cytest: panic
--panic: error
--
--[trace]
--@MainPath():5:5 Foo.@set:
--    panic('error')
--    ^
--@MainPath():8:1 main:
--f.foo = 234
--^
--