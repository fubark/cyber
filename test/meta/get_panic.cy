type Foo:
    a    int

    func $get(name String):
        panic('error')

var f = Foo{a=123}
f.foo

--cytest: error
--panic: error
--
--main:5:9 $get:
--        panic('error')
--        ^
--main:8:3 main:
--f.foo
--  ^
--