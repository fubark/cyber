type Foo:
    a    int

    func $set(name String, val any):
        panic('error')

var f = Foo{a: 123}
f.foo = 234

--cytest: error
--panic: error
--
--main:5:9 $set:
--        panic('error')
--        ^
--main:8:3 main:
--f.foo = 234
--  ^
--