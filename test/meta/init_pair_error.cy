type Foo:
    a int

    func '$initPair'(key String, value any):
        pass

var f = Foo{a: 123, b: 234}

--cytest: error
--CompileError: Type `Foo` can not initialize with `$initPair` since it does not have a default record initializer.
--
--main:7:12:
--var f = Foo{a: 123, b: 234}
--           ^
--