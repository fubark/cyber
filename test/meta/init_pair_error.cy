type Foo:
    a Fiber

    fn $initPair(key string, value any):
        pass

var f = Foo{a=123, b=234}

--cytest: error
--CompileError: Unsupported zero initializer for `Fiber`.
--
--main:7:12:
--var f = Foo{a=123, b=234}
--           ^
--