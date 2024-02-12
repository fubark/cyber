type Foo:
    var a int

func foo(f ++Foo):
    pass

--cytest: error
--CompileError: `+Foo` is already a value type.
--
--main:4:13:
--func foo(f ++Foo):
--            ^
--