type Foo[T type]:
    a int

fn (&Foo) foo():
    pass

--cytest: error
--CompileError: A method must be bound to a template's instance `Foo[]`.
--
--main:4:1:
--fn (&Foo) foo():
--^~~~~~~~~~~~~~~~
--