type Foo:
    a int

foo := ^Foo{a=123}

a := move foo.a

--cytest: error
--CompileError: Cannot move a member from a shared reference.
--
--@MainPath():6:11:
--a := move foo.a
--          ^~~~~
--