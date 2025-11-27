type Foo:
    a int

fn (&Foo) @init_record(pairs []Pair[str, Object]):
    pass

f := Foo{123, 234}

--cytest: error
--CompileError: Expected record initializer.
--
--@MainPath():7:9:
--f := Foo{123, 234}
--        ^~~~~~~~~~
--