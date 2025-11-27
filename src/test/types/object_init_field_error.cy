type S:
    a float

fn foo():
    return 123

s := ^S{a=false}

--cytest: error
--CompileError: Expected type `float`, got `bool`.
--
--@MainPath():7:11:
--s := ^S{a=false}
--          ^~~~~
--