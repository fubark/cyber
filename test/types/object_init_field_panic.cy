type S:
    a float

fn foo() dyn:
    return 123

var s = S{a=foo()}

--cytest: error
--panic: Expected type `float`, found `int`.
--
--main:7:13 main:
--var s = S{a=foo()}
--            ^
--