type S:
    a float

o := ^S{a=123}
o.a = 'abc'

--cytest: error
--CompileError: Expected type `float`, got `str`.
--
--main:5:7:
--o.a = 'abc'
--      ^~~~~
--