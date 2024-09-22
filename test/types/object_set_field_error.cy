type S:
    a float

var o = S{a=123}
o.a = 'abc'

--cytest: error
--CompileError: Expected type `float`, got `string`.
--
--main:5:7:
--o.a = 'abc'
--      ^
--