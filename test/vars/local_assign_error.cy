a := 123.0
a = "123"

--cytest: error
--CompileError: Expected type `float`, got `str`.
--
--main:2:5:
--a = "123"
--    ^~~~~
--