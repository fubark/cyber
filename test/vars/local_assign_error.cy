var a float = 123 
a = "123"

--cytest: error
--CompileError: Expected type `float`, got `string`.
--
--main:2:5:
--a = "123"
--    ^
--