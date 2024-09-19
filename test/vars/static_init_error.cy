var .a float = 'abc'

--cytest: error
--CompileError: Expected type `float`, got `String`.
--
--main:1:16:
--var .a float = 'abc'
--               ^
--