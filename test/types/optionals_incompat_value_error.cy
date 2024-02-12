var a ?int = 'abc'

--cytest: error
--CompileError: Expected type `?int`, got `String`.
--
--main:1:15:
--var a ?int = 'abc'
--              ^
--