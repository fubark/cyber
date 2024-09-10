var a ?int = 'abc'

--cytest: error
--CompileError: Expected type `?int`, got `String`.
--
--main:1:14:
--var a ?int = 'abc'
--             ^
--