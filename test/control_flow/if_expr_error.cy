var a = if (false) 123 else '456'

--cytest: error
--CompileError: Expected type `int`, got `String`.
--
--main:1:30:
--var a = if (false) 123 else '456'
--                             ^
--