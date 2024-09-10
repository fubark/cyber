var a = if (false) 123 else '456'

--cytest: error
--CompileError: Expected type `int`, got `String`.
--
--main:1:29:
--var a = if (false) 123 else '456'
--                            ^
--