a := if (false) 123 else '456'

--cytest: error
--CompileError: Expected type `int`, got `str`.
--
--@MainPath():1:26:
--a := if (false) 123 else '456'
--                         ^~~~~
--