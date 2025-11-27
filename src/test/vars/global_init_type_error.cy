global a float = 'abc'

--cytest: error
--CompileError: Expected type `float`, got `str`.
--
--@MainPath():1:18:
--global a float = 'abc'
--                 ^~~~~
--