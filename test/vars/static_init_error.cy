var .a float = {_}

--cytest: error
--CompileError: Expected type `float`, got `List[dyn]`.
--
--main:1:16:
--var .a float = {_}
--               ^
--