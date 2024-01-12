func foo() int:
    return 1.2

--cytest: error
--CompileError: Expected type `int`, got `float`.
--
--main:2:12:
--    return 1.2
--           ^
--