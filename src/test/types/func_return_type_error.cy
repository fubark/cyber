fn foo() -> int:
    return 1.2

--cytest: error
--CompileError: Expected type `int`, got `float`.
--
--@MainPath():2:12:
--    return 1.2
--           ^~~
--