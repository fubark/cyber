my foo(a int):
    pass

--cytest: error
--CompileError: Type specifier not allowed in `my` declaration. Declare typed functions with `func`.
--
--main:1:8:
--my foo(a int):
--       ^
--