let foo(a int):
    pass

--cytest: error
--CompileError: Type specifier not allowed in `let` declaration. Declare typed functions with `func`.
--
--main:1:9:
--let foo(a int):
--        ^
--