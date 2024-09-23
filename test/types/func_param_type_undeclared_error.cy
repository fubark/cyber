fn foo(a Vec2):
    pass

--cytest: error
--CompileError: Could not find the symbol `Vec2`.
--
--main:1:10:
--fn foo(a Vec2):
--         ^
--