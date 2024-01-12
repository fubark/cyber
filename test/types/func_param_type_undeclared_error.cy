func foo(a Vec2):
    pass

--cytest: error
--CompileError: Could not find the symbol `Vec2`.
--
--main:1:12:
--func foo(a Vec2):
--           ^
--