a := 123
print(as[str] a)

--cytest: error
--CompileError: Cannot cast `int` to `str`.
--
--@MainPath():2:15:
--print(as[str] a)
--              ^
--