foo := str.is_ascii_digit
foo(1.0)

--cytest: error
--CompileError: Expected argument `byte`, found `float`, when calling `fn (byte) -> bool`.
--
--@MainPath():2:5:
--foo(1.0)
--    ^~~
--