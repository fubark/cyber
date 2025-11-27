global a int = 123

const b = a

--cytest: error
--CompileError: Cannot reference runtime variable at compile-time.
--
--@MainPath():3:11:
--const b = a
--          ^
--