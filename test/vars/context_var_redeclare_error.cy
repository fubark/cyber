context foo int

--cytest: error
--CompileError: Context variable `foo` does not exist.
--
--main:1:9:
--context foo int
--        ^
--