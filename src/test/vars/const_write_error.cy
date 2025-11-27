const a = 123

a = 234

--cytest: error
--CompileError: Cannot assign to a constant.
--
--@MainPath():3:1:
--a = 234
--^~~~~~~
--