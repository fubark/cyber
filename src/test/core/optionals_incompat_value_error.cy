a := ?int('abc')

--cytest: error
--CompileError: Expected argument `?int`, found `str`, when calling `fn @init(?int) -> ?int`.
--
--@MainPath():1:11:
--a := ?int('abc')
--          ^~~~~
--