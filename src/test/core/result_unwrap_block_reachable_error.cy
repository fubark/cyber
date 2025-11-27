res := Result[int](error.Invalid)
val := res !else |err|:
    pass

--cytest: error
--CompileError: Expected unreachable block end.
--
--@MainPath():2:8:
--val := res !else |err|:
--       ^~~~~~~~~~~~~~~~
--