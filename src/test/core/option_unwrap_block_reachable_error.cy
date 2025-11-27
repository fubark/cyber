res := ?int(none)
val := res ?else:
    pass

--cytest: error
--CompileError: Expected unreachable block end.
--
--@MainPath():2:8:
--val := res ?else:
--       ^~~~~~~~~~
--