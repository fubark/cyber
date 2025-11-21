res := ?int(none)
val := res ?else:
    pass

--cytest: error
--CompileError: Expected unreachable block end.
--
--main:2:8:
--val := res ?else:
--       ^~~~~~~~~~
--