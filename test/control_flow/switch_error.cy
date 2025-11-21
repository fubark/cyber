i := 10
res := switch i:
    case 0 => 'one'
    case 1 => 'two'

--cytest: error
--CompileError: Expected `else` case since switch does not handle all cases.
--
--main:2:8:
--res := switch i:
--       ^~~~~~~~~
--