var i = 10
var res = switch i:
    case 0 => 'one'
    case 1 => 'two'

--cytest: error
--CompileError: Expected `else` case since switch is not exhaustive.
--
--main:2:11:
--var res = switch i:
--          ^
--