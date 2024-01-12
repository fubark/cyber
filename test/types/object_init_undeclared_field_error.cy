type S object:
    var a

var o = [S b: 100]

--cytest: error
--CompileError: Field `b` does not exist in `S`.
--
--main:4:12:
--var o = [S b: 100]
--           ^
--