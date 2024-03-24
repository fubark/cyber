type S:
    a any

var o = S{b: 100}

--cytest: error
--CompileError: Field `b` does not exist in `S`.
--
--main:4:11:
--var o = S{b: 100}
--          ^
--