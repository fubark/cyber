type S struct:
    a S

var o = S{}

--cytest: error
--CompileError: Can not zero initialize `S` because of circular dependency.
--
--main:4:10:
--var o = S{}
--         ^
--