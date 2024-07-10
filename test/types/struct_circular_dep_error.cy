type S struct:
    a S

var o = S{}

--cytest: error
--CompileError: Structs can not contain a circular dependency.
--
--main:2:7:
--    a S
--      ^
--