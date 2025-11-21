type S struct:
    a S

o := S{}

--cytest: error
--CompileError: Structs can not contain a circular dependency.
--
--main:2:7:
--    a S
--      ^
--main:1:1:
--type S struct:
--^~~~~~~~~~~~~~
--