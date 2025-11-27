type S struct:
    a S

o := S{}

--cytest: error
--CompileError: Structs can not contain a circular dependency.
--
--@MainPath():2:7:
--    a S
--      ^
--@MainPath():1:1:
--type S struct:
--^~~~~~~~~~~~~~
--