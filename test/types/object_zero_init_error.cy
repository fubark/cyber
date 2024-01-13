type S:
    var a S

var o = [S:]

--cytest: error
--CompileError: Can not zero initialize `S` because of circular dependency.
--
--main:4:9:
--var o = [S:]
--        ^
--