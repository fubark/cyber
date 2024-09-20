type S:
    a ^S

var o = S{}

--cytest: error
--CompileError: Unsupported zero initializer for `^S`.
--
--main:4:10:
--var o = S{}
--         ^
--