var .a = b
var .b = a

--cytest: error
--CompileError: Referencing `a` creates a circular dependency in the module.
--
--main:2:10:
--var .b = a
--         ^
--