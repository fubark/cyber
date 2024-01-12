var Root.a = b
var Root.b = a

--cytest: error
--CompileError: Referencing `a` creates a circular dependency in the module.
--
--main:2:14:
--var Root.b = a
--             ^
--