var a = 1
var foo = func():
    -- Captured for read.
    print a
    -- Attempting to declare `a`.
    var a = 3

--cytest: error
--CompileError: `a` already references a parent local variable.
--
--main:6:9:
--    var a = 3
--        ^
--