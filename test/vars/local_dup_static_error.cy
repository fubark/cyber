var Root.a = 1
var foo = func():
    print a
    -- Attempting to declare `a`.
    var a = 3

--cytest: error
--CompileError: `a` already references a static variable.
--
--main:5:9:
--    var a = 3
--        ^
--