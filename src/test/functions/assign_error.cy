fn toNum(a int):
    pass

fn foo(a int):
    pass

foo = toNum

--cytest: error
--CompileError: Cannot reassign to a static function.
--
--@MainPath():7:1:
--foo = toNum
--^~~~~~~~~~~
--