fn toNum(a int):
    pass

fn foo(a int):
    pass

foo = toNum

--cytest: error
--CompileError: Can not reassign to a namespace function.
--
--main:7:1:
--foo = toNum
--^
--