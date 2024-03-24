func toNum(a int):
    pass

func foo(a int):
    pass

foo = toNum

--cytest: error
--CompileError: Can not reassign to a namespace function.
--
--main:7:1:
--foo = toNum
--^
--