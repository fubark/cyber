type A:
    b use B

type B:
    a use A

var v = A{}

--cytest: error
--CompileError: Circular embedding detected
