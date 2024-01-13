type S:
    var a float

func foo(): return []

var o = [S a: 123]
o.a = foo()

--cytest: error
--panic: Assigning to `float` field with incompatible type `List`.
--
--main:7:1 main:
--o.a = foo()
--^
--