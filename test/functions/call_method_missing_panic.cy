type S:
    var a

my o = [S:]
o.foo()

--cytest: error
--panic: `func foo(any) any` can not be found in `S`.
--
--main:5:1 main:
--o.foo()
--^
--