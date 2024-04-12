use test

type S:
    a any

let o = test.erase(S{})
o.foo()

--cytest: error
--panic: The method `foo` can not be found in `S`.
--
--main:7:1 main:
--o.foo()
--^
--