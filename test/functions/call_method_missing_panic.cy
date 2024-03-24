type S:
    a any

let o = S{}
o.foo()

--cytest: error
--panic: The method `foo` can not be found in `S`.
--
--main:5:1 main:
--o.foo()
--^
--