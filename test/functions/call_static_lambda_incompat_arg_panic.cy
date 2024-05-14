func foo(a int):
    return a
var bar = foo
bar(1.0)

--cytest: error
--panic: Incompatible call arguments `(float) any`
--to the lambda `func (int) dyn`.
--
--main:4:1 main:
--bar(1.0)
--^
--