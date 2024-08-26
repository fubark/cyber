func foo(a int) int:
    return a
var bar = foo
bar(1.0)

--cytest: error
--panic: Incompatible call arguments `(float)`
--to the lambda `func (int) int`.
--
--main:4:1 main:
--bar(1.0)
--^
--