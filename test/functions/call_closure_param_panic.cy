var b = 2
var foo = func (a int) int:
    return a + b
foo(1.0)

--cytest: error
--panic: Incompatible call arguments `(float)`
--to the lambda `func (int) int`.
--
--main:4:1 main:
--foo(1.0)
--^
--