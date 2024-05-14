var b = 2
var foo = func (a int):
    return a + b
foo(1.0)

--cytest: error
--panic: Incompatible call arguments `(float) any`
--to the lambda `func (int) dyn`.
--
--main:4:1 main:
--foo(1.0)
--^
--