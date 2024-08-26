var foo = func (a int) int:
    return a

foo(1.0)

--cytest: error
--panic: Incompatible call arguments `(float)`
--to the lambda `func (int) int`.
--
--main:4:1 main:
--foo(1.0)
--^
--