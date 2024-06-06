var foo = isDigit
foo(1.0)

--cytest: error
--panic: Incompatible call arguments `(float)`
--to the lambda `func (int) bool`.
--
--main:2:1 main:
--foo(1.0)
--^
--