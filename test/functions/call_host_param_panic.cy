var foo = isDigit
foo(1.0)

--cytest: error
--panic: Incompatible call arguments `(float) any`
--to the lambda `func (int) bool`.
--
--main:2:1 main:
--foo(1.0)
--^
--