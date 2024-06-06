var foo = func (a int):
    return a

foo(1.0)

--cytest: error
--panic: Incompatible call arguments `(float)`
--to the lambda `func (int) dyn`.
--
--main:4:1 main:
--foo(1.0)
--^
--