type Foo[T type]:
    a T

f := Foo[string, int]{a='abc'}

--cytest: error
--CompileError: Expected 1 template parameters.
--
--main:4:6:
--f := Foo[string, int]{a='abc'}
--     ^~~~~~~~~~~~~~~~
--