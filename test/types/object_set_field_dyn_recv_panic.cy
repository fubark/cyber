type S:
    a float

dyn o = S{a=123}
o.a = 'abc'

--cytest: error
--panic: Assigning to `float` field with incompatible type `string`.
--
--main:5:3 main:
--o.a = 'abc'
--  ^
--