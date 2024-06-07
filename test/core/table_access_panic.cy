var o = {a=123}
o.b

--cytest: error
--panic: Missing field.
--
--main:2:3 main:
--o.b
--  ^
--