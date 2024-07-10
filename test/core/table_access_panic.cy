var o = {a=123}
o.b

--cytest: error
--panic: Missing field.
--
--<host>: host function
--main:2:3 main:
--o.b
--  ^
--