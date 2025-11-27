var o = {a=123}
o.b

--cytest: error
--panic: Missing field.
--
--@MainPath():2:3 main:
--o.b
--  ^
--