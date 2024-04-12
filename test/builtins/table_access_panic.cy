var o = {a: 123}
o.b

--cytest: error
--panic: Field not found in value.
--
--builtins:193:16 $get:
--        return table_data[name]
--               ^
--main:2:3 main:
--o.b
--  ^
--