var a = {}
a['foo']

--cytest: error
--panic: Field not found in value.
--
--main:2:1 main:
--a['foo']
--^
--