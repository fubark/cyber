var a = Map{}
a['foo']

--cytest: error
--panic: Missing key in map.
--
--main:2:1 main:
--a['foo']
--^
--