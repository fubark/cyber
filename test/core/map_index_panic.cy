a := Map[str, str]{}
_ = a['foo']

--cytest: panic
--panic: MissingKey
--
--[trace]
--main:2:5 main:
--_ = a['foo']
--    ^
--