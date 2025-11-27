a := Map[str, str]{}
_ = a['foo']

--cytest: panic
--panic: MissingKey
--
--[trace]
--@MainPath():2:5 main:
--_ = a['foo']
--    ^
--