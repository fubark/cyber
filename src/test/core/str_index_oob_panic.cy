s := 'abc'
_ = s[100]

--cytest: panic
--panic: OutOfBounds
--
--[trace]
--@MainPath():2:5 main:
--_ = s[100]
--    ^
--