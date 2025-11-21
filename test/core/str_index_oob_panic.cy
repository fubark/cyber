s := 'abc'
_ = s[100]

--cytest: panic
--panic: OutOfBounds
--
--[trace]
--main:2:5 main:
--_ = s[100]
--    ^
--