s := 'abc'
_ = s[-1]

--cytest: panic
--panic: OutOfBounds
--
--[trace]
--main:2:5 main:
--_ = s[-1]
--    ^
--