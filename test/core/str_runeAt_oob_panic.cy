s := 'abc'
_ = s.rune_at(100)

--cytest: panic
--panic: OutOfBounds
--
--[trace]
--main:2:5 main:
--_ = s.rune_at(100)
--    ^
--