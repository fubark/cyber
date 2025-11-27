s := 'abc'
_ = s.rune_at(-1)

--cytest: panic
--panic: OutOfBounds
--
--[trace]
--@MainPath():2:5 main:
--_ = s.rune_at(-1)
--    ^
--