use test

fn main():
    -- Tests that main function is run.
    test.eq(1, 2)

--cytest: panic
--panic: Expected `1`, found `2`.
--
--[trace]
--main:5:5 main:
--    test.eq(1, 2)
--    ^
--main: $init
--