use test

fn main():
    -- Tests that main function is run.
    test.eq(1, 2)

--cytest: panic
--panic: Expected `1`, found `2`.
--
--[trace]
--@MainPath():5:5 main:
--    test.eq(1, 2)
--    ^
--@MainPath(): main
--