use test

-- TODO: Test declared context variable.

-- TODO: Test `mem` instead.
context test_int int

test.eq(test_int, 123)

--cytest: pass