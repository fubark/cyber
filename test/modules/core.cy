use t 'test'

--| Tests sensitive to line numbers.
-- -- stackTrace(), current frame.
-- t.eq(stackTrace(), '''main:6:5 main:
--     throw error.Boom
--     ^
-- ''')

-- -- stackTrace(), one frame before and one frame after.
-- fn foo2():
--     stackTrace()
--     t.eq(errorReport(), '''main:15:5 foo2:
--     throw error.Boom
--     ^
-- main:18:9 foo:
--         foo2()
--         ^
-- main:30:1 main:
-- foo()
-- ^
-- ''')
-- fn foo():
--     foo2()
-- foo()

-- error(), see error_test.cy

-- to_print_string()
t.eq(to_print_string(123), '123')

--cytest: pass