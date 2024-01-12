import t 'test'

var a = 123
-- no error, must returns argument.
t.eq(must(a), 123)

--cytest: pass