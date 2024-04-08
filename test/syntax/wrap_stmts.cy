use t 'test'

-- Expressions are allowed to wrap to the next line.
var a = 0
if true or
   true:
  a = 10
t.eq(a, 10)

--cytest: pass