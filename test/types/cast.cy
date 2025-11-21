use t 'test'

-- int to float
var i int = 123
t.eq(123.0, as i)

-- float to int
var f float = 123
t.eq(123, as f)

-- Infer as[T] from ?T target.
i = 123
var opt ?i32 = as i
t.eq(i32(123), opt.?)

-- Infer as[T] from !T target.
var res !i32 = as i
t.eq(i32(123), res!)

--cytest: pass