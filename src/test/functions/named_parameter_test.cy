use t 'test'

fn foo(a, b):
  return a + b * 10

t.eq(foo(1, 3), 31)
t.eq(foo(1, b: 3), 31)
t.eq(foo(a: 1, b: 3), 31)
t.eq(foo(b: 3, a: 1), 31)