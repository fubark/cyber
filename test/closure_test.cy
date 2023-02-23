-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'

-- Closure read over number in main block.
a = 123
foo = () => a
try t.eq(foo(), 123)

-- Closure write over number in main block.
a2 = 123
foo = func():
  capture a2 = 234
foo()
try t.eq(a2, 234)

-- Closure over local number in function.
f = func():
  a = 123
  return () => a
fn = f()
try t.eq(fn(), 123)

-- Closure over local number in function using a param.
f = func():
  a = 123
  return b => a + b
fn = f()
try t.eq(fn(1), 124)

-- Closure over local number in function using a param in parentheses.
f = func():
  a = 123
  return (b) => a + b
fn = f()
try t.eq(fn(1), 124)

-- Closure over local number in function using a multiple params.
f = func():
  a = 123
  return (b, c) => a + b + c
fn = f()
try t.eq(fn(1, 2), 126)

-- Closure over local retained object in function.
f = func():
  a = [ 123 ]
  return () => a[0]
fn = f()
try t.eq(fn(), 123)

-- Closure with more than 3 captured vars forces allocation outside of object pool.
a3 = 123
b = 234
c = 345
d = 456
foo = () => a3 + b + c + d
try t.eq(foo(), 1158)