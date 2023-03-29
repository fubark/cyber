-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'

-- Detect end of block.
f = func ():
  return 123
t.eq(f(), 123)
f = func ():
  a = 123
  return a
t.eq(f(), 123)

-- Using tabs.
f = func ():
	return 123
t.eq(f(), 123)
f = func ():
		a = 123
		return a
t.eq(f(), 123)

-- Comment before end of block.
f = func ():
  return 123
  -- Comment.
t.eq(f(), 123)

-- Indented comment at the end of the source.
f = func ():
  return 123
     -- Comment.

-- Continue from parent indentation.
f = func ():
  if false:
    pass
  return 123 
t.eq(f(), 123)

-- Continue from grand parent indentation.
f = func ():
  if false:
    if false:
      pass
  return 123 
t.eq(f(), 123)
