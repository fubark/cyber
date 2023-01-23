-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'

-- if branch.
foo = 0
if true:
  foo = 123
else:
  foo = 456
try t.eq(foo, 123)

-- else branch.
foo = 0
if false:
  foo = 123
else:
  foo = 456
try t.eq(foo, 456)

-- else if branch.
foo = 0
if false:
  foo = 456
else true:
  foo = 123
else:
  foo = 456
try t.eq(foo, 123)

-- Single line block.
foo = 0
if true: foo = 123
try t.eq(foo, 123)

foo = 0
if true: foo = 123
else: foo = 234
try t.eq(foo, 123)

foo = 0
if false: foo = 123
else: foo = 234
try t.eq(foo, 234)

foo = 0
if false: foo = 456
else true: foo = 123
else: foo = 456
try t.eq(foo, 123)