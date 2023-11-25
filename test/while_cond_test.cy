-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'

-- Basic.
var i = 0
while i != 10:
  i += 1
t.eq(i, 10)

-- break
i = 0
while i != 10:
  i += 1
  break
t.eq(i, 1)

-- continue
i = 0
var count = 0
while i != 10:
  i += 1
  if i == 2:
    continue
  count += 1
t.eq(count, 9)

-- continue releases locals.
i = 0
while i != 10:
  var a = []
  i += 1
  if i == 2:
    continue

-- Single line block.
i = 0
while i != 10: i += 1
t.eq(i, 10)