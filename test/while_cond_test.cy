import t 'test'

-- Basic.
i = 0
while i != 10:
  i += 1
try t.eq(i, 10)

-- break
i = 0
while i != 10:
  i += 1
  break
try t.eq(i, 1)

-- continue
i = 0
count = 0
while i != 10:
  i += 1
  if i == 2:
    continue
  count += 1
try t.eq(count, 9)