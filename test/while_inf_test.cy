import t 'test'

-- Infinite loop clause.
i = 0
while:
  i += 1
  if i == 10:
    break
try t.eq(i, 10)

-- Break from else.
while:
  if false:
    pass
  else:
    break

-- Continue.
i = 0
count = 0
while:
  i += 1
  if i == 4:
    continue
  count += 1
  if i == 10:
    break
try t.eq(count, 9)