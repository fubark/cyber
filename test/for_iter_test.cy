import t 'test'

-- Basic.
list = [1, 2, 3]
sum = 0
for list each it:
   sum += it
try t.eq(sum, 6)

-- Loop iterator var overwrites the user var.
elem = 123
list = [1, 2, 3]
for list each elem:
  pass
try t.eq(elem, none)

-- Break.
list = [1, 2, 3]
sum = 0
for list each it:
   if it == 3:
      break
   sum += it
try t.eq(sum, 3)

-- Continue.
list = [1, 2, 3]
sum = 0
for list each it:
   if it == 1:
      continue
   sum += it
try t.eq(sum, 5)