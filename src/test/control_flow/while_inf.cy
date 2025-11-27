use t 'test'

-- Infinite loop clause.
i := 0
while:
    i += 1
    if i == 10:
        break
t.eq(i, 10)

-- Break from else.
while:
    if false:
        pass
    else:
        break

-- Continue.
i = 0
count := 0
while:
    i += 1
    if i == 4:
        continue
    count += 1
    if i == 10:
        break
t.eq(count, 9)

-- Single line block.
i = 0
j := ^0
inc := fn() -> int:
    j.* += 1
    return j.*
while: if inc() == 10: break
t.eq(j.*, 10)

--cytest: pass