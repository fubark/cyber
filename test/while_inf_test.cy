import t 'test'

-- Infinite loop clause.
var i = 0
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
var count = 0
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
var inc = func(): 
    i += 1
    return i
while: if inc() == 10: break
t.eq(i, 10)