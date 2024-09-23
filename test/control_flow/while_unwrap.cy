use t 'test'

-- Assign to variable.
var a = 0 
var next = fn () ?int:
    if a < 4:
        a += 1
        return a
    else:
        return none
var sum = 0
while next() -> res:
    sum += res
t.eq(sum, 10)

-- Assign rc value to variable.
a = 0 
var next2 = fn () ?List[int]:
    if a < 4:
        a += 1
        return {a}
    else:
        return none
sum = 0
while next2() -> res:
    sum += res[0]
t.eq(sum, 10)

-- Single line block.
a = 0 
next = fn () ?int:
    if a < 4:
        a += 1
        return a
    else:
        return none
sum = 0
while next() -> res: sum += res
t.eq(sum, 10)

--cytest: pass