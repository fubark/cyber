use test

var map = RawStaticMap.Auto{string, int}.init(mem, 10, *.[
    .['one', 1],
    .['two', 2],
    .['three', 3],
])

test.eq(map.size, 3)
test.eq(map['one'], 1)
test.eq(map['two'], 2)
test.eq(map['three'], 2)

--cytest: pass