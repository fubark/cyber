use test

-- With methods.
let Counter{ count }:
    let inc():
        count += 1
        
let c = Counter{ count: 0 }
test.eq(c.count, 0)
c.inc()
test.eq(c.count, 1)

-- Just fields.
let Vec3{x, y, z}
let v = Vec3{x: 1, y: 2, z: 3}
test.eq(v.x, 1)
test.eq(v.y, 2)
test.eq(v.z, 3)

-- Set missing field.
v.payload = 999
test.eq(v.payload, 999)

-- Indexing.
v[123] = 'abc'
test.eq(v[123], 'abc')

-- Table with no fields declared.
let A{}
let a = A{}

--cytest: pass