import test

type S object:
    var a float

-- Zero initialize missing field.
var o = [S:]
test.eq(o.a, 0.0)

type T object:
  var a S
  var b float

-- Zero initialize missing field. Nested object.
var t = [T:]
test.eq(t.a.a, 0.0)
test.eq(t.b, 0.0)

--cytest: pass