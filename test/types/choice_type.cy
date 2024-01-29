import test

type Shape enum:
    case rectangle Rectangle
    case circle    object:
        var radius float
    case triangle  object:
        var base   float
        var height float
    case line      float
    case point

type Rectangle:
    var width  float
    var height float

-- Switch case on object payload.
var s = [Shape.rectangle width: 123, height: 234]
switch s:
case .rectangle -> r:
    test.eq(r.width, 123.0)
    test.eq(r.height, 234.0)
else:
    test.fail()

-- Switch case on object payload. Expr.
my res = switch s:
case .rectangle -> r => r
else => test.fail()
test.eq(res.width, 123.0)
test.eq(res.height, 234.0)

-- Switch case on unnamed object payload.
s = [Shape.circle radius: 10]
switch s:
case .circle -> c:
    test.eq(c.radius, 10.0)
else:
    test.fail()

-- Switch case on unnamed object payload. Expr.
res = switch s:
case .circle -> c => c
else => test.fail()
test.eq(res.radius, 10.0)

-- Switch case on primitive payload.
s = [Shape line: 20]
switch s:
case .line -> l:
    test.eq(l, 20.0)
else:
    test.fail()

-- Switch case on primitive payload. Expr.
res = switch s:
case .line -> l => l
else => test.fail()
test.eq(res, 20.0)

-- Switch case on empty payload.
s = [Shape.point:]
switch s:
case .point:
    pass
else:
    test.fail()

-- Switch case on empty payload. Expr.
res = switch s:
case .point => 123
else => test.fail()
test.eq(res, 123)

-- Switch else case.
s = [Shape.point:]
switch s:
case .circle -> c:
    test.fail()
else:
    pass

-- Switch else case. Expr.
res = switch s:
case .circle -> c => test.fail()
else => 234
test.eq(res, 234)

--cytest: pass