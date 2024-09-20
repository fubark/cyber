use test

type Shape enum:
    case rectangle Rectangle
    case circle    struct(radius float)
    case triangle  struct(base float, height float)
    case line      float
    case point

type Rectangle:
    width  float
    height float

-- Switch case on object payload.
var s = Shape.rectangle{width=123, height=234}
switch s
case .rectangle -> r:
    test.eq(r.width, 123.0)
    test.eq(r.height, 234.0)
else:
    test.fail()

-- Switch case on object payload. Expr.
dyn res = switch s:
    case .rectangle -> r => r
    else => Shape.rectangle{width=0, height=0}
test.eq(res.width, 123.0)
test.eq(res.height, 234.0)

-- Switch case on unnamed object payload.
s = Shape.circle{radius=10}
switch s
case .circle -> c:
    test.eq(c.radius, 10.0)
else:
    test.fail()

-- Switch case on unnamed object payload. Expr.
res = switch s:
    case .circle -> c => c
    else => Shape.circle{radius=0}
test.eq(res.radius, 10.0)

-- Switch case on primitive payload.
s = Shape.line(20)
switch s
case .line -> l:
    test.eq(l, 20.0)
else:
    test.fail()

-- Switch case on primitive payload. Expr.
res = switch s:
    case .line -> l => l
    else => Shape.line(0)
test.eq(res, 20.0)

-- Switch case on empty payload.
s = Shape.point
switch s
case .point:
    pass
else:
    test.fail()

-- Switch case on empty payload. Expr.
s = Shape.point
res = switch s:
    case .point => 123
    else => 0
test.eq(res, 123)

-- Switch else case.
s = Shape.point
switch s
case .circle -> c:
    test.fail()
else:
    pass

-- Switch else case. Expr.
res = switch s:
    case .circle -> c => 0
    else => 234
test.eq(res, 234)

-- Unwrapping payload.
var rect = Rectangle{width=123, height=234}
s = Shape.rectangle(rect)
test.assert(s.!rectangle == rect)

-- Builtin choicetag.
s = Shape.line(123)
test.eq(choicetag(s), .line)

-- Choice type also generates a enum `Tag` type.
var tag = Shape.Tag.rectangle
test.eq(tag, .rectangle)

--cytest: pass