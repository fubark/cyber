use test
use meta

type Shape enum:
    case rectangle Rectangle
    case circle    Circle
    case line      float
    case point

type Circle:
    radius float

type Rectangle:
    width  float
    height float

-- Switch case on object payload.
s := Shape.rectangle({width=123, height=234})
switch s:
    case .rectangle |r|:
        test.eq(123.0, r.width)
        test.eq(234.0, r.height)
    else:
        test.fail()

-- Switch case on object payload. Expr.
var res Shape = switch s:
    case .rectangle |r| => Shape.rectangle(r)
    else => Shape.rectangle({width=0, height=0})
test.eq(123.0, res.!rectangle.width)
test.eq(234.0, res.!rectangle.height)

-- Switch case on primitive payload.
s = Shape.line(20)
switch s:
    case .line |l|:
        test.eq(l, 20.0)
    else:
        test.fail()

-- Switch case on primitive payload. Expr.
res = switch s:
    case .line |l| => Shape.line(l)
    else => Shape.line(0)
test.eq(20.0, res.!line)

-- Switch case on empty payload.
s = Shape.point
switch s:
    case .point:
        pass
    else:
        test.fail()

-- Switch case on empty payload. Expr.
s = Shape.point
res2 := switch s:
    case .point => 123
    else => 0
test.eq(123, res2)

-- Switch else case.
s = Shape.point
switch s:
    case .circle |c|:
        test.fail()
    else:
        pass

-- Switch else case. Expr.
res2 = switch s:
    case .circle |c| => 0
    else => 234
test.eq(res2, 234)

-- Unwrapping payload.
rect := Rectangle{width=123, height=234}
s = Shape.rectangle(rect)
test.eq(s.!rectangle.width, rect.width)
test.eq(s.!rectangle.height, rect.height)

-- Choice type also generates a enum `Tag` type.
tag := Shape.Tag.rectangle
test.eq(tag, .rectangle)

-- switch: capture borrow to `NoCopy` payload.
type Choice enum:
    case a Array[int]

c := Choice.a({})
switch c:
    case .a |&a|:
        test.eq(0, a.len())

--cytest: pass