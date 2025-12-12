use test
use meta

-- choice_tag()
type Shape enum:
    case line  float
    case point
s := Shape.line(123)
test.eq(meta.choice_tag(s), .line)

-- is_none()
opt := ?int(none)
test.eq(meta.is_none(opt), true)
test.eq(meta.is_none(?int(1)), false)
opt = 123
test.eq(meta.is_none(opt), false)

--cytest: pass