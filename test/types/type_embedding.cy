use test

-- Basic type embedding tests
type Base:
    a int

fn (&Base) double() -> int:
    return $a * 2

type Container:
    b use Base

c := Container{b = Base{a=123}}
test.eq(123, c.a)
test.eq(246, c.double())

type ContainerTwo:
    a int
    b use Base

c2 := ContainerTwo{a=999, b = Base{a=123}}
test.eq(999, c2.a)
test.eq(123, c2.b.a)
test.eq(246, c2.double())

--cytest: pass
