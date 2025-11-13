use t 'test'

-- Basic type embedding tests
type Base:
    a int

    func double(self) int:
        return a * 2

type Container:
    b use Base

var c = Container{b = Base{a=123}}
print c.a
print c.double()


type ContainerTwo:
    a int
    b use Base

var c2 = ContainerTwo{a=999, b = Base{a=123}}
print c2.a
print c2.double()
print c2.b.a

--cytest: pass
