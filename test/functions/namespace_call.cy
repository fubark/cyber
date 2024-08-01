use test

type FooS struct:
    a int

func FooS.$call(i int):
    return i

test.eq(FooS(10), 10)

type FooCS cstruct:
    a int

func FooCS.$call(i int):
    return i

test.eq(FooCS(10), 10)

type FooO:
    a int

func FooO.$call(i int):
    return i

test.eq(FooO(10), 10)

type FooE enum:
    case a

func FooE.$call(i int):
    return i

test.eq(FooE(10), 10)

type FooT trait:
    func foo(self) void

func FooT.$call(i int):
    return i

test.eq(FooT(10), 10)

--cytest: pass