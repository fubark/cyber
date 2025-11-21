use test

type FooS struct:
    a int

fn FooS :: @init(i int) -> int:
    return i

test.eq(FooS(10), 10)

type FooCS cstruct:
    a int

fn FooCS :: @init(i int) -> int:
    return i

test.eq(FooCS(10), 10)

type FooO:
    a int

fn FooO :: @init(i int) -> int:
    return i

test.eq(FooO(10), 10)

type FooE enum:
    case a

fn FooE :: @init(i int) -> int:
    return i

test.eq(FooE(10), 10)

type FooT trait:
    fn foo() -> void

fn FooT :: @init(i int) -> int:
    return i

test.eq(FooT(10), 10)

--cytest: pass