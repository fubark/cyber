use test

-- Init generator without starting.
#[generator]
fn foo(list ^ArrayStore[int]) -> int:
    list << 123
list := ^ArrayStore[int]{}
gen := foo(list)
test.eq(list.len(), 0)

-- Start generator with yield at start.
#[generator]
fn foo2(list ^ArrayStore[int]) -> int:
    yield list.len()
    list << 123
gen = foo2(list)
test.eq(0, gen.next().?)
test.eq(list.len(), 0)

-- Generator with managed type.
#[generator]
fn foo3(list ^ArrayStore[int]) -> ^int:
    yield ^list.len()
    list << 123
list = ^{}
gen2 := foo3(list)
test.eq(0, gen2.next().?.*)

-- Start generator without yield.
#[generator]
fn foo4(list ^ArrayStore[int]) -> int:
    list << 123
list = ^{}
gen = foo4(list)
test.eq(?int(none), gen.next())

-- next() returns final value.
#[generator]
fn foo5(list ^ArrayStore[int]) -> int:
    list << 123
    return list[0]
list = ^{}
gen = foo5(list)
test.eq(123, gen.next().?)

-- -- Start generator with yield in nested function.
-- fn bar() -> int:
--     alist := []Object{}
--     yield 123
-- fn foo5(list []int) -> int:
--     yield* bar()
--     list.append(123)
-- list = {}
-- gen = foo5(list)
-- test.eq(123, gen.next().?)
-- test.eq(list.len(), 0)

-- Continue to resume generator.
#[generator]
fn foo6(list ^ArrayStore[int]) -> int:
    list << 123
    yield list.len()
    list << 234
    yield list.len()
list = ^{}
gen = foo6(list)
test.eq(1, gen.next().?)
test.eq(2, gen.next().?)

-- Generator status.
#[generator]
fn foo7() -> int:
    test.eq(static_gen.status(), .running)
type Fn7 = fn() -> int
global static_gen ^Generator[Fn7] = foo7()
test.eq(static_gen.status(), .paused)
_ = static_gen.next()
test.eq(static_gen.status(), .done)

-- Resuming after generator is done is a nop.
#[generator]
fn foo8() -> int:
    yield 123
gen3 := foo8()
_ = gen3.next()
_ = gen3.next()
test.eq(gen3.status(), .done)
-- gen3.next()
-- test.eq(gen3.status(), $done)

-- Recursive call in generator.
fn sum(n int) -> int:
    if n == 0:
        return 0
    return n + sum(n - 1)
#[generator]
fn genSum(n int) -> int:
    yield sum(n)
gen4 := genSum(20)
test.eq(210, gen4.next().?)

-- -- coinit lambda
-- foof := (list []Object):
--     list.append(123)
-- list = {}
-- f = coinit(foof, list)
-- coresume f
-- test.eq(list[0], 123)

--cytest: pass