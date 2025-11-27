use t 'test'

-- Reading from a static variable before it is declared.
t.eq(a, 123)

global a int = 123

-- Reading from a static variable.
t.eq(a, 123)

-- Type is inferred from initializer.
t.eqType(int, type.of(a))

-- Invoke as function.
type Fn1 = fn() -> int
global a1 Fn1 = fn() -> int:
    return 123
type Fn2 = fn(int) -> int
global a2 Fn2 = fn(a int) -> int:
    return a + 1
type Fn3 = fn(int, int) -> int
global a3 Fn3 = fn(a int, b int) -> int:
    return a + b
t.eq(a1(), 123)
t.eq(a2(123), 124)
t.eq(a3(123, 321), 444)

-- -- Invoke method.
-- var .a4 = {1, 2, 3}
-- t.eq(a4.len(), 3)
-- a4.append(4)
-- t.eq(a4.len(), 4)

-- -- Using object initializer.
-- type Foo:
--     foo Object
-- var .a5 = Foo{foo=123}
-- t.eq(a5.foo, 123)

-- Type specifier.
global a6 float = 123.0
t.eq(a6, 123.0)

-- Type specifier infer type.
global a7 float = 123
t.eq(a7, 123.0)

-- Declaration over using builtin module. 
global print int = 123
t.eq(print, 123)

-- Global vars are loaded even if they are not referenced.
global b4 int = 123
global a4 int = load()

fn load() -> int:
    b4 = 234
    return 0
t.eq(b4, 234)

-- Initializer calling a pure function.
global b5 int = foo()

fn foo() -> int:
    return 123

--cytest: pass