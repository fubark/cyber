use t 'test'

type Foo:
    a int

fn foo(a Foo) -> bool:
    return a.a == 123

-- Literal.
t.eq(foo(Foo{a=123}), true)
        
-- From var.
o := Foo{a=123}
t.eq(foo(o), true)

--cytest: pass