use test

type A:
    a ^int

a := ^A{a=^123}

-- Since ref is an rvalue, need to own the ref (+1 rc).
foo(a.a, a)

fn foo(param ^int, parent ^A):
    test.eq(123, param.*)
    -- If caller didn't retain, this would invalidate `param`.
    parent.a = ^234
    test.eq(123, param.*)

--cytest: pass