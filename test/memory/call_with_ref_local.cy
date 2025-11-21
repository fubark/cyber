use test

a := ^123

-- Since ref is from local, the ref is guaranteed to exist so no need to own (+0 rc).
-- NOTE: although `move` might complicate this.
foo(a)

fn foo(param ^int):
    test.eq(123, param.*)

--cytest: pass