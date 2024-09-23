type Bar:
    b float

type Foo:
    field Bar

fn foo(a Foo):
    pass

--cytest: pass