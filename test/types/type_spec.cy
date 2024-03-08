type Bar:
    b float

type Foo:
    field Bar

func foo(a Foo):
    pass

--cytest: pass