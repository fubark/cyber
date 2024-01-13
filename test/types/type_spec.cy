type Bar:
    var b float

type Foo:
    var field Bar

func foo(a Foo):
    pass

--cytest: pass