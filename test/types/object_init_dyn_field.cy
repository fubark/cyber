type S:
    a dyn

func foo() int:
    return 123

var s = S{a=foo()}
s = S{a=123}

--cytest: pass