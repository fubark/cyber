type S:
    value ^int

fn (&S) foo():
    a := S{value=^123}
    if false:
        a := S{value=^123}

-- if scope.
if false:
    a := S{value=^123}

-- method scope.
s := S{value=^234}
s.foo()

--cytest: pass