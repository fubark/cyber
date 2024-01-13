type S:
    my a

func foo():
    return 123

var s = [S a: foo()]
s = [S a: 123]

--cytest: pass