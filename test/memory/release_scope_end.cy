type S:
    value any

    func foo():
        var a = S{value=123}
        if false:
            var a = S{value=123}

-- if scope.
if false:
    var a = S{value=123}

-- method scope.
var s = S{value=234}
s.foo()

--cytest: pass