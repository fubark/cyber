use t 'test'

var err = error.FileNotFound
t.eq(typeof(err), error)
t.eq(err, error.FileNotFound)

-- error.value()
err = error.Oops
t.eq(err.sym(), .Oops)

-- error.<call>()
err = error(symbol.Oops)
t.eq(err.sym(), .Oops)

--cytest: pass