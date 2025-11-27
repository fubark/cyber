use t 'test'

err := error.FileNotFound
t.eqType(error, type.of(err))
t.eq(error.FileNotFound, err)

-- error.value()
err = error.Oops
t.eq(@Oops, err.sym())

-- error.<call>()
err = error(@Oops)
t.eq(@Oops, err.sym())

--cytest: pass