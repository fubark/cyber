-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'

var err = error.FileNotFound
t.eq(typesym(err), .error)
t.eq(err, error.FileNotFound)

-- error.value()
err = error.Oops
t.eq(err.value(), .Oops)

-- error.<call>()
err = error(.Oops)
t.eq(err.value(), .Oops)