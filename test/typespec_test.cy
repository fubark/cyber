-- Copyright (c) 2023 Cyber (See LICENSE)
import a 'test_mods/a.cy'

type Bar object:
    var b float

type Foo object:
    var field Bar     -- Allowed after field ident.
    var field2 a.Bar  -- Prefix path also allowed.

-- Allowed after func param.
func foo(a Bar, b a.Bar):
    pass