-- Copyright (c) 2023 Cyber (See LICENSE)
import a 'test_mods/a.cy'

type Bar object:
    b number

type Foo object:
    field Bar     -- Allowed after field ident.
    field2 a.Bar  -- Prefix path also allowed.

-- Allowed after func param.
func foo(a Bar, b a.Bar):
    pass