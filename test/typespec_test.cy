-- Copyright (c) 2023 Cyber (See LICENSE)
import a 'test_mods/a.cy'

object Bar:
    b number

object Foo:
    field Bar     -- Allowed after field ident.
    field2 a.Bar  -- Prefix path also allowed.

-- Allowed after func param.
func foo(a Bar, b a.Bar):
    pass