use a 'test_mods/a.cy'

type Foo:
    field a.Bar  -- Prefix path also allowed.

func foo(a a.Bar):
    pass

--cytest: pass