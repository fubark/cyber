use a 'test_mods/a.cy'

type Foo:
    field a.Bar  -- Prefix path also allowed.

fn foo(a a.Bar):
    pass

--cytest: pass