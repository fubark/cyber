use t 'test'

-- Test: Local variable with same name as field is allowed
type ShadowMembers:
    value int

fn (&ShadowMembers) shadow() -> int:
    value := 100  -- OK: declares new local variable
    return value  -- Returns local, not field

a := ShadowMembers{value=42}
t.eq(100, a.shadow())
t.eq(42, a.value)  -- Field unchanged

--cytest: pass
