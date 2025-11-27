#[consteval]
fn GetType(a int) -> type:
    if a == 0:
        return bool
    else:
        return string

i := 0
GetType(i)

--cytest: error
--CompileError: Could not find the symbol `i`.
--
--@MainPath():9:9:
--GetType(i)
--        ^
--