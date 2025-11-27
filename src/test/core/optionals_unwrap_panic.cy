a := ?int(none)
b := a.?

--cytest: panic
--panic: Expected active choice tag `1`, found `0`.
--
--[trace]
--@MainPath():2:6 main:
--b := a.?
--     ^
--