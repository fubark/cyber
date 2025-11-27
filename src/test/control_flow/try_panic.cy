res := Result[int](error.Invalid)
_ = res!

--cytest: panic
--panic: Unwrapped error `error.Invalid`.
--
--[trace]
--@MainPath():2:5 main:
--_ = res!
--    ^
--