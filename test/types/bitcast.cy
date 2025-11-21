use test

-- int -> Ptr[void]
test.eq(as[Ptr[void]] -1, @bitCast(Ptr[void], -1))

-- Ptr[S] -> Ptr[T]
test.eq(as[Ptr[byte]] 1, @bitCast(Ptr[byte], as[Ptr[str]] 1))

--cytest: pass