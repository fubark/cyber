use test

-- rsize -> Ptr[void]
addr := rsize.ones
test.eq(as[Ptr[void]] addr, @bitCast(Ptr[void], addr))

-- Ptr[S] -> Ptr[T]
test.eq(as[Ptr[byte]] 1, @bitCast(Ptr[byte], as[Ptr[str]] 1))

--cytest: pass