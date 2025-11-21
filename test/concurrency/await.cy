use test
use cy

-- Await already completed future.
f := Future.complete(123)
test.eq(123, f.await())

-- -- Await completion from queued task.
-- r := ^FutureResolver.new(int)
-- queueTask(|_|):
--     r.complete(234)
-- test.eq(234, await r.future())

-- -- Await from function call.
-- fn foo() -> int:
--     r := ^FutureResolver.new(int)
--     queueTask(|_|):
--         r.complete(234)
--     return await r.future()
-- test.eq(234, foo())

--cytest: pass