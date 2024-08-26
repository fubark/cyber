use test
use cy

-- Await non future value.
test.eq(await 'abc', 'abc')

-- Await already completed future.
var f = Future.complete(123)
test.eq(await f, 123)

-- Await completion from queued task.
var r = FutureResolver.new(int)
queueTask(():
    r.complete(234)
test.eq(await r.future(), 234)

-- Await from function call.
func foo() int:
    var r = FutureResolver.new(int)
    queueTask(():
        r.complete(234)
    return await r.future()
test.eq(foo(), 234)

--cytest: pass