use os

var start = os.now()

fn fib(n int) int:
    if n < 2:
        return n
    return fib(n - 1) + fib(n - 2)

var res = fib(30)
print("time: ${(os.now() - start) * 1000}")
print res