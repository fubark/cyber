const start = Date.now()

function fib(n) {
    if (n < 2) {
        return n
    }
    return fib(n - 1) + fib(n - 2)
}

const res = fib(30)
console.log(`time: ${Date.now() - start}`)
console.log(res)
