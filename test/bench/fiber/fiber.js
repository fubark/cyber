let count = 0

function* inc() {
    count += 1
    yield
    count += 1
}

let fibers = []
for (let i = 0; i < 100000; i++) {
    const fiber = inc()
    fiber.next()
    fibers.push(fiber)
}

for (let fiber of fibers) {
    fiber.next()
}

console.log(count)