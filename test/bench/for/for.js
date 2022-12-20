let list = []

for (let i = 0; i < 1000000; i += 1) {
    list.push(i)
}

let sum = 0
for (let i of list) {
    sum += i
}

console.log(sum)
