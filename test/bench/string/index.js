let str = 'abcdefghijklmnopqrstuvwxyz123456'.repeat(1000000);
str = str.concat('waldo');

let start = Date.now();
let idx = 0
for (let i = 0; i < 50; i += 1) {
    idx = str.indexOf('waldo');
}
console.log(`idx: ${idx} ms: ${Date.now() - start}`);