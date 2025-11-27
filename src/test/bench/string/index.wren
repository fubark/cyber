// Unable to compare with other languages, since building the string (repeat) is too slow and takes up too much memory.
var str = "abcdefghijklmnopqrstuvwxyz123456" * 1000000
str = str + "waldo"

var start = System.clock
var idx
for (i in 0...100) {
    idx = str.indexOf("waldo")
}

System.print("idx: %(idx) ms: %((System.clock - start) * 1000)")