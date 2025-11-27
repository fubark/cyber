use os

var start = os.now()

type Node:
    value  int

fn Heap.new() Heap:
    return Heap{nodes=.{}}

type Heap:
    nodes List[Node]

    fn insert(self, n Node):
        var pos = self.nodes.len()
        self.nodes.append(n)
        self.siftUp(pos)

    fn siftUp(self, pos int):
        while pos != 0:
            var parent = (pos - 1) / 2
            if self.nodes[parent].value < self.nodes[pos].value:
                self.swap(pos, parent)
                pos = parent

    fn siftDown(self, pos int):
        while:
            var left = pos * 2 + 1
            if left >= self.nodes.len():
                break
            if self.nodes[left].value > self.nodes[pos].value:
                self.swap(pos, left)
                pos = left
                continue
            var right = left + 1
            if right >= self.nodes.len():
                break
            if self.nodes[right].value > self.nodes[pos].value:
                self.swap(pos, right)
                pos = right
                continue
            break

    fn swap(self, a, b int):
        var temp = self.nodes[a]
        self.nodes[a] = self.nodes[b]
        self.nodes[b] = temp

    fn pop(self) Node:
        var top = self.nodes[0]
        self.nodes[0] = self.nodes[self.nodes.len()-1]
        self.nodes.remove(self.nodes.len()-1)
        self.siftDown(0)
        return top

var h = Heap.new()
for 1..20000 -> i:
    h.insert(Node{value=i}) 

var sum = 0
for 1..20000 -> i:
    sum = sum + h.pop().value

print "time: ${(os.now() - start) * 1000}"
print sum
