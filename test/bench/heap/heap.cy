use os

var start = os.now()

type Node:
    left   dyn
    right  dyn
    parent dyn
    value  dyn

    func getLeftmost(self) dyn:
        if self.left == false:
            return self
        return self.left.getLeftmost()

    func getRightmost(self) dyn:
        if self.right == false:
            return self
        return self.right.getRightmost()

    func getLeftSibling(self) dyn:
        if self.parent.right == self:
            return self.parent.left
        else:
            return self.parent.getLeftSibling2(1)

    func getLeftSibling2(self, height int) dyn:
        if self.parent.right == self:
            return self.parent.left.getRightN(height)
        else:
            return self.parent.getLeftSibling2(height + 1)

    func getRightSibling(self) dyn:
        if self.parent.left == self:
            return self.parent.right
        else:
            return self.parent.getRightSibling2(1)

    func getRightSibling2(self, height int) dyn:
        if self.parent.left == self:
            return self.parent.right.getLeftN(height)
        else:
            return self.parent.getRightSibling2(height + 1)

    func getRightN(self, n int) dyn:
        if n == 1:
            return self.right
        else:
            return self.right.getRightN(n - 1)

    func getLeftN(self, n int) dyn:
        if n == 1:
            return self.left
        else:
            return self.left.getLeftN(n - 1)

func Heap.new() Heap:
    return Heap{root=false, size=0, last=false}

type Heap:
    root dyn
    size dyn
    last dyn

    func insert(self, value int):
        if self.root == false:
            self.root = Node{left=false, right=false, parent=false, value=value}
            self.last = self.root
            self.size = 1
            return
        dyn new = false
        if int(self.size + 1) & int(self.size) == 0:
            -- Insert at left most node.
            dyn parent = self.root.getLeftmost()
            new = Node{
                left   = false,
                right  = false,
                parent = parent,
                value  = value,
            }
            parent.left = new
            self.last = new
        else:
            -- Insert after last node.
            if self.size % 2 == 0:
                new = Node{
                    left   = false,
                    right  = false,
                    parent = self.last.parent,
                    value  = value,
                }
                self.last.parent.right = new
                self.last = new
            else:
                dyn sibling = self.last.parent.getRightSibling()
                new = Node{
                    left   = false,
                    right  = false,
                    parent = sibling,
                    value  = value,
                }
                sibling.left = new
                self.last = new
        self.size = self.size + 1
        self.siftUp(new)

    func swapUp(self, node Node):
        if self.last == node:
            self.last = node.parent
        dyn parentSave = node.parent
        dyn parentLeft = parentSave.left
        dyn parentRight = parentSave.right
        dyn parentParent = parentSave.parent

        parentSave.left = node.left
        if node.left != false:
            node.left.parent = parentSave
        parentSave.right = node.right
        if node.right != false:
            node.right.parent = parentSave
        parentSave.parent = node

        if parentLeft == node:
            node.left = parentSave
            node.right = parentRight
            if parentRight != false:
                parentRight.parent = node
        else:
            node.left = parentLeft
            node.right = parentSave
            if parentLeft != false:
                parentLeft.parent = node
        node.parent = parentParent

        if parentParent == false:
            self.root = node
        else:
            if parentParent.left == parentSave:
                parentParent.left = node
            else:
                parentParent.right = node

    func siftUp(self, node Node):
        if node.parent == false:
            return
        if node.value > node.parent.value:
            self.swapUp(node)
            if self.root != node:
                self.siftUp(node)

    func siftDown(self, node Node):
        if node.left != false and node.left.value > node.value:
            self.swapUp(node.left)
            self.siftDown(node)
        else node.right != false and node.right.value > node.value:
            self.swapUp(node.right)
            self.siftDown(node)

    func popTop(self) Node:
        if self.size == 1:
            var res = self.root
            self.root = false
            self.last = false
            self.size = 0
            return res

        if self.size <= 3:
            dyn top = self.root
            self.swapUp(self.last)
            top.parent = false
            if self.size == 3:
                self.root.right = false
                self.last = self.root.left
            else:
                self.root.left = false
                self.last = self.root
            self.size = self.size - 1
            return top

        dyn newLast = false
        if int(self.size) & int(self.size - 1) == 0:
            newLast = self.root.getRightmost()
        else:
            newLast = self.last.getLeftSibling()

        -- Move last to top and remove last.
        self.root.left.parent = self.last
        self.last.left = self.root.left
        self.root.right.parent = self.last
        self.last.right = self.root.right

        if self.last.parent.left == self.last:
            self.last.parent.left = false
        else:
            self.last.parent.right = false
        self.last.parent = false

        var top = self.root
        self.root = self.last
        self.last = newLast
        self.size = self.size - 1

        self.siftDown(self.root)
        return top

var h = Heap.new()
for 1..20000 -> i:
    h.insert(i) 

var sum = 0
for 1..20000 -> i:
    sum = sum + h.popTop().value

print("time: $((os.now() - start) * 1000)")
print(sum)
