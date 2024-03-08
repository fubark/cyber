import os

var start = os.now()

type Node:
    var left   dynamic
    var right  dynamic
    var parent dynamic
    var value  dynamic

    func getLeftmost():
        if self.left == none:
            return self
        return self.left.getLeftmost()

    func getRightmost():
        if self.right == none:
            return self
        return self.right.getRightmost()

    func getLeftSibling():
        if self.parent.right == self:
            return self.parent.left
        else:
            return self.parent.getLeftSibling2(1)

    func getLeftSibling2(height):
        if self.parent.right == self:
            return self.parent.left.getRightN(height)
        else:
            return self.parent.getLeftSibling2(height + 1)

    func getRightSibling():
        if self.parent.left == self:
            return self.parent.right
        else:
            return self.parent.getRightSibling2(1)

    func getRightSibling2(height):
        if self.parent.left == self:
            return self.parent.right.getLeftN(height)
        else:
            return self.parent.getRightSibling2(height + 1)

    func getRightN(n):
        if n == 1:
            return self.right
        else:
            return self.right.getRightN(n - 1)

    func getLeftN(n):
        if n == 1:
            return self.left
        else:
            return self.left.getLeftN(n - 1)

func Heap.new() Heap:
    return [Heap root: none, size: 0, last: none]

type Heap:
    var root dynamic
    var size dynamic
    var last dynamic

    func insert(value):
        if self.root == none:
            self.root = [Node left: none, right: none, parent: none, value: value]
            self.last = self.root
            self.size = 1
            return
        my new = none
        if int(self.size + 1) & int(self.size) == 0:
            -- Insert at left most node.
            my parent = self.root.getLeftmost()
            new = [Node
                left: none,
                right: none,
                parent: parent,
                value: value,
            ]
            parent.left = new
            self.last = new
        else:
            -- Insert after last node.
            if self.size % 2 == 0:
                new = [Node
                    left: none,
                    right: none,
                    parent: self.last.parent,
                    value: value,
                ]
                self.last.parent.right = new
                self.last = new
            else:
                my sibling = self.last.parent.getRightSibling()
                new = [Node
                    left: none,
                    right: none,
                    parent: sibling,
                    value: value,
                ]
                sibling.left = new
                self.last = new
        self.size = self.size + 1
        self.siftUp(new)

    func swapUp(node):
        if self.last == node:
            self.last = node.parent
        my parentSave = node.parent
        my parentLeft = parentSave.left
        my parentRight = parentSave.right
        my parentParent = parentSave.parent

        parentSave.left = node.left
        if node.left != none:
            node.left.parent = parentSave
        parentSave.right = node.right
        if node.right != none:
            node.right.parent = parentSave
        parentSave.parent = node

        if parentLeft == node:
            node.left = parentSave
            node.right = parentRight
            if parentRight != none:
                parentRight.parent = node
        else:
            node.left = parentLeft
            node.right = parentSave
            if parentLeft != none:
                parentLeft.parent = node
        node.parent = parentParent

        if parentParent == none:
            self.root = node
        else:
            if parentParent.left == parentSave:
                parentParent.left = node
            else:
                parentParent.right = node

    func siftUp(node):
        if node.parent == none:
            return
        if node.value > node.parent.value:
            self.swapUp(node)
            if self.root != node:
                self.siftUp(node)

    func siftDown(node):
        if node.left != none and node.left.value > node.value:
            self.swapUp(node.left)
            self.siftDown(node)
        else node.right != none and node.right.value > node.value:
            self.swapUp(node.right)
            self.siftDown(node)

    func popTop():
        if self.size == 1:
            var res = self.root
            self.root = none
            self.last = none
            self.size = 0
            return res

        if self.size <= 3:
            my top = self.root
            self.swapUp(self.last)
            top.parent = none
            if self.size == 3:
                self.root.right = none
                self.last = self.root.left
            else:
                self.root.left = none
                self.last = self.root
            self.size = self.size - 1
            return top

        var newLast = none
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
            self.last.parent.left = none
        else:
            self.last.parent.right = none
        self.last.parent = none

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
