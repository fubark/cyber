type Node:
    left
    right
    parent
    value

    func getLeftmost(self):
        if self.left is none:
            return self
        return self.left.getLeftmost()

    func getRightmost(self):
        if self.right is none:
            return self
        return self.right.getRightmost()

    func getLeftSibling(self):
        if self.parent.right is self:
            return self.parent.left
        else:
            return self.parent.getLeftSibling2(1)

    func getLeftSibling2(self, height):
        if self.parent.right is self:
            return self.parent.left.getRightN(height)
        else:
            return self.parent.getLeftSibling2(height + 1)

    func getRightSibling(self):
        if self.parent.left is self:
            return self.parent.right
        else:
            return self.parent.getRightSibling2(1)

    func getRightSibling2(self, height):
        if self.parent.left is self:
            return self.parent.right.getLeftN(height)
        else:
            return self.parent.getRightSibling2(height + 1)

    func getRightN(self, n):
        if n is 1:
            return self.right
        else:
            return self.right.getRightN(n - 1)

    func getLeftN(self, n):
        if n is 1:
            return self.left
        else:
            return self.left.getLeftN(n - 1)

type Heap:
    root
    size
    last

    func new():
        return Heap{ root: none, size: 0, last: none }
    
    func insert(self, value):
        if self.root is none:
            self.root = Node{ left: none, right: none, parent: none, value: value }
            self.last = self.root
            self.size = 1
            return
        if (self.size + 1) & self.size is 0:
            -- Insert at left most node.
            parent = self.root.getLeftmost()
            new = Node{
                left: none
                right: none
                parent: parent
                value: value
            }
            parent.left = new
            self.last = new
        else:
            -- Insert after last node.
            if self.size % 2 is 0:
                new = Node{
                    left: none
                    right: none
                    parent: self.last.parent
                    value: value
                }
                self.last.parent.right = new
                self.last = new
            else:
                sibling = self.last.parent.getRightSibling()
                new = Node{
                    left: none
                    right: none
                    parent: sibling
                    value: value
                }
                sibling.left = new
                self.last = new
        self.size = self.size + 1
        self.siftUp(new)

    func swapUp(self, node):
        if self.last is node:
            self.last = node.parent
        parentSave = node.parent
        parentLeft = parentSave.left
        parentRight = parentSave.right
        parentParent = parentSave.parent

        parentSave.left = node.left
        if node.left is not none:
            node.left.parent = parentSave
        parentSave.right = node.right
        if node.right is not none:
            node.right.parent = parentSave
        parentSave.parent = node

        if parentLeft is node:
            node.left = parentSave
            node.right = parentRight
            if parentRight is not none:
                parentRight.parent = node
        else:
            node.left = parentLeft
            node.right = parentSave
            if parentLeft is not none:
                parentLeft.parent = node
        node.parent = parentParent

        if parentParent is none:
            self.root = node
        else:
            if parentParent.left is parentSave:
                parentParent.left = node
            else:
                parentParent.right = node

    func siftUp(self, node):
        if node.parent is none:
            return
        if node.value > node.parent.value:
            self.swapUp(node)
            if self.root is not node:
                self.siftUp(node)

    func siftDown(self, node):
        if (node.left is not none) and node.left.value > node.value:
            self.swapUp(node.left)
            self.siftDown(node)
        else (node.right is not none) and node.right.value > node.value:
            self.swapUp(node.right)
            self.siftDown(node)

    func popTop(self):
        if self.size is 1:
            res = self.root
            self.root = none
            self.last = none
            self.size = 0
            return res

        if self.size <= 3:
            top = self.root
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

        if self.size & (self.size - 1) is 0:
            newLast = self.root.getRightmost()
        else:
            newLast = self.last.getLeftSibling()

        -- Move last to top and remove last.
        self.root.left.parent = self.last
        self.last.left = self.root.left
        self.root.right.parent = self.last
        self.last.right = self.root.right

        if self.last.parent.left is self.last:
            self.last.parent.left = none
        else:
            self.last.parent.right = none
        self.last.parent = none

        top = self.root
        self.root = self.last
        self.last = newLast
        self.size = self.size - 1

        self.siftDown(self.root)
        return top

h = Heap.new()
for 1..20000 as i:
    h.insert(i) 

sum = 0
for 1..20000 as i:
    sum = sum + h.popTop().value

print(sum)