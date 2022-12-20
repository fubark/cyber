class Node:
    def __init__(self, left, right, parent, value):
        self.left = left
        self.right = right
        self.parent = parent
        self.value = value

    def getLeftmost(self):
        if self.left is None:
            return self
        return self.left.getLeftmost()

    def getRightmost(self):
        if self.right is None:
            return self
        return self.right.getRightmost()

    def getLeftSibling(self):
        if self.parent.right is self:
            return self.parent.left
        else:
            return self.parent.getLeftSibling2(1)

    def getLeftSibling2(self, height):
        if self.parent.right is self:
            return self.parent.left.getRightN(height)
        else:
            return self.parent.getLeftSibling2(height + 1)

    def getRightSibling(self):
        if self.parent.left is self:
            return self.parent.right
        else:
            return self.parent.getRightSibling2(1)

    def getRightSibling2(self, height):
        if self.parent.left is self:
            return self.parent.right.getLeftN(height)
        else:
            return self.parent.getRightSibling2(height + 1)

    def getRightN(self, n):
        if n == 1:
            return self.right
        else:
            return self.right.getRightN(n - 1)

    def getLeftN(self, n):
        if n == 1:
            return self.left
        else:
            return self.left.getLeftN(n - 1)

class Heap:

    def __init__(self):
        self.root = None
        self.size = 0
        self.last = None
    
    def insert(self, value):
        if self.root is None:
            self.root = Node(None, None, None, value)
            self.last = self.root
            self.size = 1
            return
        if (self.size + 1) & self.size == 0:
            # Insert at left most node.
            parent = self.root.getLeftmost()
            new = Node(None, None, parent, value)
            parent.left = new
            self.last = new
        else:
            # Insert after last node.
            if self.size % 2 == 0:
                new = Node(None, None, self.last.parent, value)
                self.last.parent.right = new
                self.last = new
            else:
                sibling = self.last.parent.getRightSibling()
                new = Node(None, None, sibling, value)
                sibling.left = new
                self.last = new
        self.size = self.size + 1
        self.siftUp(new)

    def swapUp(self, node):
        if self.last is node:
            self.last = node.parent
        parentSave = node.parent
        parentLeft = parentSave.left
        parentRight = parentSave.right
        parentParent = parentSave.parent

        parentSave.left = node.left
        if node.left is not None:
            node.left.parent = parentSave
        parentSave.right = node.right
        if node.right is not None:
            node.right.parent = parentSave
        parentSave.parent = node

        if parentLeft is node:
            node.left = parentSave
            node.right = parentRight
            if parentRight is not None:
                parentRight.parent = node
        else:
            node.left = parentLeft
            node.right = parentSave
            if parentLeft is not None:
                parentLeft.parent = node
        node.parent = parentParent

        if parentParent is None:
            self.root = node
        else:
            if parentParent.left is parentSave:
                parentParent.left = node
            else:
                parentParent.right = node

    def siftUp(self, node):
        if node.parent is None:
            return
        if node.value > node.parent.value:
            self.swapUp(node)
            if self.root is not node:
                self.siftUp(node)

    def siftDown(self, node):
        if (node.left is not None) and node.left.value > node.value:
            self.swapUp(node.left)
            self.siftDown(node)
        elif (node.right is not None) and node.right.value > node.value:
            self.swapUp(node.right)
            self.siftDown(node)

    def popTop(self):
        if self.size == 1:
            res = self.root
            self.root = None
            self.last = None
            self.size = 0
            return res

        if self.size <= 3:
            top = self.root
            self.swapUp(self.last)
            top.parent = None
            if self.size == 3:
                self.root.right = None
                self.last = self.root.left
            else:
                self.root.left = None
                self.last = self.root
            self.size = self.size - 1
            return top

        if self.size & (self.size - 1) == 0:
            newLast = self.root.getRightmost()
        else:
            newLast = self.last.getLeftSibling()

        # Move last to top and remove last.
        self.root.left.parent = self.last
        self.last.left = self.root.left
        self.root.right.parent = self.last
        self.last.right = self.root.right

        if self.last.parent.left is self.last:
            self.last.parent.left = None
        else:
            self.last.parent.right = None
        self.last.parent = None

        top = self.root
        self.root = self.last
        self.last = newLast
        self.size = self.size - 1

        self.siftDown(self.root)
        return top

h = Heap()
for i in range(1, 20000):
    h.insert(i) 

sum = 0
for i in range(1, 20000):
    sum = sum + h.popTop().value

print(sum)