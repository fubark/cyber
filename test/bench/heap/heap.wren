class Node {
    left { _left }
    left=(v) { _left = v}
    right { _right }
    right=(v) { _right = v}
    parent { _parent }
    parent=(v) { _parent = v}
    value { _value }
    value=(v) { _value = v}

    construct new(left, right, parent, value) {
        _left = left
        _right = right
        _parent = parent
        _value = value
    }

    getLeftmost() {
        if (_left == null) {
            return this
        }
        return _left.getLeftmost()
    }

    getRightmost() {
        if (_right == null) {
            return this
        }
        return _right.getRightmost()
    }

    getLeftSibling() {
        if (_parent.right == this) {
            return _parent.left
        } else {
            return _parent.getLeftSibling2(1)
        }
    }

    getLeftSibling2(height) {
        if (_parent.right == this) {
            return _parent.left.getRightN(height)
        } else {
            return _parent.getLeftSibling2(height + 1)
        }
    }

    getRightSibling() {
        if (_parent.left == this) {
            return _parent.right
        } else {
            return _parent.getRightSibling2(1)
        }
    }

    getRightSibling2(height) {
        if (_parent.left == this) {
            return _parent.right.getLeftN(height)
        } else {
            return _parent.getRightSibling2(height + 1)
        }
    }

    getRightN(n) {
        if (n == 1) {
            return _right
        } else {
            return _right.getRightN(n - 1)
        }
    }

    getLeftN(n) {
        if (n == 1) {
            return _left
        } else {
            return _left.getLeftN(n - 1)
        }
    }
}

class Heap {
    root { _root }
    root=(v) { _root=v }
    size { _size }
    size=(v) { _size=v }
    last { _last }
    last=(v) { _last=v }

    construct new(root, size, last) {
        _root = root
        _size = size
        _last = last
    }
    
    insert(value) {
        if (_root == null) {
            _root = Node.new(null, null, null, value)
            _last = _root
            _size = 1
            return
        }
        var newN = null
        if (((_size + 1) & _size) == 0) {
            // Insert at left most node.
            var parent = _root.getLeftmost()
            newN = Node.new(null, null, parent, value)
            parent.left = newN
            _last = newN
        } else {
            // Insert after last node.
            if (_size % 2 == 0) {
                newN = Node.new(null, null, _last.parent, value)
                _last.parent.right = newN
                _last = newN
            } else {
                var sibling = _last.parent.getRightSibling()
                newN = Node.new(null, null, sibling, value)
                sibling.left = newN
                _last = newN
            }
        }
        _size = _size + 1
        this.siftUp(newN)
    }

    swapUp(node) {
        if (_last == node) {
            _last = node.parent
        }
        var parentSave = node.parent
        var parentLeft = parentSave.left
        var parentRight = parentSave.right
        var parentParent = parentSave.parent

        parentSave.left = node.left
        if (node.left != null) {
            node.left.parent = parentSave
        }
        parentSave.right = node.right
        if (node.right != null) {
            node.right.parent = parentSave
        }
        parentSave.parent = node

        if (parentLeft == node) {
            node.left = parentSave
            node.right = parentRight
            if (parentRight != null) {
                parentRight.parent = node
            }
        } else {
            node.left = parentLeft
            node.right = parentSave
            if (parentLeft != null) {
                parentLeft.parent = node
            }
        }
        node.parent = parentParent

        if (parentParent == null) {
            _root = node
        } else {
            if (parentParent.left == parentSave) {
                parentParent.left = node
            } else {
                parentParent.right = node
            }
        }
    }

    siftUp(node) {
        if (node.parent == null) {
            return
        }
        if (node.value > node.parent.value) {
            this.swapUp(node)
            if (_root != node) {
                this.siftUp(node)
            }
        }
    }

    siftDown(node) {
        if ((node.left != null) && node.left.value > node.value) {
            this.swapUp(node.left)
            this.siftDown(node)
        } else if ((node.right != null) && node.right.value > node.value) {
            this.swapUp(node.right)
            this.siftDown(node)
        }
    }

    popTop() {
        if (_size == 1) {
            var res = _root
            _root = null
            _last = null
            _size = 0
            return res
        }

        if (_size <= 3) {
            var top = _root
            this.swapUp(_last)
            top.parent = null
            if (_size == 3) {
                _root.right = null
                _last = _root.left
            } else {
                _root.left = null
                _last = _root
            }
            _size = _size - 1
            return top
        }

        var newLast = null
        if ((_size & (_size - 1)) == 0) {
            newLast = _root.getRightmost()
        } else {
            newLast = _last.getLeftSibling()
        }

        // Move last to top and remove last.
        _root.left.parent = _last
        _last.left = _root.left
        _root.right.parent = _last
        _last.right = _root.right

        if (_last.parent.left == _last) {
            _last.parent.left = null
        } else {
            _last.parent.right = null
        }
        _last.parent = null

        var top = _root
        _root = _last
        _last = newLast
        _size = _size - 1

        this.siftDown(_root)
        return top
    }
}

var h = Heap.new(null, 0, null)
for (i in 1...20000) {
    h.insert(i) 
}

var sum = 0
for (i in 1...20000) {
    sum = sum + h.popTop().value
}

System.print(sum)