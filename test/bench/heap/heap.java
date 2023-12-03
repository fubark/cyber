class Node {
    Node left;
    Node right;
    Node parent;
    int value;

    Node(Node left, Node right, Node parent, int value) {
        this.left = left;
        this.right = right;
        this.parent = parent;
        this.value = value;
    }

    Node getLeftmost() {
        if (this.left == null) {
            return this;
        }
        return this.left.getLeftmost();
    }

    Node getRightmost() {
        if (this.right == null) {
            return this;
        }
        return this.right.getRightmost();
    }

    Node getLeftSibling() {
        if (this.parent.right == this) {
            return this.parent.left;
        } else {
            return this.parent.getLeftSibling2(1);
        }
    }

    Node getLeftSibling2(int height) {
        if (this.parent.right == this) {
            return this.parent.left.getRightN(height);
        } else {
            return this.parent.getLeftSibling2(height + 1);
        }
    }

    Node getRightSibling() {
        if (this.parent.left == this) {
            return this.parent.right;
        } else {
            return this.parent.getRightSibling2(1);
        }
    }

    Node getRightSibling2(int height) {
        if (this.parent.left == this) {
            return this.parent.right.getLeftN(height);
        } else {
            return this.parent.getRightSibling2(height + 1);
        }
    }

    Node getRightN(int n) {
        if (n == 1) {
            return this.right;
        } else {
            return this.right.getRightN(n - 1);
        }
    }

    Node getLeftN(int n) {
        if (n == 1) {
            return this.left;
        } else {
            return this.left.getLeftN(n - 1);
        }
    }
}

class Heap {
    Node root;
    int size;
    Node last;

    Heap(Node root, int size, Node last) {
        this.root = root;
        this.size = size;
        this.last = last;
    }
    
    void insert(int value) {
        if (this.root == null) {
            this.root = new Node(null, null, null, value);
            this.last = this.root;
            this.size = 1;
            return;
        }
        Node newN;
        if (((this.size + 1) & this.size) == 0) {
            // Insert at left most node.
            var parent = this.root.getLeftmost();
            newN = new Node(null, null, parent, value);
            parent.left = newN;
            this.last = newN;
        } else {
            // Insert after last node.
            if (this.size % 2 == 0) {
                newN = new Node(null, null, this.last.parent, value);
                this.last.parent.right = newN;
                this.last = newN;
            } else {
                var sibling = this.last.parent.getRightSibling();
                newN = new Node(null, null, sibling, value);
                sibling.left = newN;
                this.last = newN;
            }
        }
        this.size = this.size + 1;
        this.siftUp(newN);
    }

    void swapUp(Node node) {
        if (this.last == node) {
            this.last = node.parent;
        }
        var parentSave = node.parent;
        var parentLeft = parentSave.left;
        var parentRight = parentSave.right;
        var parentParent = parentSave.parent;

        parentSave.left = node.left;
        if (node.left != null) {
            node.left.parent = parentSave;
        }
        parentSave.right = node.right;
        if (node.right != null) {
            node.right.parent = parentSave;
        }
        parentSave.parent = node;

        if (parentLeft == node) {
            node.left = parentSave;
            node.right = parentRight;
            if (parentRight != null) {
                parentRight.parent = node;
            }
        } else {
            node.left = parentLeft;
            node.right = parentSave;
            if (parentLeft != null) {
                parentLeft.parent = node;
            }
        }
        node.parent = parentParent;

        if (parentParent == null) {
            this.root = node;
        } else {
            if (parentParent.left == parentSave) {
                parentParent.left = node;
            } else {
                parentParent.right = node;
            }
        }
    }

    void siftUp(Node node) {
        if (node.parent == null) {
            return;
        }
        if (node.value > node.parent.value) {
            this.swapUp(node);
            if (this.root != node) {
                this.siftUp(node);
            }
        }
    }

    void siftDown(Node node) {
        if ((node.left != null) && node.left.value > node.value) {
            this.swapUp(node.left);
            this.siftDown(node);
        } else if ((node.right != null) && node.right.value > node.value) {
            this.swapUp(node.right);
            this.siftDown(node);
        }
    }

    Node popTop() {
        if (this.size == 1) {
            var res = this.root;
            this.root = null;
            this.last = null;
            this.size = 0;
            return res;
        }

        if (this.size <= 3) {
            var top = this.root;
            this.swapUp(this.last);
            top.parent = null;
            if (this.size == 3) {
                this.root.right = null;
                this.last = this.root.left;
            } else {
                this.root.left = null;
                this.last = this.root;
            }
            this.size = this.size - 1;
            return top;
        }

        Node newLast;
        if ((this.size & (this.size - 1)) == 0) {
            newLast = this.root.getRightmost();
        } else {
            newLast = this.last.getLeftSibling();
        }

        // Move last to top and remove last.
        this.root.left.parent = this.last;
        this.last.left = this.root.left;
        this.root.right.parent = this.last;
        this.last.right = this.root.right;

        if (this.last.parent.left == this.last) {
            this.last.parent.left = null;
        } else {
            this.last.parent.right = null;
        }
        this.last.parent = null;

        var top = this.root;
        this.root = this.last;
        this.last = newLast;
        this.size = this.size - 1;

        this.siftDown(this.root);
        return top;
    }
}

class HeapMain {
    public static void main(String args[]) {
        long start = System.currentTimeMillis();

        Heap h = new Heap(null, 0, null);
        for (int i = 1; i < 20000; i++) {
            h.insert(i);
        }
        int sum = 0;
        for (var i = 1; i < 20000; i++) {
            sum = sum + h.popTop().value;
        }
        
        System.out.println("time: " + (System.currentTimeMillis() - start));
        System.out.println(sum);
    }
}