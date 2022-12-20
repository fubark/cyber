Node = {}
Node.__index = Node

function Node:new(left, right, parent, value)
    local self = setmetatable({}, Node)
    self.left = left
    self.right = right
    self.parent = parent
    self.value = value
    return self
end

function Node:getLeftmost()
    if (self.left == null) then
        return self
    end
    return self.left:getLeftmost()
end

function Node:getRightmost()
    if (self.right == null) then
        return self
    end
    return self.right:getRightmost()
end

function Node:getLeftSibling()
    if (self.parent.right == self) then
        return self.parent.left
    else
        return self.parent:getLeftSibling2(1)
    end
end

function Node:getLeftSibling2(height)
    if (self.parent.right == self) then
        return self.parent.left:getRightN(height)
    else
        return self.parent:getLeftSibling2(height + 1)
    end
end

function Node:getRightSibling()
    if (self.parent.left == self) then
        return self.parent.right
    else
        return self.parent:getRightSibling2(1)
    end
end

function Node:getRightSibling2(height)
    if (self.parent.left == self) then
        return self.parent.right:getLeftN(height)
    else
        return self.parent:getRightSibling2(height + 1)
    end
end

function Node:getRightN(n) 
    if (n == 1) then
        return self.right
    else
        return self.right:getRightN(n - 1)
    end
end

function Node:getLeftN(n)
    if (n == 1) then
        return self.left
    else
        return self.left:getLeftN(n - 1)
    end
end

Heap = {}
Heap.__index = Heap

function Heap:new(root, size, last)
    local self = setmetatable({}, Heap)
    self.root = root
    self.size = size
    self.last = last
    return self
end
    
function Heap:insert(value) 
    if (self.root == null) then
        self.root = Node:new(null, null, null, value)
        self.last = self.root
        self.size = 1
        return
    end
    local newN
    if ((self.size + 1) & self.size == 0) then
        -- Insert at left most node.
        local parent = self.root:getLeftmost()
        newN = Node:new(null, null, parent, value)
        parent.left = newN
        self.last = newN
    else
        -- Insert after last node.
        if (self.size % 2 == 0) then
            newN = Node:new(null, null, self.last.parent, value)
            self.last.parent.right = newN
            self.last = newN
        else
            local sibling = self.last.parent:getRightSibling()
            newN = Node:new(null, null, sibling, value)
            sibling.left = newN
            self.last = newN
        end
    end
    self.size = self.size + 1
    self:siftUp(newN)
end

function Heap:swapUp(node)
    if (self.last == node) then
        self.last = node.parent
    end
    local parentSave = node.parent
    local parentLeft = parentSave.left
    local parentRight = parentSave.right
    local parentParent = parentSave.parent

    parentSave.left = node.left
    if (node.left ~= null) then
        node.left.parent = parentSave
    end
    parentSave.right = node.right
    if (node.right ~= null) then
        node.right.parent = parentSave
    end
    parentSave.parent = node

    if (parentLeft == node) then
        node.left = parentSave
        node.right = parentRight
        if (parentRight ~= null) then
            parentRight.parent = node
        end
    else
        node.left = parentLeft
        node.right = parentSave
        if (parentLeft ~= null) then
            parentLeft.parent = node
        end
    end
    node.parent = parentParent

    if (parentParent == null) then
        self.root = node
    else
        if (parentParent.left == parentSave) then
            parentParent.left = node
        else
            parentParent.right = node
        end
    end
end

function Heap:siftUp(node)
    if (node.parent == null) then
        return
    end
    if (node.value > node.parent.value) then
        self:swapUp(node)
        if (self.root ~= node) then
            self:siftUp(node)
        end
    end
end

function Heap:siftDown(node)
    if ((node.left ~= null) and node.left.value > node.value) then
        self:swapUp(node.left)
        self:siftDown(node)
    elseif ((node.right ~= null) and node.right.value > node.value) then
        self:swapUp(node.right)
        self:siftDown(node)
    end
end

function Heap:popTop()
    if (self.size == 1) then
        local res = self.root
        self.root = null
        self.last = null
        self.size = 0
        return res
    end

    if (self.size <= 3) then
        local top = self.root
        self:swapUp(self.last)
        top.parent = null
        if (self.size == 3) then
            self.root.right = null
            self.last = self.root.left
        else
            self.root.left = null
            self.last = self.root
        end
        self.size = self.size - 1
        return top
    end

    local newLast
    if (self.size & (self.size - 1) == 0) then
        newLast = self.root:getRightmost()
    else
        newLast = self.last:getLeftSibling()
    end

    -- Move last to top and remove last.
    self.root.left.parent = self.last
    self.last.left = self.root.left
    self.root.right.parent = self.last
    self.last.right = self.root.right

    if (self.last.parent.left == self.last) then
        self.last.parent.left = null
    else
        self.last.parent.right = null
    end
    self.last.parent = null

    local top = self.root
    self.root = self.last
    self.last = newLast
    self.size = self.size - 1

    self:siftDown(self.root)
    return top
end

local h = Heap:new(null, 0, null)
for i = 1, 20000-1 do
    h:insert(i) 
end

local sum = 0
for i = 1, 20000-1 do
    sum = sum + h:popTop().value
end

io.write(sum .. "\n")