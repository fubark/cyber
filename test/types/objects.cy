use t 'test'

type Node:
    value int

-- `object` keyword is optional.
type NodeA object:
    value2 int

var .snode = Node{value: 123}

-- Initialize.
var n1 = Node{value: 123}
t.eq(n1.value, 123)
var n2 = NodeA{value2: 234}
t.eq(n2.value2, 234)

-- Initialize, infer type.
var left Node = {value: 123}
t.eq(left.value, 123)

-- Init and default field to int(0).
var n3 = NodeHeap{}
t.eq(n3.value, 0)

type NodeHeap:
    value dynamic

-- Init with heap value.
n3 = NodeHeap{value: [123]}
t.eq(n3.value[0], 123)

-- Get field from declared static var.
snode.value = 123
var f = func():
    return snode.value
t.eq(f(), 123)

-- Assign to object field.
n1 = Node{value: 123}
n1.value = 234
t.eq(n1.value, 234)

-- Op assign to object field.
var nw = Node{value: 123}
nw.value += 1
t.eq(nw.value, 124)

-- Assign to object field when the object is a temp local.
let nodes = [ Node{value: 123} ]
nodes[0].value = 234
t.eq(nodes[0].value, 234)

-- Op assign to object field when the object is a temp local.
nodes = [ Node{value: 123} ]
nodes[0].value += 1
t.eq(nodes[0].value, 124)

-- Set object field after declared as a static var.
snode.value = 123
f = func():
    snode.value = 234
    t.eq(snode.value, 234)
f()
t.eq(snode.value, 234)

-- Set to field with heap value.
n3 = NodeHeap{value: [123]}
n3.value = 234
t.eq(n3.value, 234)

-- Struct to string returns struct's name. 
n1 = Node{value: 123}
t.eq(String(n1), 'Node')

type W:
    a any
    b any

-- Initialize fields with commas.
var w = W{a: 1, b: 2}
t.eq(w.a, 1)
t.eq(w.b, 2)

-- Initialize fields with commas and newlines.
w = W{
  a: 1,
  b: 2,
}
t.eq(w.a, 1)
t.eq(w.b, 2)

-- Big structs (allocated outside of heap pages).
type BigNode:
    a any
    b any
    c any
    d any
    e any
var n4 = BigNode{a: 1, b: 2, c: 3, d: 4, e: 5}
t.eq(n4.a, 1)
t.eq(n4.b, 2)
t.eq(n4.c, 3)
t.eq(n4.d, 4)
t.eq(n4.e, 5)

-- Multiple structs with the same field names but different offsets.
type Node1:
    a any
    b any
type Node2:
    b any
    a any
type Node3:
    a any
    b any
var na = Node1{a: 1, b: 2}
var nb = Node2{a: 3, b: 4}
var nc = Node3{a: 5, b: 6}
t.eq(na.a, 1)
t.eq(na.b, 2)
t.eq(nb.a, 3)
t.eq(nb.b, 4)
t.eq(nc.a, 5)
t.eq(nc.b, 6)

-- Using Object sym as a value.
var sym = Node
t.eq(typeof(sym), metatype)
t.eq(String(sym), 'type: Node')

-- Dynamic variable.
let val = t.erase(Node{value: 123})
var dst Node = val
t.eq(dst.value, 123)

--cytest: pass