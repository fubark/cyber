import t 'test'

type Node:
    var value int

-- `object` keyword is optional.
type NodeA object:
    var value2 int

var .snode = [Node value: 123]

-- Initialize.
my n = [Node value: 123]
t.eq(n.value, 123)
n = [NodeA value2: 234]
t.eq(n.value2, 234)

-- Initialize, infer type.
var left Node = [value: 123]
t.eq(left.value, 123)

-- Init and default field to none.
n = [NodeHeap:]
t.eq(n.value, none)

type NodeHeap:
    var value any

-- Init with heap value.
n = [NodeHeap value: [123]]
t.eq(n.value[0], 123)

-- Get field from declared static var.
snode.value = 123
var f = func():
    return snode.value
t.eq(f(), 123)

-- Assign to object field.
n = [Node value: 123]
n.value = 234
t.eq(n.value, 234)

-- Op assign to object field.
var nw = [Node value: 123]
nw.value += 1
t.eq(nw.value, 124)

-- Assign to object field when the object is a temp local.
var nodes = [ [Node value: 123] ]
nodes[0].value = 234
t.eq(nodes[0].value, 234)

-- Op assign to object field when the object is a temp local.
nodes = [ [Node value: 123] ]
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
n = [NodeHeap value: [123]]
n.value = 234
t.eq(n.value, 234)

-- Struct to string returns struct's name. 
n = [Node value: 123]
t.eq(String(n), 'Node')

type W:
    var a
    var b

-- Initialize fields with commas.
var w = [W a: 1, b: 2]
t.eq(w.a, 1)
t.eq(w.b, 2)

-- Initialize fields with commas and newlines.
w = [W
  a: 1,
  b: 2,
]
t.eq(w.a, 1)
t.eq(w.b, 2)

-- Big structs (allocated outside of heap pages).
type BigNode:
    var a
    var b
    var c
    var d
    var e
n = [BigNode a: 1, b: 2, c: 3, d: 4, e: 5]
t.eq(n.a, 1)
t.eq(n.b, 2)
t.eq(n.c, 3)
t.eq(n.d, 4)
t.eq(n.e, 5)

-- Multiple structs with the same field names but different offsets.
type Node1:
    var a
    var b
type Node2:
    var b
    var a
type Node3:
    var a
    var b
var n1 = [Node1 a: 1, b: 2]
var n2 = [Node2 a: 3, b: 4]
var n3 = [Node3 a: 5, b: 6]
t.eq(n1.a, 1)
t.eq(n1.b, 2)
t.eq(n2.a, 3)
t.eq(n2.b, 4)
t.eq(n3.a, 5)
t.eq(n3.b, 6)

-- Using Object sym as a value.
var sym = Node
t.eq(typesym(sym), .metatype)
t.eq(String(sym), 'type: Node')

--cytest: pass