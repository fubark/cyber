import t 'test'

type Node object:
  value

var snode: Node{ value: 123 }

-- Initialization.
n = Node{ value: 123 }
t.eq(n.value, 123)

-- Init and default field to none.
n = Node{}
t.eq(n.value, none)

-- Init with heap value.
n = Node{ value: [123] }
t.eq(n.value[0], 123)

-- Get field from declared static var.
snode.value = 123
f = func():
  static snode
  return snode.value
t.eq(f(), 123)

-- Assign to object field.
n = Node{ value: 123 }
n.value = 234
t.eq(n.value, 234)

-- Op assign to object field.
nw = Node{ value: 123 }
nw.value += 1
t.eq(nw.value, 124)

-- Assign to object field when the object is a temp local.
nodes = [ Node{ value: 123 } ]
nodes[0].value = 234
t.eq(nodes[0].value, 234)

-- Op assign to object field when the object is a temp local.
nodes = [ Node{ value: 123 } ]
nodes[0].value += 1
t.eq(nodes[0].value, 124)

-- Set object field after declared as a static var.
snode.value = 123
f = func():
  static snode
  snode.value = 234
  t.eq(snode.value, 234)
try f()
t.eq(snode.value, 234)

-- Set to field with heap value.
n = Node{ value: [123] }
n.value = 234
t.eq(n.value, 234)

-- Struct to string returns struct's name. 
n = Node{ value: 123 }
t.eq(string(n), 'Node')

-- Initialize fields without commas.
type W object:
  a
  b
w = W{
  a: 1
  b: 2
}
t.eq(w.a, 1)
t.eq(w.b, 2)

-- Initialize fields with commas.
w = W{ a: 1, b: 2 }
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
type BigNode object:
  a
  b
  c
  d
  e
n = BigNode{ a: 1, b: 2, c: 3, d: 4, e: 5 }
t.eq(n.a, 1)
t.eq(n.b, 2)
t.eq(n.c, 3)
t.eq(n.d, 4)
t.eq(n.e, 5)

-- Multiple structs with the same field names but different offsets.
type Node1 object:
  a
  b
type Node2 object:
  b
  a
type Node3 object:
  a
  b
n1 = Node1{ a: 1, b: 2 }
n2 = Node2{ a: 3, b: 4 }
n3 = Node3{ a: 5, b: 6 }
t.eq(n1.a, 1)
t.eq(n1.b, 2)
t.eq(n2.a, 3)
t.eq(n2.b, 4)
t.eq(n3.a, 5)
t.eq(n3.b, 6)

-- Using Object sym as a value.
sym = Node
t.eq(typesym(sym), #type)
t.eq(string(sym), 'Object Symbol (Node)')