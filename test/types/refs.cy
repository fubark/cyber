use t 'test'

type Node:
    value int

-- var .snode = ^Node{value=123}

-- Initialize.
var n1 = ^Node{value=123}

-- Access member.
t.eq(n1.value, 123)

-- Deref.
var dn = n1.*
t.eq(typeOf(dn), Node)
t.eq(dn.value, 123)

-- -- Initialize, infer type.
-- var left ^Node = .{value=123}
-- t.eq(left.value, 123)

type NodeHeap:
    value List[int]

-- Init with heap value.
var n3 = ^NodeHeap{value=.{123}}
t.eq(n3.value[0], 123)

type NodeHeap2:
    value int

-- Init and default field to int(0).
var n4 = ^NodeHeap2{}
t.eq(n4.value, 0)

-- -- Get field from declared static var.
-- snode.value = 123
-- var f = func() int:
--     return snode.value
-- t.eq(f(), 123)

-- Assign to object field.
n1 = ^Node{value=123}
n1.value = 234
t.eq(n1.value, 234)

-- Op assign to object field.
var nw = ^Node{value=123}
nw.value += 1
t.eq(nw.value, 124)

-- Assign to object field where the object comes from an expression.
var nodes = List[^Node]{ ^Node{value=123} }
nodes[0].value = 234
t.eq(nodes[0].value, 234)

-- Op assign to object field where the object comes from an expression.
nodes = .{ ^Node{value=123} }
nodes[0].value += 1
t.eq(nodes[0].value, 124)

-- -- Set object field after declared as a static var.
-- snode.value = 123
-- var f2 = func():
--     snode.value = 234
--     t.eq(snode.value, 234)
-- f2()
-- t.eq(snode.value, 234)

-- Set to field with heap value.
n3 = ^NodeHeap{value=.{123}}
n3.value = .{}
t.eq(n3.value.len(), 0)

-- To string returns struct's name. 
n1 = ^Node{value=123}
t.eq(string(n1), '^Node')

type W:
    a any
    b any

-- Initialize fields with commas.
var w = ^W{a=1, b=2}
t.eq(w.a, 1)
t.eq(w.b, 2)

-- Initialize fields with commas and newlines.
w = ^W{
  a = 1,
  b = 2,
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
var n5 = ^BigNode{a=1, b=2, c=3, d=4, e=5}
t.eq(n5.a, 1)
t.eq(n5.b, 2)
t.eq(n5.c, 3)
t.eq(n5.d, 4)
t.eq(n5.e, 5)

-- Using ref type as a value.
var sym = ^Node
t.eq(string(sym), 'type: ^Node')

-- Dynamic variable.
dyn val = ^Node{value=123}
var dst ^Node = val
t.eq(dst.value, 123)

--cytest: pass