use t 'test'

type Node:
    value int

global snode ^Node = ^Node{value=123}

-- Initialize.
n1 := ^Node{value=123}

-- Access member.
t.eq(n1.value, 123)

-- Deref.
dn := n1.*
t.eqType(Node, type.of(dn))
t.eq(dn.value, 123)

-- -- Initialize, infer type.
-- left ^Node := {value=123}
-- t.eq(left.value, 123)

type NodeHeap:
    n ^Node

-- Init with heap value.
n3 := ^NodeHeap{n=^Node{value=123}}
t.eq(n3.n.value, 123)

-- Get field from declared static var.
snode.value = 123
f := fn() -> int:
    return snode.value
t.eq(f(), 123)

-- Assign to object field.
n1 = ^Node{value=123}
n1.value = 234
t.eq(n1.value, 234)

-- Op assign to object field.
nw := ^Node{value=123}
nw.value += 1
t.eq(nw.value, 124)

-- Assign to object field where the object comes from an expression.
nodes := []^Node{ ^Node{value=123} }
nodes[0].value = 234
t.eq(nodes[0].value, 234)

-- Op assign to object field where the object comes from an expression.
nodes = { ^Node{value=123} }
nodes[0].value += 1
t.eq(nodes[0].value, 124)

-- Set object field after declared as a static var.
snode.value = 123
f2 := fn():
    snode.value = 234
    t.eq(snode.value, 234)
f2()
t.eq(snode.value, 234)

-- Set to field with heap value.
n3 = ^NodeHeap{n=^Node{value=123}}
n3.n = ^Node{value=234}
t.eq(n3.n.value, 234)

-- To string returns struct's name. 
n1 = ^Node{value=123}
t.eq('^Node', to_print_string(n1))

type W:
    a int
    b int

-- Initialize fields with commas.
w := ^W{a=1, b=2}
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
    a int
    b int
    c int
    d int
    e int
n5 := ^BigNode{a=1, b=2, c=3, d=4, e=5}
t.eq(n5.a, 1)
t.eq(n5.b, 2)
t.eq(n5.c, 3)
t.eq(n5.d, 4)
t.eq(n5.e, 5)

-- Downcast to primitive type.
a := Object(^1)
t.eq(1, a.downcast(^int).*)

-- Downcast to struct type.
a = Object(^Foo{a=123})
t.eq(123, a.downcast(^Foo).a)
type Foo:
    a int

-- Downcast to string type.
a = Object(^'abc')
t.eq('abc', a.downcast(^str).*)

-- Downcast from global.
global sa Object = Object(^'abc')
t.eq('abc', sa.downcast(^str).*)
sa = Object(^1)
t.eq(1, sa.downcast(^int).*)

-- Object.type() for builtin types.
t.eq(2, Object(^true).type())
t.eq(13, Object(^1.23).type())
t.eq(6, Object(^123).type())
t.eq(23, Object(^'abc').type())
t.eq(15, Object(^@sym).type())
t.eq(14, Object(^error.oops).type())

--cytest: pass