use t 'test'

type Node:
    value int

fn (&Node) get() -> int:
    return $value

fn (&Node) get2(param int) -> int:
    return $value + param

fn (&Node) get3(param int, param2 int) -> int:
    return $value + param - param2

fn (&Node) get4() -> int:
    a := $value
    return a + $value

fn (^Node) get5() -> int:
    f := fn() => self.value
    return f()

fn (&Node) set():
    $value = 1

fn (&Node) set2(param int):
    $value += param

fn (&Node) set3(param int):
    $value = 1 + param

fn Node :: getFn() -> int:
    return 123

fn Node :: getFn2(param int) -> int:
    return 123 + param

fn Node :: getFn3(param int, param2 int) -> int:
    return 123 + param - param2

n := Node{value=123}

-- Get.
t.eq(n.get(), 123)

-- Get with regular param.
n = Node{value=123}
t.eq(n.get2(321), 444)

-- Get with many regular params.
n = Node{value=123}
t.eq(n.get3(321, 1), 443)

-- Get, assign field to variable.
n.value = 123
t.eq(n.get4(), 246)

-- Get with closure.
n2 := ^Node{value=123}
t.eq(n2.get5(), 123)

-- Set.
n.set()
t.eq(n.value, 1)

-- Set with op assign.
n.value = 1
n.set2(2)
t.eq(n.value, 3)

-- Set with param.
n.set3(10)
t.eq(n.value, 11)

-- Static func, no params.
t.eq(Node.getFn(), 123)

-- Static func, one params.
t.eq(Node.getFn2(321), 444)

-- Static func, many params.
t.eq(Node.getFn3(321, 1), 443)

-- Static variable method call.
global sn Node = Node{value=123}
sn.set()
t.eq(sn.get(), 1)

--cytest: pass