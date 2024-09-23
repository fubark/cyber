use t 'test'

fn Node.flatGet2(self) int:
    return 123

type Node:
    value int

    fn get(self) int:
        return self.value

    fn get2(self, param int) int:
        return self.value + param

    fn get3(self, param int, param2 int) int:
        return self.value + param - param2

    fn get4(self) int:
        var a = self.value
        return a + self.value

    fn get5(self) int:
        var f = fn() => self.value
        return f()

    fn set(self):
        self.value = 1

    fn set2(self, param int):
        self.value += param

    fn set3(self, param int):
        self.value = 1 + param

fn Node.getFn() int:
    return 123

fn Node.getFn2(param int) int:
    return 123 + param

fn Node.getFn3(param int, param2 int) int:
    return 123 + param - param2

fn Node.flatGet(self) int:
    return 123

var n = Node{value=123}

-- Flat method declaration.
t.eq(n.flatGet(), 123)
t.eq(n.flatGet2(), 123)

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
n.value = 123
t.eq(n.get5(), 123)

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
var .sn = Node{value=123}
sn.set()
t.eq(sn.get(), 1)

--cytest: pass