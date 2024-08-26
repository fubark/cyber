use t 'test'

func Node.flatGet2(self) int:
    return 123

type Node:
    value int

    func get(self) int:
        return self.value

    func get2(self, param int) int:
        return self.value + param

    func get3(self, param int, param2 int) int:
        return self.value + param - param2

    func get4(self) int:
        var a = self.value
        return a + self.value

    func get5(self) int:
        var f = func() => self.value
        return f()

    func set(self):
        self.value = 1

    func set2(self, param int):
        self.value += param

    func set3(self, param int):
        self.value = 1 + param

func Node.getFn() int:
    return 123

func Node.getFn2(param int) int:
    return 123 + param

func Node.getFn3(param int, param2 int) int:
    return 123 + param - param2

func Node.flatGet(self) int:
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

type S
func S.foo(a float) float:
    return a

-- Object function, infer arg type.
t.eq(S.foo(2), 2.0)

--cytest: pass