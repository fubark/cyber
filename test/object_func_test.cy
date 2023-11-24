import t 'test'

func Node.flatGet2(self):
    return 123

type Node object:
    var value

    func getEx():
        return self.value

    func getEx2(param):
        return self.value + param

    func getEx3(param, param2):
        return self.value + param - param2

    func setEx():
        self.value = 1

    func setEx2(param):
        self.value += param

    func setEx3(param):
        self.value = 1 + param

    func get():
        return value

    func get2(param):
        return value + param

    func get3(param, param2):
        return value + param - param2

    func get4():
        var a = value
        return a + value

    func get5():
        var f = () => value
        return f()

    func set():
        value = 1

    func set2(param):
        value += param

    func set3(param):
        value = 1 + param

func Node.getFn():
    return 123

func Node.getFn2(param):
    return 123 + param

func Node.getFn3(param, param2):
    return 123 + param - param2

func Node.flatGet(self):
    return 123

var n = [Node value: 123]

-- Flat method declaration.
t.eq(n.flatGet(), 123)
t.eq(n.flatGet2(), 123)

-- Explicit get.
t.eq(n.getEx(), 123)

-- Explicit get with regular param.
n = [Node value: 123]
t.eq(n.getEx2(321), 444)

-- Explicit get with many regular params.
n = [Node value: 123]
t.eq(n.getEx3(321, 1), 443)

-- Implicit get.
t.eq(n.get(), 123)

-- Implicit get with regular param.
n = [Node value: 123]
t.eq(n.get2(321), 444)

-- Implicit get with many regular params.
n = [Node value: 123]
t.eq(n.get3(321, 1), 443)

-- Implicit get with subsequent use of object member alias.
n.value = 123
t.eq(n.get4(), 246)

-- Implicit get from closure.
n.value = 123
t.eq(n.get5(), 123)

-- Explicit set.
n.setEx()
t.eq(n.value, 1)

-- Explicit set with op assign.
n.value = 1
n.setEx2(2)
t.eq(n.value, 3)

-- Explicit set with param.
n.setEx3(10)
t.eq(n.value, 11)

-- Implicit set.
n.set()
t.eq(n.value, 1)

-- Implicit set with op assign.
n.value = 1
n.set2(2)
t.eq(n.value, 3)

-- Implicit set with param.
n.set3(10)
t.eq(n.value, 11)

-- Static func, no params.
t.eq(Node.getFn(), 123)

-- Static func, one params.
t.eq(Node.getFn2(321), 444)

-- Static func, many params.
t.eq(Node.getFn3(321, 1), 443)
