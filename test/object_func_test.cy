import t 'test'

type Node object:
  value

  meth getEx():
    return self.value

  meth getEx2(param):
    return self.value + param

  meth getEx3(param, param2):
    return self.value + param - param2

  meth setEx():
    self.value = 1

  meth setEx2(param):
    self.value += param

  meth setEx3(param):
    self.value = 1 + param

  meth get():
    return value

  meth get2(param):
    return value + param

  meth get3(param, param2):
    return value + param - param2

  meth get4():
    var a = value
    return a + value

  meth get5():
    var f = () => value
    return f()

  meth set():
    value = 1

  meth set2(param):
    value += param

  meth set3(param):
    value = 1 + param

  func getFn():
    return 123

  func getFn2(param):
    return 123 + param

  func getFn3(param, param2):
    return 123 + param - param2

var n = Node{ value: 123 }

-- Explicit get.
t.eq(n.getEx(), 123)

-- Explicit get with regular param.
n = Node{ value: 123 }
t.eq(n.getEx2(321), 444)

-- Explicit get with many regular params.
n = Node{ value: 123 }
t.eq(n.getEx3(321, 1), 443)

-- Implicit get.
t.eq(n.get(), 123)

-- Implicit get with regular param.
n = Node{ value: 123 }
t.eq(n.get2(321), 444)

-- Implicit get with many regular params.
n = Node{ value: 123 }
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