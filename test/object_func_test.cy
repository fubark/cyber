import t 'test'

type Node object:
  value

  func get(self):
    return self.value

  func get2(self, param):
    return self.value + param

  func get3(self, param, param2):
    return self.value + param - param2

  func get4():
    return 123

  func get5(param):
    return 123 + param

  func get6(param, param2):
    return 123 + param - param2

  func get7(self):
    return value

  func get8(self):
    var a = value
    -- Test subsequent use of object member alias.
    return a + value

  func get9(self):
    -- Test closure over object member.
    var f = () => value
    return f()

var n = Node{ value: 123 }

-- self param.
t.eq(n.get(), 123)

-- self param with regular param.
n = Node{ value: 123 }
t.eq(n.get2(321), 444)

-- self param with many regular param.
n = Node{ value: 123 }
t.eq(n.get3(321, 1), 443)

-- Static method, no params.
t.eq(Node.get4(), 123)

-- Static method, one params.
t.eq(Node.get5(321), 444)

-- Static method, many params.
t.eq(Node.get6(321, 1), 443)

-- Variables in methods can shadow object members.
t.eq(n.get7(), 123)
t.eq(n.get8(), 246)
t.eq(n.get9(), 123)