var varNum: 123
var varList: [1, 2, 3]
var varMap: { a: 1, b: 2, c: 3 }
var varFunc: func():
    return 345
var varFunc1: func(a):
    return a + 1
var varFunc2: func(a, b):
    return a + b

var varNoExport: 123

func fn():
    return 234
func fn1(a):
    return a + 1
func fn2(a, b):
    return a + b

func barNoExport():
    return 234

-- Test that there is no main block execution for imported modules.
panic(#ExecutedModuleMain)

func declAssign(val) number = number

var initOnce: incInitOnce(initOnceCount)
var initOnceCount: 0
func incInitOnce(cur):
    static initOnceCount
    initOnceCount = cur + 1
    return initOnceCount

-- Tests dependency generation, so set resulting symbol's natural order before the dependency.
var varDepRes: varDep
var varDep: 123
func funcDepRes() = funcDep
var funcDep: func ():
    return 123

func sameFuncName():
    return 123

func useNumber(a):
    return number(a)

type Vec2 object:
    x number
    y number

    func new(x, y):
        return Vec2{ x: x, y: y }

type Bar object:
    a number