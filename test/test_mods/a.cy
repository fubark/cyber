export var varNum = 123
export var varList = [1, 2, 3]
export var varMap = { a: 1, b: 2, c: 3 }
export var varFunc = func():
    return 345
export var varFunc1 = func(a):
    return a + 1
export var varFunc2 = func(a, b):
    return a + b

var varNoExport = 123

export func fn():
    return 234
export func fn1(a):
    return a + 1
export func fn2(a, b):
    return a + b

func barNoExport():
    return 234

-- Test that there is no main block execution for imported modules.
panic(#ExecutedModuleMain)

export func declAssign(val) = number

export var initOnce = incInitOnce()
var initOnceCount = 0
func incInitOnce():
    static initOnceCount
    initOnceCount += 1
    return initOnceCount

-- Tests dependency generation, so set resulting symbol's natural order before the dependency.
export var varDepRes = varDep
var varDep = 123
export func funcDepRes() = funcDep
var funcDep = func ():
    return 123

export func sameFuncName():
    return 123

export func useNumber(a):
    return number(a)

export object Vec2:
    x number
    y number