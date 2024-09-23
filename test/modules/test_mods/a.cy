var .varInt = 123
var .varTypedInt int = 123
var .varAny any = {1, 2, 3}
var .varList List[int] = .{1, 2, 3}
var .varMap Map = Map{a=1, b=2, c=3}
var .varFunc = fn() int:
    return 345
var .varFunc1 = fn(a int) int:
    return a + 1
var .varFunc2 = fn(a int, b int) int:
    return a + b

var .varNoExport = 123

fn func() int:
    return 234
fn func1(a int) int:
    return a + 1
fn func2(a int, b int) int:
    return a + b

fn barNoExport() int:
    return 234

fn toInt(val any) int:
    return int(val)

var .initOnce = incInitOnce(initOnceCount)
var .initOnceCount = 0
fn incInitOnce(cur int) int:
    initOnceCount = cur + 1
    return initOnceCount

-- Tests dependency generation, so set resulting symbol's natural order before the dependency.
var .varDepRes = varDep
var .varDep = 123

fn sameFuncName() int:
    return 123

fn useInt(a any) int:
    return toInt(a)

type Vec2:
    x float
    y float

fn Vec2.new(x float, y float) Vec2:
    return Vec2{x=x, y=y}
type Vec2Alias -> Vec2

type Bar:
    a float

type TFoo[T type]:
    a T