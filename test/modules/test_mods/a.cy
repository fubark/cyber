global g_int int = 123
global g_obj Object = Object(^Vec2{x=1, y=2})
global g_arr Array[int] = {1, 2, 3}
global g_slice []int = {1, 2, 3}
global g_map Map[str, int] = {a=1, b=2, c=3}

type Fn1 = fn() -> int
global varFunc Fn1 = |_|:
    return 345

type Fn2 = fn(int) -> int
global varFunc1 Fn2 = |a|:
    return a + 1

type Fn3 = fn(int, int) -> int
global varFunc2 Fn3 = |a, b|:
    return a + b

global varNoExport int = 123

fn func() -> int:
    return 234
fn func1(a int) -> int:
    return a + 1
fn func2(a int, b int) -> int:
    return a + b

fn barNoExport() -> int:
    return 234

fn toInt(val float) -> int:
    return int(val)

fn sameFuncName() -> int:
    return 123

fn useInt(a float) -> int:
    return toInt(a)

type Vec2:
    x float
    y float

fn Vec2 :: new(x float, y float) -> Vec2:
    return Vec2{x=x, y=y}
type Vec2Alias = Vec2

type Bar:
    a float

type TFoo[T Any]:
    a T