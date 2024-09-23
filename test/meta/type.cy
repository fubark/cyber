use t 'test'

-- Get runtime type.
t.eq(type(123), int)
dyn a = 'abc'
t.eq(type(a), string)

-- id() for builtin types.
t.eq((any).id(), 10)
t.eq((bool).id(), 2)
t.eq((float).id(), 8)
t.eq((int).id(), 7)
t.eq((string).id(), 21)
t.eq((symbol).id(), 6)
t.eq((Map).id(), 15)
t.eq((error).id(), 3)
t.eq((Fiber).id(), 23)
t.eq((type).id(), 11)

-- typeInfo()
var int_t = typeInfo(int).!int_t
t.eq(int_t.sign, true)
t.eq(int_t.bits, 64)

int_t = typeInfo(byte).!int_t
t.eq(int_t.sign, false)
t.eq(int_t.bits, 8)

var float_t = typeInfo(float).!float_t
t.eq(float_t.bits, 64)

t.eq(typeInfo(bool).!bool_t, _)

t.eq(typeInfo(void).!void_t, _)

t.eq(typeInfo(type).!type_t, _)

t.eq(typeInfo(error).!error_t, _)

type Area trait:
    fn area(self) float
var trait_t = typeInfo(Area).!trait_t
t.eq(trait_t.name, 'Area')

var array_t = typeInfo([10]int).!array_t
t.eq(array_t.len, 10)
t.eq(array_t.elem, int)

var option_t = typeInfo(?int).!opt_t
t.eq(option_t.elem, int)

var ptr_t = typeInfo(*int).!ptr_t
t.eq(ptr_t.child, int)

type Shape enum:
    case rectangle Rectangle
    case line  float
    case point
type Rectangle:
    width  float
    height float
var choice_t = typeInfo(Shape).!choice_t
t.eq(choice_t.name.?, 'Shape')
t.eq(choice_t.cases.len(), 3)
t.eq(choice_t.cases[0].name, 'rectangle')
t.eq(choice_t.cases[0].type, Rectangle)
t.eq(choice_t.cases[1].name, 'line')
t.eq(choice_t.cases[1].type, float)
t.eq(choice_t.cases[2].name, 'point')
t.eq(choice_t.cases[2].type, void)

var enum_t = typeInfo(Shape.Tag).!enum_t
t.eq(enum_t.name.?, 'Tag')
t.eq(enum_t.cases.len(), 3)
t.eq(enum_t.cases[0].name, 'rectangle')
t.eq(enum_t.cases[1].name, 'line')
t.eq(enum_t.cases[2].name, 'point')

type S:
    a int
    b float
var struct_t = typeInfo(S).!struct_t
t.eq(struct_t.name.?, 'S')
t.eq(struct_t.fields.len(), 2)
t.eq(struct_t.fields[0].name, 'a')
t.eq(struct_t.fields[0].type, int)
t.eq(struct_t.fields[1].name, 'b')
t.eq(struct_t.fields[1].type, float)

type FnPtr -> fn(int, float) string
var func_t = typeInfo(FnPtr).!func_t
t.eq(func_t.kind, .ptr)
t.eq(func_t.ret, string)
t.eq(func_t.params.len(), 2)
t.eq(func_t.params[0].type, int)
t.eq(func_t.params[1].type, float)

type FnUnion -> Fn(int, float) string
func_t = typeInfo(FnUnion).!func_t
t.eq(func_t.kind, .union)
t.eq(func_t.ret, string)
t.eq(func_t.params.len(), 2)
t.eq(func_t.params[0].type, int)
t.eq(func_t.params[1].type, float)

-- Referencing type name path.
use os
t.eq(type(os.CArray), type)

--cytest: pass