use t 'test'

-- type.of()
t.eqType(bool, type.of(true))
t.eqType(bool, type.of(false))
t.eqType(int, type.of(123))
t.eqType(float, type.of(123.0))
t.eqType(Ptr[void], type.of(as[Ptr[void]] 123))
t.eqType(str, type.of('abc'))
t.eqType(str, type.of('abc🦊'))
t.eqType(symbol, type.of(@abc))
t.eqType(error, type.of(error.Foo))
t.eqType([]int, type.of([]int{}))
t.eqType(Map[str, int], type.of(Map[str, int]{}))

-- -- typeinfo()
-- #var int_t = typeinfo(int).!int_t
-- t.eq(true, int_t.sign)
-- t.eq(64, int_t.bits)

-- int_t = typeinfo(byte).!int_t
-- t.eq(int_t.sign, false)
-- t.eq(int_t.bits, 8)

-- var float_t = typeinfo(float).!float_t
-- t.eq(float_t.bits, 64)

-- t.eq(typeinfo(bool).!bool_t, _)

-- t.eq(typeinfo(void).!void_t, _)

-- t.eq(typeinfo(type).!type_t, _)

-- t.eq(typeinfo(error).!error_t, _)

-- type Area trait:
--     fn area(self) -> float
-- var trait_t = typeinfo(Area).!trait_t
-- t.eq(trait_t.name, 'Area')

-- var array_t = typeinfo([10]int).!array_t
-- t.eq(array_t.len, 10)
-- t.eq(array_t.elem, int)

-- var option_t = typeinfo(?int).!opt_t
-- t.eq(option_t.elem, int)

-- var ptr_t = typeinfo(*int).!ptr_t
-- t.eq(ptr_t.child, int)

-- type Shape enum:
--     case rectangle Rectangle
--     case line  float
--     case point
-- type Rectangle:
--     width  float
--     height float
-- var choice_t = typeinfo(Shape).!choice_t
-- t.eq(choice_t.name.?, 'Shape')
-- t.eq(choice_t.cases.len(), 3)
-- t.eq(choice_t.cases[0].name, 'rectangle')
-- t.eq(choice_t.cases[0].type, Rectangle)
-- t.eq(choice_t.cases[1].name, 'line')
-- t.eq(choice_t.cases[1].type, float)
-- t.eq(choice_t.cases[2].name, 'point')
-- t.eq(choice_t.cases[2].type, void)

-- var enum_t = typeinfo(Shape.Tag).!enum_t
-- t.eq(enum_t.name.?, 'Tag')
-- t.eq(enum_t.cases.len(), 3)
-- t.eq(enum_t.cases[0].name, 'rectangle')
-- t.eq(enum_t.cases[1].name, 'line')
-- t.eq(enum_t.cases[2].name, 'point')

-- type S:
--     a int
--     b float
-- var struct_t = typeinfo(S).!struct_t
-- t.eq(struct_t.name.?, 'S')
-- t.eq(struct_t.fields.len(), 2)
-- t.eq(struct_t.fields[0].name, 'a')
-- t.eq(struct_t.fields[0].type, int)
-- t.eq(struct_t.fields[1].name, 'b')
-- t.eq(struct_t.fields[1].type, float)

-- use FnPtr = fn(int, float) -> str
-- var func_t = typeinfo(FnPtr).!func_t
-- t.eq(func_t.kind, .ptr)
-- t.eq(func_t.ret, str)
-- t.eq(func_t.params.len(), 2)
-- t.eq(func_t.params[0].type, int)
-- t.eq(func_t.params[1].type, float)

-- use FnUnion = Fn(int, float) -> str
-- func_t = typeinfo(FnUnion).!func_t
-- t.eq(func_t.kind, .union)
-- t.eq(func_t.ret, str)
-- t.eq(func_t.params.len(), 2)
-- t.eq(func_t.params[0].type, int)
-- t.eq(func_t.params[1].type, float)

-- type.name()
t.eq('int', type.name(int))
t.eq('float', type.name(float))
t.eq('bool', type.name(bool))
t.eq('error', type.name(error))
t.eq('symbol', type.name(symbol))
t.eq('str', type.name(str))
use os
t.eq('ArgOption', type.name(os.ArgOption))

--cytest: pass