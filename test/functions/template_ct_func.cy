use test

func GetType[ID String] type:
    if ID == 'bool':
        return bool
    else ID == 'int':
        return int
    else ID == 'String':
        return String
    else:
        throw error.Unsupported

var a GetType['bool'] = true
test.eq(a, true)

var b GetType['int'] = 123
test.eq(b, 123)

var c GetType['String'] = 'xyz'
test.eq(c, 'xyz')

-- Specialize type templates.
type GenFoo[T type]:
    a T

func Foo[T type] type:
    if eqType[T, int]:
        return int
    else:
        return GenFoo[T]

var d Foo[int] = 123
test.eq(d, 123)

var e Foo[float] = .{a=123}
test.eq(e.a, 123.0)

--cytest: pass