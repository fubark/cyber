use test

const int_c = 123
const float_c = 123.0
const str_c = 'abc'
const switch_c = switch 10:
    case 10 => true
    else => false

test.eq(123, int_c)
test.eq(123.0, float_c)
test.eq('abc', str_c)
test.eq(true, switch_c)

-- With type specifier.
const int_c2 int = 123
test.eq(123, int_c2)

-- Can be referenced from static function.
fn foo():
    test.eq(123, int_c)
foo()

--cytest: pass