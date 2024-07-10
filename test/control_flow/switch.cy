use t 'test'

-- Switch no case.
var a = 123
let res = 0
switch a
case 0 : res = 1
case 10: res = 2
t.eq(res, 0)

-- Switch number case.
a = 123
res = 0
switch a
case 0  : res = 1
case 10 : res = 2
case 123: res = 3
t.eq(res, 3)

-- Switch else case.
a = 123
res = 0
switch a
case 0  : res = 1
case 10 : res = 2
else    : res = -1
t.eq(res, -1)

-- Switch mulitple conds.
a = 123
res = 0
switch a
case 0       : res = 1
case 10, 123 : res = 2
else         : res = -1
t.eq(res, 2)

-- Switch multiple conds new line.
a = 123
res = 0
switch a
case 0       : res = 1
case 10,
    123      : res = 2
else         : res = -1
t.eq(res, 2)

-- -- Switch break.
-- res = 0
-- switch 10:
-- case 0 : res = 1
-- case 10:
--     res = 2
--     if res == 2: break case
--     res = 3
-- t.eq(res, 2)

-- Switch in a nested block. Tests parsing.
func foo():
    var a = 123
    let res = 0
    switch a
    case 0 :
        res = 1
    case 10:
        res = 2
    else:
        res = -1
    t.eq(res, -1)
foo()

-- Switch assign block.
res = switch 'one':
    case 'one' => 1
    case 'two' => 2
    else       => -1
t.eq(res, 1)

res = switch 'two':
    case 'one' => 1
    case 'two' => 2
    else       => -1
t.eq(res, 2)

res = switch 'three':
    case 'one' => 1
    case 'two' => 2
    else       => -1
t.eq(res, -1)

-- Assign switch to static var.
var .varRes = switch 'one':
    case 'one' => 1
    case 'two' => 2
    else       => -1
t.eq(varRes, 1)

var .varRes2 = switch 'two':
    case 'one' => 1
    case 'two' => 2
    else       => -1
t.eq(varRes2, 2)

var .varRes3 = switch 'three':
    case 'one' => 1
    case 'two' => 2
    else       => -1
t.eq(varRes3, -1)

-- Switch with nested rvalue.
var list = { a='foo' }
res = switch list.a:
    case 'foo' => 1
    else => -1
t.eq(res, 1)

-- Switch case returns.
func foo2():
    switch 'one':
        case 'one': return 1
        else      : return -1
t.eq(foo2(), 1)

--cytest: pass