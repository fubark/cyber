use t 'test'

-- Switch case.
a := 123
res := 0
switch a:
    case 0: res = 1
    case 10: res = 2
    case 123: res = 3
    else: res = -1
t.eq(3, res)

-- Switch else only.
a = 123
res = 0
switch a:
    else: res = -1
t.eq(-1, res)

-- Switch multiple cond case.
a = 123
res = 0
switch a:
    case 0: res = 1
    case 10, 123: res = 2
    else: res = -1
t.eq(2, res)

-- Switch multiple cond case with new line.
a = 123
res = 0
switch a:
    case 0: res = 1
    case 10,
        123: res = 2
    else: res = -1
t.eq(2, res)

-- Switch case fallthrough.
a = 0
res = 0
switch a:
    case 0
    case 123:
        res = 2
    else: res = -1
t.eq(2, res)

-- Match range case.
a = 50
res = 0
switch a:
    case 0..100:
        res = 1
    else:
        res = -1
t.eq(1, res)

-- Match range case.
a = 50
res = 0
switch a:
    case 0..100:
        res = 1
    else:
        res = -1
t.eq(1, res)

-- Don't match range case.
a = 150
switch a:
    case 0..100:
        res = 1
    else:
        res = -1
t.eq(-1, res)

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
fn foo():
    a := 123
    res := 0
    switch a:
        case 0:
            res = 1
        case 10:
            res = 2
        else:
            res = -1
    t.eq(-1, res)
foo()

-- Switch assign block.
res = switch 'one':
    case 'one' => 1
    case 'two' => 2
    else       => -1
t.eq(1, res)

res = switch 'two':
    case 'one' => 1
    case 'two' => 2
    else       => -1
t.eq(2, res)

res = switch 'three':
    case 'one' => 1
    case 'two' => 2
    else       => -1
t.eq(-1, res)

-- -- Assign switch to static var.
-- .varRes := switch 'one':
--     case 'one' => 1
--     case 'two' => 2
--     else       => -1
-- t.eq(varRes, 1)

-- global varRes2 = switch 'two':
--     case 'one' => 1
--     case 'two' => 2
--     else       => -1
-- t.eq(varRes2, 2)

-- global varRes3 = switch 'three':
--     case 'one' => 1
--     case 'two' => 2
--     else       => -1
-- t.eq(varRes3, -1)

-- Switch with nested rvalue.
map := Map[str, str]{ a='foo' }
res = switch map['a']:
    case 'foo' => 1
    else => -1
t.eq(1, res)

-- Switch case returns.
fn foo2() -> int:
    switch 'one':
        case 'one': return 1
        else      : return -1
t.eq(1, foo2())

-- switch: borrow copyable control expression
res = 0
c := 123
switch &c:
    case 123: res = 1
    else: pass
t.eq(1, res)

--cytest: pass