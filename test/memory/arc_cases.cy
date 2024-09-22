use t 'test'

var a any = 'abc'

-- Temporary rc if expr cond is released before entering body.
a = if (string(1) == '1') 123 else 234

-- Temporary rc if stmt cond is released before entering body.
if string(1) == '1':
    pass

-- Temporary rc else cond is released before entering body. 
if false:
    pass
else string(1) == '1':
    pass

-- Temporary rc where cond is released before entering body.
while string(1) == '1':
    break

-- Binary metatype operand does not reuse dst for temp since it is retained.
var f = func() bool:
    return float == bool
f()

-- if expr returns rcCandidate value if else clause is a RC string and if clause is a non-RC string.
dyn b = 123
a = if (false) 'abc' else '$(b)'
var c = '$(a)'  -- If `a` isn't marked as a rcCandidate, `a` would be freed here and at the end of the block.

-- Field retains local rec so that noerr prerelease doesn't free the rec too early.
type Node:
    val int
dyn foo = Node{val=123}
foo = foo.val
t.eq(foo, 123)

--cytest: pass