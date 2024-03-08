import t 'test'

-- Temporary rc if expr cond is released before entering body.
var a = if (String(1) == '1') 123 else false

-- Temporary rc if stmt cond is released before entering body.
if String(1) == '1':
    pass

-- Temporary rc else cond is released before entering body. 
if false:
    pass
else String(1) == '1':
    pass

-- Temporary rc where cond is released before entering body.
while String(1) == '1':
    break

-- b's narrow type becomes `any` after the if branch, `a = b` should generate copyRetainSrc.
my b = []
if false:
    b = 123
a = b

-- Binary metatype operand does not reuse dst for temp since it is retained.
var f = func():
    return float == bool
f()

-- if expr returns rcCandidate value if else clause is a RC string and if clause is a non-RC string.
b = 123
a = if (false) 'abc' else '$(b)'
var c = '$(a)'  -- If `a` isn't marked as a rcCandidate, `a` would be freed here and at the end of the block.

-- Field retains local rec so that noerr prerelease doesn't free the rec too early.
type Node:
    val int
my foo = [Node val: 123]
foo = foo.val
t.eq(foo, 123)

--cytest: pass