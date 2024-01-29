import t 'test'

-- Temporary rc if expr cond is released before entering body. (cond always evaluates true for rc objects)
var a = String(1) ? 123 else false

-- Temporary rc if stmt cond is released before entering body. (cond always evaluates true for rc objects)
if String(1):
    pass

-- Temporary rc else cond is released before entering body. 
if false:
    pass
else String(1):
    pass

-- Temporary rc where cond is released before entering body.
while String(1):
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
a = false ? 'abc' else '$(b)'
var c = '$(a)'  -- If `a` isn't marked as a rcCandidate, `a` would be freed here and at the end of the block.

--cytest: pass