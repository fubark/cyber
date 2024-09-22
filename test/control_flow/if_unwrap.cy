use test

var opt ?string = 'abc'
var unwrapped = ''
if opt -> v:
    unwrapped = v

test.eq(unwrapped, 'abc')

-- Else.
opt = none
if opt -> v:
    unwrapped = v
else:
    unwrapped = ''
test.eq(unwrapped, '')

--cytest: pass