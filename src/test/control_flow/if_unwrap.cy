use test

opt := ?str('abc')
unwrapped := ''
if opt |v|:
    unwrapped = v

test.eq(unwrapped, 'abc')

-- Else.
opt = none
if opt |v|:
    unwrapped = v
else:
    unwrapped = ''
test.eq(unwrapped, '')

--cytest: pass