use t 'test'

-- Lift.
a := ^123

-- Using lifted in sub expression.
c := '%{a.*}'

-- Temporary rc if expr cond is released before entering body.
a = if (str(1) == '1') ^123 else ^234
a = if (false) ^123 else ^(234 + a.*)

-- Temporary rc if stmt cond is released before entering body.
if str(1) == '1':
    pass

-- Temporary rc else cond is released before entering body. 
if false:
    pass
else str(1) == '1':
    pass

-- Temporary rc where cond is released before entering body.
while str(1) == '1':
    break

--cytest: pass