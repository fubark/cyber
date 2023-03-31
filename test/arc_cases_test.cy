import t 'test'

-- Temporary rc cond is released before entering body. (cond always evaluates true for rc objects)
if string(1):
    pass

-- Temporary rc else cond is released before entering body. 
if false:
    pass
else string(1):
    pass

-- Temporary rc where cond is released before entering body.
while string(1):
    break