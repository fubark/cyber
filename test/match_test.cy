-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'

-- Match no case.
a = 123
res = 0
match a:
    0: res = 1
    10: res = 2
try t.eq(res, 0)

-- Match number case.
a = 123
res = 0
match a:
    0: res = 1
    10: res = 2
    123: res = 3
try t.eq(res, 3)

-- Match else case.
a = 123
res = 0
match a:
    0: res = 1
    10: res = 2
    else: res = 3
try t.eq(res, 3)

-- Match group case.
a = 123
res = 0
match a:
    0: res = 1
    10, 123: res = 2
    else: res = 3
try t.eq(res, 2)

-- Match assign block.
res = 0
res = match 'one':
    'one': 1
    'two': 2
try t.eq(res, 1)
res = match 'two':
    'one': 1
    'two': 2
try t.eq(res, 2)
res = match 'three':
    'one': 1
    'two': 2
try t.eq(res, none)

-- var match assign block.
var varRes: match 'one':
    'one': 1
    'two': 2
try t.eq(varRes, 1)
var varRes2: match 'two':
    'one': 1
    'two': 2
try t.eq(varRes2, 2)
var varRes3: match 'three':
    'one': 1
    'two': 2
try t.eq(varRes3, none)