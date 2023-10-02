-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'

-- Match no case.
var a = 123
var res = 0
match a:
case 0: res = 1
case 10: res = 2
t.eq(res, 0)

-- Match number case.
a = 123
res = 0
match a:
case 0: res = 1
case 10: res = 2
case 123: res = 3
t.eq(res, 3)

-- Match else case.
a = 123
res = 0
match a:
case 0: res = 1
case 10: res = 2
else: res = 3
t.eq(res, 3)

-- Match group case.
a = 123
res = 0
match a:
case 0: res = 1
case 10, 123: res = 2
else: res = 3
t.eq(res, 2)

-- Match assign block.
res = 0
res = match 'one':
case 'one': 1
case 'two': 2
t.eq(res, 1)

res = match 'two':
case 'one': 1
case 'two': 2
t.eq(res, 2)

res = match 'three':
case 'one': 1
case 'two': 2
t.eq(res, none)

-- var match assign block.
var varRes: match 'one':
case 'one': 1
case 'two': 2
t.eq(varRes, 1)

var varRes2: match 'two':
case 'one': 1
case 'two': 2
t.eq(varRes2, 2)

var varRes3: match 'three':
case 'one': 1
case 'two': 2
t.eq(varRes3, none)