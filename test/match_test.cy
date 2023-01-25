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