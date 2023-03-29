-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'

-- Default notation.
t.eq(1, 1)
t.eq(-1, -1)
t.eq(-(-1), 1)

-- Scientific notation.
t.eq(1.23e2, 123)
t.eqNear(123e-2, 1.23)

-- Hex notation.
t.eq(0xff, 255)
t.eq(0xFF, 255)

-- Octal notation.
t.eq(0o77, 63)

-- Binary notation.
t.eq(0b11, 3)

-- UTF-8 Rune notation.
t.eq(0u'a', 97)
t.eq(0u'A', 65)
t.eq(0u'\'', 39)    -- Escape single quote.
t.eq(0u'\\', 92)    -- Escape backslash.
t.eq(0u'🐶', 128054)
t.eq(0u'🦊', 129418)