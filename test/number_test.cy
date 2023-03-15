-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'

-- Default notation.
try t.eq(1, 1)
try t.eq(-1, -1)
try t.eq(-(-1), 1)

-- Scientific notation.
try t.eq(1.23e2, 123)
try t.eqNear(123e-2, 1.23)

-- Hex notation.
try t.eq(0xff, 255)
try t.eq(0xFF, 255)

-- Octal notation.
try t.eq(0o77, 63)

-- Binary notation.
try t.eq(0b11, 3)

-- UTF-8 Rune notation.
try t.eq(0u'a', 97)
try t.eq(0u'A', 65)
try t.eq(0u'\'', 39)    -- Escape single quote.
try t.eq(0u'\\', 92)    -- Escape backslash.
try t.eq(0u'🐶', 128054)
try t.eq(0u'🦊', 129418)