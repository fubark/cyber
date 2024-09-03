use test

-- if
var a = 0
#if true:
    a = 1
test.eq(a, 1)

-- if and else-if
a = 0
#if false:
    a = 0
#else true:
    a = 1
test.eq(a, 1)

-- if, else-if, and else
a = 0
#if false:
    a = 0
#else false:
    a = 0
#else:
    a = 1
test.eq(a, 1)

-- if and else
a = 0
#if false:
    a = 0
#else:
    a = 1
test.eq(a, 1)

--cytest: pass