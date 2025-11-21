use test

type S:
    a int
    b int
    c C
    d ?int
    e ^int

type C enum:
    case a int
    case b ?int
    case c D

type D:
    a int
    b int

s := S{a=1, b=2, c=C.a(123), d=none, e=^234}

-- Move value member.
b := move s.b
test.eq(2, b)

-- Can still access other members.
test.eq(1, s.a)

-- Move reference member.
e := move s.e
test.eq(234, e.*)

-- Move choice primitive payload.
ca := move s.c.!a
test.eq(123, ca)

s = S{a=1, b=2, c=C.c({a=123, b=234}), d=none, e=^234}

-- Move choice struct payload.
cc := move s.c.!c
test.eq(123, cc.a)
test.eq(234, cc.b)

s = S{a=1, b=2, c=C.b(123), d=234, e=^345}

-- Move unwrap option.
d := move s.d.?
test.eq(234, d)

-- Move unwrap choice, unwrap option.
cb := move s.c.!b.?
test.eq(123, cb)

--cytest: pass