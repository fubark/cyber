use t 'test'

-- Test: Can users break type embedding with 3 different objects?
-- This file tests various scenarios with 3 objects to find potential vulnerabilities

-- Scenario 1: Linear chain embedding (A embeds B, B embeds C)
type Base:
    value int

type Middle:
    base use Base
    extra String

type Top:
    middle use Middle
    count int

var chain = Top{
    middle=Middle{
        base=Base{value=42},
        extra="test"
    },
    count=100
}

-- Test deep access through chain
t.eq(chain.middle.base.value, 42)
t.eq(chain.middle.extra, "test")
t.eq(chain.count, 100)

-- Scenario 2: Multiple embeddings (A embeds B and C)
type TypeB:
    b_field int

type TypeC:
    c_field String

type TypeA:
    b use TypeB
    c use TypeC
    a_field float

var multi = TypeA{
    b=TypeB{b_field=10},
    c=TypeC{c_field="hello"},
    a_field=3.14
}

-- Test multiple embedded access
t.eq(multi.b.b_field, 10)
t.eq(multi.c.c_field, "hello")
t.eq(multi.a_field, 3.14)

-- Scenario 3: Diamond pattern (A embeds B and C, B and C both have same field name)
type BaseX:
    shared int
    x_only String

type BaseY:
    shared int
    y_only String

type Diamond:
    x use BaseX
    y use BaseY
    own int

var diamond = Diamond{
    x=BaseX{shared=1, x_only="x"},
    y=BaseY{shared=2, y_only="y"},
    own=3
}

-- Test diamond access (should access through explicit paths)
t.eq(diamond.x.shared, 1)
t.eq(diamond.y.shared, 2)
t.eq(diamond.x.x_only, "x")
t.eq(diamond.y.y_only, "y")
t.eq(diamond.own, 3)

-- Scenario 4: Name collision with parent (child shadows embedded)
type CollisionBase:
    value int
    name String

type CollisionChild:
    base use CollisionBase
    value int  -- Shadows base.value
    name String  -- Shadows base.name

var collision = CollisionChild{
    base=CollisionBase{value=100, name="base"},
    value=200,
    name="child"
}

-- Test that child fields take precedence
t.eq(collision.value, 200)  -- Child field
t.eq(collision.name, "child")  -- Child field
t.eq(collision.base.value, 100)  -- Explicit base access
t.eq(collision.base.name, "base")  -- Explicit base access

-- Scenario 5: Complex nesting with methods
type MethodBase:
    val int
    func getVal(self) int:
        return self.val

type MethodMiddle:
    base use MethodBase
    multiplier int
    func compute(self) int:
        return self.base.getVal() * self.multiplier

type MethodTop:
    middle use MethodMiddle
    offset int

var methods = MethodTop{
    middle=MethodMiddle{
        base=MethodBase{val=5},
        multiplier=3
    },
    offset=10
}

-- Test method calls through embeddings
t.eq(methods.middle.base.getVal(), 5)
t.eq(methods.middle.compute(), 15)

-- Scenario 6: Empty embedded types
type Empty1:
    _dummy int

type Empty2:
    _dummy int

type Empty3:
    _dummy int

type MultiEmpty:
    e1 use Empty1
    e2 use Empty2
    e3 use Empty3
    value int

var empty = MultiEmpty{
    e1=Empty1{},
    e2=Empty2{},
    e3=Empty3{},
    value=42
}

t.eq(empty.value, 42)

-- Scenario 7: Three-way circular attempt (should be caught by compiler)
-- This is commented out as it should fail compilation
-- type CircA:
--     b use CircB
-- type CircB:
--     c use CircC
-- type CircC:
--     a use CircA

-- Scenario 8: Stress test - many fields in embedded types
type StressBase1:
    f1 int
    f2 int
    f3 int

type StressBase2:
    f4 String
    f5 String
    f6 String

type StressBase3:
    f7 float
    f8 float
    f9 float

type StressTop:
    b1 use StressBase1
    b2 use StressBase2
    b3 use StressBase3
    own int

var stress = StressTop{
    b1=StressBase1{f1=1, f2=2, f3=3},
    b2=StressBase2{f4="a", f5="b", f6="c"},
    b3=StressBase3{f7=1.1, f8=2.2, f9=3.3},
    own=999
}

-- Test all fields accessible
t.eq(stress.b1.f1, 1)
t.eq(stress.b1.f2, 2)
t.eq(stress.b1.f3, 3)
t.eq(stress.b2.f4, "a")
t.eq(stress.b2.f5, "b")
t.eq(stress.b2.f6, "c")
t.eq(stress.b3.f7, 1.1)
t.eq(stress.b3.f8, 2.2)
t.eq(stress.b3.f9, 3.3)
t.eq(stress.own, 999)

--cytest: pass
