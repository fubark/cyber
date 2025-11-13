use t 'test'

-- Basic type embedding tests
type Base:
    x int
    y String

type Container:
    base use Base
    value int

-- Test basic embedded field access
var c = Container{base=Base{x=42, y="hello"}, value=100}
t.eq(c.base.x, 42)
t.eq(c.base.y, "hello")
t.eq(c.value, 100)

-- Test field assignment through embedding
c.base.x = 200
t.eq(c.base.x, 200)
c.base.y = "world"
t.eq(c.base.y, "world")

-- Test multiple embeddings
type Base2:
    z float

type MultiContainer:
    base1 use Base
    base2 use Base2
    extra int

var m = MultiContainer{
    base1=Base{x=1, y="a"},
    base2=Base2{z=3.14},
    extra=99
}
t.eq(m.base1.x, 1)
t.eq(m.base1.y, "a")
t.eq(m.base2.z, 3.14)
t.eq(m.extra, 99)

-- Test conflict resolution (child takes precedence)
type ConflictingBase:
    value int
    func method(self) String:
        return "base"

type ConflictingContainer:
    base use ConflictingBase
    value int      -- Shadows Base.value
    func method(self) String:  -- Shadows Base.method()
        return "container"

var conf = ConflictingContainer{
    base=ConflictingBase{value=10},
    value=20
}
t.eq(conf.value, 20)        -- Child field takes precedence
t.eq(conf.base.value, 10)   -- Explicit access to base field
t.eq(conf.method(), "container")     -- Child method takes precedence
t.eq(conf.base.method(), "base")     -- Explicit access to base method

-- Test method embedding
type MethodBase:
    value int
    
    func getValue(self) int:
        return self.value
    
    func setValue(self, v int):
        self.value = v

type MethodContainer:
    base use MethodBase
    extra int

var meth = MethodContainer{
    base=MethodBase{value=50},
    extra=25
}
t.eq(meth.base.getValue(), 50)
meth.base.setValue(75)
t.eq(meth.base.getValue(), 75)

-- Test hidden embedded fields
type HiddenContainer:
    -secret use Base
    visible int

var hidden = HiddenContainer{
    secret=Base{x=123, y="hidden"},
    visible=456
}
t.eq(hidden.secret.x, 123)
t.eq(hidden.secret.y, "hidden")
t.eq(hidden.visible, 456)

-- Test nested object initialization with embeddings
type NestedBase:
    inner int

type NestedContainer:
    nested use NestedBase
    outer int

var nested = NestedContainer{
    nested=NestedBase{inner=999},
    outer=888
}
t.eq(nested.nested.inner, 999)
t.eq(nested.outer, 888)

-- Test type inference with embeddings
type InferredBase:
    data String

type InferredContainer:
    base use InferredBase
    count int

var inferred = InferredContainer{
    base=InferredBase{data="test"},
    count=5
}
t.eq(inferred.base.data, "test")
t.eq(inferred.count, 5)

-- Test embedding with different field types
type TypedBase:
    num int
    text String
    flag bool
    dec float

type TypedContainer:
    base use TypedBase
    other byte

var typed = TypedContainer{
    base=TypedBase{num=42, text="hello", flag=true, dec=3.14},
    other=byte(65)  -- ASCII 'A'
}
t.eq(typed.base.num, 42)
t.eq(typed.base.text, "hello")
t.eq(typed.base.flag, true)
t.eq(typed.base.dec, 3.14)
t.eq(typed.other, byte(65))

-- Test empty embedded type
type EmptyBase:
    _dummy int

type EmptyContainer:
    empty use EmptyBase
    value int

var empty = EmptyContainer{
    empty=EmptyBase{},
    value=100
}
t.eq(empty.value, 100)

--cytest: pass
