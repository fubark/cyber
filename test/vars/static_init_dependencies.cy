use t 'test'

-- Declaration that depends on another.
var .a = 123
var .b = a + 321
var .c = a + b
t.eq(a, 123) 
t.eq(b, 444) 
t.eq(c, 567) 

-- Depends on and declared before another.
var .c2 = a2 + b2
var .b2 = a2 + 321
var .a2 = 123
t.eq(a2, 123) 
t.eq(b2, 444) 
t.eq(c2, 567) 

-- Dependent read runs after initial declaration.
var .a3 = load(b3)
var .b3 = 123
fn load(arg int) dyn:
    b3 = 234
t.eq(b3, 234)

-- Static vars are loaded even if they are not referenced.
var .b4 = 123
var .a4 = load()
fn load() dyn:
    b4 = 234
t.eq(b4, 234)

--cytest: pass