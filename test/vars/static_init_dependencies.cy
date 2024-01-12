import t 'test'

-- Declaration that depends on another.
var Root.a = 123
var Root.b = a + 321
var Root.c = a + b
t.eq(a, 123) 
t.eq(b, 444) 
t.eq(c, 567) 

-- Depends on and declared before another.
var Root.c2 = a2 + b2
var Root.b2 = a2 + 321
var Root.a2 = 123
t.eq(a2, 123) 
t.eq(b2, 444) 
t.eq(c2, 567) 

-- Dependent read runs after initial declaration.
var Root.a3 = load(b3)
var Root.b3 = 123
func load(arg):
    b3 = 234
t.eq(b3, 234)

-- Static vars are loaded even if they are not referenced.
var Root.b4 = 123
var Root.a4 = load()
func load():
    b4 = 234
t.eq(b4, 234)

--cytest: pass