use t 'test'
use cy

-- parse()
res = cy.parse('var .foo = 123')
t.eq(res['decls'][0]['type'], 'variable')
t.eq(res['decls'][0]['name'], 'foo')

res = cy.parse('type foo = bar')
t.eq(res['decls'][0]['type'], 'typeAlias')
t.eq(res['decls'][0]['name'], 'foo')

res = cy.parse('func foo(): pass')
t.eq(res['decls'][0]['type'], 'func')
t.eq(res['decls'][0]['name'], 'foo')

res = cy.parse('func Foo.foo(): pass')
t.eq(res['decls'][0]['type'], 'func')
t.eq(res['decls'][0]['name'], 'Foo.foo')

res = cy.parse("use foo 'bar'")
t.eq(res['decls'][0]['type'], 'use_import')
t.eq(res['decls'][0]['name'], 'foo')

res = cy.parse("type foo:\n  a any")
t.eq(res['decls'][0]['type'], 'object')
t.eq(res['decls'][0]['name'], 'foo')

res = cy.parse("type foo enum:\n  case a")
t.eq(res['decls'][0]['type'], 'enum_t')
t.eq(res['decls'][0]['name'], 'foo')

-- parseCyon()
let val = cy.parseCyon('123')
t.eq(val, 123)
val = cy.parseCyon('"foo"')
t.eq(val, 'foo')
val = cy.parseCyon('true')
t.eq(val, true)
val = cy.parseCyon('false')
t.eq(val, false)
val = cy.parseCyon('[]')
t.eqList(val, [])
val = cy.parseCyon('[1, 2, 3]')
t.eqList(val, [1, 2, 3])
val = cy.parseCyon('{}')
t.eq(val.size(), 0)
val = cy.parseCyon('{ a: 123 }')
t.eq(val.size(), 1)
t.eq(val['a'], 123)

-- toCyon()
var cyon = cy.toCyon(123)
t.eq(cyon, '123')
cyon = cy.toCyon(123.0)
t.eq(cyon, '123.0')
cyon = cy.toCyon('foo')
t.eq(cyon, "'foo'")
cyon = cy.toCyon(true)
t.eq(cyon, 'true')
cyon = cy.toCyon(false)
t.eq(cyon, 'false')
cyon = cy.toCyon([])
t.eq(cyon, '[]')
cyon = cy.toCyon([1, 2, 3])
t.eq(cyon, '''[
    1,
    2,
    3,
]''')
cyon = cy.toCyon({})
t.eq(cyon, '{}')
cyon = cy.toCyon({ a: 123 })
t.eq(cyon, '''{
    a: 123,
}''')