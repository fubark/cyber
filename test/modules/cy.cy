use t 'test'
use cy

-- eval()
dyn res = cy.eval('1')
t.eq(res, 1)
res = cy.eval('1 + 2')
t.eq(res, 3)
res = cy.eval('''
func mul(a int, b int) int:
    return a * b
mul(10, 5)''')
t.eq(res, 50)
res = cy.eval('"hello " + 123')
t.eq(res, 'hello 123')
t.throws(func () => cy.eval('a'), error.EvalError)

-- parse()
res = cy.parse('var .foo = 123')
t.eq(res.decls[0]['type'], 'staticDecl')
t.eq(res.decls[0]['name'], 'foo')

res = cy.parse('type foo -> bar')
t.eq(res.decls[0]['type'], 'typeAliasDecl')
t.eq(res.decls[0]['name'], 'foo')

res = cy.parse('func foo(): pass')
t.eq(res.decls[0]['type'], 'funcDecl')
t.eq(res.decls[0]['name'], 'foo')

res = cy.parse('func Foo.foo(): pass')
t.eq(res.decls[0]['type'], 'funcDecl')
t.eq(res.decls[0]['name'], 'Foo.foo')

res = cy.parse("use foo 'bar'")
t.eq(res.decls[0]['type'], 'import_stmt')
t.eq(res.decls[0]['name'], 'foo')

res = cy.parse("type foo:\n  a any")
t.eq(res['decls'][0]['type'], 'objectDecl')
t.eq(res['decls'][0]['name'], 'foo')

res = cy.parse("type foo enum:\n  case a")
t.eq(res.decls[0]['type'], 'enumDecl')
t.eq(res.decls[0]['name'], 'foo')

-- parseCyon()
dyn val = cy.parseCyon('123')
t.eq(val, 123)
val = cy.parseCyon('"foo"')
t.eq(val, 'foo')
val = cy.parseCyon('true')
t.eq(val, true)
val = cy.parseCyon('false')
t.eq(val, false)
val = cy.parseCyon('{_}')
t.eqList(val as List[dyn], {_})
val = cy.parseCyon('{1, 2, 3}')
t.eqList(val as List[dyn], {1, 2, 3})
val = cy.parseCyon('{}')
t.eq(val.size(), 0)
val = cy.parseCyon('{a=123}')
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
cyon = cy.toCyon({_})
t.eq(cyon, '{_}')
cyon = cy.toCyon({1, 2, 3})
t.eq(cyon, '''{
    1,
    2,
    3,
}''')
cyon = cy.toCyon({})
t.eq(cyon, '{}')
cyon = cy.toCyon({a=123})
t.eq(cyon, '''{
    a = 123,
}''')

--cytest: pass
