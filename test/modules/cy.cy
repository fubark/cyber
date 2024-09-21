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
res = cy.eval('"hello ${123}"')
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
t.eq(res.decls[0]['type'], 'struct_decl')
t.eq(res.decls[0]['name'], 'foo')

res = cy.parse("type foo struct:\n  a any")
t.eq(res.decls[0]['type'], 'struct_decl')
t.eq(res.decls[0]['name'], 'foo')

res = cy.parse("type foo enum:\n  case a")
t.eq(res.decls[0]['type'], 'enumDecl')
t.eq(res.decls[0]['name'], 'foo')

--cytest: pass
