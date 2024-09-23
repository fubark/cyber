use t 'test'
use cy

-- eval()
dyn res = cy.eval('1')
t.eq(res, 1)
res = cy.eval('1 + 2')
t.eq(res, 3)
res = cy.eval('''
fn mul(a int, b int) int:
    return a * b
mul(10, 5)''')
t.eq(res, 50)
res = cy.eval('"hello ${123}"')
t.eq(res, 'hello 123')
t.throws(fn() => cy.eval('a'), error.EvalError)

-- parse()
var pres = cy.parse('var .foo = 123')
t.eq(pres.decls[0]['type'], 'staticDecl')
t.eq(pres.decls[0]['name'], 'foo')

pres = cy.parse('type foo -> bar')
t.eq(pres.decls[0]['type'], 'typeAliasDecl')
t.eq(pres.decls[0]['name'], 'foo')

pres = cy.parse('fn foo(): pass')
t.eq(pres.decls[0]['type'], 'funcDecl')
t.eq(pres.decls[0]['name'], 'foo')

pres = cy.parse('fn Foo.foo(): pass')
t.eq(pres.decls[0]['type'], 'funcDecl')
t.eq(pres.decls[0]['name'], 'Foo.foo')

pres = cy.parse("use foo 'bar'")
t.eq(pres.decls[0]['type'], 'import_stmt')
t.eq(pres.decls[0]['name'], 'foo')

pres = cy.parse("type foo:\n  a any")
t.eq(pres.decls[0]['type'], 'struct_decl')
t.eq(pres.decls[0]['name'], 'foo')

pres = cy.parse("type foo struct:\n  a any")
t.eq(pres.decls[0]['type'], 'struct_decl')
t.eq(pres.decls[0]['name'], 'foo')

pres = cy.parse("type foo enum:\n  case a")
t.eq(pres.decls[0]['type'], 'enumDecl')
t.eq(pres.decls[0]['name'], 'foo')

--cytest: pass
