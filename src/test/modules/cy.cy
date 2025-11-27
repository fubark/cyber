use t 'test'
use cy
use meta

-- eval()
res := cy.eval('''
fn main() -> int:
    return 1
''')!
t.eq(1, res.downcast(^int).*)

res = cy.eval('''
fn main() -> int:
    return 1 + 2
''')!
t.eq(3, res.downcast(^int).*)

res = cy.eval('''
fn mul(a int, b int) -> int:
    return a * b
fn main() -> int:
    return mul(10, 5)
''')!
t.eq(50, res.downcast(^int).*)

res = cy.eval(```
fn main() -> str:
    return "hello %{123}"
```)!
t.eq('hello 123', res.downcast(^str).*)
t.eq(error.EvalError, cy.eval('a').unwrapError())

-- parse()
fn parseStmts(src str) -> []^cy.Node:
    res := cy.parse(src)
    return res.root.?.!root.stmts

stmts := parseStmts('global foo int = 123')
t.eq(cy.Node.Tag.global_decl, meta.choice_tag(cy.Node, stmts[0]))
t.eq('foo', stmts[0].!global_decl.name.name())

stmts = parseStmts('type foo = bar')
t.eq(cy.Node.Tag.type_alias_decl, meta.choice_tag(cy.Node, stmts[0]))
t.eq('foo', stmts[0].!type_alias_decl.name.name())

stmts = parseStmts('fn foo(): pass')
t.eq(cy.Node.Tag.func_decl, meta.choice_tag(cy.Node, stmts[0]))
t.eq('foo', stmts[0].!func_decl.name.name())

stmts = parseStmts('fn Foo :: foo(): pass')
t.eq(cy.Node.Tag.func_decl, meta.choice_tag(cy.Node, stmts[0]))
t.eq('Foo', stmts[0].!func_decl.parent.?.name())
t.eq('foo', stmts[0].!func_decl.name.name())

stmts = parseStmts("use foo 'bar'")
t.eq(cy.Node.Tag.import_stmt, meta.choice_tag(cy.Node, stmts[0]))
t.eq('foo', stmts[0].!import_stmt.name.name())

stmts = parseStmts("type foo:\n  a int")
t.eq(cy.Node.Tag.struct_decl, meta.choice_tag(cy.Node, stmts[0]))
t.eq('foo', stmts[0].!struct_decl.name.name())

stmts = parseStmts("type foo struct:\n  a int")
t.eq(cy.Node.Tag.struct_decl, meta.choice_tag(cy.Node, stmts[0]))
t.eq('foo', stmts[0].!struct_decl.name.name())

stmts = parseStmts("type foo enum:\n  case a int")
t.eq(cy.Node.Tag.enum_decl, meta.choice_tag(cy.Node, stmts[0]))
t.eq('foo', stmts[0].!enum_decl.name.name())

--cytest: pass
