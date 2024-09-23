use cy

--| Starts an isolated REPL session.
--| Invokes `cy.repl(replReadLine)`.
fn repl() void:
    cy.repl(replReadLine)

--| Default implementation to read a line from the CLI for a REPL.
@host fn replReadLine(prefix String) String