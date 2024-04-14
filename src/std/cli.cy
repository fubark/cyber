use cy

--| Starts an isolated REPL session.
--| Invokes `cy.repl(replReadLine)`.
func repl() void:
    cy.repl(replReadLine)

--| Default implementation to read a line from the CLI for a REPL.
#host func replReadLine(prefix String) String