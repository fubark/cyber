use cy

--| Related to the command line.
--| Sample usage:
--| ```cy
--| use cli
--| cli.repl()
--| ```

--| Starts an isolated REPL session.
--| Invokes `cy.repl(replReadLine)`.
fn repl() -> void:
    cy.repl(replReadLine_)

--| Default implementation to read a line from the CLI for a REPL.
#[bind] fn replReadLine(prefix str) -> str

fn replReadLine_(prefix str) -> Future[str]:
    line := replReadLine(prefix)
    return Future.complete(line)