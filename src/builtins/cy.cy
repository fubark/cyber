--| Parses Cyber source string into a structured map object.
--| Currently, only metadata about static declarations is made available but this will be extended to include an AST.
#host func parse(src String) Map

--| Parses a CYON string into a value.
#host func parseCyon(src String) any

--| Starts an isolated REPL session.
--| The callback `read_line(prefix String) String` is responsible for obtaining the input.
#host func repl(read_line any) void

--| Encodes a value to CYON string.
#host func toCyon(val any) String
