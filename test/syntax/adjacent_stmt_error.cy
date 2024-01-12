if true: foo = 123 foo = 234

--cytest: error
--ParseError: Expected end of line or file. Got ident.
--
--main:1:20:
--if true: foo = 123 foo = 234
--                   ^
--