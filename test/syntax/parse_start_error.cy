var
var Root.a = 123
var Root.b = 234

--cytest: error
--ParseError: Expected local name identifier.
--
--main:1:4:
--var
--   ^
--