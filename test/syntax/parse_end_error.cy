var .a = 123
var .b = 234

--cytest: error
--ParseError: Expected local name identifier.
--
--main:12:4:
--var
--   ^
--

var