#[bind] var a

--cytest: error
--ParseError: Expected declaration statement.
--
--main:1:9:
--#[bind] var a
--        ^
--