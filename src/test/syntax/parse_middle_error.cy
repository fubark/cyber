global a int = 123
global
global b int = 234

--cytest: error
--ParseError: Expected global name.
--
--@MainPath():2:7:
--global
--      ^
--