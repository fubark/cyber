global a int = 123
global b int = 234

--cytest: error
--ParseError: Expected global name.
--
--@MainPath():12:7:
--global
--      ^
--

global