if true:
return 123

--cytest: error
--ParseError: Block requires at least one statement. Use the `pass` statement as a placeholder.
--
--main:2:1:
--return 123
--^
--