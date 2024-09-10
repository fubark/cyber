var a = `🦊a`

--cytest: error
--CompileError: Invalid UTF-8 Rune.
--
--main:1:9:
--var a = `🦊a`
--        ^
--