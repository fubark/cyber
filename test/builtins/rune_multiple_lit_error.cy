var a = `🦊a`

--cytest: error
--CompileError: Invalid UTF-8 Rune.
--
--main:1:10:
--var a = `🦊a`
--         ^
--