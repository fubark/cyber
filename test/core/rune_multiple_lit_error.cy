a := u'🦊a'

--cytest: error
--CompileError: Invalid UTF-8 Rune.
--
--main:1:6:
--a := u'🦊a'
--     ^~~~~
--