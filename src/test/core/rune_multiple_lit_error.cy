a := u'🦊a'

--cytest: error
--CompileError: Invalid UTF-8 Rune.
--
--@MainPath():1:6:
--a := u'🦊a'
--     ^~~~~
--