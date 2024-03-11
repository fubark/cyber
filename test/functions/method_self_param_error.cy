type A:
    func b(self A):
        pass

--cytest: error
--CompileError: `self` param is not allowed in an implicit method declaration.
--
--main:2:12:
--    func b(self A):
--           ^
--