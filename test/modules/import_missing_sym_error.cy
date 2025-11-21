use a 'test_mods/a.cy'
b := a.missing

--cytest: error
--CompileError: Can not find the symbol `missing` in `a`.
--
--@AbsPath(test/modules/import_missing_sym_error.cy):2:8:
--b := a.missing
--       ^~~~~~~
--