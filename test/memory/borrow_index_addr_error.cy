i := 123
elem_borrow := &i[1]

--cytest: error
--CompileError: Cannot borrow. Missing `@index_addr`.
--
--main:2:17:
--elem_borrow := &i[1]
--                ^~~~
--