span := [&]int{1, 2, 3}
lift := ^span

--cytest: error
--CompileError: Cannot lift the borrow container type `[&]int`.
--
--@MainPath():2:9:
--lift := ^span
--        ^~~~~
--