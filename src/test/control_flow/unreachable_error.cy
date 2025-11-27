-- Skip test. For now, statements are still allowed after unreachable for convenience. 
    
panic('end')
var a = 1

--cytest: error
--CompileError: Unreachable statement.
--
--@MainPath():2:1:
--var a = 1
--^
--