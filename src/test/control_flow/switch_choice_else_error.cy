type Number enum:
    case one void
    case two void

switch Number.one:
    case .one:
        pass
    case .two:
        pass
    else:
        pass

--cytest: error
--CompileError: Unexpected `else` case. All cases are handled.
--
--@MainPath():10:5:
--    else:
--    ^~~~~
--