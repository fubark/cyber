type Number enum:
    case one void
    case two void

switch Number.one:
    case .one:
        pass

--cytest: error
--CompileError: Unhandled cases in `switch`. `else` can be used to handle the remaining cases.
--
--main:5:1:
--switch Number.one:
--^~~~~~~~~~~~~~~~~~
--