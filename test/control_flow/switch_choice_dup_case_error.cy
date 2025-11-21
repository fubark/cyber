type Number enum:
    case one void
    case two void

switch Number.one:
    case .one:
        pass
    case .one:
        pass

--cytest: error
--CompileError: Duplicate case.
--
--main:8:10:
--    case .one:
--         ^~~~
--