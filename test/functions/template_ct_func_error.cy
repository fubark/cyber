use test

func GetType[ID String] type:
    if ID == 'bool':
        return bool
    else ID == 'int':
        return int
    else ID == 'String':
        return String
    else:
        throw error.Unsupported

var a GetType['foo'] = true

--cytest: error
--CompileError: error.Panic
--
--main:11:9:
--        throw error.Unsupported
--        ^
--