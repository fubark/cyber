use test

def GetType[ID string] type:
    if ID == 'bool':
        return bool
    else ID == 'int':
        return int
    else ID == 'string':
        return string
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