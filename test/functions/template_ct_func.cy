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

var a GetType['bool'] = true
test.eq(a, true)

var b GetType['int'] = 123
test.eq(b, 123)

var c GetType['String'] = 'xyz'
test.eq(c, 'xyz')

--cytest: pass