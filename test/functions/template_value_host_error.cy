@host def GetType[ID string] type:
    return int

--cytest: error
--CompileError: A value template can not be binded to a `@host` function. Consider invoking a `@host` function in the template body instead.
--
--main:1:7:
--@host def GetType[ID string] type:
--      ^
--