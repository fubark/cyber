-- An empty array is expandable but is not an immutable type.
const a ^int = ^123

--cytest: error
--CompileError: Expected const eligible type, found `^int`.
--
--main:2:9:
--const a ^int = ^123
--        ^~~~
--