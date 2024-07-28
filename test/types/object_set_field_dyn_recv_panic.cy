use t 'test'

type S:
    a float

let o = t.erase(S{a=123})
o.a = {_}

--cytest: error
--panic: Assigning to `float` field with incompatible type `List`.
--
--main:7:3 main:
--o.a = {_}
--  ^
--