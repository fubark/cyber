import t 'test'

type S:
  var a float

my o = t.erase([S a: 123])
o.a = []

--cytest: error
--panic: Assigning to `float` field with incompatible type `List`.
--
--main:7:1 main:
--o.a = []
--^
--