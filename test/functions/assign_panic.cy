func toNum(a):
    pass

func foo():
    pass

foo = toNum

--cytest: error
--panic: Assigning to static function `func () dynamic` with a different function signature `func (dynamic) dynamic`.
--
--main:7:1 main:
--foo = toNum
--^
--