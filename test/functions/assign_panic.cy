func toNum(a int):
    pass

func foo():
    pass

foo = toNum

--cytest: error
--panic: Assigning to static function `func () dynamic` with a different function signature `func (int) dynamic`.
--
--main:7:1 main:
--foo = toNum
--^
--