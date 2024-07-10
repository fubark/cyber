type Shape trait:
    func area(self) float

type Circle:
    with Shape
    radius float

--cytest: error
--CompileError: `Circle` does not implement `func area(Shape) float` from `Shape`.
--
--main:5:10:
--    with Shape
--         ^
--