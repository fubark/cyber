type Shape trait:
    fn area() -> float

type Circle:
    with Shape
    radius float

--cytest: error
--CompileError: `Circle` does not implement `fn area(Shape) -> float` from `Shape`.
--
--@MainPath():5:10:
--    with Shape
--         ^~~~~
--@MainPath():4:1:
--type Circle:
--^~~~~~~~~~~~
--