type Vec2:
    x float
    y float

type Vec = Vec2

fn Vec :: foo(self):
    pass

--cytest: error
--CompileError: Cannot declare from type alias.
--
--@MainPath():7:11:
--fn Vec :: foo(self):
--          ^~~
--