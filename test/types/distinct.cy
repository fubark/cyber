use test

type Vec2:
    x float
    y float

type Vec Vec2:
    fn add(self) float:
        return self.x + self.y

var v = Vec{x=1, y=2}
test.eq(v.x, 1.0)
test.eq(v.y, 2.0)
test.eq(v.add(), 3.0)

--cytest: pass