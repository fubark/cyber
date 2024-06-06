use test

type Shape trait:
    func area() float

type Circle:
    with Shape
    radius float

    func area() float:
        return 3.14 * self.radius^2

type Rectangle:
    with Shape
    width  float
    height float

    func area() float:
        return self.width * self.height

var s Shape = Circle{ radius: 2 }
test.eqNear(s.area(), 12.56)

s = Rectangle{ width: 4, height: 5 }
test.eq(s.area(), 20)

--cytest: pass