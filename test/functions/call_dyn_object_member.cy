use test

type Foo:
    member dyn

    func foo(self) dyn:
        return self.member()

var f = Foo{member=func () => 3}  
test.eq(f.foo(), 3)

--cytest: pass