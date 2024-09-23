use test

type Foo:
    member dyn

    fn foo(self) dyn:
        return self.member()

var f = Foo{member=fn () => 3}  
test.eq(f.foo(), 3)

--cytest: pass