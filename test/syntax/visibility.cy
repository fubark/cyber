-- Type declaration.
-type Foo:
    a  int
    -b int

-- Type alias.
-type Foo2 = Foo

-- Type template
-type Foo4[T type]:
    a int
 
-- Function declaration.
-fn foo():
    pass

-- Variable declaration.
-global foov int = 123

--cytest: pass