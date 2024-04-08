use test

let .a = 123
test.eq(-a, -123)

--cytest: pass