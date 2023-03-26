---
title: "Control Flow"
weight: 3
---

# Control Flow.
Cyber provides the common constructs to branch and enter loops.

## Branching.
Use `if` and `else` statements to branch the execution of your code depending on conditions. The `else` clause can contain a condition which is only evaluated if the previous if or conditional else clause was `false`. 
```cy
a = 10
if a == 10:
    print 'a is 10'
else a == 20:
    print 'a is 20'
else:
    print 'neither 10 nor 20'
```
An `if` expression also needs the `then` keyword. Conditional `else` clauses are not allowed in an `if` expression.:
```cy
a = 10
str = if a == 10 then 'red' else 'blue'
```
Use `and` and `or` logical operators to combine conditions:
```cy
a = 10
if a > 5 and a < 15:
    print 'a is between 5 and 15'
if a == 20 or a == 10: 
    print 'a is 10 or 20'
```

## Iterations.
Infinite and conditional loops start with the `while` keyword. An infinite loop continues to run the code in the block until a `break` or `return` is reached.
When the `while` clause contains a condition, the loop continues to run until the condition is evaluated to `false`.
```cy
-- Infinite loop.
while:
    pass

running = true
while running:
    -- Keep looping until `running` is false.
    pass
```

You can use the optional `while` loop to continue the loop until the expression evaluates to the `none` value. The unwrapped optional value is copied to the variable declared after `some`.
```cy
iter = dir.walk()
while iter.next() some entry:
    print entry.name
```

`for` loops can iterate over a range that starts at a number (inclusive) to a target number (exclusive). When the range operator `..` is replaced with `..=`, the target number is inclusive. The range can be given a custom step.
```cy
for 0..100 each i:
    print i    -- 0, 1, 2, ... , 99

for 0..100, 10 each i:
    print i    -- 0, 10, 20, ... , 90

for 100..0, 1 each i:
    print i    -- 100, 99, 98, ... , 1

for 100..=0, 1 each i:
    print i    -- 100, 99, 98, ... , 0
```
The `for` clause can iterate over an `Iterable` object. An Iterable type contains an `iterator()` method that returns an `Iterator` object. An Iterator type contains a `next()` method that returns the next value or `none` when finished.
You can iterate lists since they are Iterable.
```cy
list = [1, 2, 3, 4, 5]

-- Iterate on values.
for list each n:
    print n
```
When the `as` clause contains two variables, the for loop will iterate a `PairIterable` object. A PairIterable type contains a `pairIterator()` method that returns a `PairIterator` object. A PairIterator type contains a `nextPair()` method that returns two values or `none` on the first value when finished. 
The list object is also a PairIterable and the key is the index of the value in the list.
```cy
-- Iterate on values and indexes.
for list each i, n:
    print '{i} -> {n}'

-- Iterate on just indexes.
for list each i, _:
    print i 
```
The `for` clause can also iterate over maps with the same idea.
```cy
map = { a: 123, b: 234 }

-- Iterate on values.
for map each v:
    print v

-- Iterate on values and keys.
for map each k, v:
    print '{k} -> {v}'

-- Iterate on just keys.
for map each k, _:
    print k
```

You can exit a loop using `break`.
```cy
for 0..10 each i:
    if i == 4:
        break
    print i
-- This loop stops printing once `i` reaches 4.
```
You can skip the rest of the loop and go to the next iteration using `continue`.
```cy
for 0..10 each i:
    if i == 4:
        continue
    print i
-- This loop prints 0 through 9 but skips 4.
```

## Matching.
Matching is similar to a switch statement. The expression to the right of `match` is evaluated and execution jumps to the declared case with the matching value. Multiple cases can be grouped together using a comma separator. An optional `else` fallback case is executed when no other cases were matched.
```cy
val = 1000
match val:
    0..100: print 'at or between 0 and 99'
    100: print 'val is 100'
    200:
        print 'val is 200'
    300, 400:
        print 'combined case'
    else:
        print 'val is {val}'
```

## Deferred Execution.