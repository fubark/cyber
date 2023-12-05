---
title: "Control Flow"
weight: 3
---

# Control Flow.
Cyber provides the common constructs to branch and enter loops.

## Branching.

### `if` statement.
Use `if` and `else` statements to branch the execution of your code depending on conditions. The `else` clause can contain a condition which is only evaluated if the previous if or conditional else clause was `false`. 
```cy
var a = 10
if a == 10:
    print 'a is 10'
else a == 20:
    print 'a is 20'
else:
    print 'neither 10 nor 20'
```

### Conditional expression.
A conditional branch expression evaluates a condition and returns either the true value or false value:
```cy
var a = 10
var str = a == 10 ? 'red' else 'blue'
```

### `and`/`or`
Use `and` and `or` logical operators to combine conditions:
```cy
var a = 10
if a > 5 and a < 15:
    print 'a is between 5 and 15'
if a == 20 or a == 10: 
    print 'a is 10 or 20'
```

## Iterations.

### Infinite loop.
The `while` keyword starts an infinite loop which continues to run the code in the block until a `break` or `return` is reached.
```cy
var count = 0
while:
    if count > 100:
        break
    count += 1
```

### Conditional loop.
When the `while` clause contains a condition, the loop continues to run until the condition is evaluated to `false`:
```cy
var running = true
var count = 0
while running:
    if count > 100:
        running = false
    count += 1
```

### Unwrapping loop. 
Using the capture operator `->` unwraps the left optional value to the right variable declaration. The loop exits when the left value is `none`:
```cy
var iter = dir.walk()
while iter.next() -> entry:
    print entry.name
```

### For range.
`for` loops can iterate over a range that starts at an `int` (inclusive) to a target `int` (exclusive).
The capture operator `->` is used to capture the loop's counter variable:
```cy
for 0..4:
    performAction() 

for 0..100 -> i:
    print i    -- 0, 1, 2, ... , 99
```

To decrement the counter instead, use `-..`:
```cy
for 100-..0 -> i:
    print i    -- 100, 99, 98, ... , 1
```

When the range operator `..` is replaced with `..=`, the target `int` is inclusive: {{<todo "*Planned Feature">}}
```cy
for 0..=100 -> i:
    print i    -- 0, 1, 2, ... , 100
```

### For each.
The `for` clause can iterate over any type that implements the `Iterable` trait. An Iterable contains an `iterator()` method which returns an `Iterator` object. The for loop continually invokes the Iterator's `next()` method until `none` is returned.

Lists can be iterated since they implement the Iterable trait. The capture operator `->` is used to capture the value returned from an iterator's `next()`:
```cy
var list = [1, 2, 3, 4, 5]

for list -> n:
    print n
```

Maps can be iterated. `next()` returns a key and value tuple:
```cy
var map = [ a: 123, b: 234 ]

for map -> entry:
    print entry[0]
    print entry[1]
```

Use the destructure syntax to extract the key and value into two separate variables:
```cy
for map -> [ key, val ]:
    print 'key $(key) -> value $(val)'
```

### For each with index.
A counting index can be declared after the each variable. The count starts at 0 for the first value:
```cy
var list = [1, 2, 3, 4, 5]

for list -> val, i:
    print 'index $(i), value $(val)'
```

### Exit loop.
Use `break` to exit a loop. This loop stops printing once `i` reaches 4:
```cy
for 0..10 -> i:
    if i == 4:
        break
    print i
```

### Next iteration.
Use `continue` to skip the rest of the loop and go to the next iteration.
This loop prints 0 through 9 but skips 4:
```cy
for 0..10 -> i:
    if i == 4:
        continue
    print i
```

## Switch matching.
The `switch` statement branches to a case block from a matching case condition. The expression that is matched against comes after switch statement. Multiple cases can be grouped together using a comma separator. An optional `else` fallback case is executed when no other cases were matched.
{{<todo "*Incomplete: Not all types can be used in the case conditions such as ranges.">}}
```cy
var val = 1000
switch val:
case 0..100: print 'at or between 0 and 99'
case 100   : print 'val is 100'
case 200:
    print 'val is 200'
case 300, 400:
    print 'combined case'
else:
    print 'val is $(val)'
```
Note that the `switch` block must be empty and requires at least one `case` block or an `else` block to come after it.

### Switch assignment.
Although `switch` can not be used freely as an expression, it can be assigned to a left variable or destination:
```cy
var shu = switch pepper:
case 'bell'     => 0
case 'anaheim'  => 500
case 'jalapeño' => 2000
case 'serrano'  => 10000
```
When declaring an assignable switch, the cases must have a return value using the syntax `case {cond} => {expr}` or `else => {expr}`.

### Switch break.
A `break` statement exits the current case block and resumes execution after the end of the switch statement: {{<todo "*Planned Feature">}}
```cy
switch value:
case 0..5:
    print value
    if value == 3:
        break case
    print value    -- Skips second print if `value` is 3.
```

## Try/Catch.
The `try catch` statement, `try else` and `try` expressions provide a way to catch a throwing error and resume execution in a different branch. Learn more about [Error Handling]({{<relref "/docs/toc/errors">}}).

## Deferred Execution.
> _Planned Feature_
