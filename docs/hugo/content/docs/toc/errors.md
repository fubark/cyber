---
title: "Error Handling"
weight: 7
---

# Error Handling.
Cyber provides a throw/catch mechanism to handle expected errors. For unexpected errors, panics can be used as a fail-fast mechanism to abort the currently running fiber.

## Error trait.
Only types that implement the `Error` trait can be thrown or attached to a panic.
Since the `Error` trait is empty, it's simple to turn any type into a throwable type.

### `error` value.
An `error` value contains a `symbol` and implements the `Error` trait. They can be created without a declaration using the error literal:
```cy
var err = error.Oops
```

Use `sym()` to obtain the underlying symbol:
```cy
print err.sym()   -- Prints ".Oops"
```

Since `error` is a primitive value, it can be compared using the `==` operator.
```cy
if err == error.Oops:
    handleOops()

-- Alternatively.
if err.sym() == .Oops:
    handleOops()
```

### Enum error.
By implementing the `Error` trait, an enum type can be throwable: {{<todo "*Planned Feature">}}
```cy
type MyError enum:
    with Error
    boom
    badArgument
    nameTooLong

var err = MyError.nameTooLong
```

## Throwing errors.
Use the `throw` keyword to throw errors. 
A thrown error continues to bubble up the call stack until it is caught by a `try` block or expression.
```cy
func fail():
    throw error.Oops      -- Throws an error with the symbol `#Oops`

func fail2():
    throw 123             -- Panic. Can only throw an error
                          -- that implement the `Error` trait.
```

`throw` can also be used as an expression.
```cy
func fail():
    var a = false or throw error.False
```

## Catching errors.

### `try` block.
The `try` block catches thrown errors and resumes execution in a followup `catch` block:
```cy
try:
    funcThatCanFail()
catch err:
    print err      -- 'error.Failed'
```

### `caught` variable.
The contextual `caught` variable is used to reference the caught error:
```cy
try:
    funcThatCanFail()
catch:
    print caught   -- 'error.Failed'
```

### `catch` matching.
An inner `catch` block contains a matching clause: {{<todo "*Planned Feature">}}
```cy
try:
    funcThatCanFail()
catch error.BadDay:
    eatSnack()
catch:
    print caught
```

Enum errors can be matched: {{<todo "*Planned Feature">}}
```cy
try:
    funcThatCanFail()
catch MyError.Boom:
    print 'Kaboom!'
catch:
    print caught
```

### `try` expression.
The `try` expression either returns a non-error result or the default value from the `catch` clause:
```cy
var res = try funcThatCanFail() catch 123
print res         -- '123'
```

Since errors bubble up automatically, any errors thrown from sub-expressions are also caught:
```cy
var res = try happyFunc(funcThatCanFail()) catch 123
print res         -- '123'
```

### Value or error.
When the `catch` clause is omitted, the `try` expression will return either the value or the error:
```cy
var res = try funcThatCanFail()
if res == error.Failed:
    print 'Result is an error.'
```

## Semantic checks.
### `throws` specifier.
The `throws` specifier indicates that a function contains a throwing expression that was not caught with `try catch`.

When a function does not have a return specifier, it's implicitly given the `throws` specifier:
```cy
func foo():
    throw error.Failure

func bar() throws:
    throw error.Failure

-- `foo` and `bar` both have the same return specifier.
```

Return types for typed functions are declared after `throws` using a comma separator:
```cy
func result(cond bool) throws, int:
    if cond:
        return 123
    else:
        throw error.Failure
```

### Requiring `throws`.
A compile-time error is issued when a typed function without a `throws` specifier contains an uncaught throwing expression: {{<todo "*Planned Feature">}}
```cy
func foo(a int) int:
    if a == 10:
        throw error.Failure -- CompileError. `foo` requires the `throws`
    else:                   -- specifier or any throwing expression must
        return a * 2        -- be caught with `try catch`.
```

## Stack trace.
When an uncaught error bubbles up to the top, its stack trace from the `throw` callsite is dumped to the console. The builtin `errorTrace()` and `errorReport()` are used to obtain the stack trace info.
```cy
try:
    funcThatCanFail()
catch:
    -- Prints the stack trace summary of the caught error.
    print errorReport()

    -- Provides structured info about the stack trace.
    var info = errorTrace()
    print info.frames.len()
```

## Unexpected errors.
An unexpected error is an error that is not meant to be handled at runtime.

### Panics.
The builtin `panic` is used as a fail-fast mechanism to quickly exit the current fiber with an error payload:
```cy
func kaboom():
    panic(error.danger)

kaboom()     -- Script ends and prints the stack trace.
```
Panics can not be caught using `try catch`. Once `panic` is invoked, the current fiber stops execution and begins to unwind its call stack. Once the error is propagated to the root, the fiber ends and transitions to a panic state. If the main fiber ends this way, the VM begins to shutdown. Otherwise, execution resumes on the next fiber which allows recovery from a panic.
