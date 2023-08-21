---
title: "Error Handling"
weight: 7
---

# Error Handling.
Cyber provides error values and try/catch mechanisms to handle expected errors. For unexpected errors, panics can be used as a fail-fast mechanism to abort the currently running fiber.

## Error value.
The `error` type is a primitive that contains either an enum value or a symbol value. Errors can wrap symbols for convenience but the underlying ID value won't be consistent. Use your own enums if you want reliable ID values. In a future version of Cyber, you'll be able to attach an optional payload value.
```cy
-- Shorthand for creating an error value with a symbol.
var err = error.Oops

-- Alternatively, use the builtin error function to wrap a symbol.
err = error(#Oops)

type MyError enum:
    boom
    badArgument
    nameTooLong

-- Creates an error that wraps an enum value.
err = error(MyError.boom)
```
Since errors are primitives, they can be compared using the `==` operator.
```cy
if err == error.Oops:
    handleOops()

-- Alternatively.
if err.value() == #Oops:
    handleOops()

-- Comparing errors with enums.
if err == error(MyError.boom)
    handleBoom()

-- Alternatively.
if err.value() == MyError.boom
    handleBoom()
```

## Throwing errors.
Use the `throw` keyword to throw errors. 
A thrown error continues to bubble up the call stack until it is caught by a `try` block or expression.
```cy
func fail():
    throw error.Oops      -- Throws an error with the symbol `#Oops`

func fail2():
    throw 123             -- Panic, Can only throw errors.
```

`throw` can also be used as an expression.
```cy
func fail():
    var a = false or throw error.False
```

## Catching errors.
The `try catch` block catches thrown errors and resumes execution in the `catch` block.
```cy
try:
    funcThatCanFail()
catch err:
    print err     -- 'error.Failed'
```

The `try else` expression either returns a non-error result or the default value from the `else` clause.
```cy
var res = try funcThatCanFail() else 123
print res         -- '123'

-- Any errors thrown from sub expressions also return the default value.
res = try happyFunc(funcThatCanFail()) else 123
print res         -- '123'
```

When `try` is used by itself, either the result or the caught error value is returned.
```cy
var res = try funcThatCanFail()
if res == error.Failed:
    print 'Result is an error.'

-- Any errors thrown from sub expressions are also caught.
res = try happyFunc(funcThatCanFail())
```

## Stack trace.
When an uncaught error bubbles up to the top, its stack trace from the `throw` callsite is dumped to the console. Cyber also provides the builtin `errorTrace()` and `errorReport()` to obtain the stack trace info.
```cy
try:
    funcThatCanFail()
catch err:
    -- Prints the stack trace summary of the caught error.
    print errorReport()

    -- Provides structured info about the stack trace.
    var info = errorTrace()
    print info.frames.len()
```

## Unexpected errors.
An unexpected error is an error that you don't plan on handling at runtime. In this scenario, you can prefer to fail-fast and `panic`.

Panics can not be caught using `try catch`. Once the builtin `panic` is invoked, the current fiber stops execution and begins to unwind its call stack. Once the error is propagated to the root, the fiber ends and transitions to a panic state. If the main fiber ends this way, the VM begins to shutdown. Otherwise, execution can resume on the next fiber which allows you to recover from a panic.
```cy
func kaboom():
    panic(#danger)

kaboom()     -- Script ends and prints the stack trace.
```