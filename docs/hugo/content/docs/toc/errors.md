---
title: "Error Handling"
weight: 7
---

# Error Handling.
In Cyber, errors are values and do not propogate up the call stack by default. Users are not forced to handle errors unless the error value is passed to a typed destination.

The `error` type is a primitive that contains either an enum value or a symbol value. Errors can wrap symbols for convenience but the underlying id value won't be consistent. Use your own enums if you want reliable id values. In a future version of Cyber, you'll be able to attach an optional payload value.
```cy
func fail():
    return error.oops     -- Returns an error with the symbol `#oops`

func fail2():
    return error(#oops)   -- Alternatively, use the builtin error function to wrap a symbol.

type MyError enum:
    boom
    badArgument
    nameTooLong

err = error(MyError.boom) -- Creates an error that wraps an enum value.
```

The `try` expression wraps a value and guarantees a non error value is returned. If the value is an error, execution stops in the current block and the error is returned to the parent call site.
```cy
func foo():
    try doSomething()
    return 123

-- If `doSomething()` returns an error, `res` contains the error and not `123`.
res = foo()
```

The `catch` expression returns a non error value or swallows the error and returns the `none` value. If a `then` clause follows, a default value is returned instead of the `none` value. An `as` clause lets you use the error inside the `then` expression.
```cy
func foo():
    return error.boom

res = catch foo()
res = catch foo() then 123
res = catch foo() as err then 123

-- A catch block.
res = catch foo() then:
    break 123
```

## Unexpected Errors.
An unexpected error is an error that you don't plan on handling at runtime. In this scenario, you can prefer to fail-fast and `panic`.

Panics are similar to exceptions in other languages. Once the builtin `panic` is invoked, the current fiber stops execution and begins to unwind its call stack. Once the error is propagated to the root, the fiber ends and transitions to a panic state. If the main fiber ends this way, the VM begins to shutdown.
```cy
func kaboom():
    panic(#danger)

kaboom()     -- Scripts ends and prints the stack trace.
```

While the panic exception is propagated up the call stack, the current fiber can catch the exception in a `recover` block.
The recover block can only be declared at the first indentation level of a scope block.
The recover block is also deferred. After its evaluation, the current call frame is done.
```cy
func kaboom():
    recover err:
        if err == #danger:
            print 'recovered from #danger'
    panic(#danger)

kaboom()    -- Prints recovered message and continues execution in this block.
```