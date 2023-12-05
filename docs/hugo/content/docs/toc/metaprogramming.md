---
title: "Metaprogramming"
bookFlatSection: true
weight: 10
---

# Metaprogramming.

## Operator overloading.
All operators are implemented as object methods.
> _Incomplete: Not all operators have transitioned to the method paradigm._ 

Normally this would impact performance, but Cyber generates specialized bytecode for builtin types like `int` and `float`. The VM performs inline caching at runtime to eliminate the overhead of evaluating on dynamic operands.

To overload an operator for an object type, declare `$prefix`, `$infix`, `$postfix` methods. See the available [builtin operators](#builtin-operators). Since operator names aren't allowed as standard identifiers, they are contained in a string literal.
```cy
type Vec2 object:
    var x float
    var y float

    func '$infix+'(o Vec2) Vec2:
        return [Vec2
            x: x + o.x,
            y: y + o.y,
        ]

    func '$prefix-'() Vec2:
        return [Vec2 x: -x, y: -y]

var a = [Vec2 x: 1, y: 2]
var b = a + [Vec2 x: 3, y: 4]
var c = -a
```

Some special operators have their own name. This example overloads the `index` operator and the `set index` operator:
```cy
type MyCollection object:
    var arr List

    func '$index'(idx):
        return arr[idx * 2]

    func '$setIndex'(idx, val):
        arr[idx * 2] = val 

var a = [MyCollection arr: [1, 2, 3, 4]]
print a[1]        -- Prints `3`
```

### Builtin operators.
A list of all supported operators:
| Operator | Name |
| -- | -- |
| Bitwise not | `$prefix~` |
| Minus | `$prefix-` |
| Greater | `$infix>` |
| Greater equal | `$infix>=` |
| Less | `$infix<` |
| Less equal | `$infix<=` |
| Add | `$infix+` |
| Subtract | `$infix-` |
| Multiply | `$infix*` |
| Divide | `$infix/` |
| Modulus | `$infix%` |
| Power | `$infix^` |
| Bitwise and | `$infix&` |
| Bitwise or | `$infix\|` |
| Bitwise xor | `$infix\|\|` |
| Bitwise left shift | `$infix<<` |
| Bitwise right shift | `$infix>>` |
| Index | `$index` |
| Set index | `$setIndex` |
| Slice | `$slice` |

### Custom operators.
> _Planned Feature_

## Magic functions.

### Call module.
Declare a `$call` function to allow invoking a module as a function.
> _Incomplete: Although $call function is supported in the VM and builtin modules use it, it is not currently enabled for user modules._ 
```cy
-- Object types are also modules.
type Vec2 object:
    var x float
    var y float

func Vec2.'$call'(x float, y float) Vec2:
    return [Vec2 x: x, y: y]

var v = Vec2(1, 2)
```

### Getter/Setter.
> _Planned Feature_

### Missing method.
Declare a `$missing` method as a fallback when a method was not found in an instance.
> _Planned Feature_
```cy
type A object:

    func '$missing'(args...):
        return args.len

var a = [A:]
print a.foo()      -- Output: '0'
print a.bar(1, 2)  -- Output: '2'
```

## Reflection.
A [`metatype`]({{<relref "/docs/toc/modules#metatype">}}) object references an internal type. Use the `typeof` builtin to get the `metatype` of a value.
```cy
var val = 123
print typeof(val)   -- 'type: float'

-- Referencing a type as a value also returns its `metatype`.
print bool          -- 'type: bool'
```

## Annotations.
Annotations are used to attach modifiers to declarative statements. The `@host` annotation is used for [embedding]({{<relref "/docs/toc/embedding">}}) to bind a host function to a Cyber function:
```cy
@host func compute() float
```

Custom annotations.
> _Planned Feature_

## Runtime eval.
> _Planned Feature_

## Generics.
> _Planned Feature_

## Compile-time.
> _Planned Feature_
