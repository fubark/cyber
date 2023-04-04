---
title: "Metaprogramming"
bookFlatSection: true
weight: 10
---

# Metaprogramming.

## Operator overloading.

## Custom operators.

## Function/Method fallback.

## Reflection.
A `metatype` object references an internal type. Use the `typeof` builtin to get the `metatype` of a value.
```cy
val = 123
print typeof(val)   -- 'type: number'

-- Referencing a type as a value also returns its `metatype`.
print boolean       -- 'type: boolean'
```

### `type metatype`
```cy
func id(self) number
-- Returns the type ID as a number.
```

## Annotations.

## Runtime eval.

## Generics.

## Compile-time.