use meta

--| Utilities for testing.
--| Sample usage:
--| ```cy
--| use test
--| 
--| a := 123 + 321
--| test.eq(444, a)
--| ```

--| Panics if `pred` is `false`.
fn assert(pred bool):
    if !pred:
        panic('Assert `true`, found `false`.')

--| Returns whether two values are equal.
--| Panics with `error.AssertError` if types or values do not match up.
fn eq(exp %T, act T):
    if exp != act:
        panic('Expected `%{exp}`, found `%{act}`.')

fn eqType(exp type, act type):
    if exp != act:
        meta.error('Expected `' + type.name(exp) + '`, found `' + type.name(act) + '`.')

--| This will be deprecated once `eq_span` can accept `AsSpan[%T]`.
fn eq_slice(exp []%T, act []T):
    eq_span(exp.span(), act.span())

--| Returns `true` if two spans have the same size and the elements are equal
--| as if `eq` was called on those corresponding elements.
fn eq_span(exp [&]%T, act [&]T):
    if exp.len() != act.len():
        fail('Length mismatch: %{exp.len()} != %{act.len()}')
    for exp |i, elem|:
        if elem != act[i]:
            fail('Expected %{elem}, found %{act[i]}.\nAt element %{i}.')

--| Returns `true` if two numbers are near each other within epsilon 1e-5.
fn eqNear(exp float, act float):
    eqNear(exp, act, 1e-5)

fn eqNear(exp float, act float, epsilon float):
    if (exp - act).abs() >= epsilon:
        fail('Expected `%{exp}` within `%{epsilon}`, found `%{act}`.')

fn fail():
    panic('AssertError')

fn fail(msg str):
    panic('AssertError: ' + msg)
    