# Questions

1. Why does `Lambda` take a `CheckableTerm`? Why does `App` take `InferrableTerm` for functions and `CheckableTerm` 
for argument? A: Better where possible for these constructors to take `CheckableTerm` arguments, as they can then embed
lambdas without annotation. For `App`, certainly not possible for both to be checkable, nor is it possible to infer the type
if the function type is Checkable and the argument type is inferrable.
2. Why is `Bound` inferrable? The type checker doesn't even handle that case, as `Bound` nodes are rewritten to 
`Free(Local(...))`. A: Everything would ideally be inferrable, as a `CheckableTerm` requires an annotation to work
with. So the question is really why is `Lamda` checkable, not inferrable? In general, there may not be enough 
information below a lambda to infer its type (e.g. \x -> 42).
3. Why are bodies of `Lambda`s rewritten? A: to check a lambda is of a function type `A -> B`, we need to check that
 the body of the lambda is of type `B` when the lambda of the variable has type `A` . This is done by creating a 
 guaranteed fresh variable `v`, adding it to the context with type `A`, and replacing any `Bound` references to it
 with a `Free`.

# Notes
* `id` and `const` are "polymorphic"? They don't have type information associated with them.
