# Questions

1. Why does `Lam` take a `CheckableTerm`?
2. Why is `Bound` inferrable? The type checker doesn't even handle that case, as `Bound` nodes are rewritten to `Free(Local(...))`.
3. Why are bodies of `Lambda`s rewritten?

# Notes
* `id` and `const` are "polymorphic"? They don't have type information associated with them.
