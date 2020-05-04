* Type checker issue:
```
λπ> let succ = (λ a n f x -> f (n a f x)) :: forall (a :: *) . ((a -> a) -> (a -> a)) -> ((a -> a) -> (a -> a))
Type mismatch. Expected type '(forall (a :: Local-0) . Local-0)', but was inferred as '*'.
```
* Unicode arrows
* Web FE
  * Split between simple / dependently typed tabs
* Pretty printer for `->`
* Assume statement syntax