* Type checker issue:
```
λπ> let idType = forall (a :: *) . a -> a
idType :: *
∀ (a :: *) . a -> a
λπ> let id = (\a x -> x) :: idType

let zero = (λa f x -> x) :: forall (a :: *) . (a -> a) -> (a -> a)
let succ = (λn a f x -> f (n a f x)) :: (forall (a :: *) . (a -> a) -> (a -> a)) -> (forall (a :: *) . (a -> a) -> (a -> a))
```
* Web FE
  * Split between simple / dependently typed tabs
