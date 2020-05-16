`docker run -it -v /Users/matt/repos/LambdaPi:/code --rm registry.gitlab.com/haskell-ci/haskell-ci/ghc-7.4.2:latest bash`

```cd /code

export PATH=$PATH:/opt/ghc/bin/
cabal update
sudo apt-get update
sudo apt-get install libreadline-dev
cabal run -w ghc-7.4.2
```

`docker commit 8e489a6624b6 lambda-pi`
`docker run -it --rm -v /Users/matt/repos/LambdaPi:/code lambda-pi bash -c 'cd /code && PATH=$PATH:/opt/ghc/bin/ cabal run -w ghc-7.4.2'`

* Why does Haskell implementation not include type checker for FinElim?

### Software Foundations Examples

`plus_id_example`:

```(\m n e -> leibniz Nat Nat (\i -> plus i i) n m e) :: forall (m :: Nat) (n :: Nat) . Eq Nat n m -> Eq Nat (plus n n) (plus m m)```

`plus_id_exercise`:

```(\n m o enm emo -> tran Nat (plus n m) (plus m m) (plus m o) (leibniz Nat Nat (\i -> plus i m) n m enm) (leibniz Nat Nat (\i -> plus m i) m o emo) ):: forall (n :: Nat) (m :: Nat) (o :: Nat) . Eq Nat n m -> Eq Nat m o -> Eq Nat (plus n m) (plus m o)```


`mult_0_plus`:

```
let multiply = natElim (\_ -> Nat -> Nat) (\n -> 0) (\p rec n -> plus n (rec n))
(\n m -> Refl Nat (multiply n m)) :: forall (n :: Nat) (m :: Nat) . Eq Nat (multiply (plus 0 n) m) (multiply n m)

```

`mult_S_1`:


```

(\n m e -> symm Nat (multiply m m) (multiply m (Succ n)) (leibniz Nat Nat (\i -> multiply m i) m (Succ n) e)) :: forall (n :: Nat) (m :: Nat) . Eq Nat m (Succ n) -> Eq Nat (multiply m (plus 1 n)) (multiply m m)

```