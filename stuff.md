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

```
LP> Fin
\ x -> Fin x :: forall x :: Nat . *
LP> FZero
\ x -> FZero x :: forall x :: Nat . Fin (Succ x)
LP> FSucc
\ x -> \ y -> FSucc x y :: forall (x :: Nat) (y :: Fin x) . Fin (Succ x)
LP> finElim
\ x -> \ y -> \ z -> \ a -> \ b -> finElim x y z a
                                     b :: forall (x :: forall (x :: Nat) (y :: Fin x) . *)
                                                 (y :: forall y :: Nat . x (1 + y) (FZero y))
                                                 (z :: forall (z :: Nat) (a :: Fin z) (b :: x z a) .
                                                       x (1 + z) (FSucc z a))
                                                 (a :: Nat)
                                                 (b :: Fin a) .
                                          x a b

finElim translation 
(\m mz ms n f -> finElim m mz ms n f)
                                       :: forall (m :: forall (n :: Nat) . Fin n -> *) . 
                                                 (forall n :: Nat . m (Succ n) (FZero n)) ->
                                                 (forall (n :: Nat) (f :: Fin n) . m n f -> m (Succ n) (FSucc n f)) ->
                                                 forall (n :: Nat) (f :: Fin n) . m n f

```

```
(\m mz ms n f -> finElim m mz ms n f) :: forall (m :: forall (n :: Nat) . Fin n -> *) . (forall n :: Nat . m (Succ n) (FZero n)) -> (forall (n :: Nat) (f :: Fin n) . m n f -> m (Succ n) (FSucc n f)) -> forall (n :: Nat) (f :: Fin n) . m n f
```

```
assume (finElim :: forall (m :: forall (n :: Nat) . Fin n -> *) . (forall n :: Nat . m (Succ n) (FZero n)) -> (forall (n :: Nat) (f :: Fin n) (b :: m n f) . m (Succ n) (FSucc n f)) -> forall (n :: Nat) (f :: Fin n) . m n f)
let finNat = finElim (\ _ _ -> Nat) (\ _ -> Zero) (\ _ _ rec -> Succ rec)
finNat 1 (FZero 0)
```



forall (x :: forall (x :: Nat) (y :: Fin x) . *) (y :: forall y :: Nat . x (Succ y) (FZero y)) (z :: forall (z :: Nat) (a :: Fin z) (b :: x z a) . x (Succ z) (FSucc z a)) (a :: Nat) (b :: Fin a) . x a b