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
* Tidy up pretty-printer