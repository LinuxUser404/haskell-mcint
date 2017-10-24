# haskell-mcint


You might need to build the dependencies for the program:
```
cabal install OpenCL
cabal install numbers
cabal install parsec
cabal install criterion
```

To build and execute in one command just run:

```
cabal run
```


To build the program run:

```
cabal install
```

The executable will be in:

```
./dist/build/haskell-mcint/
```

Tested with:

```
ghc-7.10.3
cabal-1.22.8
haskell-platform-2013.2
base-4.8.2.0
```
