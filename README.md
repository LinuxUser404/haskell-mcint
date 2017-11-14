# haskell-mcint


You might need to build the dependencies for the program:
```
cabal install c2hs
cabal install
```
The program needs external Sobol sequence generator.
```
g++ ./src/sobol.cc -o ./src/sobol
```

To build(without building dependencies) and execute in one command just run:

```
cabal run
```

The executable will be in:

```
./dist/build/haskell-mcint/
```

Tested with:

```
ghc-8.0.2
cabal-1.24
haskell-platform-2013.2
base-4.9.1.0
```
