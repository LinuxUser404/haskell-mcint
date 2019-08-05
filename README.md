# Parallel Quasi-Monte Carlo Integration
[![GitHub](https://img.shields.io/github/license/LinuxUser404/haskell-mcint)](https://github.com/LinuxUser404/haskell-mcint/blob/master/LICENSE)
[![Build Status](https://travis-ci.org/LinuxUser404/haskell-mcint.svg?branch=master)](https://travis-ci.org/LinuxUser404/haskell-mcint)

This program creates an OpenCL kernel for a given integrand and schedules its
evaluation on a parallel device(CPU or GPU depending on what is available on the platform).

It uses Sobol sequence to generate the points where the function needs to be evaluated and averages the result.

To use the program one might need to satisfy the dependencies on the packages this program depends on and have a working OpenCL installation.

Check your OpenCL:
```
clinfo
```

One of the dependencies needs `gsl-config`(part of GNU Scientific Library). Check if you have it:
```
gsl-config --version
```

Only cabal versions 2.4 and above are supported. Check your cabal version:
```
cabal --version
```

Haskell `OpenCL` package(one of the dependencies) requires `c2hs` tools, which in
turn depends on `alex` and `happy`. Make sure they are installed before you proceed.
You might need to run these commands in following order:
```
cabal v2-install alex
cabal v2-install happy
cabal v2-install c2hs
```

You are good to go. Build the project and run it:
```
git clone https://github.com/LinuxUser404/haskell-mcint.git
cd ./haskell-mcint
cabal v2-build
cabal v2-run
```
It shouldn't take more than 10 minutes to finish the calculations(assuming the computation is scheduled on a single CPU core at around 2 GHz).

The computation will hang your desktop for the duration if scheduled on the main GPU.
Though usually it takes only a fraction of a second to complete.

NVidia GPU drivers have build-in time limit which aborts the computation on the
main GPU after 5 seconds. This safety mechanism can be turned off if you are on Linux.
Same as with CUDA computations.

AMD GPUs have not been tested yet. Dependencies might differ if you are on Windows or Mac.

# Roadmap
This is the old version of the algorithm, where the random sequence was computed on a CPU without proper optimizations and created a massive performance overhead. I had to switch to C++ to solve the problem in time for my masters project back in 2018, yet I plan to port those changes back here when/if I have time.
