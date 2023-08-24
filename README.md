StringMap
=========


About
-----

An efficient implementation of maps from strings to arbitrary values.
Values can associated with an arbitrary byte key.
Searching for keys is very fast, but the prefix tree probably consumes
more memory than "Data.Map". The main differences are the special
'prefixFind' functions, which can be used to perform prefix queries.

StringMap was originally developed as part of the holumbus project of the FH-Wedel.
It is now a separate cabal package.

Version 1.0.0

Installation
------------

If you just want to use this StringMap, just use the hackage repository:
$ cabal update && cabal install data-stringmap

Documentation
-------------

The documetation is also available on Hackage:
http://hackage.haskell.org/package/data-stringmap


Developmemt
------------

just run
$ cabal install
in the root directory. Everything else should be done automatically by cabal.


Benchmarking
------------

Execute
```
$ cd StringMap
$ cp benchmarks/space/en_US.dict
$ cabal bench
...
Running 1 benchmarks...
Benchmark bench-all: RUNNING...
benchmarking lookup
time                 27.78 ms   (25.56 ms .. 29.25 ms)
                     0.981 R²   (0.955 R² .. 0.995 R²)
mean                 31.97 ms   (30.56 ms .. 33.15 ms)
std dev              2.811 ms   (2.310 ms .. 3.555 ms)
variance introduced by outliers: 34% (moderately inflated)

benchmarking insert
time                 57.58 ms   (55.47 ms .. 59.29 ms)
                     0.997 R²   (0.994 R² .. 1.000 R²)
mean                 56.74 ms   (55.23 ms .. 58.51 ms)
std dev              3.083 ms   (2.058 ms .. 4.828 ms)
variance introduced by outliers: 15% (moderately inflated)
...
Benchmark bench-all: FINISH
```