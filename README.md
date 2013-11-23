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

Version 0.9

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



