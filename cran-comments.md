## Test environments
* local OS X install, R 3.5.0
* ubuntu 14.04 (on travis-ci), R 3.5.0
* Windows Server 2012 (on appveyor), R 3.5.0
* valgrind on r-hub, R 3.4.4
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.

## Reverse Dependencies
There are 5 reverse dependencies: `autothresholdr`, `detrendr`, `exampletestr`, `ijtiff` and `nandb`. This update breaks `autothresholdr` but a fix is ready to go (I'm the maintainer of `autothresholdr` too). See https://github.com/rorynolan/filesstrings/blob/master/revdep/checks.rds for full check results.
