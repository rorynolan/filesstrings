


## Test environments

* local OS X install, R 3.5.1

* ubuntu 14.04 (on travis-ci), R 3.5.1

* Windows Server 2012 (on appveyor), R 3.5.1

* win-builder (devel and release)



## R CMD check results

There were no ERRORs, WARNINGs or NOTEs.



## Reverse Dependencies

There are 5 reverse dependencies: `autothresholdr`, `detrendr`, `exampletestr`, `ijtiff` and `nandb`. This update breaks `nandb`. 

I am the maintainer of `nandb` and an update to this package (which will depend on this newest version of `filesstrings`) is ready to go so as soon as the new `filesstrings` is on CRAN with binaries built, I'll submit the `nandb` fix.

See https://github.com/rorynolan/filesstrings/blob/master/revdep/checks.rds for full check results.
