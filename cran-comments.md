## Test environments
* local OS X install, R 3.5.1
* ubuntu 14.04 (on travis-ci), R 3.5.1
* Windows Server 2012 (on appveyor), R 3.5.1
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.

## Reverse Dependencies
There are 5 reverse dependencies: `autothresholdr`, `detrendr`, `exampletestr`, `ijtiff` and `nandb`. This update does not break any. See https://github.com/rorynolan/filesstrings/blob/master/revdep/checks.rds for full check results.

## Fix
This is a fix for failing CRAN checks on mac.
