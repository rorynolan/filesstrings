## Test environments
* local OS X install, R 3.4.2
* ubuntu 12.04 (on travis-ci), R 3.4.2
* Windows Server 2012 (on appveyor), R 3.4.2
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.

## Reverse Dependencies
There are 3 reverse dependencies: `exampletestr`, `detrendr`, and `nandb`. The latter currently has its issues too. I am the maintainer of it and am working on it to get it fixed. I have been given a deadline of 5th January for it and I expect to have it fixed by 1st January.

## Fix
* This package was submitted to CRAN recently, but experienced warnings when the new R-devel came out (because the new devel doesn't like examples to change the working directory). This is a fix to address this new requirement in R-devel.
