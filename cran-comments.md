## Test environments
* local OS X install, R 3.4.3
* ubuntu 12.04 (on travis-ci), R 3.4.3
* Windows Server 2012 (on appveyor), R 3.4.3
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.

## Reverse Dependencies
There are 2 reverse dependencies: `exampletestr`, `ijtiff`. This update breaks neither. In fact, this update is needed for an update to `ijtiff` which is needed for a fix to the `detrendr` package, which is currently broken.
