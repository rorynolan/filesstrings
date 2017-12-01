## Test environments
* local OS X install, R 3.4.2
* ubuntu 12.04 (on travis-ci), R 3.4.2
* Windows Server 2012 (on appveyor), R 3.4.2
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.

## Reverse Dependencies
There are 3 reverse dependencies: `detrendr`, `exampletestr` and `nandb`. This update breaks two of them. Updates to all 3 to come in line with the update of this package are ready to go and will be submitted immediately when this package is accepted (I am the maintainer of all 3). 
