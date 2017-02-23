## Test environments
* local OS X install, R 3.3.2
* ubuntu 12.04 (on travis-ci), R 3.3.2
* Windows Server 2012 (on appveyor), R 3.3.2
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.

## My first submission
This is the package time I have ever submitted anything to CRAN. This is however my second attempt for this package. Thanks to Kurt for his kind response to my first attempt. The problem was 
Found no calls to: ‘R_registerRoutines’, ‘R_useDynamicSymbols’
which I was able to fix by following Giorgio Spedicato's answer at
http://stackoverflow.com/questions/42313373/r-cmd-check-note-found-no-calls-to-r-registerroutines-r-usedynamicsymbols

