# filesstrings 0.3.0
The first edition that I think may be CRAN-worthy.

## 0.3.1
Fix problem of
Found no calls to: ‘R_registerRoutines’, ‘R_useDynamicSymbols’
by following Giorgio Spedicato's answer at
http://stackoverflow.com/questions/42313373/r-cmd-check-note-found-no-calls-to-r-registerroutines-r-usedynamicsymbols

# 0.3.2
A fix to make the package compatible with the new version of 'readr' courtest of Jim Hester.
Minor documentation improvements.
StrReverse() is removed. Use stringi::stri_reverse() instead.

