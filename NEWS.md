### 2.0.2

#### BUG FIXES
* Fix to `all_equal()` for dealing with arrays.


### 2.0.1

#### BUG FIXES
* The new R doesn't like it when the working directory is changed by running examples. This required a fix which this patch provides.


## 2.0.0

#### NEW FEATURES
* Functions which were previously deprecated are now defunct. This brings the package completely in line with the tidyverse style.

#### MINOR IMPROVEMENTS
* `count_matches()` and `str_nth_instance_indices()` are much faster.
* `merge_tables_on_disk()` and `paste_different_lengths()` are gone. They didn't belong.

#### BUG FIXES
* `before_last_dot()` now works in the case where the input has no dots, returning the input.


### 1.1.0

#### NEW FEATURES
* Added `first` and `last` companions for `nth` functions.

#### MINOR IMPROVEMENTS
* Minor documentation improvements.


### 1.1.0

#### NEW FEATURES
* Everything has been redone to conform with the tidyverse style guide.


## 1.0.0

#### NEW FEATURES
* The package is now peer-reviewed and has an accompanying paper in the journal of open-source software, which can be cited. See `citation("filesstrings")`.


### 0.4.2

#### NEW FEATURES
* The package now has the http://contributor-covenenant.org code of conduct.
* The package now has the functions `file.move()` and `dir.delete()` to conform with the `base` naming pattern of such functions.

#### DEFUNCT
* `PutFilesInDir()` is gone.

#### MINOR IMPROVEMENTS
* The functionality that `PutFilesInDir()` had is now default for `MoveFiles()`.
* The README and vignettes are improved.


### 0.4.1

#### BUG FIXES
* Minor fix to `AllEqual()`.


### 0.4.0

#### BUG FIXES
* Fix bug in `AllEqual()` and improve its documentation.
* Improve `NA` handling of `ExtractNumerics()` and its documentation.

#### MINOR IMPROVEMENTS
* Improve README and vignette.


### 0.3.2

#### BUG FIXES
* A fix to make the package compatible with the new version of 'readr' courtesy of Jim Hester.

#### MINOR IMPROVEMENTS
* Minor documentation improvements.

#### DEFUNCT
* `StrReverse()` is removed. Use `stringi::stri_reverse()` instead.


### 0.3.1

#### BUG FIXES
* Fixed problem of
`Found no calls to: ‘R_registerRoutines’, ‘R_useDynamicSymbols’`
by following Giorgio Spedicato's answer at
http://stackoverflow.com/questions/42313373/r-cmd-check-note-found-no-calls-to-r-registerroutines-r-usedynamicsymbols


### filesstrings 0.3.0
* The first edition that I think may be CRAN-worthy.
