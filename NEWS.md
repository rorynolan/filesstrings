# `filesstrings` 3.4.0

## NEW FEATURES
* Add github.io website to DESCRIPTION.


# `filesstrings` 3.3.0

## NEW FEATURES
* Code is now robust to changes to the `strex` package.


# `filesstrings` 3.2.4

## BUG FIXES
* Insist on bug-fixed `strex` v1.6.0.


# `filesstrings` 3.2.3

## BUG FIXES
* Insist on bug-fixed `stringi` v1.7.8 and `strex` v1.4.3.


# `filesstrings` 3.2.2

## BUG FIXES
* Remove `LazyData` from `DESCRIPTION` (was causing CRAN note).


# `filesstrings` 3.2.1

## BUG FIXES
* R version 3.5 or greater is needed for `strex` >= 1.4.


# `filesstrings` 3.2.0

## BUG FIXES
* Insist on `strex` >= 1.4 which has improved argument matching.


# `filesstrings` 3.1.7

## BUG FIXES
* Post `strex` v1.3.0 release adjustments.


# `filesstrings` 3.1.6

## BUG FIXES
* Ready the package not to break on next release of `strex` (v1.3.0).


# `filesstrings` 3.1.5

## BUG FIXES
* Fix `trim_anything()` docs.
* Insist on latest `strex` (v1.1.1).


# `filesstrings` 3.1.4

## BUG FIXES
* Be ready not to break for `strex` v1.1.0.


# `filesstrings` 3.1.3

## BUG FIXES
* Demand bug-fixed `strex` v1.0.3.


# `filesstrings` 3.1.2

## BUG FIXES
* `all_equal()` was dealing inconsistently with attributes.


# `filesstrings` 3.1.1

## BUG FIXES
* Inherit bug fixes in `strex` v1.0.1.


# `filesstrings` 3.1.0

## BREAKING CHANGES
* `str_get_currency()` and `str_get_currencies()` are gone. They are replaced by `str_extract_currencies()` and `str_nth_currency()`, `str_first_currency()` and `str_last_currency()`.
* `str_nth_instance_indices()` has been renamed to `str_locate_nth()` and `str_first_instance_indices()` and `str_last_instance_indices()` have been renamed to `str_locate_first()` and `str_locate_last()`. The aliases `locate_nth()`, `locate_first()` and `locate_last()` have been added.  

## NEW FUNCTIONS
* `str_elems()` for extracting several single elements from a string.

## NEW FUNCTION ALIASES
* All string manipulation functions are now optionally prepended by `str_`, for example  `str_before_last_dot()` is a new alias for `before_last_dot()` and `str_extract_numbers()` is a new alias for `extract_numbers()`.
* `nice_nums()` now has the aliases `str_nice_nums()`, `str_alphord_nums()` and `alphord_nums()`.


# `filesstrings` 3.0.1

## BUG FIXES
* Depend on latest, least buggy `strex`.


# `filesstrings` 3.0.0

## DEFUNCT
* `str_with_patterns()` is defunct in light of `stringr::str_subset()`.
* `count_matches()` is defunct in light of `stringr::str_count()`.

## BUG FIXES
* Depend on a version of `strex` which is reliable on mac.


# `filesstrings` 2.7.1

## BUG FIXES
* Insist on `strex` v0.1.1; v0.1.0 didn't pass on mac on CRAN.


# `filesstrings` 2.7.0

## NEW FEATURES
* All of the `stringr`-style string manipulation is now done by the `strex` package, which `filesstrings` now depends upon. That was the main functionality of `filesstrings`, so `filesstrings` is going to be more dormant from now on.


# `filesstrings` 2.6.0

## NEW FEATURES
* Added functions `nth_number_before_mth()` and friends (there are 8 friends).

## MINOR IMPROVEMENTS
* Improvements to documentation.


# `filesstrings` 2.5.0

## NEW FEATURES
* Improvement to `all_equal()` and its documentation.

## BUG FIXES
* Fix to message of `file.move()`.


# `filesstrings` 2.4.0

## NEW FEATURES
* Added `nth_number_after_mth()` and friends (there are 8 friends).

## DEFUNCT
* Defunct functions are now gone completely (as opposed to triggering a `.Defunct()` call).



# `filesstrings` 2.3.0

## MINOR IMPROVEMENTS
* Add an `overwrite` argument to `file.move()` with default value `FALSE` to ensure that files are not accidentally overwritten.


# `filesstrings` 2.2.0

## MINOR IMPROVEMENTS
* Speed up `extract_numbers()`.

## BUG FIXES
* Fix some bad typing in C++ code.
* `remove_dirs()` was claiming to have deleted directories that weren't there.


# `filesstrings` 2.1.0

## NEW FEATURES
* There is a new function `match_arg()` for argument matching which is inspired by  `RSAGA::match.arg.ext()`. Its behaviour is almost identical but `RSAGA` is a heavy package to depend upon so `filesstrings::match_arg()` might be handy for package developers.


# `filesstrings` 2.0.4

## BUG FIXES
* Fix bug in `extract_numbers()` which arose when integerish numbers outside the 32-bit integer range reared their heads.


# `filesstrings` 2.0.3

## BUG FIXES
* Fix to message in `remove_filename_spaces()`.


# `filesstrings` 2.0.2

## BUG FIXES
* Fix to `all_equal()` for dealing with arrays.


# `filesstrings` 2.0.1

## BUG FIXES
* The new R doesn't like it when the working directory is changed by running examples. This required a fix which this patch provides.


# `filesstrings` 2.0.0

## NEW FEATURES
* Functions which were previously deprecated are now defunct. This brings the package completely in line with the tidyverse style.

## MINOR IMPROVEMENTS
* `count_matches()` and `str_nth_instance_indices()` are much faster.
* `merge_tables_on_disk()` and `paste_different_lengths()` are gone. They didn't belong.

## BUG FIXES
* `before_last_dot()` now works in the case where the input has no dots, returning the input.


# `filesstrings` 1.1.0

## NEW FEATURES
* Added `first` and `last` companions for `nth` functions.

## MINOR IMPROVEMENTS
* Minor documentation improvements.


# `filesstrings` 1.1.0

## NEW FEATURES
* Everything has been redone to conform with the tidyverse style guide.


# `filesstrings` 1.0.0

## NEW FEATURES
* The package is now peer-reviewed and has an accompanying paper in the journal of open-source software, which can be cited. See `citation("filesstrings")`.


# `filesstrings` 0.4.2

## NEW FEATURES
* The package now has the http://contributor-covenenant.org code of conduct.
* The package now has the functions `file.move()` and `dir.delete()` to conform with the `base` naming pattern of such functions.

## DEFUNCT
* `PutFilesInDir()` is gone.

## MINOR IMPROVEMENTS
* The functionality that `PutFilesInDir()` had is now default for `MoveFiles()`.
* The README and vignettes are improved.


# `filesstrings` 0.4.1

## BUG FIXES
* Minor fix to `AllEqual()`.


# `filesstrings` 0.4.0

## BUG FIXES
* Fix bug in `AllEqual()` and improve its documentation.
* Improve `NA` handling of `ExtractNumerics()` and its documentation.

## MINOR IMPROVEMENTS
* Improve README and vignette.


# `filesstrings` 0.3.2

## BUG FIXES
* A fix to make the package compatible with the new version of 'readr' courtesy of Jim Hester.

## MINOR IMPROVEMENTS
* Minor documentation improvements.

## DEFUNCT
* `StrReverse()` is removed. Use `stringi::stri_reverse()` instead.


# `filesstrings` 0.3.1

## BUG FIXES
* Fixed problem of
`Found no calls to: ‘R_registerRoutines’, ‘R_useDynamicSymbols’`
by following Giorgio Spedicato's answer at
http://stackoverflow.com/questions/42313373/r-cmd-check-note-found-no-calls-to-r-registerroutines-r-usedynamicsymbols


# `filesstrings` 0.3.0
* The first edition that I think may be CRAN-worthy.
