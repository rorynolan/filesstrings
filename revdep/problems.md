# detrendr

Version: 0.6.1

## Newly fixed

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════════════════════════════════
      OK: 46 SKIPPED: 0 WARNINGS: 1 FAILED: 14
      1. Error: best_tau works (@test-best_params.R#5) 
      2. Error: best_l works (@test-best_params.R#41) 
      3. Error: best_degree works (@test-best_params.R#82) 
      4. Error: best_swaps() works (@test-best_params.R#114) 
      5. Error: detrending works (@test-detrend.R#3) 
      6. Error: detrending errors correctly (@test-detrend.R#170) 
      7. Error: detrending entire derectories works (@test-dir_detrend.R#11) 
      8. Error: file_detrend() deals with other directories correctly (@test-dir_detrend.R#176) 
      9. Error: sum_pillars() and mean_pillars() work (@test-utils.R#20) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 34-37 (single-images.Rmd) 
    Error: processing vignette 'single-images.Rmd' failed with diagnostics:
    could not find function "intmat_list_bind_nth_rows"
    Execution halted
    ```

## In both

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# exampletestr

Version: 1.4.1

## Newly fixed

*   checking examples ... ERROR
    ```
    ...
    [34mAuthors@R[39m (parsed):
        * First Last <first.last@example.com> [aut, cre] (<https://orcid.org/YOUR-ORCID-ID>)
    [34mDescription[39m: What the package does (one paragraph).
    [34mLicense[39m: What license it uses
    [34mEncoding[39m: UTF-8
    [34mLazyData[39m: true
    [32m✔[39m Writing [34m'NAMESPACE'[39m
    [32m✔[39m Setting active project to [34m'<no active project>'[39m
    > file.copy(system.file("extdata", c("detect.R", "match.R"),
    +                       package = "exampletestr"),
    +           paste0(tempdir(), "/R"))
    [1] TRUE TRUE
    > make_test_shell_fun("str_detect()", document = TRUE, open = FALSE,
    +                     pkg_dir = tempdir())
    Running [90m`roxygen2::roxygenize()`[39m . . .
    Updating roxygen version in /private/var/folders/l_/2mwm03p55zg7zjykv084hhvr0000gn/T/RtmpJsmCKz/DESCRIPTION
    Loading RtmpJsmCKz
    Error in intmat_list_bind_nth_rows(instances, n - 1) : 
      could not find function "intmat_list_bind_nth_rows"
    Calls: make_test_shell_fun ... str_after_nth -> str_nth_instance_indices -> %>% -> eval -> eval
    Execution halted
    ```

## In both

*   checking tests ...
    ```
    ...
    < https       NEWS.md:31
    < rorynolan   NEWS.md:31
    < verisons    NEWS.md:24
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > test_check("exampletestr")
      [31m──[39m [31m1. Error: `make_tests_shells_file()` works (@test-exemplar.R#211) [39m [31m──────────────────────────────────────[39m
      more 'from' files than 'to' files
      1: expect_true(all(file.copy(system.file("extdata", c("detect.R", "match.R"), package = "exampletestr"), "R"))) at testthat/test-exemplar.R:211
      2: quasi_label(enquo(object), label, arg = "object")
      3: eval_bare(get_expr(quo), get_env(quo))
      4: file.copy(system.file("extdata", c("detect.R", "match.R"), package = "exampletestr"), "R")
      5: stop("more 'from' files than 'to' files")
      
      ══ testthat results  ═══════════════════════════════════════════════════════════════════════════════════════
      OK: 21 SKIPPED: 0 WARNINGS: 7 FAILED: 1
      1. Error: `make_tests_shells_file()` works (@test-exemplar.R#211) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# nandb

Version: 2.0.0

## Newly fixed

*   checking examples ... ERROR
    ```
    Running examples in ‘nandb-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: brightness
    > ### Title: Calculate brightness from image series.
    > ### Aliases: brightness
    > 
    > ### ** Examples
    > 
    > img <- ijtiff::read_tif(system.file('extdata', '50.tif', package = 'nandb'))
    Error in intmat_list_bind_nth_rows(instances, n - 1) : 
      could not find function "intmat_list_bind_nth_rows"
    Calls: <Anonymous> ... str_before_nth -> str_nth_instance_indices -> %>% -> eval -> eval
    Execution halted
    ```

*   checking tests ...
    ```
    ...
    < Kmer        NEWS.md:36
    < linux       NEWS.md:20
    < tidyverse   NEWS.md:27
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════════════════════════════════
      OK: 48 SKIPPED: 1 WARNINGS: 1 FAILED: 17
      1. Error: brightness works (@test-brightness.R#5) 
      2. Error: brightness_folder works (@test-brightness.R#40) 
      3. Error: brightness_timeseries works (@test-brightness.R#92) 
      4. Error: cc_brightness() works (@test-cc_brightness.R#3) 
      5. Error: cc_brightness_timeseries() works (@test-cc_brightness.R#51) 
      6. Error: cc_brightness_folder() works (@test-cc_brightness.R#126) 
      7. Error: cc_brightness_timeseries_folder() works (@test-cc_brightness.R#152) 
      8. Error: cc_number() works (@test-cc_number.R#3) 
      9. Error: cc_number_timeseries() works (@test-cc_number.R#51) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 35-38 (single-images.Rmd) 
    Error: processing vignette 'single-images.Rmd' failed with diagnostics:
    could not find function "intmat_list_bind_nth_rows"
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘MASS’ ‘stats’
      All declared Imports should be used.
    ```

