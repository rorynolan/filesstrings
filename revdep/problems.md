# autothresholdr

<details>

* Version: 1.3.3
* Source code: https://github.com/cran/autothresholdr
* URL: https://rorynolan.github.io/autothresholdr/, https://www.github.com/rorynolan/autothresholdr
* BugReports: https://www.github.com/rorynolan/autothresholdr/issues
* Date/Publication: 2019-06-12 14:40:03 UTC
* Number of recursive dependencies: 78

Run `revdep_details(,"autothresholdr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘autothresholdr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: auto_thresh
    > ### Title: Automatically threshold an array of non-negative integers.
    > ### Aliases: auto_thresh auto_thresh_mask auto_thresh_apply_mask mask
    > ###   apply_mask
    > 
    > ### ** Examples
    > 
    > img_location <- system.file("extdata", "eg.tif", package = "autothresholdr")
    > img <- ijtiff::read_tif(img_location)
    Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
      namespace ‘strex’ 1.1.0 is being loaded, but >= 1.1.1 is required
    Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      11: asNamespace(ns)
      12: loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]])
      13: stop(gettextf("namespace %s %s is being loaded, but %s %s is required", sQuote(package), version, 
             zop, zversion), domain = NA)
      
      ══ testthat results  ════════════════════════════════════════════════════════════════════════════
      OK: 0 SKIPPED: 0 WARNINGS: 0 FAILED: 5
      1. Error: (unknown) (@test_auto_thresh_methods.R#3) 
      2. Error: mean_stack_thresh works (@test_stack_thresh.R#3) 
      3. Error: auto_thresh works (@test_thresh.R#4) 
      4. Error: auto_thresh works with matrices (@test_thresh.R#73) 
      5. Error: translate_fail works (@test_utils.R#4) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# detrendr

<details>

* Version: 0.6.2
* Source code: https://github.com/cran/detrendr
* URL: https://rorynolan.github.io/detrendr, https://www.github.com/rorynolan/detrendr
* BugReports: https://www.github.com/rorynolan/detrendr/issues
* Date/Publication: 2019-06-15 15:40:03 UTC
* Number of recursive dependencies: 84

Run `revdep_details(,"detrendr")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
    ...
    < Potential spelling errors:
    <   WORD           FOUND IN
    < Permanentize   NEWS.md:5
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ════════════════════════════════════════════════════════════════════════════
      OK: 52 SKIPPED: 8 WARNINGS: 0 FAILED: 16
      1. Error: best_tau works (@test-best_params.R#4) 
      2. Error: best_l works (@test-best_params.R#40) 
      3. Error: best_degree works (@test-best_params.R#81) 
      4. Error: best_swaps() works (@test-best_params.R#113) 
      5. Failure: detrended_img works (@test-class-constructors.R#5) 
      6. Error: detrended_img works (@test-class-constructors.R#14) 
      7. Error: detrending works (@test-detrend.R#4) 
      8. Error: detrending errors correctly (@test-detrend.R#171) 
      9. Error: detrending entire derectories works (@test-dir-detrend.R#11) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# ijtiff

<details>

* Version: 2.0.0
* Source code: https://github.com/cran/ijtiff
* URL: https://ropensci.github.io/ijtiff
* BugReports: https://www.github.com/ropensci/ijtiff/issues
* Date/Publication: 2019-06-10 14:30:02 UTC
* Number of recursive dependencies: 77

Run `revdep_details(,"ijtiff")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
    > if (require(EBImage)) {
    +   img <- read_tif(system.file("img", "Rlogo.tif", package = "ijtiff"))
    +   str(img)
    +   str(as_EBImage(img))
    +   img <- read_tif(system.file("img", "2ch_ij.tif", package = "ijtiff"))
    +   str(img)
    +   str(as_EBImage(img))
    + }
    Loading required package: EBImage
    
    Attaching package: ‘EBImage’
    
    The following object is masked from ‘package:ijtiff’:
    
        display
    
    Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
      namespace ‘strex’ 1.1.0 is being loaded, but >= 1.1.1 is required
    Calls: read_tif ... namespaceImportFrom -> asNamespace -> loadNamespace
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ════════════════════════════════════════════════════════════════════════════
      OK: 15 SKIPPED: 0 WARNINGS: 0 FAILED: 24
      1. Error: `as_EBImage()` works (@test-as_EBImage-examples.R#3) 
      2. Error: `count_imgs()` works (@test-count_imgs-examples.R#6) 
      3. Error: display works (@test-graphics.R#2) 
      4. Error: Package 2-channel example I/O works (@test-io.R#3) 
      5. Error: Package RGB I/O works (@test-io.R#56) 
      6. Error: 8-bit unsigned integer TIFF I/O works (@test-io.R#66) 
      7. Error: 16-bit unsigned integer TIFF I/O works (@test-io.R#78) 
      8. Error: 32-bit unsigned integer TIFF I/O works (@test-io.R#90) 
      9. Error: Float (real-numbered) TIFF I/O works (@test-io.R#102) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

# nandb

<details>

* Version: 2.0.2
* Source code: https://github.com/cran/nandb
* URL: https://rorynolan.github.io/nandb, https://github.com/rorynolan/nandb
* BugReports: https://github.com/rorynolan/nandb/issues
* Date/Publication: 2019-06-17 08:10:05 UTC
* Number of recursive dependencies: 93

Run `revdep_details(,"nandb")` for more info

</details>

## Newly broken

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
    > img <- ijtiff::read_tif(system.file("extdata", "50.tif", package = "nandb"))
    Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
      namespace ‘strex’ 1.1.0 is being loaded, but >= 1.1.1 is required
    Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ════════════════════════════════════════════════════════════════════════════
      OK: 30 SKIPPED: 0 WARNINGS: 0 FAILED: 19
      1. Error: brightness works (@test-brightness.R#5) 
      2. Error: brightness_folder works (@test-brightness.R#40) 
      3. Error: brightness_timeseries works (@test-brightness.R#92) 
      4. Error: cc_brightness() works (@test-cc_brightness.R#3) 
      5. Error: cc_brightness_timeseries() works (@test-cc_brightness.R#61) 
      6. Error: cc_brightness_folder() works (@test-cc_brightness.R#149) 
      7. Error: cc_brightness_timeseries_folder() works (@test-cc_brightness.R#175) 
      8. Error: cc_number() works (@test-cc_number.R#3) 
      9. Error: cc_number_timeseries() works (@test-cc_number.R#61) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘utils’
      All declared Imports should be used.
    ```

