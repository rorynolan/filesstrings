## `detrendr` 0.6.1

#### BUG FIXES
* Require necessary version of `glue`.
* Temporary fix for upcoming R 3.6 which patches `base::sample()`. A more permanent fix should be provided when R 3.6 has landed.

## `detrendr` 0.6.0 

#### NEW FEATURES 
* A `pkgdown` website!

#### MINOR IMPROVEMENTS
* Robin Hood parameter finding is now repeated several (at least 9) times to find a sensible consensus value.
* Robin Hood parameter finding includes an adjustment step to avoid over-estimating the number of swaps required.


## `detrendr` 0.5.2

#### BUG FIXES
* Detrending was not working well for images which had dimension 1 in x or y.


## `detrendr` 0.5.1

#### BUG FIXES
* Fix tests for CRAN fedora and mac.


## `detrendr` 0.5.0

#### NEW FEATURES
* The package no longer depends on `RSAGA`, making it lighter.
* _Robin Hood_ detrending has been added.


## `detrendr` 0.4.0

#### NEW FEATURES
* Add the option to detrend for the purpose of FCS or FFS.


## `detrendr` 0.3.0

#### NEW FEATURES
* Batch processing: detrend an entire folder with the likes of `dir_detrend_exp()`.

#### BUG FIXES
* Asymmetric images caused R to crash.
* `NA`s in simulated brightnesses were needlessly causing the automatic parameter-finding routines to fail.


## `detrendr` 0.2.0

#### NEW FEATURES
* The process of extending time series prior to smoothing is not done any more. This was introducing errors for images with low counts. Smoothing works fine without it. Dropping this extension gives a massive improvement in detrending speed :-)
* TIFF I/O and image display are now taken care of by the `ijtiff` package.
* Images are now represented in the style of an `ijtiff::ijtiff_img`.

#### DEFUNCT
* This package no longer exports functions for TIFF I/O nor image display.


## `detrendr` detrendr 0.1.0
* The first CRAN-worthy version.
