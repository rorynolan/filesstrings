## `nandb` 2.0.0

#### MAJOR CHANGES
* Robin Hood detrending is now used instead of exponential smoothing detrending. This is a breaking change.

#### NEW FEATURES
* The `timeseries` functions now have the option to `overlap` for better time resolution.
* A `pkgdown` website!


## `nandb` 1.1.0

#### NEW FEATURES
* `matrix_raster_plot()` is back. I thought it didn't really fit but a user said they missed when it was gone so it's back now.


## `nandb` 1.0.1

#### BUG FIXES
* Fix issues on CRAN linux devel clang.


## `nandb` 1.0.0

#### NEW FEATURES
* The package is now peer-reviewed and published in the journal *Bioinformatics*. See `citation("nandb")`.
* The package style is now in accordance with the tidyverse style guide.
* `brightness()` and `number()` now include options to set `offset`, S-factor, `readout_noise` and `gamma` correction terms.
* `brightness()` and `number()` now enable calculation of both definitions ("B" and "epsilon"; "N" and "n") of brightness and number.
* Detrending is outsourced to the `detrendr` package. This new package makes detrending more accurate and much faster.
* TIFF I/O and graphics `display()` are now outsourced to the `ijtiff` package. This means R users no longer have to tell `nandb` how many channels are in the images.
* The package now has its own S3 class system.


#### BUG FIXES 
* Kmer calculations are no longer possible. The way in which they were done was over-simple.


## `nandb` 0.2.1

#### BUG FIXES
* Compatible with `filesstrings` 1.1.0.


## `nandb` 0.2.0

* The first version that I consider CRAN-worthy.



