pkgname <- "nandb"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('nandb')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("brightness")
### * brightness

flush(stderr()); flush(stdout())

### Name: brightness
### Title: Calculate brightness from image series.
### Aliases: brightness

### ** Examples




cleanEx()
nameEx("brightness_folder")
### * brightness_folder

flush(stderr()); flush(stdout())

### Name: brightness_folder
### Title: Brightness calculations for every image in a folder.
### Aliases: brightness_folder

### ** Examples

## Not run: 
##D setwd(tempdir())
##D img <- ijtiff::read_tif(system.file("extdata", "50.tif", package = "nandb"))
##D ijtiff::write_tif(img, "img1.tif")
##D ijtiff::write_tif(img, "img2.tif")
##D brightness_folder(def = "B", thresh = "Huang")
## End(Not run)



cleanEx()
nameEx("brightness_timeseries")
### * brightness_timeseries

flush(stderr()); flush(stdout())

### Name: brightness_timeseries
### Title: Create a brightness time-series.
### Aliases: brightness_timeseries

### ** Examples




cleanEx()
nameEx("brightness_timeseries_folder")
### * brightness_timeseries_folder

flush(stderr()); flush(stdout())

### Name: brightness_timeseries_folder
### Title: Brightness time-series calculations for every image in a folder.
### Aliases: brightness_timeseries_folder

### ** Examples

## Not run: 
##D setwd(tempdir())
##D img <- ijtiff::read_tif(system.file("extdata", "50.tif", package = "nandb"))
##D ijtiff::write_tif(img, "img1.tif")
##D ijtiff::write_tif(img, "img2.tif")
##D brightness_timeseries_folder(def = "e", thresh = "tri", frames_per_set = 20)
## End(Not run)



cleanEx()
nameEx("cc_brightness")
### * cc_brightness

flush(stderr()); flush(stdout())

### Name: cc_brightness
### Title: Cross-correlated brightness.
### Aliases: cc_brightness

### ** Examples




cleanEx()
nameEx("cc_brightness_folder")
### * cc_brightness_folder

flush(stderr()); flush(stdout())

### Name: cc_brightness_folder
### Title: Cross-correlated brightness calculations for every image in a
###   folder.
### Aliases: cc_brightness_folder

### ** Examples

## Not run: 
##D setwd(tempdir())
##D ijtiff::write_tif(img, "a.tif")
##D ijtiff::write_tif(img, "ab.tif")
##D cc_brightness_folder()
##D list.files()
## End(Not run)



cleanEx()
nameEx("cc_brightness_timeseries")
### * cc_brightness_timeseries

flush(stderr()); flush(stdout())

### Name: cc_brightness_timeseries
### Title: Create a cross-correlated brightness time-series.
### Aliases: cc_brightness_timeseries

### ** Examples




cleanEx()
nameEx("cc_brightness_timeseries_folder")
### * cc_brightness_timeseries_folder

flush(stderr()); flush(stdout())

### Name: cc_brightness_timeseries_folder
### Title: Cross-correlated brightness time-series calculations for every
###   image in a folder.
### Aliases: cc_brightness_timeseries_folder

### ** Examples

## Not run: 
##D setwd(tempdir())
##D ijtiff::write_tif(img, "a.tif")
##D ijtiff::write_tif(img, "ab.tif")
##D cc_brightness_timeseries_folder(frames_per_set = 25)
##D list.files()
## End(Not run)




cleanEx()
nameEx("cc_number")
### * cc_number

flush(stderr()); flush(stdout())

### Name: cc_number
### Title: Cross-correlated number.
### Aliases: cc_number

### ** Examples




cleanEx()
nameEx("cc_number_folder")
### * cc_number_folder

flush(stderr()); flush(stdout())

### Name: cc_number_folder
### Title: Cross-correlated number calculations for every image in a
###   folder.
### Aliases: cc_number_folder

### ** Examples

## Not run: 
##D setwd(tempdir())
##D ijtiff::write_tif(img, "a.tif")
##D ijtiff::write_tif(img, "ab.tif")
##D cc_number_folder()
##D list.files()
## End(Not run)



cleanEx()
nameEx("cc_number_timeseries")
### * cc_number_timeseries

flush(stderr()); flush(stdout())

### Name: cc_number_timeseries
### Title: Create a cross-correlated number time-series.
### Aliases: cc_number_timeseries

### ** Examples




cleanEx()
nameEx("cc_number_timeseries_folder")
### * cc_number_timeseries_folder

flush(stderr()); flush(stdout())

### Name: cc_number_timeseries_folder
### Title: Cross-correlated number time-series calculations for every image
###   in a folder.
### Aliases: cc_number_timeseries_folder

### ** Examples

## Not run: 
##D setwd(tempdir())
##D ijtiff::write_tif(img, "a.tif")
##D ijtiff::write_tif(img, "ab.tif")
##D cc_number_timeseries_folder(frames_per_set = 25)
##D list.files()
## End(Not run)




cleanEx()
nameEx("cross_var")
### * cross_var

flush(stderr()); flush(stdout())

### Name: cross_var
### Title: Calculate the _cross-variance_ of two vectors.
### Aliases: cross_var

### ** Examples

cross_var(0:3, 2:5)



cleanEx()
nameEx("cross_var_pillars")
### * cross_var_pillars

flush(stderr()); flush(stdout())

### Name: cross_var_pillars
### Title: Calculate the _cross-variance_ of corresponding pillars of 3d
###   arrays.
### Aliases: cross_var_pillars

### ** Examples

x <- array(1:27, dim = rep(3, 3))
y <- array(0:26, dim = rep(3, 3))
cross_var_pillars(x, y)



cleanEx()
nameEx("matrix_raster_plot")
### * matrix_raster_plot

flush(stderr()); flush(stdout())

### Name: matrix_raster_plot
### Title: Make a raster plot of a matrix.
### Aliases: matrix_raster_plot

### ** Examples




cleanEx()
nameEx("median_filter")
### * median_filter

flush(stderr()); flush(stdout())

### Name: median_filter
### Title: Smooth and median filters with options for handling NAs.
### Aliases: median_filter smooth_filter

### ** Examples

m <- matrix(1:9, nrow = 3)
m[2:3, 2:3] <- NA
print(m)
median_filter(m)
median_filter(m, na_rm = TRUE)
median_filter(m, na_count = TRUE)

smooth_filter(m)
smooth_filter(m, na_rm = TRUE)
smooth_filter(m, na_count = TRUE)




cleanEx()
nameEx("number")
### * number

flush(stderr()); flush(stdout())

### Name: number
### Title: Calculate number from image series.
### Aliases: number

### ** Examples




cleanEx()
nameEx("number_folder")
### * number_folder

flush(stderr()); flush(stdout())

### Name: number_folder
### Title: Number calculations for every image in a folder.
### Aliases: number_folder

### ** Examples

## Not run: 
##D setwd(tempdir())
##D img <- ijtiff::read_tif(system.file("extdata", "50.tif", package = "nandb"))
##D ijtiff::write_tif(img, "img2.tif")
##D number_folder(def = "n", thresh = "Huang", parallel = 2)
## End(Not run)



cleanEx()
nameEx("number_timeseries")
### * number_timeseries

flush(stderr()); flush(stdout())

### Name: number_timeseries
### Title: Create a number time-series.
### Aliases: number_timeseries

### ** Examples




cleanEx()
nameEx("number_timeseries_folder")
### * number_timeseries_folder

flush(stderr()); flush(stdout())

### Name: number_timeseries_folder
### Title: Number time-series calculations for every image in a folder.
### Aliases: number_timeseries_folder

### ** Examples

## Not run: 
##D setwd(tempdir())
##D img <- ijtiff::read_tif(system.file("extdata", "50.tif", package = "nandb"))
##D ijtiff::write_tif(img, "img1.tif")
##D ijtiff::write_tif(img, "img2.tif")
##D number_timeseries_folder(def = "n", thresh = "Huang", frames_per_set = 20)
## End(Not run)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
