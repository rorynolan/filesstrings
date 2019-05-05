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

img <- ijtiff::read_tif(system.file('extdata', '50.tif', package = 'nandb'))
ijtiff::display(img[, , 1, 1])
b <- brightness(img, "e", thresh = "Huang")
b <- brightness(img, "B", thresh = "tri")



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
##D img <- ijtiff::read_tif(system.file('extdata', '50.tif', package = 'nandb'))
##D ijtiff::write_tif(img, 'img1.tif')
##D ijtiff::write_tif(img, 'img2.tif')
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

img <- ijtiff::read_tif(system.file('extdata', '50.tif', package = 'nandb'))
bts <- brightness_timeseries(img, "e", frames_per_set = 20, thresh = "Huang")



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
##D img <- ijtiff::read_tif(system.file('extdata', '50.tif', package = 'nandb'))
##D ijtiff::write_tif(img, 'img1.tif')
##D ijtiff::write_tif(img, 'img2.tif')
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

img <- ijtiff::read_tif(system.file("extdata", "two_ch.tif",
                                    package = "nandb"))
ijtiff::display(detrendr::mean_pillars(img[, , 1, ]))
ijtiff::display(detrendr::mean_pillars(img[, , 2, ]))
b <- brightness(img, def = "e", thresh = "Huang", filt = "median")
ijtiff::display(b[, , 1, 1])
ijtiff::display(b[, , 2, 1])
cc_b <- cc_brightness(img, thresh = "Huang")
ijtiff::display(cc_b[, , 1, 1])



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
##D ijtiff::write_tif(img, 'a.tif')
##D ijtiff::write_tif(img, 'ab.tif')
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

img <- ijtiff::read_tif(system.file('extdata', 'two_ch.tif',
                        package = 'nandb'))
cc_bts <- cc_brightness_timeseries(img, 10, thresh = "Huang",
                                    filt = 'median', parallel = 2)
ijtiff::display(cc_bts[, , 1, 1])



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
##D ijtiff::write_tif(img, 'a.tif')
##D ijtiff::write_tif(img, 'ab.tif')
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

img <- ijtiff::read_tif(system.file("extdata", "two_ch.tif",
                                    package = "nandb"))
ijtiff::display(detrendr::mean_pillars(img[, , 1, ]))
ijtiff::display(detrendr::mean_pillars(img[, , 2, ]))
n <- number(img, def = "n", thresh = "Huang", filt = "median")
ijtiff::display(n[, , 1, 1])
ijtiff::display(n[, , 2, 1])
cc_n <- cc_number(img, thresh = "Huang")
ijtiff::display(cc_n[, , 1, 1])



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
##D ijtiff::write_tif(img, 'a.tif')
##D ijtiff::write_tif(img, 'ab.tif')
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

img <- ijtiff::read_tif(system.file('extdata', 'two_ch.tif',
                                    package = 'nandb'))
cc_nts <- cc_number_timeseries(img, 10, thresh = "Huang",
                               filt = 'median', parallel = 2)
ijtiff::display(cc_nts[, , 1, 1])




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
##D ijtiff::write_tif(img, 'a.tif')
##D ijtiff::write_tif(img, 'ab.tif')
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

img <- ijtiff::read_tif(system.file('extdata', '50.tif', package = 'nandb'))
ijtiff::display(img[, , 1, 1])
matrix_raster_plot(img[, , 1, 1])
b <- brightness(img, def = "B", detrend = FALSE, thresh = "Huang")
matrix_raster_plot(b, scale_name = 'brightness')
matrix_raster_plot(b, scale_name = 'brightness', log_trans = TRUE)
matrix_raster_plot(b, scale_name = 'brightness', log_trans = TRUE,
                   include_breaks = 1.35)
matrix_raster_plot(b, scale_name = 'brightness', log_trans = TRUE,
                   breaks = 1:3)
matrix_raster_plot(b, scale_name = 'brightness',
                   ranges = seq(0.5, 3, length.out = 6),
                   range_names = paste0(1:5, 'mer'))
matrix_raster_plot(b, scale_name = "brightness",
                   ranges = seq(0.5, 3, length.out = 6),
                   range_names = paste0(1:5, "mer"), log_trans = TRUE)
matrix_raster_plot(b, scale_name = "brightness",
                   include_breaks = 1.25, range_names = NULL,
                   log_trans = FALSE)
matrix_raster_plot(b, scale_name = "brightness",
                   include_breaks = 1.25, log_trans = TRUE)
matrix_raster_plot(b, scale_name = "brightness",
                   limits = c(1, 1.25), clip = TRUE)
matrix_raster_plot(b, scale_name = "brightness",
                   include_breaks = 1.25)




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

img <- ijtiff::read_tif(system.file('extdata', '50.tif', package = 'nandb'))
ijtiff::display(img[, , 1, 1])
num <- number(img, "N", thresh = "Huang")
num <- number(img, "n", thresh = "tri")



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
##D img <- ijtiff::read_tif(system.file('extdata', '50.tif', package = 'nandb'))
##D ijtiff::write_tif(img, 'img2.tif')
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

img <- ijtiff::read_tif(system.file('extdata', '50.tif', package = "nandb"))
nts <- number_timeseries(img, "n", frames_per_set = 20, thresh = "Huang")



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
##D img <- ijtiff::read_tif(system.file('extdata', '50.tif', package = 'nandb'))
##D ijtiff::write_tif(img, 'img1.tif')
##D ijtiff::write_tif(img, 'img2.tif')
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
