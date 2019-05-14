pkgname <- "detrendr"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('detrendr')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("best_degree")
### * best_degree

flush(stderr()); flush(stdout())

### Name: best_degree
### Title: Find the best polynomial degree for polynomial detrending.
### Aliases: best_degree

### ** Examples

## Not run: 
##D ## These examples are not run on CRAN because they take too long.
##D ## You can still try them for yourself.
##D img <- ijtiff::read_tif(system.file('extdata', 'bleached.tif',
##D                                     package = 'detrendr'))
##D best_degree(img, parallel = 2)
## End(Not run)



cleanEx()
nameEx("best_l")
### * best_l

flush(stderr()); flush(stdout())

### Name: best_l
### Title: Find the best length parameter for boxcar detrending.
### Aliases: best_l

### ** Examples

## Not run: 
##D ## These examples are not run on CRAN because they take too long.
##D ## You can still try them for yourself.
##D img <- ijtiff::read_tif(system.file('extdata', 'bleached.tif',
##D                                     package = 'detrendr'))
##D best_l(img, parallel = 2, purpose = "FFS")
## End(Not run)




cleanEx()
nameEx("best_swaps")
### * best_swaps

flush(stderr()); flush(stdout())

### Name: best_swaps
### Title: Find the best 'swaps' parameter for _Robin Hood_ detrending.
### Aliases: best_swaps

### ** Examples

## Not run: 
##D ## These examples are not run on CRAN because they take too long.
##D ## You can still try them for yourself.
##D img <- ijtiff::read_tif(system.file('extdata', 'bleached.tif',
##D                                     package = 'detrendr'))
##D best_swaps(img)
## End(Not run)




cleanEx()
nameEx("best_tau")
### * best_tau

flush(stderr()); flush(stdout())

### Name: best_tau
### Title: Find the best tau parameter for exponential smoothing
###   detrending.
### Aliases: best_tau

### ** Examples

## Not run: 
##D ## These examples are not run on CRAN because they take too long.
##D ## You can still try them for yourself.
##D img <- ijtiff::read_tif(system.file('extdata', 'bleached.tif',
##D                                     package = 'detrendr'))[, , 1, ]
##D best_tau(img, parallel = 2)
## End(Not run)




cleanEx()
nameEx("brightness_pillars")
### * brightness_pillars

flush(stderr()); flush(stdout())

### Name: brightness_pillars
### Title: Get the brightness of pillars of a 3d array.
### Aliases: brightness_pillars

### ** Examples

aaa <- array(1:16, dim = c(2, 2, 4))
brightness_pillars(aaa)




cleanEx()
nameEx("detrend-directory")
### * detrend-directory

flush(stderr()); flush(stdout())

### Name: detrend-directory
### Title: Detrend all TIFF images in an entire folder.
### Aliases: detrend-directory dir_detrend_robinhood dir_detrend_rh
###   dir_detrend_boxcar dir_detrend_exp dir_detrend_polynom

### ** Examples

## Not run: 
##D setwd(tempdir())
##D file.copy(c(system.file("extdata", "bleached.tif", package = "detrendr"),
##D             system.file("img", "2ch_ij.tif", package = "ijtiff")),
##D           ".")
##D dir_detrend_robinhood(thresh = "huang")
##D dir_detrend_boxcar(l = "auto", thresh = "tri", purpose = "FFS")
##D dir_detrend_exp(tau = "auto", thresh = "tri", purpose = "FCS")
##D dir_detrend_polynom(degree = "auto", thresh = "huang", purpose = "FFS")
## End(Not run)



cleanEx()
nameEx("detrending")
### * detrending

flush(stderr()); flush(stdout())

### Name: detrending
### Title: Detrend images.
### Aliases: detrending img_detrend_robinhood img_detrend_rh
###   img_detrend_boxcar img_detrend_exp img_detrend_polynom

### ** Examples

## Not run: 
##D ## These examples are not run on CRAN because they take too long.
##D ## You can still try them for yourself.
##D 
##D img <- ijtiff::read_tif(system.file('extdata', 'bleached.tif',
##D                                     package = 'detrendr'))
##D corrected <- img_detrend_rh(img)
##D corrected <- img_detrend_boxcar(img, "auto", purpose = "fcs", parallel = 2)
##D corrected10 <- img_detrend_boxcar(img, 10, purpose = "fcs", parallel = 2)
##D corrected50 <- img_detrend_boxcar(img, 50, purpose = "fcs", parallel = 2)
##D corrected <- img_detrend_exp(img, "auto", purpose = "ffs", parallel = 2)
##D corrected10 <- img_detrend_exp(img, 10, purpose = "ffs", parallel = 2)
##D corrected50 <- img_detrend_exp(img, 50, purpose = "fcs", parallel = 2)
##D corrected <- img_detrend_polynom(img, "auto", purpose = "ffs", parallel = 2)
##D corrected2 <- img_detrend_polynom(img, 2, purpose = "ffs", parallel = 2)
## End(Not run)



cleanEx()
nameEx("mean_frames")
### * mean_frames

flush(stderr()); flush(stdout())

### Name: mean_frames
### Title: Get the sums/means of frames in a 3-dimensional array.
### Aliases: mean_frames sum_frames

### ** Examples

a <- array(seq_len(2 ^ 3), dim = rep(2, 3))
sum_frames(a)
mean_frames(a)




cleanEx()
nameEx("pillar-stats")
### * pillar-stats

flush(stderr()); flush(stdout())

### Name: pillar-stats
### Title: Get the sums/means/medians/variances of pillars of an
###   ijtiff_img-style array.
### Aliases: pillar-stats sum_pillars mean_pillars median_pillars
###   var_pillars

### ** Examples

aaa <- array(seq_len(2 ^ 4), dim = rep(2, 4))  # a 2-channel, 2-frame array
sum_pillars(aaa)
mean_pillars(aaa)
median_pillars(aaa)
var_pillars(aaa)




cleanEx()
nameEx("rfromboxes")
### * rfromboxes

flush(stderr()); flush(stdout())

### Name: rfromboxes
### Title: Randomly draw balls from boxes.
### Aliases: rfromboxes

### ** Examples

balls <- 1:10
rfromboxes(40, balls)
rfromboxes(40, balls, weights = c(rep(1, 9), 0))




cleanEx()
nameEx("rtoboxes")
### * rtoboxes

flush(stderr()); flush(stdout())

### Name: rtoboxes
### Title: Randomly place balls in boxes.
### Aliases: rtoboxes

### ** Examples

rtoboxes(30, 7)
rtoboxes(30, 7, capacities = c(rep(1, 3), rep(7, 4)))
rtoboxes(30, 7, capacities = c(rep(1, 3), rep(70, 4)),
         weights = c(rep(0.1, 6), 1))




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
