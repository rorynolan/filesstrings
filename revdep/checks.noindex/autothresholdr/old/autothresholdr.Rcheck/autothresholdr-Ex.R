pkgname <- "autothresholdr"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('autothresholdr')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("auto_thresh")
### * auto_thresh

flush(stderr()); flush(stdout())

### Name: auto_thresh
### Title: Automatically threshold an image
### Aliases: auto_thresh auto_thresh_mask auto_thresh_apply_mask mask
###   apply_mask

### ** Examples

img_location <- system.file("extdata", "eg.tif", package = "autothresholdr")
img <- ijtiff::read_tif(img_location)
auto_thresh(img, "huang")
auto_thresh(img, "tri")
auto_thresh(img, "Otsu")
auto_thresh(img, 9)
mask <- auto_thresh_mask(img, "huang")
ijtiff::display(mask[, , 1, 1])
masked <- auto_thresh_apply_mask(img, "huang")
ijtiff::display(masked[, , 1, 1])
masked <- auto_thresh_apply_mask(img, 25)
ijtiff::display(masked[, , 1, 1])



cleanEx()
nameEx("mean_stack_thresh")
### * mean_stack_thresh

flush(stderr()); flush(stdout())

### Name: mean_stack_thresh
### Title: Threshold every image frame in an image stack based on their
###   mean.
### Aliases: mean_stack_thresh

### ** Examples

img <- ijtiff::read_tif(system.file('extdata', '50.tif',
                                    package = 'autothresholdr'))
ijtiff::display(img[, , 1, 1])
img_thresh_mask <- mean_stack_thresh(img, 'Otsu')
ijtiff::display(img_thresh_mask[, , 1, 1])
ijtiff::display(img[, , 1, 1])
img_thresh_mask <- mean_stack_thresh(img, 'Huang')
ijtiff::display(img_thresh_mask[, , 1, 1])




cleanEx()
nameEx("med_stack_thresh")
### * med_stack_thresh

flush(stderr()); flush(stdout())

### Name: med_stack_thresh
### Title: Threshold every image frame in a stack based on their median.
### Aliases: med_stack_thresh

### ** Examples

img <- ijtiff::read_tif(system.file('extdata', '50.tif',
                                    package = 'autothresholdr'))
ijtiff::display(img[, , 1, 1])
img_thresh_mask <- med_stack_thresh(img, 'Otsu')
ijtiff::display(img_thresh_mask[, , 1, 1])
ijtiff::display(img[, , 1, 1])
img_thresh_mask <- med_stack_thresh(img, 'Triangle')
ijtiff::display(img_thresh_mask[, , 1, 1])




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
