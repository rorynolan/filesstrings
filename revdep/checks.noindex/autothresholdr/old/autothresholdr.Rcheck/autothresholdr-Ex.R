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
### Title: Automatically threshold an array of non-negative integers.
### Aliases: auto_thresh auto_thresh_mask auto_thresh_apply_mask mask
###   apply_mask

### ** Examples




cleanEx()
nameEx("mean_stack_thresh")
### * mean_stack_thresh

flush(stderr()); flush(stdout())

### Name: mean_stack_thresh
### Title: Threshold every image frame in an image stack based on their
###   mean.
### Aliases: mean_stack_thresh

### ** Examples




cleanEx()
nameEx("med_stack_thresh")
### * med_stack_thresh

flush(stderr()); flush(stdout())

### Name: med_stack_thresh
### Title: Threshold every image frame in a stack based on their median.
### Aliases: med_stack_thresh

### ** Examples




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
