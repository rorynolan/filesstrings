pkgname <- "exampletestr"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('exampletestr')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("make-test-shells")
### * make-test-shells

flush(stderr()); flush(stdout())

### Name: make-test-shells
### Title: Create test shells.
### Aliases: make-test-shells make_test_shell_fun make_tests_shells_file
###   make_tests_shells_pkg

### ** Examples

## Not run: 
##D pkg_dir <- "~/mylilpkg"
##D usethis::create_package(pkg_dir, rstudio = FALSE, open = FALSE)
##D fs::file_copy(
##D   system.file("extdata", c("detect.R", "match.R"),
##D     package = "exampletestr"
##D   ),
##D   paste0(pkg_dir, "/R")
##D )
##D make_test_shell_fun("str_detect()", pkg_dir,
##D   document = TRUE, roxytest = TRUE
##D )
##D make_test_shell_fun("str_detect()", pkg_dir,
##D   document = TRUE, open = FALSE
##D )
##D make_tests_shells_file("detect", pkg_dir,
##D   document = FALSE, open = FALSE
##D )
##D make_tests_shells_pkg(pkg_dir,
##D   overwrite = TRUE, document = FALSE
##D )
##D fs::dir_delete(pkg_dir)
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
