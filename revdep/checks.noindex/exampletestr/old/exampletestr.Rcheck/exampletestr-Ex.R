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

usethis::create_package(tempdir(check = TRUE), open = FALSE)
file.copy(system.file("extdata", c("detect.R", "match.R"),
                      package = "exampletestr"),
          paste0(tempdir(), "/R"))
make_test_shell_fun("str_detect()", document = TRUE, open = FALSE,
                    pkg_dir = tempdir())
make_tests_shells_file("detect", document = FALSE, open = FALSE,
                       pkg_dir = tempdir())
make_tests_shells_pkg(overwrite = TRUE, document = FALSE,
                      pkg_dir = tempdir())




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
