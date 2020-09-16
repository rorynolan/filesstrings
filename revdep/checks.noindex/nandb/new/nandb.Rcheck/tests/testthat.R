library(testthat)
library(nandb)

if (!nandb:::win32bit()) test_check("nandb")
