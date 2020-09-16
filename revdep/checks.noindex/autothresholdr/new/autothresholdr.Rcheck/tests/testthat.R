library(testthat)
library(autothresholdr)

if (!autothresholdr:::win32bit()) test_check("autothresholdr")
