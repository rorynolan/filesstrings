test_that("print method works", {
  verify_output(
    test_path("testthat-files", "printing_of_Rlogo-banana.txt"),
    print(
      read_tif(test_path("testthat-figs", "Rlogo-banana-red.tif"))
    )
  )
})
