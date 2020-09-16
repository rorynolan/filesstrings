context("graphics")

test_that("display works", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("EBImage")
  skip_if(getRversion() < "4.0")
  img <- read_tif(system.file("img", "Rlogo.tif", package = "ijtiff"))
  vdiffr::expect_doppelganger("raster R logo", display(img, method = "r"))
  img <- read_tif(system.file("img", "Rlogo.tif", package = "ijtiff")) %>%
    as_EBImage() %>%
    EBImage::resize(h = dim(.)[1] / 4, w = dim(.)[2] / 4) %>%
    as_ijtiff_img()
  vdiffr::expect_doppelganger(
    "reduced resolution basic R logo",
    display(img, basic = TRUE)
  )
  img <- read_tif(system.file("img", "Rlogo.tif", package = "ijtiff"))
  vdiffr::expect_doppelganger(
    "raster grey R logo",
    display(img[, , , ], method = "r")
  )
  vdiffr::expect_doppelganger(
    "raster grey r logo (blue channel)",
    display(img[, , 2, ], method = "r")
  )
  vdiffr::expect_doppelganger(
    "raster R logo (green channel)",
    display(img[, , 3, 1])
  )
  img <- read_tif(test_path("testthat-figs", "Rlogo-banana-red.tif"))
  vdiffr::expect_doppelganger("R logo banana red", display(img, method = "r"))
})
