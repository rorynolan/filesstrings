context("Graphics")

test_that("display works", {
  img <- read_tif(system.file("img", "Rlogo.tif", package = "ijtiff"))
  grDevices::pdf(tempfile(fileext = ".pdf"))
  expect_null(display(img, method = "r"))
  expect_null(display(img, basic = TRUE))
  expect_null(display(img[, , , ], method = "r"))
  expect_null(display(img[, , 2, ], method = "r"))
  expect_null(display(img[, , , 1], basic = TRUE))
  expect_null(display(img[, , 3, 1], method = "r"))
  x <- display(img)
  if (!is.null(x)) expect_is(display(img), c("displayWidget", "htmlwidget"))
  x <- display(img[, , , ])
  if (!is.null(x)) {
    expect_is(x, c("displayWidget", "htmlwidget"))
  }
  x <- display(img[, , 2, ])
  if (!is.null(x)) {
    expect_is(x, c("displayWidget", "htmlwidget"))
  }
  x <- display(img[, , 3, 1])
  if (!is.null(x)) {
    expect_is(x, c("displayWidget", "htmlwidget"))
  }
  grDevices::dev.off()
})
