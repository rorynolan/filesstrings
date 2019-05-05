## ----setup, include=FALSE------------------------------------------------
if (utils::packageVersion("knitr") >= "1.20.15") {
  knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>",
    fig.width = 7, fig.height = 6,
    tidy = "styler"
  )
} else {
  knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>",
    fig.width = 7, fig.height = 6
  )
}

## ----load libraries, results='hide'--------------------------------------
library(autothresholdr)

## ----the image-----------------------------------------------------------
img <- ijtiff::read_tif(system.file("extdata", "fiji_eg.tif", 
                                    package = "autothresholdr"))
dim(img)
ijtiff::display(img)  # displays first channel, first frame

## ----guess twenty--------------------------------------------------------
ijtiff::display(img[, , 1, 1] > 20)

## ----thresh mask apply---------------------------------------------------
auto_thresh(img, "tri")
ijtiff::display(mask(img, "tri"))
ijtiff::display(apply_mask(img, "tri"))

