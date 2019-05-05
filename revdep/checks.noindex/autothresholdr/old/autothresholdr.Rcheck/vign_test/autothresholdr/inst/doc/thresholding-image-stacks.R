## ----setup, include = FALSE----------------------------------------------
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
library(magrittr)
apply_on_pillars <- function(arr3d, FUN) {
  if (length(dim(arr3d)) == 4 && dim(arr3d)[3] == 1) arr3d %<>% {.[, , 1, ]}
  apply(arr3d, c(1, 2), FUN) %>% {
    if (length(dim(.)) == 3) {
      aperm(., c(2, 3, 1))
    } else {
      .
    }
  }
}

## ----50-tif--------------------------------------------------------------
img <- ijtiff::read_tif(system.file("extdata", "50.tif", 
                                    package = "autothresholdr"))
dim(img)

## ----first-3-frames, fig.height=2, echo=FALSE----------------------------
first3 <- matrix(max(img), ncol = 3 * dim(img)[2] + 2, nrow = dim(img)[1])
for (i in 1:3) {
  first3[, (i - 1) * dim(img)[2] + i + seq_len(dim(img)[2]) - 1] <- 
    img[, , 1, i]
}
ijtiff::display(first3)

## ----last-3-frames, fig.height=2, echo=FALSE-----------------------------
last3 <- matrix(max(img), ncol = 3 * dim(img)[2] + 2, nrow = dim(img)[1])
for (i in 1:3) {
  last3[, (i - 1) * dim(img)[2] + i + seq_len(dim(img)[2]) - 1] <- 
    img[, , 1, dim(img)[4] - (i - 1)]
}
ijtiff::display(last3)

## ----naive---------------------------------------------------------------
library(autothresholdr)
naiively_threshed_img <- apply_mask(img, "tri")
attr(naiively_threshed_img, "thresh")  # The threshold chosen by "Triangle" is 4

## ----naiive-first-3-frames, fig.height=2, echo=FALSE---------------------
first3 <- matrix(max(naiively_threshed_img), ncol = 3 * dim(naiively_threshed_img)[2] + 2, nrow = dim(naiively_threshed_img)[1])
for (i in 1:3) {
  first3[, (i - 1) * dim(naiively_threshed_img)[2] + i + seq_len(dim(naiively_threshed_img)[2]) - 1] <- 
    naiively_threshed_img[, , 1, i]
}
ijtiff::display(first3)

## ----naiive-last-3-frames, fig.height=2, echo=FALSE----------------------
last3 <- matrix(max(naiively_threshed_img), ncol = 3 * dim(naiively_threshed_img)[2] + 2, nrow = dim(naiively_threshed_img)[1])
for (i in 1:3) {
  last3[, (i - 1) * dim(naiively_threshed_img)[2] + i + seq_len(dim(naiively_threshed_img)[2]) - 1] <- 
    naiively_threshed_img[, , 1, dim(naiively_threshed_img)[4] - (i - 1)]
}
ijtiff::display(last3)

## ----sometimes-sometimes-not, echo=FALSE---------------------------------
naiively_threshed_img %>% 
  apply_on_pillars(function(x) {
    (sum(is.na(x)) > 0) && (sum(is.na(x)) < length(x))}) %>% 
  ijtiff::display()

## ----stack-threshs-------------------------------------------------------
ijtiff::display(mean_stack_thresh(img, "tri"))
ijtiff::display(med_stack_thresh(img, "tri"))

