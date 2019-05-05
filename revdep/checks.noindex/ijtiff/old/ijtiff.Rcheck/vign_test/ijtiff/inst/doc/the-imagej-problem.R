## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(magrittr)

## ----2 channel path------------------------------------------------------
path_2ch_ij <- system.file("img", "Rlogo-banana-red_green.tif", 
                           package = "ijtiff")

## ----magickally display gif, echo=FALSE----------------------------------
magick::image_read(system.file("img", "Rlogo-banana.gif", package = "ijtiff"))

## ----red and green banana, echo=FALSE, message=FALSE, dpi=300, fig.height=1, warning=FALSE, fig.width=2----
rgbanana_tif <- system.file("img", "Rlogo-banana-red_green.tif",
                            package = "ijtiff") %>% 
  ijtiff::read_tif()
d <- dim(rgbanana_tif)
reds <- purrr::map(seq_len(d[4]), ~ rgbanana_tif[, , 1, .]) %>% 
  purrr::reduce(cbind)
greens <- purrr::map(seq_len(d[4]), ~ rgbanana_tif[, , 2, .]) %>% 
  purrr::reduce(cbind)
to_display <- array(0, dim = c(2 * nrow(reds), ncol(reds), 3, 1))
to_display[seq_len(nrow(reds)), , 1, ] <- reds
to_display[seq_len(nrow(reds)) + nrow(reds), , 2, ] <- greens
ijtiff::display(to_display)

## ----original tiff import------------------------------------------------
img <- tiff::readTIFF(path_2ch_ij, all = TRUE)
str(img)  # 10 images
img[[1]][100:110, 50:60]  # print a section of the first image in the series

## ----ijtiff import-------------------------------------------------------
img <- ijtiff::read_tif(path_2ch_ij)
dim(img)  # 2 channels, 3 frames
img[100:110, 50:60, 1, 1]  # print a section of the first channel, first frame

