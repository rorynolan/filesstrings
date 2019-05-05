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
set.seed(1)

## ----create-high-low-----------------------------------------------------
x <- c(sample.int(9, 2e5, replace = TRUE), sample(51:99, 8e5, replace = TRUE))

## ----plot-high-low, message=FALSE----------------------------------------
library(ggplot2)
library(dplyr)
tibble(x = x) %>% 
  ggplot() + aes(x) + stat_density(bw = 3)

## ----try-all-------------------------------------------------------------
library(autothresholdr)
thresh_methods <- c("IJDefault", "Huang", "Huang2", "Intermodes", "IsoData", 
                    "Li", "Mean", "MinErrorI", "Minimum", "Moments", "Otsu",
                    "Percentile", "RenyiEntropy", "Shanbhag", "Triangle")
thresholds <- purrr::map_chr(thresh_methods, ~auto_thresh(x, .)) %>% 
  tibble(method = thresh_methods, threshold = .)
print(thresholds)

## ----successes-----------------------------------------------------------
filter(thresholds, threshold >= 10, threshold <= 49)

## ----tri-----------------------------------------------------------------
auto_thresh(x, "huang")
auto_thresh(x, "tri")
auto_thresh(x,  "otsu")

