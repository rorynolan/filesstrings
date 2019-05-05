## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, comment = "#>", 
                      fig.width = 7, fig.height = 6)
pacman::p_load(dplyr, tidyr, MASS, mgcv, ggplot2, gridExtra)
set.seed(1)

## ----load----------------------------------------------------------------
pacman::p_load(detrendr, ijtiff)

