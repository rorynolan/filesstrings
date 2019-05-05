## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, comment = "#>", 
                      fig.width = 7, fig.height = 6)
pacman::p_load(dplyr, tidyr, MASS, mgcv, ggplot2, gridExtra)
set.seed(1)

## ----load----------------------------------------------------------------
pacman::p_load(detrendr, ijtiff)

## ----read----------------------------------------------------------------
path <- system.file("extdata", "2ch100frame.tif", package = "detrendr")
print(path)
my_img <- read_tif(path)

## ----dim-----------------------------------------------------------------
dim(my_img)

## ----plot-mean-profile, echo=FALSE, fig.height=3, message=FALSE----------
plot_tbl <- tibble(
  frame = seq_len(dim(my_img)[4]),
  channel1 = apply(autothresholdr::mean_stack_thresh(my_img[, , 1, ], "h"), 4, 
                   mean, na.rm = TRUE),
  channel2 = apply(autothresholdr::mean_stack_thresh(my_img[, , 2, ], "h"), 4, 
                   mean, na.rm = TRUE)
  ) %>% 
  gather(ch, mean, -frame)
ch2range <- plot_tbl %>% 
  filter(ch == "channel2") %>% 
  pull(mean) %>% 
  range() %>% 
  round()
pl <- ggplot(filter(plot_tbl, ch == "channel1"), aes(frame, mean)) + 
  geom_smooth() + geom_point() + ggtitle("Channel 1") + ylim(0.75, 0.95)
pr <- ggplot(filter(plot_tbl, ch == "channel2"), aes(frame, mean)) + 
  geom_smooth() + geom_point() + ggtitle("Channel 2") + ylim(26, 29)
gridExtra::grid.arrange(pl, pr, nrow = 1)

## ----display-mean, echo=FALSE, fig.height=4------------------------------
graphics::par(mfrow = c(1, 2))
display(mean_pillars(my_img[, , 1, ]))
display(mean_pillars(my_img[, , 2, ]))

## ----display-thresholded-mean, echo=FALSE, fig.height=4------------------
graphics::par(mfrow = c(1, 2))
display(mean_pillars(autothresholdr::mean_stack_thresh(my_img[, , 1, ], "h")))
display(mean_pillars(autothresholdr::mean_stack_thresh(my_img[, , 2, ], "h")))

## ----autothresholdr-code-------------------------------------------------
pacman::p_load(autothresholdr)
my_img_threshed <- mean_stack_thresh(my_img, "Huang")

## ----detrend-------------------------------------------------------------
my_detrended_img <- img_detrend_robinhood(my_img_threshed)

## ----plot-detrended-mean-profile, echo=FALSE, fig.height=3, message=FALSE----
plot_tbl <- tibble(
  frame = seq_len(dim(my_detrended_img)[4]),
  channel1 = apply(my_detrended_img[, , 1, ], 3, mean, na.rm = TRUE),
  channel2 = apply(my_detrended_img[, , 2, ], 3, mean, na.rm = TRUE)
) %>% 
  gather(ch, mean, -frame)
pl <- ggplot(filter(plot_tbl, ch == "channel1"), aes(frame, mean)) + 
  geom_smooth() + geom_point() + ggtitle("Channel 1") + ylim(0.75, 0.95)
pr <- ggplot(filter(plot_tbl, ch == "channel2"), aes(frame, mean)) + 
  geom_smooth() + geom_point() + ggtitle("Channel 2") + ylim(26, 29)
gridExtra::grid.arrange(pl, pr, nrow = 1)

