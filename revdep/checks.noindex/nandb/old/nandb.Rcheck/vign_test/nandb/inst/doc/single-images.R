## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, comment = "#>", 
                      fig.width = 7, fig.height = 6)
set.seed(1)
pacman::p_load(MASS, mgcv, ggplot2, dplyr, tidyr)

## ----load----------------------------------------------------------------
pacman::p_load(nandb, ijtiff)

## ----read----------------------------------------------------------------
path <- system.file("extdata", "two_ch.tif", package = "nandb")
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

## ----display-thresholded-mean, echo=FALSE, fig.height=4------------------
graphics::par(mfrow = c(1, 2))
display(
  detrendr::mean_pillars(
    autothresholdr::mean_stack_thresh(my_img[, , 1, ], "h")
  )
)
display(
  detrendr::mean_pillars(
    autothresholdr::mean_stack_thresh(my_img[, , 2, ], "h")
  )
)

## ----brightness----------------------------------------------------------
my_brightness_img <- brightness(my_img, def = "e", 
                                thresh = "Huang", detrend = TRUE)

## ----brightness-ts-------------------------------------------------------
my_brightness_ts_img <- brightness_timeseries(my_img, def = "e", 
                                              frames_per_set = 50,
                                              thresh = "Huang", detrend = TRUE)

## ----brightness-ts-overlapped--------------------------------------------
my_brightness_ts_img_overlapped <- brightness_timeseries(my_img, def = "e", 
                                                         frames_per_set = 50,
                                                         overlap = TRUE,
                                                         thresh = "Huang", 
                                                         detrend = TRUE)

## ----write-tif, eval=FALSE-----------------------------------------------
#  write_tif(my_brightness_img, "desired/path/of/my-brightness-img")
#  write_tif(my_brightness_ts_img, "desired/path/of/my-brightness-ts-img")
#  write_tif(my_brightness_ts_img_overlapped,
#            "desired/path/of/my-brightness-ts-img-overlapped")

## ----brightness density, message=FALSE, fig.width=7, fig.height=7--------
db <- density(my_brightness_img[, , 1, ], na.rm = TRUE)[c("x", "y")] %>% 
  as_tibble()
ggplot(db, aes(x, y)) + geom_line() + 
  labs(x = "brightness", y = "frequency") +
  xlim(-2, 2) + ggtitle("Channel 1 brightness distribution")

## ----immobile plot, fig.width=7, fig.height=7----------------------------
immobile_brightnesses <- matrix(rpois(50 * 10^6, 50), nrow = 10^6) %>% 
  {matrixStats::rowVars(.) / rowMeans(.) - 1}
di <- density(immobile_brightnesses) %>% {data.frame(x = .$x, y = .$y)}
rbind(mutate(db, mobility = "mobile"), mutate(di, mobility = "immobile")) %>%
  mutate(mobility = factor(mobility)) %>% 
  ggplot(aes(x, y)) + geom_line(aes(colour = mobility)) + 
  labs(x = "brightness", y = "frequency") +
  xlim(-2, 2) + 
  ggtitle("Channel 1 brightness distribution compared to immobile entities")

