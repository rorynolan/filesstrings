pkgs <- c("hexSticker", "tidyverse", "here", "magick")
invisible(lapply(pkgs, library, character.only = TRUE))

df <- tibble(no_trend = rpois(51, 3) + 48,
             time = seq_along(no_trend),
             trend = no_trend + seq(25, -25)) %>%
  gather(trend, value, c(trend, no_trend)) %>%
  mutate(trend = as_factor(trend, levels = c("no_trend", "trend")))
p <- ggplot(df, aes(x = time, y = value, color = trend)) +
  geom_line(size = 1) + scale_color_manual(values = c("red", "green")) +
  theme_void() + theme_transparent(legend.position = "none")

sticker(p, package = "detrendr",
        filename = here("junk", "sticker.png"),
        s_x = 1, s_y = .84, s_width = 1.3, s_height = 1,
        url = "github.com/rorynolan/detrendr",
        p_y = 1.5, p_color = "white",
        u_x = 1.13, u_color = "black", u_size = 0.8,
        h_color = "yellow", h_fill = "purple")
image_read(here("junk", "sticker.png"))
