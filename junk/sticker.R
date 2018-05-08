pkgs <- c("hexSticker", "tidyverse", "here", "magick")
invisible(lapply(pkgs, library, character.only = TRUE))

sticker(here("junk", "filesstrings.png"), package = "filesstrings",
        filename = here("junk", "sticker.png"),
        s_x = 1, s_y = .75, s_height = 1,
        url = "github.com/rorynolan/filesstrings",
        p_y = 1.4, p_color = "black",
        u_x = 1.13, u_color = "white", u_size = 0.8,
        h_color = "black", h_fill = "orange")
image_read(here("junk", "sticker.png"))
