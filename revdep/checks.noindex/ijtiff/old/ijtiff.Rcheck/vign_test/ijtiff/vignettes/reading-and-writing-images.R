## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----dancing-banana-path-------------------------------------------------
path_dancing_banana <- system.file("img", "Rlogo-banana.tif",
                                   package = "ijtiff")
print(path_dancing_banana)

