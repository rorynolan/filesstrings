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

