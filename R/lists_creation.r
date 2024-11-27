library(tidyverse)


bl <- matrix(
  c(
    "am", "drat_cat"
  ),
  ncol = 2,
  byrow = TRUE,
  dimnames = list(
    NULL, c("from", "to")
    )
  )

wl <- matrix(
  c(
    "am", "mpg_cat",
    "hp_cat", "disp_cat",
    "disp_cat", "qsec_cat",
    "hp_cat", "disp_cat"
  ),
  ncol = 2,
  byrow = TRUE,
  dimnames = list(
    NULL, c("from", "to")
    )
  )

# bl
# wl

# write.table(bl, file = "blacklist_cars.txt", row.names = FALSE, col.names = FALSE)
# write.table(wl, file = "whitelist_cars.txt", row.names = FALSE, col.names = FALSE)


