library(tidyverse)

# data <- read_csv(
#   here::here("R", "loan_data_only_cat.csv")
# )


bl <- matrix(
  c(
    "loan_status", "credit_score_brack",
    "loan_status", "cred_hist_length_brack",
    "loan_status", "loan_percent_income_brack",
    "loan_status", "loan_int_rate_brack",
    "loan_status", "loan_amnt_brack",
    "loan_status", "person_income_brack",
    "loan_status", "person_age_brack",
    "loan_status", "person_gender",
    "loan_status", "person_education",
    "loan_status", "person_home_ownership",
    "loan_status", "loan_intent",
    "loan_status", "previous_loan_defaults_on_file"
  ),
  ncol = 2,
  byrow = TRUE,
  dimnames = list(
    NULL, c("from", "to")
    )
  )

wl <- matrix(
  c(
    "loan_intent", "loan_status"
  ),
  ncol = 2,
  byrow = TRUE,
  dimnames = list(
    NULL, c("from", "to")
    )
  )

# bl
# wl

write.table(bl, file = here::here("R", "blacklist.txt"), row.names = FALSE, col.names = FALSE)
write.table(wl, file = here::here("R", "whitelist.txt"), row.names = FALSE, col.names = FALSE)


