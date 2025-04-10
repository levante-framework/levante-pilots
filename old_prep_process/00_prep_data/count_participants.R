sites <- c("co_pilot", "de_pilot", "ca_pilot", "us_pilot")
count_vars <- list("dataset", "sex", "valid_dob")

this_year <- year(today())
max_age <- 15

participants <- sites |>
  set_names() |>
  map(\(site) read_rds(here(glue("00_prepped_data/{site}/participants.rds")))) |>
  list_rbind(names_to = "site")

participants_coded <- participants |>
  mutate(sex = sex |> fct_recode("female" = "F", "male" = "M") |> na_if(""),
         valid_birth_month = !is.na(birth_month) &
           birth_month >= 1 & birth_month <= 12,
         valid_birth_year = !is.na(birth_year) &
           birth_year >= this_year - max_age & birth_year <= this_year,
         valid_dob = valid_birth_month & valid_birth_year)

participant_counts <- map(count_vars, \(var) {
  svar <- sym(var)
  participants_coded |>
    count(site, !!svar) |>
    rename(value = !!svar) |>
    mutate(variable = var, .before = value,
           value = as.character(value))
}) |>
  list_rbind()

write_rds(participant_counts, here(glue("00_prepped_data/participant_counts.rds")), compress = "gz")
