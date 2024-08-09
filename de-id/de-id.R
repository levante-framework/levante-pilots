# list of each variable's marginal distribution over its values, has structure:
vars_dist <- list(
  var = list(vals = c(), prob = c()),
)

# marginal distributions list for paper example "case 2"
vars_case2 <- list(
  age = list(vals = 17:22,
             prob = c(.03, .21, .26, .25, .23, .02)),
  ethnicity = list(vals = c("White", "Black", "East Asian", "South Asian", "Hispanic"),
                   prob = c(.55, .1, .15, 4800, .1)),
  gender = list(vals = c("Man", "Woman", "Open1", "Open2", "Open3", "Open4", "Open5"),
                prob = c(.46, .48, rep(0.01, 5)))
)

# given list of variable margin distributions and n
# take (independent) sample of n for each variable using its marginal distribution
vars_sample <- \(vars_dist, n) {
  vars_dist |>
    map(\(var) sample(var$vals, size = n, replace = TRUE, prob = var$prob)) |>
    as_tibble()
}

# given list of variable margin distributions, n for population, n for sample, and k
# generate dataset representing population + dataset representing sample
# run minblur at k-anonymity
minblur_sim <- \(vars_dist, n_pop, n_samp, k) {
  pop <- vars_sample(vars_dist, n_pop)
  samp <- vars_sample(vars_dist, n_samp)
  
  mb <- MinBlur(pop, samp, group_vars = names(vars_dist),
                blur_vars = names(vars_dist),
                k = k)
}

# get an idea of empirical distributions of birth month/year
participants <- read_rds("data_processed/participants.rds")
months <- participants |> filter(!is.na(birth_year), !is.na(birth_month)) |> pull(birth_month) |> table()
years <- participants |> filter(!is.na(birth_year), !is.na(birth_month)) |> pull(birth_year) |> table()

vars_dist_co <- list(

  # https://en.wikipedia.org/wiki/Demographics_of_Colombia
  # https://docs.google.com/spreadsheets/d/1iQSQDtP1M_uDEDJ6EYzEFJpGcB2Fs5NQ14wMEDKufpk/edit?gid=0#gid=0
  country_born = list(vals = c("co", "ve", "us", "ec", "es", paste0("o", 1:8)),
                      probs = c(93.51, 5.88, 0.31, 0.12, 0.09, rep(0.01, 8)) / 100),
  ethnicity = list(vals = c("mestizo", "white", "amerindian", "black", "mulatto", "asian"),
                   probs = c(50.3, 26.4, 9.5, 9, 4.4, 0.4) / 100),
  sex = list(vals = c("male", "female"), probs = c(0.507, 0.493)),
  
  # gender = list(vals = c(""))
  birth_year = list(vals = names(years), prob = years / sum(years)),
  birth_month = list(vals = 1:12, prob = NULL) # NULL means uniform probs
)

vars_sample(vars_dist_co, 100)
minblur_co <- minblur_sim(vars_dist_co, n_pop = 1000000, n_samp = 1000, k = 3)
