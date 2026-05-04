library(dplyr)
library(tidyr)
library(stringr)
library(glue)
library(mirt)

# item parameters from ROAR
# https://github.com/yeatmanlab/roar-pa/blob/main/src/experiment/config/corpus/en/test-cat.csv
pa_corpus_en <- readr::read_csv("pa_corpus_cat_en.csv")
# pa_corpus_es <- readr::read_csv("inst/pa_corpus_es.csv") |> select(-`...1`)
# pa_corpus <- bind_rows(pa_corpus_en |> mutate(language = "en", .before = everything()),
#                        pa_corpus_es |> mutate(language = "es", .before = everything())) |>

# tidy up params, create item UIDs
pa_params <- pa_corpus_en |>
  mutate(language = "en", .before = everything()) |>
  mutate(item = glue("pa_{str_to_lower(trial_type)}_{goal}") |> as.character()) |>
  select(language, item,
         a1 = composite.a, b = composite.b, g = composite.c, u = composite.d) |>
  mutate(d = -b * a1) |> select(-b) |>
  pivot_longer(c(a1, d, g, u)) |>
  select(item, name, value) |>
  mutate(item = paste0(item, "-1"))

# create fake data to scaffold PA model
dummy_n <- 10
pa_dummy <- pa_params |>
  distinct(item) |>
  expand_grid(i = 1:dummy_n) |>
  group_by(item) |>
  mutate(value = c(rep(0, dummy_n / 2), rep(1, dummy_n / 2)) |> sample()) |>
  ungroup() |>
  mutate(item = paste0(item, "-1")) |>
  pivot_wider(names_from = item, values_from = value) |>
  tibble::column_to_rownames("i")

# set up mirt model based on dummy data
mod_pars <- mirt(pa_dummy, pars = "values", itemtype = "Rasch")

# substitute known param values
mod_vals <- mod_pars |> #as_tibble() |>
  rename(value_pars = value) |>
  left_join(pa_params) |>
  mutate(value = if_else(is.na(value), value_pars, value)) |>
  select(-value_pars) |>
  relocate(value, .after = parnum)

# set up mirt model with known param values
mod <- mirt(pa_dummy, pars = mod_vals, TOL = NaN)

# create model record from model
mod_rec <- modelrecord(mod, rownames(pa_dummy))

task <- "pa"
itemtype <- "rasch"
nfact <- 1
var_name <- "language"
subset_val <- "en-ROAR"

mod_file <- glue("{task}_{str_to_lower(itemtype)}_f{nfact}.rds")
registry_dir <- "02_scoring_outputs/model_registry"
out_dir <- file.path(registry_dir, task, paste0("by_", var_name), subset_val)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
readr::write_rds(mod_rec, file.path(out_dir, mod_file), compress = "gz")
