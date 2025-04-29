library(dplyr)
library(purrr)
library(rairtable)
library(redivis)

# connect to 
item_table <- airtable(table = "pilot-item", base = "appIk9XNTZZns1F1F")
pilot_items <- read_airtable(item_table) |> as_tibble()

trial_items <- pilot_items |>
  select(item_uid, group, entry, trials, chance) |>
  mutate(trials = map_chr(trials, \(tt) paste(tt, collapse = ",")) |> map_chr(jsonlite::toJSON)) |>
  arrange(item_uid)

# connect to item_metadata redivis dataset, create next version if needed
item_metadata <- redivis$organization("levante")$dataset("item_metadata:czjv")
item_metadata <- item_metadata$create_next_version(if_not_exists = TRUE)

# connect to survey_items table, upload new survey_items df
trial_table <- item_metadata$table("trial_items")
trial_table$update(upload_merge_strategy = "replace")
trial_table$upload("trial_items")$create(trial_items, if_not_exists = FALSE, rename_on_conflict = TRUE)

# release new item_metadata dataset
item_metadata$release()
