library(dplyr)
library(redivis)
library(googlesheets4)
googlesheets4::gs4_auth()

# google sheet with survey items:
# https://docs.google.com/spreadsheets/d/1CthXFU6ZWCa0_BSIx2_HKobGM6WKRl6XG6UI8adOuv8
codebook <- "1CthXFU6ZWCa0_BSIx2_HKobGM6WKRl6XG6UI8adOuv8"
codesheets <- list("caregiver" = "Caregiver Forms",
                   "student" = "Child Forms",
                   "teacher" = "Teacher Forms")

# read in codesheet for each survey type
get_coding <- function(survey_sheet, survey_type) {
  read_sheet(codebook, sheet = survey_sheet, na = c("", "NA")) |>
    filter(!is.na(variable_name), !str_detect(item_text, "Instructions")) |>
    select(variable_name, contains("construct"), question_type, contains("survey_part"),
           reverse_coded, response_options)
}
survey_coding <- imap(codesheets, get_coding)

# combine codesheets
survey_items <- survey_coding |>
  bind_rows(.id = "survey_type") |>
  filter(!str_detect(variable_name, "Intro")) |>
  mutate(survey_part = if_else(is.na(survey_part), survey_type, survey_part)) |>
  group_by(survey_type) |>
  mutate(variable_order = 1:n()) |>
  ungroup() |>
  mutate(values = response_options |> str_split(", "),
         values_str = if_else(is.na(values), "", map_chr(values, jsonlite::toJSON))) |>
  select(survey_type, variable = variable_name, variable_order,
         contains("construct"), question_type, survey_part,
         reverse_coded, values = values_str)
# write_csv(survey_items, "survey_items.csv", na = "")

# connect to item_metadata redivis dataset, create next version if needed
item_metadata <- redivis$organization("levante")$dataset("item_metadata:czjv")
item_metadata <- item_metadata$create_next_version(if_not_exists = TRUE)

# connect to survey_items table, upload new survey_items df
survey_table <- item_metadata$table("survey_items:tfw0")
survey_table$update(upload_merge_strategy = "replace")
survey_table$upload("survey_items")$create(survey_items)
# st <- survey_table$to_tibble()

# release new item_metadata dataset
item_metadata$release()
