library(redivis)
library(tidyverse)

source(here("03_summaries/scores_helper.R"))

run_ages <- read_rds(here(glue("01_fetched_data/run_data.rds"))) |>
  select(site, dataset, task_id, user_id, run_id, age)

scores <- combine_scores()

task_scores <- scores |>
  mutate(metric_type = if_else(str_detect(metric_type, "ability"), "ability", metric_type)) |>
  filter(!is.na(metric_value)) |>
  inner_join(task_metrics) |>
  mutate(model = model |>
           fct_recode(independent = "no pooling IRT",
                      multigroup = "partial pooling IRT",
                      pooled = "full pooling IRT",
                      roar = "ROAR IRT")) |>
  unite("score_type", model, metric_type, sep = " ", remove = FALSE, na.rm = TRUE)

dataset_scores <- task_scores |>
  filter(task_id != "mefs") |>
  filter(!str_detect(score_type, "multigroup|pooled")) |>
  # filter(is.na(model) | model == "independent") |>
  select(dataset, run_id, user_id, task_id, score_type, score = metric_value, age) |>
  mutate(age = round(age, 1),
         score = round(score, 2))
  
dataset_scores_nested <- dataset_scores |>
  # filter(is.na(dataset))
  nest(data = -dataset)
  # count(task_id, item_task, metric_type)

upload_dataset_scores <- \(dataset_name) {
  ds_scores <- dataset_scores_nested |> filter(dataset == dataset_name) |> pull(data) |> pluck(1)
  dataset <- redivis$organization("levante")$dataset(dataset_name, version = "current")
  dataset <- dataset$create_next_version(if_not_exists = TRUE)
  scores_table <- dataset$table("scores")
  scores_table$update(upload_merge_strategy = "replace")
  scores_table$upload("scores")$create(ds_scores, if_not_exists = FALSE, rename_on_conflict = TRUE)
  dataset$release()
}

# upload_dataset_scores("co_bogota_pilot")
# upload_dataset_scores("co_rural_pilot")
