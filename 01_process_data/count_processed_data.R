sites <- c("ca_pilot", "co_pilot", "de_pilot", "us_pilot")

task_data_nested <- sites |>
  set_names() |>
  map(\(s) read_rds(here(glue("01_processed_data/{s}/task_data_nested.rds")))) |>
  list_rbind(names_to = "site")

task_data_combined <- task_data_nested |>
  select(-task_id) |>
  unnest(data) |>
  rename(child_id = user_id)

n_spec <- "n_{str_remove(.col, '_id')}"

dataset_counts <- task_data_combined |>
  group_by(site, dataset) |>
  summarise(across(c("child_id", "run_id", "parent1_id", "parent2_id", "teacher_id"),
                   n_distinct, .names = n_spec)) |>
  ungroup()
write_rds(dataset_counts, here(glue("01_processed_data/processed_data_counts/dataset_counts.rds")))

task_counts <- task_data_combined |>
  group_by(task_id) |>
  summarise(across(c("child_id", "run_id", "item_id", "trial_id", "corpus_trial_type"),
                   n_distinct, .names = n_spec))
write_rds(task_counts, here(glue("01_processed_data/processed_data_counts/task_counts.rds")))

task_dataset_counts <- task_data_combined |>
  group_by(site, dataset, task_id) |>
  summarise(across(c("child_id", "run_id", "item_id", "trial_id", "corpus_trial_type"),
                   n_distinct, .names = n_spec)) |>
  ungroup()
write_rds(task_counts, here(glue("01_processed_data/processed_data_counts/task_dataset_counts.rds")))
