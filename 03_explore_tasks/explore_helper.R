load_task_data <- function(task_names) {
  task_data_nested <- read_rds(here(glue("01_fetched_data/task_data_nested.rds")))
  
  run_ages <- read_rds(here(glue("01_fetched_data/run_data.rds"))) |>
    select(site, run_id, age)
  
  task_data_nested |>
    unnest(data) |>
    filter(task_id %in% task_names) |>
    left_join(run_ages)
}
