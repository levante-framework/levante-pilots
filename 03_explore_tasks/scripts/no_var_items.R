param_tasks <- c("math",
                 "matrix",
                 "mrot",
                 "sds",
                 "trog")

task_data <- task_data_nested |> unnest(data) |> filter(item_task %in% param_tasks)

item_summary <- task_data |> dedupe_items() |>
  group_by(item_uid, item_inst) |>
  summarise(item_mean = mean(correct, na.rm = TRUE), 
            item_n = length(correct)) |> # item means
  ungroup() #|>
  # filter(item_mean > 0, item_mean < 1, item_n > item_n_min) # need to be between 0 and 1


item_summary |>
  filter(item_mean == 0 | item_mean == 1) |>
  filter(str_detect(item_inst, "-[0-9]+$")) |>
  distinct(item_uid, item_mean, item_n)
  
