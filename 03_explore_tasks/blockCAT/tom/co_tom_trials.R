
co_trials <- redivis$
  user("levante")$
  dataset("pilot-uniandes-co-rural")$
  query("SELECT * FROM trials WHERE task_id = 'theory-of-mind'")$
  to_tibble()

co_trials |>
  filter(!str_detect(item, "hostile")) |>
  mutate(story = str_extract(item, "^[0-9]+") |> as.integer(),
         pre_cutoff = server_timestamp < lubridate::ymd_hms("2025-07-01 00:00:00")) |>
  # filter(pre_cutoff) |>
  group_by(story, pre_cutoff) |>
  summarise(runs = n_distinct(run_id)) |>
  ungroup() |>
  mutate(pre_cutoff = if_else(pre_cutoff, "pre_cutoff", "post_cutoff")) |>
  pivot_wider(names_from = pre_cutoff, values_from = runs, values_fill = 0)
  # group_by(story) |>
  # summarise(earliest = min(server_timestamp),
  #           latest = max(server_timestamp),
  #           n = n())

co_runs <- redivis$
  user("levante")$
  dataset("pilot-uniandes-co-rural")$
  query("SELECT * FROM runs LEFT JOIN variants ON runs.variant_id = variants.variant_id WHERE runs.task_id = 'theory-of-mind'")$
  to_tibble()

co_runs |>
  mutate(pre_cutoff = time_started < lubridate::ymd_hms("2025-07-01 00:00:00"),
         pre_cutoff = if_else(pre_cutoff, "pre_cutoff", "post_cutoff")) |>
  group_by(pre_cutoff, max_time, max_incorrect) |>
  summarise(n_runs = n_distinct(run_id), n_users = n_distinct(user_id)) |>
  ungroup() |>
  arrange(desc(pre_cutoff))
  

co_trials_proc <- redivis$
  user("levante")$
  dataset("pilot-uniandes-co-rural-processed:bxgv")$
  query("SELECT * FROM trials WHERE task_id = 'theory-of-mind'")$
  to_tibble()

co_scores_proc <- redivis$
  user("levante")$
  dataset("pilot-uniandes-co-rural-processed:bxgv")$
  query("SELECT * FROM scores WHERE scores.task_id = 'theory-of-mind'")$
  to_tibble()
