task_categories <- tribble(
  ~task, ~task_category,
  "hearts-and-flowers", "executive function",
  "same-different-selection", "executive function",
  "memory-game", "executive function",
  "mefs", "executive function",
  "egma-math", "math",
  "matrix-reasoning", "reasoning",
  "mental-rotation", "spatial cognition",
  "trog", "language",
  "vocab", "language",
  "pa-es", "reading",
  "sre-es", "reading",
  "swr-es", "reading",
  "pa", "reading",
  "sre", "reading",
  "swr", "reading",
  "sre-de", "reading",
  "swr-de", "reading",
  # "emotion-reasoning", "social cognition",
  "theory-of-mind", "social cognition",
  "hostile-attribution", "social cognition"
) |> mutate(task_category = task_category |> str_to_sentence() |> fct_inorder())

task_metrics <- tribble(
  ~task, ~metric_type,
  "hearts-and-flowers", "ability",
  "same-different-selection", "ability",
  "memory-game", "ability",
  "mefs", "total_score",
  "egma-math", "ability",
  "matrix-reasoning", "ability",
  "mental-rotation", "ability",
  "trog", "ability",
  "vocab", "ability",
  "pa-es", "prop_correct",
  "sre-es", "guessing_adjusted_number_correct",
  "swr-es", "ability",
  "pa", "prop_correct",
  "sre", "guessing_adjusted_number_correct",
  "swr", "prop_correct",
  "sre-de", "guessing_adjusted_number_correct",
  "swr-de", "prop_correct",
  # "emotion-reasoning", "prop_correct",
  "theory-of-mind", "ability",
  "hostile-attribution", "prop_correct"
)

combine_scores <- \() {
  score_files <- list.files(here("02_scored_data/scores"), pattern = "*.rds",
                            full.names = TRUE)
  score_list <- score_files |> map(read_rds)
  # exclude_tasks <- c("hostile-attribution", "pa-es")
  # score_list <- read_rds("scores/combined_scores.rds")
  
  run_ages <- participants |>
    select(user_id, ages) |>
    unnest(ages)
  
  scores_noages <- score_list |>
    bind_rows() |>
    rename(task = task_id) 
  
  mefs_age_guesses <- scores_noages |>
    filter(task == "mefs") |>
    left_join(run_ages |> group_by(user_id) |> summarise(age = mean(age, na.rm=TRUE)))
  
  scores <- scores_noages |>
    filter(task != "mefs") |>
    left_join(run_ages) |>
    bind_rows(mefs_age_guesses) |> # add mefs back in
    filter(!is.na(age)) |>
    # filter(!is.na(age), age >= 5, age <= 12) |>
    left_join(task_categories) |>
    mutate(task = str_replace(task, "-es", ""),
           task = str_replace(task, "-de", "") )|>
    group_by(site, task) |>
    mutate(site_task_n = n_distinct(user_id),
           site_task_label = glue("{task}\n(n = {site_task_n})")) |>
    group_by(task) |>
    mutate(task_label = glue("{task}\n(n = {n_distinct(user_id)})")) |>
    ungroup() |>
    mutate(site_label = site |>
             # fct_relevel("co_pilot", "de_pilot", "ca_pilot") |>
             fct_recode("Canada" = "ca_pilot",
                        "Colombia" = "co_pilot",
                        "Germany" = "de_pilot",
                        "US" = "us_pilot")) |>
    group_by(site) |>
    mutate(site_label = glue("{site_label} (n = {n_distinct(user_id)})")) |>
    ungroup()
}
