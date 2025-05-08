# task_categories <- tribble(
#   ~task, ~task_category,
#   "hearts-and-flowers", "executive function",
#   "same-different-selection", "executive function",
#   "memory-game", "executive function",
#   "mefs", "executive function",
#   "egma-math", "math",
#   "matrix-reasoning", "reasoning",
#   "mental-rotation", "spatial cognition",
#   "trog", "language",
#   "vocab", "language",
#   "pa-es", "reading",
#   "sre-es", "reading",
#   "swr-es", "reading",
#   "pa", "reading",
#   "sre", "reading",
#   "swr", "reading",
#   "sre-de", "reading",
#   "swr-de", "reading",
#   # "emotion-reasoning", "social cognition",
#   "theory-of-mind", "social cognition",
#   "hostile-attribution", "social cognition"
# ) |> mutate(task_category = task_category |> str_to_sentence() |> fct_inorder())

task_info <- tribble(
  ~item_task , ~task                    , ~task_category,
  "hf"       , "hearts & flowers"       , "executive function",
  "sds"      , "same & different"       , "executive function",
  "mg"       , "memory"                 , "executive function",
  "math"     , "math"                   , "math",
  "matrix"   , "pattern matching"       , "reasoning",
  "mrot"     , "shape rotation"         , "spatial cognition",
  "trog"     , "sentence understanding" , "language",
  "vocab"    , "vocabulary"             , "language",
  "tom"      , "theory of mind"         , "social cognition",
  "ha"       , "social attribution"     , "social cognition",
  "pa"       , "language sounds"        , "reading",
  "sre"      , "sentence reading"       , "reading",
  "swr"      , "word reading"           , "reading",
  "pa-es"    , "language sounds"        , "reading",
  "sre-es"   , "sentence reading"       , "reading",
  "swr-es"   , "word reading"           , "reading",
  "sre-de"   , "sentence reading"       , "reading",
  "swr-de"   , "word reading"           , "reading",
  "mefs"     , "MEFS"                   , "executive function"
) |> mutate(task = task |> str_to_title() |> fct_inorder(),
            task_category = task_category |> str_to_title() |> fct_inorder())

task_metrics <- tribble(
  ~item_task,   ~metric_type,
  "hf"       , "ability",
  "sds"      , "ability",
  "mg"       , "ability",
  "math"     , "ability",
  "matrix"   , "ability",
  "mrot"     , "ability",
  "trog"     , "ability",
  "vocab"    , "ability",
  "tom"      , "ability",
  "ha"       , "ability",
  "pa"       , "total_correct",
  "sre"      , "guessing_adjusted_number_correct",
  "swr"      , "ability",
  "mefs"     , "total_score"
)

combine_scores <- \() {
  # score_files <- list.files(here("02_scored_data/scores"), pattern = "*.rds",
                            # full.names = TRUE)

  scores_sumscores <- readRDS(here("02_scoring_outputs","scores","sumscores.rds")) |>
    filter(metric_type == "total_correct")
  
  scores_general <- readRDS(here("02_scoring_outputs","scores","scores_general.rds"))
    # mutate(model = "guessing_adjusted_number_correct")
  scores_thetas <- readRDS(here("02_scoring_outputs","scores", "roar_thetas.rds"))
    # mutate(model = "ROAR IRT")
  
  scores_irt <- readRDS(here("02_scoring_outputs","scores","independent_scores.rds")) |>
    mutate(model = "no pooling IRT")
  scores_multigroup <- readRDS(here("02_scoring_outputs","scores","multigroup_scores.rds")) |>
    mutate(model = "partial pooling IRT")
  scores_fullpooling <- readRDS(here("02_scoring_outputs","scores","fullpooling_scores.rds")) |>
    mutate(model = "full pooling IRT")
  
  scores_noages <- bind_rows(scores_sumscores, scores_general, scores_thetas,
                             scores_irt, scores_multigroup, scores_fullpooling) |>
    # rename(task = item_task) |>
    select(-task_id)
  
  # score_list <- score_files |> map(read_rds)
  # score_files <- list.files(here("02_scored_data/scores"), pattern = "*.rds",
  #                           full.names = TRUE)
  # score_list <- score_files |> map(read_rds)

  # run_ages <- participants |>
  #   select(user_id, ages) |>
  #   unnest(ages)
  
  # scores_noages <- score_list |>
  #   bind_rows() |>
  #   rename(task = task_id) 
  
  mefs_age_guesses <- scores_noages |>
    filter(item_task == "mefs") |>
    left_join(run_ages |> group_by(user_id) |> summarise(age = mean(age, na.rm = TRUE)))
  
  scores <- scores_noages |>
    filter(item_task != "mefs") |>
    left_join(run_ages) |>
    bind_rows(mefs_age_guesses) |> # add mefs back in
    filter(!is.na(age)) |>
    # filter(!is.na(age), age >= 5, age <= 12) |>
    left_join(task_info) |>
    # mutate(task = str_replace(task, "-es", ""),
    #        task = str_replace(task, "-de", "") )|>
    group_by(site, task) |>
    mutate(site_task_label = glue("{task}\n(n = {n_distinct(run_id)})")) |>
    group_by(task) |>
    mutate(task_label = glue("{task}\n(n = {n_distinct(run_id)})")) |>
    ungroup() |>
    mutate(site_label = site |>
             # fct_relevel("co_pilot", "de_pilot", "ca_pilot") |>
             fct_recode("Canada" = "ca_pilot",
                        "Colombia" = "co_pilot",
                        "Germany" = "de_pilot",
                        "US" = "us_pilot")) |>
    group_by(site) |>
    mutate(site_label = glue("{site_label} (n = {n_distinct(run_id)})")) |>
    ungroup()
}
