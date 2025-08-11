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
)

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
  "pa"       , "prop_correct",
  "sre"      , "guessing_adjusted_number_correct",
  "swr"      , "ability",
  "mefs"     , "standard_score"
)

run_info <- read_rds(here(glue("01_fetched_data/run_data.rds"))) |>
  select(site, dataset, language, task_id, variant_id, variant_name,
         user_id, run_id, adaptive, age, time_started)

score_files <- c("sumscores.rds", "roar_thetas.rds", "scores_custom.rds", "registry_scores.rds")

scores <- score_files |>
  map(\(f) read_rds(here("02_scoring_outputs", "scores", f))) |>
  bind_rows() |>
  left_join(run_info)

mefs <- read_rds(here("02_scoring_outputs", "scores", "scores_mefs.rds"))

scores_combined <- scores |>
  bind_rows(mefs) |>
  filter(!is.na(age)) |>
  left_join(task_info)

task_scores <- scores_combined |>
  mutate(metric_type = if_else(str_detect(metric_type, "ability"), "ability", metric_type)) |>
  filter(!is.na(metric_value)) |>
  inner_join(task_metrics) |>
  filter(!str_detect(model_class, "by_language")) |>
  group_by(item_task, user_id, variant_id) |>
  mutate(run_index = row_number()) |>
  ungroup()

task_scores |> group_by(run_id) |> filter(n() > 1)

write_rds(task_scores, here("02_scoring_outputs/scores/scores_combined.rds"))
