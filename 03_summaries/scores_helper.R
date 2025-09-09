task_info <- tribble(
  ~item_task , ~task                    , ~task_category,
  "hf"       , "Hearts & Flowers"       , "Executive function",
  "sds"      , "Same & Different"       , "Executive function",
  "mg"       , "Memory"                 , "Executive function",
  "math"     , "Math"                   , "Math",
  "matrix"   , "Pattern Matching"       , "Reasoning",
  "mrot"     , "Shape Rotation"         , "Reasoning",
  "trog"     , "Sentence Understanding" , "Language",
  "vocab"    , "Vocabulary"             , "Language",
  "tom"      , "Theory of Mind"         , "Social cognition",
  "ha"       , "Social Attribution"     , "Social cognition",
  "pa"       , "Language Sounds"        , "Reading",
  "sre"      , "Sentence Reading"       , "Reading",
  "swr"      , "Word Reading"           , "Reading",
  "pa-es"    , "Language Sounds"        , "Reading",
  "sre-es"   , "Sentence Reading"       , "Reading",
  "swr-es"   , "Word Reading"           , "Reading",
  "sre-de"   , "Sentence Reading"       , "Reading",
  "swr-de"   , "Word Reading"           , "Reading",
  "mefs"     , "MEFS"                   , "Executive function"
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
  "pa"       , "prop_correct",
  "sre"      , "guessing_adjusted_number_correct",
  "swr"      , "ability",
  "mefs"     , "standard_score"
)

core_tasks <- c("hf","sds","mg","math","matrix","mrot","trog","vocab","tom")

run_info <- read_rds(here(glue("01_fetched_data/run_data.rds"))) |>
  select(site, dataset, language, task_id, user_id, run_id, adaptive, age)

combine_scores <- \() {

  score_files <- c("sumscores.rds", "roar_thetas.rds", "scores_custom.rds", "registry_scores.rds")
  
  scores <- score_files |>
    map(\(f) read_rds(here("02_scoring_outputs", "scores", f))) |>
    bind_rows() |>
    left_join(run_info)
  
  mefs <- read_rds(here("02_scoring_outputs", "scores", "scores_mefs.rds"))
  
  scores |>
    bind_rows(mefs) |>
    filter(!is.na(age)) |>
    # filter(!is.na(age), age >= 5, age <= 12) |>
    left_join(task_info) |>
    group_by(site, task) |>
    mutate(site_task_label = glue("{task}\n(n = {n_distinct(run_id)})")) |>
    group_by(task) |>
    mutate(task_label = glue("{task}\n(n = {n_distinct(run_id)})")) |>
    ungroup() |>
    mutate(site_label = site |>
             fct_recode("Canada" = "pilot_western_ca",
                        "Colombia" = "pilot_uniandes_co",
                        "Germany" = "pilot_leuphana_de",
                        "LangCog" = "pilot_langcog_us",
                        "SparkLab" = "partner_sparklab_us")) |>
    group_by(site) |>
    mutate(site_label = glue("{site_label} (n = {n_distinct(run_id)})")) |>
    ungroup()
}
