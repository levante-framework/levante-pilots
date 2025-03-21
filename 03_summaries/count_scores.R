source(here("03_summaries/scores_helper.R"))

scores <- combine_scores()

n_spec <- "n_{str_remove(.col, '_id')}"

scores_counts <- scores |>
  rename(child_id = user_id) |>
  group_by(site) |>
  summarise(across(c("child_id", "run_id", "task"),
                   n_distinct, .names = n_spec)) |>
  ungroup()

write_rds(scores_counts, here(glue("02_scored_data/scores/scores_counts.rds")))
