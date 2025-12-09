percentify <- \(s) as.character(glue("{sprintf('%.1f', s)}%"))

run_data <- read_rds(here("01_fetched_data/run_data.rds"))
runs_filtered <- read_rds(here("01_fetched_data/runs_filtered.rds"))

# all runs
run_counts <- run_data |> count(site, task_id, name = "n_total_runs")

# runs filtered to completed, no straightlining, first in admin
run_filtered_counts <- runs_filtered |> count(site, task_id, name = "n_filtered_runs")

# runs in trial data (must have item info)
# trial_data <- read_rds(here(glue("01_fetched_data/trial_data.rds")))
# trial_run_counts <- trial_data |> distinct(site, task_id, run_id) |> count(site, task_id, name = "n_trials_runs")

# runs in filtered trial data (both above filters + not all invalid trials)
task_data <- read_rds(here(glue("01_fetched_data/task_data_nested.rds"))) |> unnest(data)
trial_filtered_run_counts <- task_data |> distinct(site, task_id, run_id) |> count(site, task_id, name = "n_trials_filtered_runs")

# scores (above runs + non-missing age)
score_counts <- all_scores |> filter(item_task != "ha") |> count(site, task_id, name = "n_scores")

# included scores (above scores + age in range + first run or large enough age gap)
included_score_counts <- included_scores |> filter(item_task != "ha") |> count(site, task_id, name = "n_included_scores")

# combined_counts <- list(run_counts, trial_run_counts, run_filtered_counts, trial_filtered_run_counts, score_counts, included_score_counts) |>
counts <- list(run_counts, run_filtered_counts, trial_filtered_run_counts, score_counts, included_score_counts) |>
  reduce(left_join) |>
  left_join(all_scores |> distinct(task_id, item_task, task) |> filter(item_task != "ha")) |>
  filter(item_task %in% core_tasks) |>
  select(-task_id, -item_task) |>
  relocate(task, .before = everything()) |>
  arrange(task) |>
  mutate(site = site |> fct_recode(!!!site_labels))

# write_csv(counts, here("04_paper/display/counts.csv"))

# counts_long <- combined_counts |>
#   pivot_longer(starts_with("n_"), names_to = "count", names_prefix = "n_", values_to = "n") |>
#   mutate(count = count |> fct_inorder())

# anti_join(
# all_scores |> filter(item_task != "ha") |> select(site, task_id, run_id),
# task_data |> distinct(site, task_id, run_id)
# )
# 
# lost_runs <- anti_join(
#   task_data |> filter(item_task %in% irt_tasks, item_task != "ha") |> distinct(site, task_id, run_id),
#   all_scores |> select(site, task_id, run_id)
# ) 
  
  # filter(site == "pilot_western_ca") |>
  # filter(timestamp == min(timestamp))
# run_data |>
#   semi_join(lost_runs) |>
#   filter(!is.na(age)) |>
#   filter(time_started == min(time_started)) |> pull(time_started)
#   pull(run_id) |> pluck(1)

site_counts <- counts |>
  group_by(site) |>
  summarise(across(starts_with("n_"), sum)) |>
  pivot_longer(cols = -site,
               names_to = "count", values_to = "n", names_prefix = "n_")

site_diffs <- site_counts |>
  group_by(site) |>
  mutate(diff = lag(n) - n,
         pct_diff = diff / max(n) * 100,
         excluded = glue("{diff} [{percentify(pct_diff)}]"),
         comparison = paste(lag(count), "-", count)) |>
  ungroup() |>
  filter(count != "total_runs") |>
  select(site, count = comparison, n = excluded)

site_endpoints <- site_counts |>
  group_by(site) |>
  slice(1, n()) |>
  ungroup() |>
  mutate(n = as.character(n))

site_table <- bind_rows(site_endpoints, site_diffs) |>
  pivot_wider(names_from = site, values_from = n) |>
  mutate(count = count |> fct_inorder() |> fct_relevel("included_scores", after = Inf)) |>
  arrange(count) |>
  mutate(count = count |> fct_recode(
    "Total number of runs" = "total_runs",
    "Excluded for being incomplete, invalid, or duplicate" = "total_runs - filtered_runs",
    "Excluded for missing item metadata" = "filtered_runs - trials_filtered_runs",
    "Excluded for missing age" = "trials_filtered_runs - scores",
    "Excluded for age out of range or too short retest gap" = "scores - included_scores",
    "Final number of scored runs" = "included_scores"
  )) |>
  rename(Count = count)

# apa_table(site_table, align = "lrrr")

site_totals <- site_counts |> filter(count == "total_runs") |> select(site, n) |> deframe()

user_totals <- run_data |>
  group_by(site) |>
  summarise(n_users = n_distinct(user_id)) |>
  mutate(site = site |> fct_recode(!!!site_labels))

user_score_counts <- coretask_scores |>
  group_by(site) |>
  summarise(n_users = n_distinct(user_id),
            n_runs = n_distinct(run_id)) |>
  mutate(site = site |> fct_recode(!!!site_labels))

collapse_value_list <- \(l) l |> imap(\(v, n) paste(n, v, sep = ": ")) |> paste(collapse = ", ")

get_count_row <- \(i) {
  site_table |> slice(i) |> select(-Count) |> collapse_value_list()
  # imap(\(v, n) paste(n, v, sep = ": ")) |> paste(collapse = ", ")
}

trial_data_subset <- read_rds(here(glue("01_fetched_data/trial_data_subset.rds")))

trial_counts <- left_join(
  trial_data_subset |> count(site, item_task, name = "n_trials"),
  task_data |> count(site, item_task, name = "n_included_trials")
) |>
  filter(item_task %in% core_tasks) |>
  mutate(site = site |> fct_recode(!!!site_labels),
         n_excluded_trials = n_trials - n_included_trials,
         pct_excluded = n_excluded_trials / n_trials * 100) |>
  mutate(excluded = glue("{n_excluded_trials} [{percentify(pct_excluded)}]"))

task_trial_counts <- trial_counts |>
  group_by(item_task) |>
  summarise(across(where(is.integer), sum)) |>
  mutate(pct_excluded = (n_trials - n_included_trials) / n_trials * 100) |>
  mutate(excluded = glue("{n_excluded_trials} [{percentify(pct_excluded)}]"))

site_trial_counts <- trial_counts |>
  group_by(site) |>
  summarise(across(where(is.integer), sum)) |>
  mutate(pct_excluded = (n_trials - n_included_trials) / n_trials * 100) |>
  mutate(excluded = glue("{n_excluded_trials} [{percentify(pct_excluded)}]"))

total_trial_counts <- trial_counts |>
  group_by() |>
  summarise(across(where(is.numeric), sum)) |>
  mutate(pct_excluded = (n_trials - n_included_trials) / n_trials * 100) |>
  mutate(excluded = glue("{n_excluded_trials} [{percentify(pct_excluded)}]"))
