library(redivis)

org <- redivis$user("levante")

raw_dataset_spec <- list(
  list(name = "pilot_uniandes_co_bogota:3j4z", version = "current"),
  list(name = "pilot_uniandes_co_rural:66d2",  version = "current"),
  list(name = "pilot_mpieva_de_main:6c0n",     version = "current"),
  list(name = "pilot_western_ca_main:97mt",    version = "current"))

raw_runs <- map(raw_dataset_spec, \(ds) org$dataset(name = ds$name, version = ds$version)$table("runs")$to_tibble())

raw_run_data <- raw_runs |>
  set_names(map(raw_dataset_spec, \(x) x$name)) |>
  list_rbind(names_to = "redivis_source") |>
  filter_out(validation_msg_run == "schema_row")

runs_coded <- raw_run_data |>
  group_by(user_id, task_id, administration_id, completed) |>
  arrange(time_started) |>
  mutate(run_number_admin = 1:n()) |>
  ungroup() |>
  # group_by(user_id, task_id) |>
  # arrange(age) |>
  # mutate(run_number = if_else(run_number_admin == 1, 1:n()),
  #        age_gap = age - age[1]) |>
  # ungroup() |>
  mutate(validation_msg_run = validation_msg_run |> replace_na(""),
         dataset = str_remove(redivis_source, "\\:.*$")) |>
  transmute(site_label = dataset |> fct_recode(!!!site_labels),
            user_id = user_id,
            run_id = run_id,
            task_id = task_id,
            incomplete = !completed,
            duplicate = completed & run_number_admin != 1,
            straightlining = str_detect(validation_msg_run, "straightlining_10"),
            few_trials = str_detect(validation_msg_run, "less_than_10_test_trials"),
            missing_age = is.na(age),
            # small_age_gap = age_gap <= 1/6,
            # run_number == 1 | (run_number == 2 & age_gap > 1/6),
            out_range_age = !is.na(age) & (age < 5 | age >= 13),
            task_bug = !is.na(task_version) & task_version == "1.0.0-beta.19" &
              task_id %in% c("matrix-reasoning", "mental-rotation", "theory-of-mind"))
write_rds(runs_coded, "runs_coded.rds", compress = "gz")

# run filters:
# not zero trials
# completed
# no straightlining
# first in admin
# not zero valid trials
# non-missing age
# first run or large enough age gap