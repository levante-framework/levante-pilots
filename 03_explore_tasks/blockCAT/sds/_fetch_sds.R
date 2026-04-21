# list of processed datasets to fetch
data_sources <- list("pilot_uniandes_co_rural_processed:bxgv",
                     "pilot_uniandes_co_bogota_processed:d0c5",
                     "pilot_mpieva_de_main_processed:8wjx",
                     "pilot_western_ca_main_processed:bgcj")

# fetch processed trials data
trials <- map(data_sources, get_trials)

# subset to mental rotation
trials_sds <- trials |>
  list_rbind() |>
  filter(item_task == "sds")

write_rds(trials_sds, "data/trials_sds.rds", compress = "gz")


runs <- map(data_sources, get_scores)

runs_sds <- runs |>
  list_rbind() |>
  filter(task_code == "sds") |>
  select(site, dataset, run_id, user_id, age, language, adaptive)

write_rds(runs_sds, "data/runs_sds.rds", compress = "gz")