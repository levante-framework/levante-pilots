# get original item names from unprocessed trials for angle disaggregation analysis
raw_datasets <- list("pilot_uniandes_co_rural:66d2",
                     "pilot_uniandes_co_bogota:3j4z",
                     "pilot_mpieva_de_main:6c0n",
                     "pilot_western_ca_main:97mt")

all_runs <- raw_datasets |>
  map(\(ds) redivis$user("levante")$dataset(ds)$table("runs")$to_tibble())

all_variants <- raw_datasets |>
  map(\(ds) redivis$user("levante")$dataset(ds)$table("variants")$to_tibble())

runs <- all_runs |> set_names(raw_datasets) |> list_rbind(names_to = "dataset")
variants <- all_variants |> list_rbind()

missing_vars <- runs |> anti_join(variants)

missing_vars |> distinct(dataset, task_id, variant_id)

variants |>
  filter(variant_id == "YNgAc6GIv6D1BmUg6vCf")
