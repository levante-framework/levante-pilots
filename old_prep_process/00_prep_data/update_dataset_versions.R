org <- redivis::organization("levante")

get_latest <- \(dataset_name) {
  ds <- org$dataset(dataset_name, version = "current")
  ds$get()
  ds$version
}

update_site <- \(site) {
  ds_file <- glue::glue("00_prep_data/{site}/_datasets.yml")
  dataset_names <- yaml::read_yaml(here::here(ds_file))
  
  dataset_latest <- dataset_names |>
    purrr::modify(\(ds) ds |> purrr::assign_in("version", get_latest(ds$name)))
  
  yaml::write_yaml(dataset_latest, ds_file)
}

sites <- c("co_pilot", "de_pilot", "ca_pilot", "us_pilot")
purrr::walk(sites, update_site)
