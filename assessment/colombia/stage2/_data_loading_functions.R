fix_table_types <- function(table_data) {
  table_data |>
    mutate(across(where(is_character),
                  \(x) x |> na_if("null") |> na_if("None"))) |>
    mutate(across(matches("birth_|rt"), as.integer),
           across(matches("difficulty"), as.double),
           across(matches("email_verified|is_reliable|is_bestrun"), as.logical))
}

get_datasets <- function(dataset_names, org_name = "levante") {
  org <- redivis::organization(org_name)
  datasets <- dataset_names |> set_names() |> map(\(dn) org$dataset(dn))
  
  get_dataset_tables <- \(ds) ds$list_tables() |> map(\(t) t$name) |> set_names() |> map(\(tn) ds$table(tn)$to_tibble())
  map(datasets, get_dataset_tables)
}

combine_datasets <- function(dataset_tables) {
  all_table_names <- map(dataset_tables, names) |> unlist() |> unique()
  dataset_tables |>
    map(\(ds) ds |> map(\(t) fix_table_types(t))) |>
    list_transpose(template = all_table_names) |>
    map(list_rbind)
}

collect_users <- function(dataset_data) {
  distinct(dataset_data$users) |>
    left_join(distinct(dataset_data$user_groups),
              by = "user_id", relationship = "many-to-many") |>
    left_join(distinct(dataset_data$groups),
              by = "group_id", suffix = c("_user", "_group"))
}
