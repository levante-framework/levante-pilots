# functions for interfacing with local model registry

regdir <- here("02_scoring_outputs", "model_registry")

# list models in registry and parse directory structure into metadata
list_models <- \() {
  mod_files <- list.files(regdir, recursive = TRUE)
  mod_files |>
    str_split("/", simplify = TRUE) |>
    as_tibble(.name_repair = "minimal") |>
    set_names(c("task", "model_set", "subset", "filename")) |>
    bind_cols(path = mod_files)
}

# read in files from path column of model df
load_models <- \(mod_df) {
  mod_df |> mutate(mod_rec = map(path, \(p) read_rds(file.path(regdir, p))))
}

# extract model specifications from filenames
code_models <- \(mod_df) {
  mod_df |>
    mutate(file = str_remove(filename, "\\.rds$")) |>
    separate_wider_delim(file, names = c("task_dup", "itemtype", "nfact", "invariance"),
                         delim = "_", cols_remove = FALSE, too_few = "align_start") |>
    select(-task_dup) |>
    mutate(invariance = invariance |> replace_na(""))
}
