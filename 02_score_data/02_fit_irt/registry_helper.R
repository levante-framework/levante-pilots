# functions for interfacing with model registry

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

# given ModelRecord object, instantiate mirt model object
model_from_record <- \(mod_rec) {
  if (model_class(mod_rec) == "SingleGroupClass") {
    mirt(data = mod_rec@data, pars = model_vals(mod_rec), TOL = NaN)
  } else if (model_class(mod_rec) == "MultipleGroupClass") {
    multipleGroup(data = mod_rec@data, group = mod_rec@groups,
                  pars = model_vals(mod_rec), TOL = NaN)
  }
}

# count number of items with non-NA responses for each run
count_items <- \(mod_rec) {
  counts <- mod_rec@data |> negate(is.na)() |> rowSums()
  props <- counts / ncol(mod_rec@data)
  props |>
    set_names(mod_rec@runs) |>
    enframe(name = "run_id", value = "prop_items")
}
