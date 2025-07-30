source(here("02_score_data/irt_helpers.R"))

regdir <- here("02_scoring_outputs", "model_registry")
mod_files <- list.files(regdir, recursive = TRUE)

mods <- mod_files |>
  str_split("/", simplify = TRUE) |>
  as_tibble(.name_repair = "minimal") |>
  set_names(c("task", "model_set", "subset", "filename")) |>
  mutate(file = mod_files) |>
  mutate(filename = str_remove(filename, "\\.[A-z]*$")) |>
  separate_wider_delim(filename, delim = "_", too_few = "align_start",
                       names = c("task_dup", "itemtype", "nfact", "invariance")) |>
  select(-task_dup)

scoring_spec <- tribble(
  ~task    , ~dataset              , ~model_set        , ~subset         , ~itemtype , ~nfact , ~invariance ,
  "math"   , "partner_sparklab_us" , "multigroup_site" , "overlap_items" , "rasch"   , "f1"   , "full"      ,
  "math"   , "de_leipzig_pilot"    , "by_language"     , "en"            , "rasch"   , "f1"   , NA
)

# get model spec entry corresponding to given task + dataset  
get_spec <- \(score_task, score_dataset) {
  mod_spec <- scoring_spec |> filter(task == score_task, dataset == score_dataset)
  if (nrow(mod_spec) == 0) stop("No model spec found for given task + dataset")
  if (nrow(mod_spec) > 1) stop("Multiple model specs found for given task + dataset")
  return(mod_spec)
}

# given trial data and model spec, load corresponding model and score data from it
rescore <- \(trial_data, mod_spec) {

  # get model spec entry corresponding to given task + dataset  
  mod_file <- mod_spec |> left_join(mods) |> pull(file)
  
  # read model record corresponding to model spec
  mod_rec <- read_rds(file.path(regdir, mod_file))
  
  # prep new data for model
  data_filtered <- trials_task_dataset |> rename(group = site) |> dedupe_items() |> remove_no_var_items()
  # data_prepped <- to_mirt_shape(data_filtered)
  
  data_wide <- data_filtered |> to_mirt_shape_grouped()
  data_prepped <- data_wide |> select(-group)
  groups <- data_wide |> pull(group)
  
  # subset data to items present in model
  overlap_items <- intersect(colnames(data_prepped), mod_rec@items)
  data_aligned <- data_prepped |> select(!!overlap_items)

  # get model parameter values
  mod_vals <- mod_rec@model_vals |> select(-parnum)
  # get data parameter structure
  if (mod_rec@model_class == "SingleGroupClass") {
    data_pars <- mirt(data = data_aligned, pars = "values")
  } else if (mod_rec@model_class == "MultipleGroupClass") {
    data_pars <- multipleGroup(data = data_aligned, group = groups, pars = "values")
  }
  # replace data parameter values with model values
  data_vals <- data_pars |>
    select(group, item, class, name, parnum) |>
    left_join(mod_vals)
  
  # set up mirt model for data using constructed parameter values  
  if (mod_rec@model_class == "SingleGroupClass") {
    mod <- mirt(data = data_aligned, pars = data_vals, TOL = NaN)
  } else if (mod_rec@model_class == "MultipleGroupClass") {
    mod <- multipleGroup(data = data_aligned, group = groups, pars = data_vals, TOL = NaN)
  }
  
  # get scores from model
  scores <- fscores(mod, method = "EAP", response.pattern = data_aligned)
  
  # return scores tibble with better names and run_ids added back in
  scores |>
    as_tibble() |>
    rename(ability = F1, se = SE_F1) |>
    mutate(run_id = rownames(data_prepped), .before = everything()) |>
    mutate(mod_spec = list(mod_spec |> select(-task, -dataset) |> as.list()))
}

rescore_task_dataset <- \(trial_data, score_task, score_dataset) {
  task_dataset <- trial_data |> filter(item_task == score_task, dataset == score_dataset)
  mod_spec <- get_spec(score_task, score_dataset)
  rescore(task_dataset, mod_spec)
}

trial_data <- task_data_nested |> unnest(data)

sparklab_math <- rescore_task_dataset(trial_data, "math", "partner_sparklab_us")
de_math <- rescore_task_dataset(trial_data, "math", "de_leipzig_pilot")
