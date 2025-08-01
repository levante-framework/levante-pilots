source("ModelRecord.R")

invariances <- list(
  configural = "",
  metric = c("free_means","free_variances", "intercepts", "slopes"),
  scalar_intercepts = c("free_variances", "intercepts"),
  scalar_slopes_and_intercepts = c("free_variances", "intercepts", "slopes"),
  full = c("intercepts" , "slopes")
)

# for a given task + subsetting variable/value, fit set of pooled models
fit_task_models_pooled <- \(task_data, models, task, subset_var, subset_val, registry_dir) {
  
  subset_var <- enquo(subset_var)
  var_name <- as_name(subset_var)
  
  # filter task data to given task + language
  trials <- task_data |>
    filter(item_task == task) |>
    filter(!!subset_var == subset_val) |>
    unnest(data)
  
  # prep data for modeling
  data_filtered <- trials |> dedupe_items() |> remove_no_var_items()
  data_prepped <- to_mirt_shape(data_filtered)
  
  # pull out chance values
  guess <- data_filtered |> distinct(item_inst, chance) |> pull(chance)
  
  # construct output directory
  out_dir <- file.path(registry_dir, task, paste0("by_", var_name), subset_val)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  # iterate over nfact + itemtype
  mods <- models |> pwalk(
    \(nfact, itemtype) {
      message(glue("fitting {task} {subset_val} model ({nfact} factor {itemtype})"))
      
      # generate model string with item constraints + dimensionality
      model_str <- generate_model_str_numeric(data_filtered, data_prepped, itemtype, nfact)
      
      # fit model!
      mod <- mirt(
        data = data_prepped,
        itemtype = itemtype,
        model = mirt.model(model_str),
        guess = guess,
        verbose = TRUE,
        technical = list(NCYCLES = 5000)
      )
      
      # construct model record out of model
      mod_rec <- modelrecord(mod, rownames(data_prepped))
      
      # save model record
      mod_file <- glue("{task}_{str_to_lower(itemtype)}_f{nfact}.rds")
      write_rds(mod_rec, file.path(out_dir, mod_file), compress = "gz")
    })
}

# wrapper: given task, fit by_language model for each language
fit_task_models <- \(task_data, models, task, registry_dir) {
  task_data |> filter(item_task == task) |> pull(language) |> unique() |>
    walk(\(lang) fit_task_models_pooled(task_data, models, task, language, lang, registry_dir),
         .progress = TRUE)
}

# wrapper: given language, fit by_language model for each task
fit_lang_models <- \(task_data, models, lang, registry_dir) {
  task_data |> filter(language == lang) |> pull(item_task) |> unique() |>
    walk(\(task) fit_task_models_pooled(task_data, models, task, language, lang, registry_dir),
         .progress = TRUE)
}


# for a given task and group variable, fit and record set of multigroup models
fit_task_multigroup_models <- \(task_data, models, task, group = site,
                                overlap_items = TRUE, registry_dir) {
  
  group <- enquo(group)
  overlap <- if (overlap_items) "overlap_items" else "all_items"
  if (!overlap_items) models <- models |> filter(invariance != "configural")
  
  # filter task data to given task
  trials <- task_data |>
    filter(item_task == task) |>
    unnest(data) |>
    rename(group = !!group)
  
  # prep data for modeling
  data_filtered <- trials |> dedupe_items() |> remove_no_var_items()
  if (overlap_items) {
    data_filtered <- data_filtered |>
      remove_nonshared_items() |>
      remove_no_var_items_bygroup()
  }
  data_wide <- data_filtered |> to_mirt_shape_grouped()
  data_prepped <- data_wide |> select(-group)
  groups <- data_wide |> pull(group)
  
  # pull out chance values
  guess <- data_filtered |> distinct(item_inst, chance) |> pull(chance)
  
  # construct output directory
  out_dir <- file.path(registry_dir, task, paste0("multigroup_", as_name(group)), overlap)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  mods <- models |> pmap(
    \(nfact, itemtype, invariance) {
      message(glue("fitting {task} multigroup model ({nfact} factor {itemtype} with {invariance} invariance)"))
      
      # generate model string with item constraints + dimensionality
      model_str <- generate_model_str_numeric(data_filtered, data_prepped, itemtype, nfact)
      
      # fit model!
      mod <- multipleGroup(
        data = data_prepped,
        itemtype = itemtype,
        model = mirt.model(model_str),
        group = data_wide$group,
        invariance = invariances[[invariance]],
        guess = guess,
        verbose = TRUE,
        technical = list(NCYCLES = 5000)
      )
      
      # construct model record out of model
      mod_rec <- modelrecord(mod, rownames(data_prepped))
      
      # save model record
      mod_file <- glue("{task}_{str_to_lower(itemtype)}_f{nfact}_{invariance}.rds")
      write_rds(mod_rec, file.path(out_dir, mod_file), compress = "gz")
      
    })
}
