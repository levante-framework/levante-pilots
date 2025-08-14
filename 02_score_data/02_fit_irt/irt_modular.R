source("ModelRecord.R")
source("anova_mirt.R")

invariances <- list(
  configural = "",
  metric = c("slopes"),
  scalar = c("free_means", "free_var", "intercepts", "slopes")
)

# for a given task + subsetting variable/value, fit set of pooled models
fit_task_models_pooled <- \(task_data, models, task, subset_var, subset_val, registry_dir) {
  
  subset_var <- enquo(subset_var)
  var_name <- as_name(subset_var)
  
  # filter task data to given task + language
  trials <- task_data |>
    filter(item_task == task) |>
    filter(!!subset_var == subset_val) |>
    # filter(language == "en") |>
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
fit_bylanguage_task <- \(task_data, models, task, registry_dir) {
  task_data |> filter(item_task == task) |> pull(language) |> unique() |>
    walk(\(lang) fit_task_models_pooled(task_data, models, task, language, lang, registry_dir),
         .progress = TRUE)
}

# wrapper: given language, fit by_language model for each task
fit_bylanguage_lang <- \(task_data, models, lang, registry_dir) {
  task_data |> filter(language == lang) |> pull(item_task) |> unique() |>
    walk(\(task) fit_task_models_pooled(task_data, models, task, language, lang, registry_dir),
         .progress = TRUE)
}


# for a given task and group variable, fit and record set of multigroup models
fit_task_models_multigroup <- \(task_data, models, task, group = site,
                                registry_dir) {
  
  group <- enquo(group)

  # filter task data to given task
  trials <- task_data |>
    filter(item_task == task) |>
    unnest(data) |>
    rename(group = !!group)
    # rename(group = site)
  
  # prep data for modeling
  data_filtered_full <- trials |> dedupe_items() |> remove_no_var_items()
  data_wide_full <- data_filtered_full |> to_mirt_shape_grouped()
  data_prepped_full <- data_wide_full |> select(-group)

  # pull out chance values
  guess_full <- data_filtered_full |> distinct(item_inst, chance) |> pull(chance)
  
  data_filtered_overlap <- data_filtered_full |>
    remove_nonshared_items() |>
    remove_no_var_items_bygroup()
  data_wide_overlap <- data_filtered_overlap |> to_mirt_shape_grouped()
  data_prepped_overlap <- data_wide_overlap |> select(-group)
  guess_overlap <- data_filtered_overlap |> distinct(item_inst, chance) |> pull(chance)
  
  # construct output directory
  out_dir <- file.path(registry_dir, task, paste0("multigroup_", as_name(group)))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  mods <- models |> pmap(
    \(nfact, itemtype, invariance) {
      message(glue("fitting {task} multigroup model ({nfact} factor {itemtype} with {invariance} invariance)"))
      
      # generate model string with item constraints + dimensionality
      model_str_overlap <- generate_model_str_numeric(data_filtered_overlap, data_prepped_overlap, itemtype, nfact)
      
      # fit overlap items model
      mod_overlap <- multipleGroup(
        data = data_prepped_overlap,
        itemtype = itemtype,
        model = mirt.model(model_str_overlap),
        group = data_wide_overlap$group,
        invariance = invariances[[invariance]],
        guess = guess_overlap,
        verbose = TRUE,
        technical = list(NCYCLES = 5000)
      )
      
      # construct model record out of model
      mod_rec_overlap <- modelrecord(mod_overlap, rownames(data_prepped_overlap))
      mod_file <- glue("{task}_{str_to_lower(itemtype)}_f{nfact}_{invariance}.rds")
      out_dir_overlap <- file.path(out_dir, "overlap_items")
      dir.create(out_dir_overlap, recursive = TRUE, showWarnings = FALSE)
      write_rds(mod_rec_overlap, file.path(out_dir_overlap, mod_file), compress = "gz")
      
      if (invariance == "configural") return()
      
      # get item parameter values from overlap model
      overlap_vals <- mod2values(mod_overlap) |> #as_tibble() |>
        select(group, item, name, fixed_value = value) |>
        # remove group mean/cov
        filter(item != "GROUP") |>
        # mark params to not be estimated
        mutate(fixed_est = FALSE)
      
      # set up parameter structure of full model
      mod_pars <- multipleGroup(
        data = data_prepped_full,
        itemtype = itemtype,
        group = data_wide_full$group,
        pars = "values"
      ) #|> as_tibble()
      
      # change values for overlap items to values from overlap model
      mod_pars_fixed <- mod_pars |> left_join(overlap_vals) |>
        mutate(value = if_else(!is.na(fixed_value), fixed_value, value),
               est = if_else(!is.na(fixed_est), fixed_est, est)) |>
        select(-fixed_value, -fixed_est)
      
      # generate model string with item constraints + dimensionality
      model_str_full <- generate_model_str_numeric(data_filtered_full, data_prepped_full, itemtype, nfact)
      
      # fit full items model
      mod_full <- multipleGroup(
        data = data_prepped_full,
        itemtype = itemtype,
        model = mirt.model(model_str_full),
        group = data_wide_full$group,
        invariance = invariances[[invariance]],
        pars = mod_pars_fixed,
        guess = guess_full,
        verbose = TRUE,
        technical = list(NCYCLES = 5000)
      )
      
      # construct model record out of model
      mod_rec_full <- modelrecord(mod_full, rownames(data_prepped_full))
      
      # save model record
      mod_file <- glue("{task}_{str_to_lower(itemtype)}_f{nfact}_{invariance}.rds")
      out_dir_full <- file.path(out_dir, "all_items")
      dir.create(out_dir_full, recursive = TRUE, showWarnings = FALSE)
      write_rds(mod_rec_full, file.path(out_dir_full, mod_file), compress = "gz")
      
    })
}

# example calls: pooled models

# fit one task + one language
# fit_task_models_pooled(task_data = task_data_irt, models = models_pooled,
#                        task = "hf", subset_var = language, subset_val = "en")

# fit one task + all languages
# fit_bylanguage_task(task_data = task_data_irt, models = models_pooled,
#                 task = "hf", registry_dir = regdir)

# fit all tasks + one language
# fit_bylanguage_lang(task_data = task_data_irt, models = models_pooled,
#                 lang = "en_us", registry_dir = regdir)

# fit all irt tasks + all languages
# irt_tasks |>
#   walk(\(task) fit_bylanguage_task(task_data = task_data_irt,
#                                 models = models_pooled,
#                                 task = task,
#                                 registry_dir = regdir))


# example calls: multigroup models

# fit one task, grouped by site
# fit_task_models_multigroup(task_data = task_data_irt, models = models_multigroup,
#                            task = "matrix", group = site, registry_dir = regdir)

# fit all irt tasks, grouped by site
# irt_tasks |>
#   walk(\(task) fit_task_models_multigroup(task_data = task_data_irt,
#                                           models = models_multigroup,
#                                           task = task,
#                                           group = site,
#                                           registry_dir = regdir))
