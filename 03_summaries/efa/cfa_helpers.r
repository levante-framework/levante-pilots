library(psych)
library(corrplot)
library(reshape2)
library(patchwork) 
library(gtools)  # natural sorting
library(lavaan)
library(GGally)

# col <- colorRampPalette(c("red", "white", "blue"))(200) # Red to white to blue

build_1factor_model_fixvar <- function(formi, item_names) {
  latent <- gsub("[^[:alnum:]_]", "_", formi)
  paste0(
    latent, " =~ NA*", item_names[1], " + ",
    paste(item_names[-1], collapse = " + "), "\n",
    latent, " ~~ 1*", latent # fix variance for identification
  )
}

# Define a function to fit CFA model for each form_construct
fit_invariance_model_1f <- function(df_val, formi, group_equal = NULL, estim = "WLSMV") {
  item_names <- setdiff(colnames(df_val), c("site", "user_id", "child_id"))
  if (length(item_names) < 3) return(NULL)
  
  model_syntax <- build_1factor_model_fixvar(formi, item_names)
  
  
  tryCatch({
    lavaan::cfa(model_syntax,
                data = df_val,
                group = "site",
                group.equal = group_equal,
                estimator = estim,
                ordered = if (estim == "WLSMV") item_names else NULL)
  }, error = function(e) NULL)
}


extract_parameters_1F <- function(fit) {
  if (is.null(fit)) return(NULL)
  
  params <- parameterEstimates(fit, standardized = FALSE)
  group_labels <- lavInspect(fit, "group.label")
  
  # Loadings
  load <- params %>%
    filter(op == "=~") %>%
    transmute(
      site = factor(group, labels = group_labels),
      item = rhs,
      value = est,
      type = "loading"
    )
  
  # Thresholds
  thresh <- params %>%
    filter(op == "|") %>%
    transmute(
      site = factor(group, labels = group_labels),
      item = lhs,
      value = est,
      type = paste0("threshold", gsub("t", "", rhs))
    )
  
  bind_rows(load, thresh)
}

extract_parameters_1F_intercept <- function(fit) {
  if (is.null(fit)) return(NULL)
  
  params <- parameterEstimates(fit, standardized = FALSE)
  group_labels <- lavInspect(fit, "group.label")
  group_ids <- unique(params$group)
  observed_vars <- lavNames(fit, type = "ov")
  
  # construct valid group label vector safely
  get_site <- function(gid) {
    if (length(group_labels) >= max(group_ids)) {
      factor(gid, levels = group_ids, labels = group_labels[group_ids])
    } else {
      as.character(gid)
    }
  }
  
  # Loadings
  load <- params %>%
    filter(op == "=~") %>%
    mutate(site = get_site(group)) %>%
    transmute(site, item = rhs, value = est, type = "loading")
  
  # Intercepts
  intercept <- params %>%
    filter(op == "~1", lhs %in% observed_vars) %>%
    mutate(site = get_site(group)) %>%
    transmute(site, item = lhs, value = est, type = "intercept")
  
  bind_rows(load, intercept)
}


extract_fscore_1f <- function(fit, df_val) {
  if (is.null(fit)) return(NULL)
  if (!lavInspect(fit, "converged")) return(NULL)
  
  fs_list <- lavPredict(fit)
  if (length(fs_list) == 0) return(NULL)
  
  
  df_split <- split(df_val, df_val$site)
  site_names <- names(fs_list)
  
  
  purrr::map2_dfr(fs_list, site_names, function(fscore_mat, site_name) {
    df_group <- df_split[[site_name]]
    
    n_fs <- nrow(fscore_mat)
    df_group <- df_group[seq_len(n_fs), ]
    
    tibble(
      site = df_group$site,
      user_id = df_group$user_id,
      fscore = as.numeric(fscore_mat[, 1])
    )
  })
}








# multi factor
build_multifactor_model_fixvar <- function(subconstruct_map) {
  # subconstruct_map: named list where names are latent factors, values are item vectors
  model_lines <- purrr::imap_chr(subconstruct_map, function(items, latent) {
    latent_clean <- gsub("[^[:alnum:]_]", "_", latent)
    paste0(
      latent_clean, " =~ NA*", items[1], " + ",  # first item loading freely estimated
      paste(items[-1], collapse = " + ")
    )
  })
  
  var_lines <- names(subconstruct_map) %>%
    gsub("[^[:alnum:]_]", "_", .) %>%
    paste0(., " ~~ 1*", .)
  
  paste(c(model_lines, var_lines), collapse = "\n")
}

build_multifactor_model_default <- function(subconstruct_map) {
  # subconstruct_map: named list where names are latent factors, values are item vectors
  
  model_lines <- purrr::imap_chr(subconstruct_map, function(items, latent) {
    latent_clean <- gsub("[^[:alnum:]_]", "_", latent)
    paste0(latent_clean, " =~ ", paste(items, collapse = " + "))
  })
  
  paste(model_lines, collapse = "\n")
}



fit_invariance_model_mf <- function(formi, df_val, data_long, group_equal = NULL, group.partial = NULL, estim="ML") {
  # map subconstruct → item 
  var_map <- data_long %>%
    distinct(form_subconstruct, variable) %>%
    group_by(form_subconstruct) %>%
    summarise(items = list(variable), .groups = "drop") %>%
    deframe()
  
  item_names <- setdiff(colnames(df_val), c("site", "user_id", "child_id"))
  if (length(item_names) < 3 || length(var_map) < 2) return(NULL)
  
  model_syntax <- build_multifactor_model_fixvar(var_map)
  
  
  tryCatch({
    lavaan::cfa(
      model_syntax,
      data = df_val,
      group = "site",
      estimator = estim,
      group.equal = group_equal,
      group.partial = group.partial,
      ordered = if (estim == "WLSMV") item_names else NULL
    )
  }, error = function(e) NULL)
}








extract_parameters_multiF <- function(fit, data_long, std = F) {
  if (is.null(fit)) return(NULL)
  
  params <- parameterEstimates(fit, standardized = std)
  group_labels <- lavInspect(fit, "group.label")
  item_map <- data_long %>%
    distinct(variable, form_subconstruct) %>%
    deframe()
  
  # Loadings
  load <- params %>%
    filter(op == "=~") %>%
    transmute(
      site = factor(group, labels = group_labels),
      factor = lhs,
      item = rhs,
      form_subconstruct = item_map[rhs],
      value = if (std) std.all else est,
      type = "loading"
    )
  
  # Intercepts (for observed variables)
  intercept <- params %>%
    filter(op == "~1", lhs %in% names(item_map)) %>%  # only for observed items
    transmute(
      site = factor(group, labels = group_labels),
      item = lhs,
      form_subconstruct = item_map[lhs],
      value = if (std) std.all else est,
      type = "intercept"
    )
  
  bind_rows(load, intercept)
}


extract_fscore_mf <- function(fit, df_val) {
  if (is.null(fit)) return(NULL)
  if (!lavInspect(fit, "converged")) return(NULL)
  
  fs_list <- lavPredict(fit)
  if (length(fs_list) == 0) return(NULL)
  
  df_split <- split(df_val, df_val$site)
  site_names <- names(fs_list)
  
  purrr::map2_dfr(fs_list, site_names, function(fscore_mat, site_name) {
    df_group <- df_split[[site_name]]
    
    n_fs <- nrow(fscore_mat)
    df_group <- df_group[seq_len(n_fs), ]
    
    bind_cols(
      tibble(
        site = df_group$site,
        user_id = df_group$user_id,
        child_id = df_group$child_id
      ),
      as.data.frame(fscore_mat)
    )
  })
}


