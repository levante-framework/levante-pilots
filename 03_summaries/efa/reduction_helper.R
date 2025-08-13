

# identify items with measurement noninvariance

iter_free_noninvar <- function(data1, df_vals1, fit_configural, fit_scalar, group_equal = c("loadings", "intercepts"), alpha = 0.05) {
  
  # record noninvariant parameters (syntax strings for group.partial)
  group.partial <- character()
  
  # results table
  noninvar_table <- tibble::tibble(
    form_subconstruct = character(),
    item = character(),
    param_type = character(),  # NEW: intercept / loading / threshold
    X2 = numeric()
  )
  
  # helper: map item -> subconstruct (once)
  fsc_map <- df_vals1 %>%
    tidyr::pivot_longer(-c(site, user_id, child_id), names_to = "variable") %>%
    dplyr::distinct(variable) %>%
    dplyr::left_join(
      data1 %>% dplyr::distinct(variable, form_subconstruct),
      by = "variable"
    )
  
  current_fit <- fit_scalar
  chi_cut <- stats::qchisq(1 - alpha, df = 1)
  # Compute the CFI for fit_configural
  cfi0 <- lavaan::fitMeasures(fit_configural, "cfi")
  
  repeat {
    # Compute current CFI
    cfi_current <- lavaan::fitMeasures(current_fit, "cfi")
    
    # Check if the absolute difference between CFI values is small
    if (!is.na(cfi_current) && !is.na(cfi0) && abs(cfi_current - cfi0) < 0.01) {
      break  # Stop if CFI difference is less than 0.01
    }
    
    # LM test
    lmtest <- lavaan::lavTestScore(current_fit)
    uni <- dplyr::arrange(lmtest$uni, dplyr::desc(X2))
    if (nrow(uni) == 0 || is.na(max(uni$X2)) || max(uni$X2) < chi_cut) break
    
    # take the largest X2
    best_row <- dplyr::slice_head(uni, n = 1)
    pt <- lavaan::parTable(current_fit)
    match_idx <- which(pt$plabel %in% c(best_row$lhs, best_row$rhs))
    if (!length(match_idx)) break
    
    op_type <- pt$op[match_idx[1]]
    lhs <- pt$lhs[match_idx[1]]
    rhs <- pt$rhs[match_idx[1]]
    
    # build lavaan syntax for group.partial
    param_str <- switch(
      op_type,
      "~1" = paste(lhs, "~ 1"),           # intercept
      "=~" = paste(lhs, "=~", rhs),       # loading
      "|"  = paste(lhs, "|",  rhs),       # threshold
      "~~" = paste(lhs, "~~", rhs),
      "~"  = paste(lhs, "~",  rhs),
      NA_character_
    )
    if (is.na(param_str)) break
    if (param_str %in% group.partial) {
      uni <- uni[-1, , drop = FALSE]
      if (nrow(uni) == 0) break
      next
    }
    
    # derive the *item name* and param_type
    item_name <- dplyr::case_when(
      op_type %in% c("~1", "|") ~ lhs,
      op_type == "=~" ~ rhs,
      TRUE ~ NA_character_
    )
    param_type <- dplyr::case_when(
      op_type == "~1" ~ "intercept",
      op_type == "=~" ~ "loading",
      op_type == "|"  ~ "threshold",
      TRUE ~ "other"
    )
    
    # map to subconstruct
    subc <- fsc_map$form_subconstruct[match(item_name, fsc_map$variable)]
    if (length(subc) == 0) subc <- NA_character_

    
    # update group.partial
    group.partial <- c(group.partial, param_str)
    
    # Try to refit model
    current_fit <- tryCatch({
      fit_invariance_model_mf(
        formi       = formi,
        df_val      = df_vals1,
        data_long   = data1,
        group_equal = group_equal,
        estim       = "ML",
        group.partial = group.partial
      )
    }, error = function(e) NULL)
    
    
    if (is.null(current_fit) || !lavaan::lavInspect(current_fit, "converged")) break
    
    
    # if the model converged then update the table
    noninvar_table <- dplyr::bind_rows(
      noninvar_table,
      tibble::tibble(
        form_subconstruct = subc,
        item = item_name,
        param_type = param_type,
        X2 = best_row$X2
      )
    )
    
  }
  
  return(noninvar_table)
}


iter_free_noninvar_less3 <- function(data1, df_vals1, fit_configural, fit_scalar, group_equal = c("loadings", "intercepts"), alpha = 0.05) {
  
  # Record non-invariant parameters (syntax strings for group.partial)
  group.partial <- character()
  
  # Results table
  noninvar_table <- tibble::tibble(
    form_subconstruct = character(),
    item = character(),
    param_type = character(),  # NEW: intercept / loading / threshold
    X2 = numeric()
  )
  
  # Helper function: map item -> subconstruct (only once)
  fsc_map <- df_vals1 %>%
    tidyr::pivot_longer(-c(site, user_id, child_id), names_to = "variable") %>%
    dplyr::distinct(variable) %>%
    dplyr::left_join(
      data1 %>% dplyr::distinct(variable, form_subconstruct),
      by = "variable"
    )
  
  current_fit <- fit_scalar
  chi_cut <- stats::qchisq(1 - alpha, df = 1)
  # Compute CFI for the configural model
  cfi0 <- lavaan::fitMeasures(fit_configural, "cfi")
  
  # identify item one-by-one
  repeat {
    # Compute current CFI
    cfi_current <- lavaan::fitMeasures(current_fit, "cfi")
    
    # Stop if the difference in CFI is smaller than the threshold
    if (!is.na(cfi_current) && !is.na(cfi0) && abs(cfi_current - cfi0) < 0.01) {
      break  # Stop if the CFI difference is less than 0.01
    }
    
    # LM test
    lmtest <- lavaan::lavTestScore(current_fit)
    uni <- dplyr::arrange(lmtest$uni, dplyr::desc(X2))  # Sort by descending X2 values
    
    # Filter rows where X2 > chi_cut
    uni_filtered <- uni[uni$X2 > chi_cut, ]
    if (nrow(uni_filtered) == 0) break  # Exit if no items have X2 > chi_cut
    
    # Iterate over the filtered rows (where X2 > chi_cut)
    for (i in 1:nrow(uni_filtered)) {
      best_row <- uni_filtered[i, , drop = FALSE]  # Get the current best row
      pt <- lavaan::parTable(current_fit)
      match_idx <- which(pt$plabel %in% c(best_row$lhs, best_row$rhs))
      
      if (!length(match_idx)) break  # Exit if no matching parameter found
      
      op_type <- pt$op[match_idx[1]]
      lhs <- pt$lhs[match_idx[1]]
      rhs <- pt$rhs[match_idx[1]]
      
      # Build lavaan syntax for the group.partial
      param_str <- switch(
        op_type,
        "~1" = paste(lhs, "~ 1"),           # intercept
        "=~" = paste(lhs, "=~", rhs),       # loading
        "|"  = paste(lhs, "|",  rhs),       # threshold
        "~~" = paste(lhs, "~~", rhs),
        "~"  = paste(lhs, "~",  rhs),
        NA_character_
      )
      if (is.na(param_str)) break
      if (param_str %in% group.partial) {
        next  # Skip if the parameter has already been freed
      }
      
      # Extract item name and parameter type
      item_name <- dplyr::case_when(
        op_type %in% c("~1", "|") ~ lhs,
        op_type == "=~" ~ rhs,
        TRUE ~ NA_character_
      )
      param_type <- dplyr::case_when(
        op_type == "~1" ~ "intercept",
        op_type == "=~" ~ "loading",
        op_type == "|"  ~ "threshold",
        TRUE ~ "other"
      )
      
      # Map item to subconstruct
      subc <- fsc_map$form_subconstruct[match(item_name, fsc_map$variable)]
      if (length(subc) == 0) subc <- NA_character_
      
      # Check if removing the current item and those already in noninvar_table leads to fewer than 3 items in the subconstruct
      remaining_items <- fsc_map %>%
        dplyr::filter(form_subconstruct == subc) %>%
        dplyr::pull(variable)
      
      # Exclude items already in noninvar_table and the current item (item_name)
      remaining_items <- setdiff(remaining_items, union(noninvar_table$item, item_name))
      
      if (length(remaining_items) < 3) {
        # If removing the item results in fewer than 3 items, skip this item
        next
      }
      
      
      # Update group.partial and prepare to refit the model
      group.partial <- c(group.partial, param_str)
      
      # Attempt to refit the model
      current_fit <- tryCatch({
        fit_invariance_model_mf(
          formi       = formi,
          df_val      = df_vals1,
          data_long   = data1,
          group_equal = group_equal,
          estim       = "ML",
          group.partial = group.partial
        )
      }, error = function(e) NULL)
      
      if (is.null(current_fit) || !lavaan::lavInspect(current_fit, "converged")) break  # Exit if model did not converge
      
      # If the model converged, update the non-invariant table
      noninvar_table <- dplyr::bind_rows(
        noninvar_table,
        tibble::tibble(
          form_subconstruct = subc,
          item = item_name,
          param_type = param_type,
          X2 = best_row$X2
        )
      )
      
      break  # Once a valid item is found and fixed, exit the loop and proceed to the next iteration
    }
  }
  
  return(noninvar_table)
}


# reduce item pool based on measurement invariance and keep at least n items per subscale

reduce_noninv_var_map <- function(data1, df_vals1, noninvar_table) {
  # 1. Sort non-invariant items by X2 descending
  noninvar_sorted <- noninvar_table %>%
    arrange(desc(X2))
  
  # 2. Build initial map: subconstruct → items
  current_map <- data1 %>%
    filter(variable %in% colnames(df_vals1)) %>%
    distinct(form_subconstruct, variable) %>%
    group_by(form_subconstruct) %>%
    summarise(items = list(variable), .groups = "drop")
  
  # Track removed items
  removed_items <- character()
  
  # 3. Iterate over sorted non-invariant items
  for (i in seq_len(nrow(noninvar_sorted))) {
    itm  <- noninvar_sorted$item[i]
    subc <- noninvar_sorted$form_subconstruct[i]
    
    # Skip if already removed
    if (itm %in% removed_items) next
    
    # Current number of items in this subconstruct
    n_items <- length(current_map$items[current_map$form_subconstruct == subc][[1]])
    
    # Skip if removing would drop below n
    if (n_items <= 3) next
    
    # Remove the item
    current_map$items[current_map$form_subconstruct == subc][[1]] <- 
      setdiff(current_map$items[current_map$form_subconstruct == subc][[1]], itm)
    
    removed_items <- c(removed_items, itm)
  }
  
  # 4. Return named list: form_subconstruct → items
  unique(unlist(current_map$items))
}


# reduce_items_and_refit

reduce_items_and_refit <- function(df_val, data_long, formi, noninvar_table, 
                                   group_equal = c("loadings", "intercepts")) {
  
  # Step 1: Keep only invariant items (plus id columns)
  invar_items <- reduce_noninv_var_map(data_long, df_val, noninvar_table)
  if (is.null(df_val) || is.null(data_long) || length(invar_items) == 0) {
    return(tibble(fit_reducted = list(NULL),
                  final_params = list(NULL),
                  item_table = list(NULL)))
  }
  
  df_val_reduced <- df_val %>%
    select(any_of(c("site", "user_id", "child_id", invar_items)))
  data_long_reduced <- data_long %>%
    filter(variable %in% invar_items)
  
  # Step 2: Fit initial scalar invariance model
  fit <- fit_invariance_model_mf(
    formi       = formi,
    df_val      = df_val_reduced,
    data_long   = data_long_reduced,
    group_equal = group_equal
  )
  
  params <- tryCatch(
    extract_parameters_multiF(fit, data_long_reduced, std = FALSE), # unstandardized
    error = function(e) return(NULL)
  )
  if (is.null(params)) {
    return(tibble(fit_reducted = list(NULL),
                  final_params = list(NULL),
                  item_table = list(NULL)))
  }
  
  subconstructs <- unique(params$form_subconstruct)
  results <- list()
  
  for (sc in subconstructs) {
    items_current <- params %>%
      filter(type == "loading", form_subconstruct == sc) %>%
      distinct(item) %>%
      pull(item)
    
    df_val_reduced_sc <- df_val
    data_long_reduced_sc <- data_long
    
    while (length(items_current) > 3) {
      # First group's loadings (unstandardized)
      first_group <- unique(params$site)[1]
      load_one_group <- params %>%
        filter(type == "loading",
               form_subconstruct == sc,
               item %in% items_current,
               site == first_group) %>%
        select(item, value)
      
      # Remove smallest absolute loading
      item_to_remove <- load_one_group %>%
        mutate(abs_load = abs(value)) %>%
        arrange(abs_load) %>%
        slice(1) %>%
        pull(item)
      
      items_current <- setdiff(items_current, item_to_remove)
      
      df_val_reduced_sc <- df_val_reduced_sc %>%
        select(-all_of(item_to_remove))
      data_long_reduced_sc <- data_long_reduced_sc %>%
        filter(variable != item_to_remove)
      
      fit <- fit_invariance_model_mf(formi, df_val_reduced_sc, data_long_reduced_sc,
                                     group_equal = group_equal)
      if (is.null(fit)) break
      
      params <- extract_parameters_multiF(fit, data_long_reduced_sc, std = FALSE)
    }
    
    # Final retained loadings for this subconstruct (first group only)
    first_group <- unique(params$site)[1]
    load_one_group <- params %>%
      filter(type == "loading",
             form_subconstruct == sc,
             item %in% items_current,
             site == first_group) %>%
      select(item, loading = value) %>%
      mutate(form_subconstruct = sc,
             invariant = !(item %in% noninvar_table$item))
    
    results[[sc]] <- load_one_group
  }
  
  # Step 3: Final reduced item set
  final_items <- bind_rows(results) %>%
    distinct(form_subconstruct, item)
  
  df_val_final <- df_val %>%
    select(any_of(c("site", "user_id", "child_id", final_items$item)))
  data_long_final <- data_long %>%
    filter(variable %in% final_items$item)
  
  final_fit <- fit_invariance_model_mf(formi, df_val_final, data_long_final,
                                       group_equal = group_equal)
  
  #final_fit_configural = fit_invariance_model_mf(formi, df_val_final, data_long_final,
  #                                               group_equal = NULL)
  scalar_params <- tryCatch(
    extract_parameters_multiF(final_fit, data_long_final, std = F),
    error = function(e) return(NULL)
  )
  if (is.null(scalar_params)) {
    return(tibble(fit_scalar_reducted = list(NULL),
                  #fit_configural_reducted = list(NULL),
                  scalar_params = list(NULL),
                  item_table = list(NULL)))
  }
  
  item_table <- bind_rows(results) %>%
    mutate(form_construct = formi) %>%
    select(form_construct, form_subconstruct, item, invariant)
  
  tibble(
    fit_scalar_reducted =   list(final_fit),
    #fit_configural_reducted =   list(final_fit_configural),
    scalar_params        = list(scalar_params),
    item_table          = list(item_table)
  )
}
