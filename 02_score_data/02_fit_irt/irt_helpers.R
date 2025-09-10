### functions to prep data for modeling

# add identifiers for each instance of each item
item_sep <- "-"
dedupe_items <- function(df) {
  df |>
    # group_by(user_id, item_uid) |>
    group_by(run_id, item_uid) |>
    mutate(instance = seq_along(item_uid)) |> # i
    ungroup() |>
    mutate(item_inst = glue("{item_uid}{item_sep}{instance}")) # item_i
}

# remove items with no variance
remove_no_var_items <- function(df, item_n_min = 1) {
  df |>
    group_by(item_inst) |>
    mutate(item_mean = mean(correct, na.rm = TRUE), 
           item_n = length(correct)) |> # item means
    ungroup() |>
    filter(item_mean > 0, item_mean < 1, item_n > item_n_min) # need to be between 0 and 1
}

# remove items that don't cross groups
# could potentially be useful if there are missing data issues
remove_singlegroup_items <- function(df, group_n_min = 2) {
  df |>
    group_by(item_uid) |>
    mutate(n_groups = n_distinct(group)) |>
    ungroup() |>
    filter(n_groups >= group_n_min) # need to be in N or more groups
}

# remove items that aren't shared across all groups
remove_nonshared_items <- function(df) {
  n_groups_total <- length(unique(df$group))
  
  df |>
    group_by(item_uid) |>
    mutate(n_groups = n_distinct(group)) |>
    ungroup() |>
    filter(n_groups == n_groups_total) # need to be in N or more groups
}

remove_no_var_items_bygroup <- function(df, item_n_min = 1) {
  df |>
    group_by(item_inst, group) |>
    mutate(item_mean = mean(correct, na.rm = TRUE), 
           item_n = length(correct)) |> # item means
    group_by(item_inst) |>
    mutate(low_var = any(item_mean == 0) | any(item_mean== 1), 
           low_n = any(item_n < item_n_min)) |>
    ungroup() |>
    filter(!low_var, !low_n) # need to be between 0 and 1
}

# format data for mirt
to_mirt_shape <- function(df) {
  df |>
    mutate(correct = as.numeric(correct)) |> # values to numeric
    select(run_id, item_inst, correct) |>
    pivot_wider(names_from = "item_inst", values_from = "correct") |> # column for each item
    column_to_rownames("run_id") # user_id to rownames
}

# format data for mirt
to_mirt_shape_grouped <- function(df) {
  df |>
    mutate(correct = as.numeric(correct)) |> # values to numeric
    select(run_id, group, item_inst, correct) |>
    pivot_wider(names_from = "item_inst", values_from = "correct") |> # column for each item
    column_to_rownames("run_id") # user_id to rownames
}

paste_c <- partial(paste, collapse = ",")

# generates the mirt model strings
# really important to note that item names cannot have spaces or hyphens because mirt
# this will throw a "number of parameters doesn't match" error
generate_model_str <- function(df, df_prepped, item_type, f) { # f = num factors
  params <- "d" # always have difficulty
  prior <- ""
  
  items <- df |> pull(item_uid) |> unique() # item ids
  #items <- df_prepped |> colnames() # note if columns are dropped in prep, this fixes matters.
  
  if (item_type != "Rasch") {
    # add slopes a[i] based on parameterization
    s <- as.numeric(str_extract(item_type, "^\\d")) - 1
    params <- c(params, paste0("a", 1:s))
    #prior <- paste0("PRIOR = (1-", length(items), ", a0, norm, 0, 5)") # do these need to be named items?
  }
  constraints <- items |> map(\(item_uid) {
    # get columns with item's instances
    matched <- colnames(df_prepped) |>
      keep(\(col) str_detect(col, glue("^{item_uid}{item_sep}")))
    if (length(matched) > 1) {
      # constraint for item instance: (item_1, item_2, param)
      map_chr(params, \(p) glue("({paste_c(matched)},{p})")) |> paste_c()
    }
  }) |> compact() |> paste_c() # combine into CONSTRAIN statement
  constraint <- if (str_length(constraints) > 1) paste0("CONSTRAIN=", constraints) else ""
  # F[i] = 1-K statement for each factor
  factors <- map_chr(1:f, \(i) glue("F{i} = 1-{ncol(df_prepped)}"))
  # combine statements
  paste(c(factors, constraint, prior), collapse = "\n")
}


# generates the mirt model strings
# this is a version of the above function that generates constraint strings with numerical indices 
# names seems to throw an error for multigroup?
generate_model_str_numeric <- function(df, df_prepped, item_type, f) { # f = num factors
  params <- "d" # always have difficulty
  prior <- ""
  
  items <- df |> pull(item_uid) |> unique() # item ids
  #items <- df_prepped |> colnames() # note if columns are dropped in prep, this fixes matters.
  
  if (item_type != "Rasch") {
    # add slopes a[i] based on parameterization
    s <- as.numeric(str_extract(item_type, "^\\d")) - 1
    params <- c(params, paste0("a", 1:s))
    #prior <- paste0("PRIOR = (1-", length(items), ", a0, norm, 0, 5)") # do these need to be named items?
  }
  constraints <- items |> map(\(item_uid) {
    # get columns with item's instances
    matched_idx <- which(str_detect(colnames(df_prepped), paste0(item_uid, item_sep)))
    
    if (length(matched_idx) > 1) {
      # constraint for item instance: (item_1, item_2, param)
      map_chr(params, \(p) glue("({paste_c(matched_idx)},{p})")) |> paste_c()
    }
  }) |> compact() |> paste_c() # combine into CONSTRAIN statement
  constraint <- if (str_length(constraints) > 1) paste0("CONSTRAIN=", constraints) else ""
  # F[i] = 1-K statement for each factor
  factors <- map_chr(1:f, \(i) glue("F{i} = 1-{ncol(df_prepped)}"))
  # combine statements
  paste(c(factors, constraint, prior), collapse = "\n")
}


# get item parameters of fitted mirt model
mirt_coefs <- function(mod) {
  coef(mod, simplify = TRUE)$items |> as_tibble(rownames = "item")
}

# get participant scores of fitted mirt model
mirt_scores <- function(mod, df, df_prepped) {
  scores <- fscores(mod, method = "EAP", verbose = FALSE)
  user_scores <- tibble(run_id = rownames(df_prepped),
                        ability = scores[,1]) # TODO: check this gives correct order
  df |> distinct(user_id, run_id) |> # took out task ID here because of multi-task models
    left_join(user_scores) 
}

# get AIC of fitted mirt model
mirt_aic <- function(mod) mod@Fit$AIC

# get BIC of fitted mirt model
mirt_bic <- function(mod) mod@Fit$BIC

# get coefficients for multigroup model
multigroup_coefs <- \(mod) {
  transpose(coef(mod, simplify = TRUE))$items |>
    map(partial(as_tibble, rownames = "item")) |>
    list_rbind(names_to = "site")
}

# get groups out of multigroup model
multigroup_extract_groups <- \(mod) {
  mod@Data$groupNames |> set_names() |> map(\(gr) extract.group(mod, group = gr))
}

# extract_groups <- \(mod) {
#   if (class(mod) == "SingleGroupClass") return(list(mod))
#   mod@Data$groupNames |> set_names() |> map(\(gr) extract.group(mod, group = gr))
# }

# get item fits out of multigroup model
multigroup_itemfit <- \(submods, fit_stats) {
  submods |>
    map(\(submod) itemfit(submod, fit_stats = fit_stats) |> as_tibble()) |>
    list_rbind(names_to = "site")
}
