### functions to prep data for modeling
item_sep = "-"
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
generate_model_str_numeric <- function(df, df_prepped, item_type, f, priors = NULL) { # f = num factors

  items <- df |> pull(item_uid) |> unique() # item ids

  # F[i] = 1-K statement for each factor
  factors <- map_chr(1:f, \(i) glue("F{i} = 1-{ncol(df_prepped)}"))
  
  params <- "d" # always have difficulty
  if (item_type != "Rasch") {
    # add slopes a[i] based on parameterization
    s <- as.numeric(str_extract(item_type, "^\\d")) - 1
    params <- c(params, paste0("a", 1:s))
  }
  
  constraints <- items |> map(\(item_uid) {
    # get columns with item's instances
    matched_idx <- which(str_detect(colnames(df_prepped), glue("^{item_uid}{item_sep}")))
    
    if (length(matched_idx) > 1) {
      # constraint for item instance: (item_1, item_2, param)
      map_chr(params, \(p) glue("({paste_c(matched_idx)},{p})")) |> paste_c()
    }
  }) |> compact() |> paste_c() # combine into CONSTRAIN statement
  constraint <- if (str_length(constraints) > 1) paste0("CONSTRAIN=", constraints) else ""
  
  # PRIOR = (2-3, 5, d, norm, 0, 1), (4, d, norm, 0, 0.5)')
  prior_terms <- priors |> imap(\(pr, param) glue("(1-{length(items)},{paste(c(param, pr),collapse = ',')})"))
  prior <- if (length(prior_terms) > 0) glue("PRIOR={paste(prior_terms, collapse = ',')}") else ""
  
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
