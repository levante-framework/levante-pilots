### functions to prep data for modeling

# filter to each users earliest run
filter_repeat_runs <- function(df) {
  df |>
    group_by(user_id) |>
    filter(server_timestamp == min(server_timestamp)) |> # user's earliest trial
    ungroup() |>
    distinct(user_id, run_id) |> # corresponding run id
    inner_join(df) # filter join
}

# add identifiers for each instance of each item
item_sep <- "_"
dedupe_items <- function(df) {
  df |>
    group_by(user_id, item_id) |>
    mutate(instance = seq_along(item_id)) |> # i
    ungroup() |>
    mutate(item_inst = glue("{item_id}{item_sep}{instance}")) # item_i
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

# format data for mirt
to_mirt_shape <- function(df) {
  df |>
    mutate(correct = as.numeric(correct)) |> # values to numeric
    select(user_id, item_inst, correct) |>
    pivot_wider(names_from = "item_inst", values_from = "correct") |> # column for each item
    column_to_rownames("user_id") # user_id to rownames
}

paste_c <- partial(paste, collapse = ",")
generate_model_str <- function(df, df_prepped, item_type, f) { # f = num factors
  params <- "d" # always have difficulty
  prior <- ""
  items <- df |> pull(item_id) |> unique() # item ids
  if (item_type != "Rasch") {
    # add slopes a[i] based on parameterization
    s <- as.numeric(str_extract(item_type, "^\\d")) - 1
    params <- c(params, paste0("a", 1:s))
    #prior <- paste0("PRIOR = (1-", length(items), ", a0, norm, 0, 5)") # do these need to be named items?
  }
  constraints <- items |> map(\(item_id) {
    # get columns with item's instances
    matched <- colnames(df_prepped) |>
      keep(\(col) str_detect(col, glue("^{item_id}{item_sep}")))
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


# wrapper to fit mirt model with supplied arguments
fit_mirt <- function(i, df, item_type, model_str, model_type, task_id, guess, verbose = FALSE) {
  message(glue("fitting row {i}: {task_id} model {item_type} with {model_type} dims"))
  # TODO: temporarily disabled guessing params due to upstream data problem
  mirt(df, itemtype = item_type, model = model_str, #guess = guess,
       technical = list(NCYCLES = 2000), verbose = verbose)
}

# get item parameters of fitted mirt model
mirt_coefs <- function(mod) {
  coef(mod, simplify = TRUE)$items |> as_tibble(rownames = "item")
}

# get participant scores of fitted mirt model
mirt_scores <- function(mod, df, df_prepped) {
  scores <- fscores(mod, method = "MAP", verbose = FALSE)
  user_scores <- tibble(user_id = rownames(df_prepped),
                        ability = scores[,1]) # TODO: check this gives correct order
  df |> distinct(user_id, run_id, task_id) |>
    left_join(user_scores) |> select(-task_id)
}

# get AIC of fitted mirt model
mirt_aic <- function(mod) mod@Fit$AIC

# get AIC of fitted mirt model
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

# get item fits out of multigroup model
multigroup_itemfit <- \(submods, fit_stats) {
  submods |>
    map(\(submod) itemfit(submod, fit_stats = fit_stats) |> as_tibble()) |>
    list_rbind(names_to = "site")
}

# format data for mirt
to_mirt_shape_grouped <- function(df) {
  df |>
    mutate(correct = as.numeric(correct)) |> # values to numeric
    select(user_id, group, item_inst, correct) |>
    pivot_wider(names_from = "item_inst", values_from = "correct") |> # column for each item
    column_to_rownames("user_id") # user_id to rownames
}
