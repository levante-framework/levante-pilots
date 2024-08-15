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
remove_no_var_items <- function(df) {
  df |>
    group_by(item_inst) |>
    mutate(item_mean = mean(correct, na.rm = TRUE)) |> # item means
    ungroup() |>
    filter(item_mean > 0, item_mean < 1) # need to be between 0 and 1
}

# format data for mirt
to_mirt_shape <- function(df) {
  df |>
    mutate(correct = as.numeric(correct)) |> # values to numeric
    select(user_id, item_inst, correct) |>
    pivot_wider(names_from = "item_inst", values_from = "correct") |> # column for each item
    column_to_rownames("user_id") # user_id to rownames
}