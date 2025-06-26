# special for SDS, code too fast/slow RTs as incorrect
recode_hf <- function(df) {
  hf_trials <- df |>
    filter(item_task == "hf") |>
    mutate(response_fast = rt_numeric < 200, response_slow = rt_numeric > 2000,
           correct = correct & !response_fast & !response_slow) |>
    select(-response_fast, -response_slow)
  df |>
    filter(item_task != "hf") |>
    bind_rows(hf_trials)
}

recode_slider <- function(df, threshold = 0.15) {
  slider_trials <- df |>
    filter(item_group == "slider") |>
    tidyr::separate_wider_delim(item, "_",
                                names = c("target", "max_value"),
                                cols_remove = FALSE) |>
    mutate(target = target |> stringr::str_replace("^0", "0."),
           across(c(target, max_value), as.numeric),
           correct = (abs(as.numeric(response) - target) / max_value < threshold)) |>
    select(-c("target", "max_value"))
  df |>
    filter(item_group != "slider") |>
    bind_rows(slider_trials)
}

source(here("02_score_data/rescore-sds.R"))
recode_sds <- function(df) {
  sds <- df |>
    filter(item_task == "sds") |> #, site != "us_pilot", item_group!="3unique") |>
    filter(!str_detect(response, "mittel|rote|gelb|blau")) |>
    filter(!(site == "ca_pilot" & timestamp < "2025-02-21"))
  
  # ToDo: apply per user and run to 3-match and 4-match (maybe test on 2-match? shouldn't influence outcome)
  dimension_indices <- c(size = 1, color = 2, shape = 3, number = 4, bgcolor = 5)
  
  sds_parsed <- sds |>
    filter(!item_group %in% c("dimensions", "same")) |>
    mutate(selection = map(response, parse_response)) |>
    select(site, run_id, trial_id, trial_type = item_group, item, response,
           selection, correct, timestamp)
  
  # Order by user, run, and timestamp (or trial_number)
  sds_trials <- sds_parsed |>
    group_by(run_id, trial_type) |>
    arrange(timestamp, .by_group = TRUE) |>
    mutate(trial_index = NA_integer_) |>
    mutate(trial_index = if_else(item == "choice1", 1L, NA_integer_)) |>
    mutate(trial_index = cumsum(!is.na(trial_index))) |>
    ungroup()
  
  sds_disambig <- sds_trials %>%
    group_by(run_id, trial_type, trial_index) %>%
    mutate(
      item = case_when(
        trial_type %in% c("3match", "4match") ~ paste0("choice", row_number()),
        TRUE ~ item
      )
    ) %>%
    ungroup()
  
  # Nest by trial
  sds_nested <- sds_disambig |>
    select(run_id, trial_type, trial_index, item, response, selection, correct, trial_id) |>
    group_by(run_id, trial_type, trial_index) |>
    arrange(item) |>
    nest(data = c(item, response, selection, correct, trial_id)) |>
    ungroup()
  
  # Score each row based on accumulating previous selections
  sds_rescored <- sds_nested |> 
    ungroup() |>
    mutate(trial_correct = map2(data, trial_type, function(df, type) {
      ignore_dims <- case_when(
        type == "same" ~ list(c("number", "bgcolor")),
        type == "2match" ~ list(c("number", "bgcolor")),
        type %in% c("3match", "4match") ~ list(c("size")),
        TRUE ~ list(character(0))
      )[[1]]
      
      previous <- list()
      map_lgl(df$selection, function(sel) {
        result <- compare_selections(sel, previous, ignore_dims)
        previous <<- append(previous, list(sel))
        result
      })
    })) |>
    unnest(c(data, trial_correct))
  
  sds_trials <- sds |>
    select(-item_uid) |>
    left_join(sds_rescored |> select(trial_id, trial_correct, trial_item = item, trial_index)) |>
    mutate(#original_correct = correct,
           correct = if_else(!is.na(trial_correct), trial_correct, correct),
           item = if_else(!is.na(trial_item), trial_item, item),
           item_uid = paste(item_task, item_group, item, sep = "_")) |>
           # original_item = item,
           # ) |>
    select(-trial_correct, -trial_item)
  
  df |>
    filter(item_task != "sds") |>
    bind_rows(sds_trials)
}
