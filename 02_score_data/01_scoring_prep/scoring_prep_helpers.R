source("rescore_sds.R")
recode_sds <- function(df) {
  sds <- df |>
    filter(item_task == "sds") |> #, site != "us_pilot", item_group!="3unique") |>
    filter(!str_detect(response, "mittel|rote|gelb|blau|grün")) |>
    filter(!(site == "pilot_western_ca" & timestamp < "2025-02-21"))
  
  # ToDo: apply per user and run to 3-match and 4-match (maybe test on 2-match? shouldn't influence outcome)
  dimension_indices <- c(size = 1, color = 2, shape = 3, number = 4, bgcolor = 5)
  
  sds_parsed <- sds |>
    filter(!item_group %in% c("dimensions", "same")) |>
    mutate(selection = map(response, parse_response)) |>
    select(site, run_id, trial_id, trial_type = item_group, item, response,
           selection, correct, timestamp)
  
  # Order by user, run, and timestamp (or trial_number)
  sds_trials <- sds_parsed |>
    group_by(run_id, item_group) |>
    arrange(timestamp, .by_group = TRUE) |>
    mutate(trial_index = NA_integer_) |>
    mutate(trial_index = if_else(item == "choice1", 1L, NA_integer_)) |>
    mutate(trial_index = cumsum(!is.na(trial_index))) |>
    ungroup() |>
    filter(trial_index != 0)
  
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
