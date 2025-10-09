# special function for rescoring DE task 1.0.0-beta.19, which had a bug
# the bug appears to have affected only tom, mrot, and matrix
# the bug is that the columns `answer` and `response` are all coded with respect 
# to the LAST row of the item bank rather than the actual item that was presented 
# to the child. to fix this, we use these responses to look up the actual answer and rwo. 

# library(redivis)
# 
# recode_de_v19 <- function(df) {
#   
#   user <- redivis$user("levante")
#   dataset <- user$dataset("pilot_leuphana_de_main:6c0n:v7_2")
#   runs_table <- dataset$table("runs:daw2")
#   trials_table <- dataset$table("trials:chpv")
#   
#   # Load original redivis tables as tidyverse tibble
#   raw_runs <- runs_table$to_tibble()
# 
#   affected_runs <- raw_runs |> 
#     filter(task_version == "1.0.0-beta.19", 
#            task_id %in% c("matrix-reasoning",
#                           "mental-rotation",
#                           "theory-of-mind"))
#   
#   # remove trials from these runs
#   removed <- filter(df, !(trial_id %in% affected_trials$trial_id))
#   
#   return(removed)
  
  # 
  # 
  # affected_trials <- df |>
  #   filter(run_id %in% pull(affected_runs, run_id)) |>
  #   left_join(select(affected_raw_trials, trial_id, original_item = item))
  #          
  # 
  # # get affected trials
  # matrix_trials <- filter(affected_trials, task_id == "matrix-reasoning")
  # mrot_trials <- filter(affected_trials, task_id == "mental-rotation")
  # tom_trials <- filter(affected_trials, task_id == "theory-of-mind")
  # 
  # # get items from item bank at the time
  # matrix_items <- read_csv(here("02_score_data","01_scoring_prep","item_data_for_bugfixes", 
  #                               "item_bank 9-11-24 - matrix-reasoning-Mars-IB-v2.csv")) |>
  #   filter(trial_type != "practice") |>
  #   mutate(trial_number = 1:n())
  # mrot_items <- read_csv(here("02_score_data","01_scoring_prep","item_data_for_bugfixes", 
  #                             "item_bank 9-11-24 - mental-rotation.csv"))
  # tom_items <- read_csv(here("02_score_data","01_scoring_prep","item_data_for_bugfixes", 
  #                                                        "item_bank 9-11-24 - theory-of-mind.csv"))
  # 
  # ### fix matrix
  # 
  # # first find the responses on the last line
  # concat_alternatives <- function(t, alts) {
  #   alts_split <- str_split(alts, ",")[[1]]
  #   
  #   return(c(t, alts_split))
  # }
  # 
  # get_true_response <- function(response, 
  #                               true_answer, 
  #                               true_response_alternatives) {
  #   
  #   response_index <- which(last_response_set == response)
  #   true_response_set <- concat_alternatives(true_answer, true_response_alternatives)
  #   
  #   # return the mapping of the original incorrect response into the true set
  #   return(true_response_set[response_index])
  # }
  # 
  # last_response_set <- concat_alternatives(matrix_items$answer[nrow(matrix_items)], 
  #                                          matrix_items$response_alternatives[nrow(matrix_items)])
  # 
  # 
  # # then look up the index of the answer that was given in terms of that last line
  # # and map it into the actual response alternatives
  # matrix_trials_fixed <- matrix_trials |>
  #   left_join(select(matrix_items, 
  #                    trial_number, 
  #                    original_item = item, 
  #                    true_answer = answer, 
  #                    true_response_alternatives = response_alternatives), 
  #             by = c("trial_number", "original_item")) |>
  #   mutate(true_response = pmap(list(response,  
  #                                    true_answer,
  #                                    true_response_alternatives), 
  #                               get_true_response), 
  #          true_correct = true_response == true_answer)
  #   
    
# }

# recode correctness + reclassify items for HF
recode_hf <- function(df) {
  hf_trials <- df |>
    filter(item_task == "hf") |>
    # code too fast/slow RTs as incorrect
    mutate(response_fast = rt_numeric < 200, response_slow = rt_numeric > 2000,
           correct = correct & !response_fast & !response_slow) |>
    select(-response_fast, -response_slow) |>
    # recode items based on whether they're same as previous item
    group_by(run_id, item_group) |>
    arrange(trial_number) |>
    mutate(hf_type = case_when(
      is.na(lag(item)) ~ "start",
      item == lag(item) ~ "stay",
      item != lag(item) ~ "switch")) |>
    ungroup() |>
    mutate(item = paste(item, hf_type, sep = "_"),
           item_uid = paste(item_group, item, sep = "_")) |>
    select(-hf_type)

  df |>
    filter(item_task != "hf") |>
    bind_rows(hf_trials)
}

# recode correctness for slider
recode_slider <- function(df, threshold) {
  slider_trials <- df |>
    filter(item_group == "slider") |>
    # get target and max values out of item
    tidyr::separate_wider_delim(item, "_",
                                names = c("target", "max_value"),
                                cols_remove = FALSE) |>
    # convert target and max values to numeric and compute if within threshold
    mutate(target = target |> stringr::str_replace("^0", "0."),
           across(c(target, max_value), as.numeric),
           correct = (abs(as.numeric(response) - target) / max_value < threshold)) |>
    # remove trials where response greater than max value (must be from a bug)
    filter(as.numeric(response) <= max_value) |>
    select(-c("target", "max_value"))
  df |>
    filter(item_group != "slider") |>
    bind_rows(slider_trials)
}

# recode correctness for items with wrong answers
recode_wrong_items <- function(df, wrong_items) {
  wrong_trials <- df |>
    right_join(wrong_items) |>
    mutate(correct = !is.na(response) & response == answer_fixed)
  df |>
    anti_join(wrong_items) |>
    bind_rows(wrong_trials)
}

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
    group_by(run_id, trial_type) |>
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
