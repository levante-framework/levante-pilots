# ----------- MISC PROCESSING FUNCTIONS

# de-dictify distractors: extract values after each colon and space, up to comma
dedict <- \(s) {
  s |> str_extract_all("(?<=: )([^,]+)") |>
    map_chr(partial(paste, collapse = ",")) |>
    str_remove_all("[{}']")
}

# adds trial indices
add_trial_number <- function (df) {
  df |>
    group_by(user_id, run_id) |>
    arrange(server_timestamp) |>
    mutate(trial_number = 1:n()) |>
    ungroup() 
}

# cleanup of item ids
clean_item_ids <- function(df) {
  df |>
    # id other items as just item
    mutate(item_id = if_else(!is.na(item_id) & item_id!="", item_id, item)) |>
    # hyphens in item names mess up mirt constraints (yes really)
    mutate(item_id = item_id |> str_replace_all("-", "_"))
}

# final variable set
# do some type conversion in case thetas are missing
get_final_variable_set <- function(df) {
  select(df, dataset, server_timestamp, matches("id"), trial_number,
         corpus_trial_type, chance, correct, rt, response, theta_estimate,
         theta_se) |>
    mutate(theta_estimate = as.numeric(theta_estimate), 
           theta_se = as.numeric(theta_se))
}



# ----------- CLEANUP OF TRIAL DATA

# cleanup of trial data
# includes removing incomplete runs
# ordering
# sanitizing distractors, computing chance
# TODO - add validation cleanup

# filter(task_id %in% irt_tasks,
#        !is.na(item),
#        !is.na(response) | !is.na(correct)) |>
# curly braces in items cause regex problems

clean_trial_data <- function (df) {
  df |>
    add_trial_number() |>
    mutate( # de-dictify distractors: Extract values after each colon and space, up to comma
      distractors_cln = str_extract_all(distractors, "(?<=: )([^,]+)") |>  
        map_chr(~ paste(.x, collapse = ", ")) |>
        str_replace_all("[{}']", "") 
    ) |>
    mutate(distractors_cln = ifelse(distractors_cln=="", NA, str_remove_all(distractors_cln, " "))) |>
    mutate(distractors = distractors |> str_count(":") |> na_if(0),
           chance = 1 / (distractors + 1)) |>
    select(dataset, matches("_id"), trial_number, trial_index, corpus_trial_type, assessment_stage, item,
           answer, chance, response, correct, rt, server_timestamp, distractors_cln, 
           theta_estimate, theta_se, completed) |>
    arrange(user_id, run_id, server_timestamp)
}

# exclude_invalid_trial_data trial data 
# remove incomplete runs
# TODO - remove runs and trials that do not pass validation
exclude_invalid_trial_data <- function (df) {
 df |> 
  filter(completed == TRUE) |>
    select(-completed)
    
}

# ----------- ITEM INFO LOADING

# load math items
load_math_items <- function() {
  read_csv(here("01_process_data/metadata/item_banks/math-item-bank-params.csv")) |>
    rename(distractors = response_alternatives) |> 
    filter(trial_type!="instructions", is.na(notes)) |>
    select(-source, -task, -block_index, -difficulty, -assessment_stage)
}

# load grammar items
load_grammar_items <- function() {
  read_csv(here("01_process_data/metadata/item_banks/trog-item-bank-full-params.csv")) |>
    rename(distractors = response_alternatives) |> 
    filter(!is.na(item)) |>
    select(-source, -task, -d, -d_sp, -assessment_stage, -prompt)
}


# load matrix reasoning item bank
load_matrix_reasoning_items <- function() {
  read_csv(here("01_process_data/metadata/item_banks/matrix-reasoning-Mars-IB-v2-item-bank.csv")) |>
    rename(distractors = response_alternatives) |> 
    filter(!is.na(item)) |>
    select(-source, -task, -orig_item_num)
}

# ----------- MATRIX REASONING

# matrix reasoning processing
process_matrix_reasoning <- function(df) {
  mr_items <- load_matrix_reasoning_items()
  df |>
    filter(task_id == "matrix-reasoning", item!="") |>
    select(-item_id) |>
    left_join(mr_items |> 
                select(answer, item_id)) 
}


# ----------- LANGUAGE AND TOM TASK CODE

# trog processing
process_grammar <- function(df) {
  trog_items <- load_grammar_items()
  
  df |>
    filter(task_id == "trog", item!="") |>
    select(-item_id) |>
    left_join(trog_items |> 
                select(answer, item_id)) 
}

# vocab processing
process_vocab <- function(df) {
  df |>
    filter(task_id == "vocab", corpus_trial_type=="test") |> # or assessment_stage=="test_response"
    mutate(item_id = paste0("vocab-",item))
  
}

#theory of mind is separated by assessment_stage +
# has special processing to identify items 

# note that as of 1/17/24 we are collapsing emotion-reasoning into theory of mind
process_tom <- function(df) {
  tom <- task_data |>
    filter(task_id == "theory-of-mind", item!="") |>
    mutate(corpus_trial_type = str_remove_all(corpus_trial_type, "_question")) |>
    mutate(task_id = fct_collapse(corpus_trial_type,
                                  "theory-of-mind" = c("false_belief", "reality_check", "reference"),
                                  "hostile-attribution" = c("action", "attribution"),
                                  "theory-of-mind" = "emotion_reasoning")) |> #count(task_id, corpus_trial_type)
    group_by(user_id, run_id, task_id, item, corpus_trial_type) |>
    mutate(i = 1:n(), n = n()) |> # sequentially number items
    ungroup() |>
    mutate(item_id = paste(item, corpus_trial_type),
           item_id = paste(item_id, i)) |>
    select(-i, -n)
}

# ----------- EXECUTIVE FUNCTION PROCESSING CODE 

# processing code for same-different-selection
process_sds <- function(df) {
  df |>
    filter(task_id == "same-different-selection", item!="") |>
    filter(corpus_trial_type != "something-same-1") |> # these have no answer (they are just an "information" trial)
    arrange(server_timestamp) |>
    mutate(different = str_extract(item, "different")) |> # trials are "different" or NA
    group_by(user_id, run_id, corpus_trial_type) |> # within subtask (e.g. 3-match)
    mutate(trial_i = consecutive_id(different), # number trials sequentially
           trial_i = if_else(is.na(different), trial_i, trial_i - 1)) |> # "different" trials are actually part of previous trial
    group_by(user_id, run_id, corpus_trial_type, trial_i) |>
    mutate(i = 1:n()) |> # sequential number within multiple "different" trials
    ungroup() |>
    mutate(response = as.character(i) |>
             fct_recode("first" = "1", "second" = "2",
                        "third" = "3", "fourth" = "4")) |>
    group_by(user_id, run_id, corpus_trial_type) |>
    mutate(trial = consecutive_id(trial_i)) |> # renumber trials sequentially
    #group_by(user_id, run_id, corpus_trial_type) |>
    #mutate(item_id = if (all(trial == 1)) paste(corpus_trial_type, i) else paste(corpus_trial_type, trial, response)) |>
    mutate(item_id = if_else(!str_detect(corpus_trial_type, "match|unique"), paste(corpus_trial_type, i), paste(corpus_trial_type, trial, response))) |>
    ungroup() |>
    select(-different, -trial_i, -i, -response, -trial)
}

# processing code for filtering and scoring trials
process_hearts_and_flowers <- function (df, add_corpus_trial_type = FALSE) {
  df <- df |>
    filter(task_id == "hearts-and-flowers") |>
    mutate(item_id = item,
           rt_num = as.numeric(rt), 
           response_fast = rt_num < 200, response_slow = rt_num > 2000) |>
    mutate(correct = !is.na(correct) & correct & !response_fast & !response_slow) |>
    select(-rt_num, -response_fast, -response_slow)
  
  # for CO data
  if (add_corpus_trial_type) {
    df <- df |>
      mutate(corpus_trial_type = str_replace(assessment_stage, " stimulus",""))
  }
  
  # clean up logging error in DE pilot data
  df <- df |>
    mutate(corpus_trial_type = case_when(
      corpus_trial_type == "1500" & trial_index < 56 ~ "hearts", 
      corpus_trial_type == "1500" & trial_index > 56 ~ "flowers", 
      # we don't know if there were ever any 1500s for H&F
      # based on check on user_id S7gqxZoYQA0GJ7CTa2nG
      #corpus_trial_type == "1500" & trial_index > 115 ~ "hearts and flowers",
      .default = corpus_trial_type)) |>
    select(-trial_index)
  
  # there are some corpus_trial_types that don't fit.
  df <- filter(df,  
               corpus_trial_type %in% c("hearts","flowers","hearts and flowers"))
  return(df)
}

# processing code for memory game - mostly vanilla but a little cleanup
process_mg <- function(df) {
  df |>
    filter(task_id == "memory-game") |>
    # bug in DE where there are some trials with double rows for each
    filter(!is.na(correct)) |>
    # fix DE corpus_trial_type bug
    mutate(corpus_trial_type = 
             case_when(
               corpus_trial_type == "" & trial_index < 40 ~ "forward", # determine based on index/order
               corpus_trial_type == "" & trial_index > 40 ~ "backward",
               .default = corpus_trial_type),
           # compute item as the number of items in the answer
           item_id =  as.character(str_count(answer, ":")))
}

# ----------- EGMA PROCESSING CODE
process_egma <- function(df) {
  math_items <- load_math_items()
  
  egma <- df |>
    filter(task_id == "egma-math", item != "") |> 
    # select(-item_id, -corpus_trial_type) |>
    select(-corpus_trial_type) |>
    rename(distractors = distractors_cln) |>
    mutate(item = case_when(item=="{'0': 0, '1': 10}" ~ "0,10", 
                            item=="{'0': 0, '1': 100}" ~ "0,100",
                            item=="{'0': 0, '1': 1000}" ~ "0,1000",
                            item=="{'0': 0, '1': 1}" ~ "0,1",
                            .default = item)) |>
    left_join(math_items |> 
                select(item, answer, item_id, distractors, corpus_trial_type)) |> 
    mutate(corpus_trial_type = case_when(
      is.na(chance) ~ "number line slider",
      item=="0,1" ~ "number line 4afc",
      item=="0,10" ~ "number line 4afc",
      item=="0,100" ~ "number line 4afc",
      item=="0,1000" ~ "number line 4afc",
      str_detect(item, "/") ~ "fraction",
      str_detect(item, "x") ~ "multiplication",
      is.na(corpus_trial_type) ~ "",
      .default = corpus_trial_type)
    ) 
  
  threshold <- 0.15
  slider_trials <- egma |> 
    filter(corpus_trial_type=="number line slider") |>
    select(-item_id) |>
    left_join(math_items |> filter(corpus_trial_type=="number line slider") |>
                select(item, answer, item_id, corpus_trial_type)) |>
    mutate(correct = pmap_lgl(list(item, answer, response), \(item, answer, response) {
      # get slider max from item ("{'0': 0, '1': [max_value]}")
      max_value <- as.numeric(str_extract(item, "\\d+$"))
      # get distance b/w response & answer, scale to max, compare to threshold
      abs(as.numeric(response) - as.numeric(answer)) / max_value < threshold
    })) |>
    mutate(chance = threshold * 2)
  
  numline4afc_trials <- egma |> 
    filter(corpus_trial_type == "number line 4afc") |>
    select(-item_id) |>
    left_join(math_items |> filter(corpus_trial_type == "number line 4afc") |>
                select(item, answer, item_id, corpus_trial_type))
  
  # recombine all of egma
  egma_numberline <- egma |>
    filter(corpus_trial_type != "number line slider", 
           corpus_trial_type != "number line 4afc") |>
    bind_rows(numline4afc_trials) |>
    bind_rows(slider_trials)
  
  return(egma_numberline)
  
}