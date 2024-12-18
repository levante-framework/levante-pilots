# de-dictify distractors: extract values after each colon and space, up to comma
dedict <- \(s) {
  s |> str_extract_all("(?<=: )([^,]+)") |>
    map_chr(partial(paste, collapse = ",")) |>
    str_remove_all("[{}']")
}

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
    df |>
      mutate(corpus_trial_type = str_replace(assessment_stage, " stimulus",""))
  }
  
  return(df)
}