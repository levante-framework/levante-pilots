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
                                names = c("answer", "max_value"),
                                cols_remove = FALSE) |>
    mutate(answer = answer |> stringr::str_replace("^0", "0."),
           across(c(answer, max_value), as.numeric),
           correct = (abs(as.numeric(response) - answer) / max_value < threshold)) |>
    select(-c("answer", "max_value"))
  df |>
    filter(item_group != "slider") |>
    bind_rows(slider_trials)
}
