sds_data <- trials_sds |>
  filter(.data$item_task == "sds" & stringr::str_detect(.data$item_group, "match")) |>
  filter(!stringr::str_detect(.data$response, "mittel|rote|gelb|blau|gr\u00FCn")) |>
  filter(!(.data$dataset == "pilot_western_ca_main" & .data$timestamp < "2025-02-21"))

sds_indexed <- sds_data |>
  mutate(different = stringr::str_detect(.data$item_original, "different")) |>
  group_by(.data$run_id, .data$item_group) |>
  arrange(.data$timestamp, .by_group = TRUE) |>
  # use positions of "choice1" to infer a trial index grouping choices together
  mutate(trial_index = if_else(.data$item == "choice1", 1, 0)) |>
  mutate(trial_index = cumsum(.data$trial_index)) |>
  # use positions of "different" prompt to infer trial index
  mutate(trial_index_s = as.numeric(!.data$different)) |>
  mutate(trial_index_s = cumsum(.data$trial_index_s)) |>
  ungroup()

sds_match <- sds_indexed |>
  # remove blocks that have any mis-indexed trials
  filter(trial_index != 0) |>
  group_by(.data$run_id, .data$item_group) |>
  filter(all(.data$trial_index == .data$trial_index_s)) |>
  # remove trials if they have any missing or invalid responses, or too few/many rows
  # e.g. only 2 rows for 3match
  mutate(match_k = stringr::str_extract(.data$item_group, "^.") |> as.numeric()) |>
  group_by(.data$run_id, .data$item_group, .data$trial_index) |>
  # filter(!any(response == "{}")) |>
  filter_out(any(response == "{}")) |>
  filter_out(any(str_count(response, ":") != 2)) |>
  filter(n() == unique(.data$match_k)) |>
  # remove trials that don't have consistent response options for every response
  filter(n_distinct(.data$distractors) == 1) |>
  # recreate choice number
  mutate(choice_i = 1:n()) |>
  ungroup() |>
  select("run_id", "trial_index", "item_group", "match_k", "choice_i", "trial_id",
         "item", resp = "response", opts = "distractors", "correct", "original_correct")

# parse response and options strings into vectors of stimuli
sds_opts <- sds_match |>
  mutate(resp_parsed = .data$resp |> purrr::map(rlevante:::parse_response) |> purrr::map(sort),
         opts_parsed = .data$opts |> purrr::map(rlevante:::parse_response) |> purrr::map(sort))

# code dimension values for each stimulus in response and options
sds_coded <- sds_opts |>
  mutate(resp_coded = purrr::map2(.data$resp_parsed, .data$item_group, rlevante:::code_dims),
         opts_coded = purrr::map2(.data$opts_parsed, .data$item_group, rlevante:::code_dims))

sds_dims <- sds_coded |>
  mutate(opts_dims = purrr::map(.data$opts_coded, rlevante:::match_opts_dims),
         resp_dims = purrr::map2(.data$resp_coded, .data$opts_dims, rlevante:::match_resp_dims)) |>
  mutate(n_matches = purrr::map_int(.data$opts_dims, sum))
# mutate(opts_dims_str = purrr::map_chr(.data$opts_dims, \(od) {
#   dims <- od[od > 0]
#   set_names(dims, recode_values(names(dims),
#                                 "size" ~ "sz",
#                                 "shape" ~ "shp",
#                                 "color" ~ "col",
#                                 "number" ~ "num",
#                                 "background" ~ "bg")) |>
#   imap_chr(\(d, dn) paste0(dn, d)) |> paste(collapse = "_")
# }))

stim_code <- tribble(
  ~from, ~to,
  "sm", "S",
  "med", "M",
  "lg", "L",
  "yellow", "y",
  "blue", "b",
  "red", "r",
  "green", "g",
  "triangle", "^",
  "circle", "o",
  "square", "=",
  "star", "*",
  "white", ")",
  "gray", "}",
  "black", "]",
  "striped", "#"
)
encode_opts <- \(opts) {
  opts |>
    map(\(o) replace_values(o, from = stim_code$from, to = stim_code$to)) |>
    map(\(o) paste(o, collapse = "")) |>
    paste(collapse = "-")
}

sds_correct <- sds_dims |>
  mutate(opts_str = map_chr(opts_coded, encode_opts),
         resp_norm = map_chr(resp_parsed, \(s) paste(s, collapse = " "))) |>
  mutate(subtrial_match = purrr::map_int(.data$resp_dims, length) > 0) |>
  select("run_id", "item_group", "match_k", "n_matches", "opts_str", "trial_index",
         "trial_id", "choice_i", "resp_norm", "subtrial_match") |>
  tidyr::nest(trials = c("trial_id", "choice_i", "resp_norm", "subtrial_match")) |>
  mutate(new = purrr::map(.data$trials, \(tr) purrr::map_lgl(1:nrow(tr), \(i) i == 1 | !(tr$resp_norm[i] %in% tr$resp_norm[1:(i-1)]))),
         correct = purrr::map2(.data$trials, .data$new, \(tr, ne) tr |> mutate(new = ne, correct = .data$subtrial_match & new))) |>
  select(-"new", -"trials") |>
  tidyr::unnest("correct") |>
  filter(match_k == n_matches)

# sds_correct_map <- sds_correct |>
#   mutate(item_uid = as.character(glue("sds_{item_group}_{opts_str}_choice{choice_i}"))) |>
#   select("trial_id", "trial_index", "item_uid", "correct")
# sds_trials_bychoice <- sds_data |>
#   select(dataset, language, run_id, trial_id, item_group, chance) |>
#   # select(-c("correct")) |>
#   inner_join(sds_correct_map) |>
#   mutate(item_task = "sds_bychoice")

sds_correct_itemized <- sds_correct |>
  group_by(run_id, match_k, trial_index) |>
  mutate(prev_matches = lag(cumsum(subtrial_match & new), default = 0),
         prev_nonmatches = lag(cumsum(!subtrial_match & new), default = 0)) |>
  ungroup() |>
  mutate(n_pairs = n_matches * (n_matches + 1) / 2,        # n all possible pairs
         n_nonmatches = n_pairs - n_matches,               # n non-matching pairs
         chance_match = n_matches / n_pairs,               # chance of match
         chance_newM = 1 - prev_matches / n_matches,       # chance of new given match
         chance_newN = 1 - prev_nonmatches / n_nonmatches, # chance of new given non-match
         chance_new = 1 - (prev_matches + prev_nonmatches) / n_pairs, # chance of new marginalized over match
         chance_correct = (n_matches - prev_matches) / n_pairs,
         item_choice = paste0("choice", choice_i),
         item_status = paste0(prev_matches, "m", prev_nonmatches, "n"),
         item_task = "sds") |>
  select(run_id, "trial_id", "trial_index", match = "subtrial_match", "new", "correct", starts_with("chance"),
         "item_task", "item_group", item_stimulus = opts_str, "item_choice", "item_status")

sds_trials <- sds_data |>
  select(dataset, run_id, trial_id, chance_original = chance) |>
  mutate(chance_original = chance_original |> replace_na(0)) |>
  inner_join(sds_correct_itemized)

sds_trials |>
  select(group = dataset, run_id, trial_index, item_group, item_stimulus,
         item_choice, item_status, match, new,
         chance_match, chance_newM, chance_newN, chance_new) |>
  write_rds("data/trials_sds_coded.rds", compres = "gz")

# sds_trials_collapsed <- sds_correct |>
#   summarise(correct = sum(correct),
#             .by = c(run_id, item_group, trial_index, opts_str)) |>
#   mutate(item_task = "sds", chance = 0) |> # TODO: guessing for poly?
#   left_join(sds_data |> distinct(run_id, dataset, language)) |>
#   select(dataset, language, "run_id", "trial_index", "correct", "item_task", "item_group",
#          item_stimulus = opts_str, "chance")
