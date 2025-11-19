all_coefs <- read_rds("02_scoring_outputs/all_coefs.rds")

sds_coefs <- all_coefs |>
  filter(task == "sds", model_set == "multigroup_site") |> #, subset == "all_items") |>
  select(model_set, subset, itemtype, invariance, group, item, a1, d) |>
  pivot_longer(c(a1, d), names_to = "param", values_to = "value") |>
  filter(!(param == "a1" & itemtype == "rasch")) |>
  separate(item, into = c("task", "block", "trial_type")) |>
  mutate(itemtype = itemtype |> as_factor() |> fct_relevel("rasch"),
         invariance = invariance |> as_factor() |> fct_relevel("configural", "scalar"),
         block = block |> fct_recode(dims = "dimensions"))

sds_coefs |>
  filter(param == "d") |>
  # filter(param == "d", !(block %in% c("dimensions", "same"))) |>
  ggplot(aes(y = value, x = block, colour = trial_type)) +
  facet_grid(vars(itemtype, invariance), vars(group)) +
  geom_hline(yintercept = 0, color = "darkgrey", linetype = "dotted") +
  geom_crossbar(aes(ymin = value, ymax = value), position = "dodge", width = 0.5, middle.linewidth = 0.5) +
  scale_color_manual(values = c(rev(ptol_pal()(4)), rev(ptol_pal()(6)))) +
  # scale_color_ptol() +
  scale_y_continuous(limits = ~range(.x, 0)) +
  labs(x = "Block", y = "Difficulty", colour = "Item") +
  theme(panel.border = element_rect())
ggsave("sds_difficulty.png", width = 11, height = 8)

sds_coefs |>
  filter(param == "d", block == "dims") |>
  # filter(param == "d", !(block %in% c("dimensions", "same"))) |>
  ggplot(aes(y = value, x = block, colour = trial_type)) +
  facet_grid(vars(itemtype, invariance), vars(group)) +
  geom_hline(yintercept = 0, color = "darkgrey", linetype = "dotted") +
  geom_crossbar(aes(ymin = value, ymax = value), position = "dodge", width = 0.5, middle.linewidth = 0.5) +
  scale_color_manual(values = rev(ptol_pal()(5))) +
  # scale_color_ptol() +
  scale_y_continuous(limits = ~range(.x, 0)) +
  labs(x = "Block", y = "Difficulty", colour = "Item") +
  theme(panel.border = element_rect())
ggsave("sds_dims.png", width = 7, height = 8)

sds_coefs |>
  filter(param == "a1", !(block %in% c("dimensions", "same"))) |>
  ggplot(aes(y = value, x = block, colour = trial_type)) +
  facet_grid(vars(itemtype, invariance), vars(group)) +
  geom_hline(yintercept = 0, color = "darkgrey", linetype = "dotted") +
  geom_crossbar(aes(ymin = value, ymax = value), position = "dodge", width = 0.5, middle.linewidth = 1) +
  scale_color_manual(values = rev(ptol_pal()(4))) +
  scale_y_continuous(limits = ~range(.x, 0)) +
  labs(x = "Block", y = "Discrimination", colour = "Item") +
  theme(panel.border = element_rect())
ggsave("sds_discrimination.png", width = 8, height = 6)

sds_coefs |>
  filter(itemtype == "2pl", !(block %in% c("dimensions", "same"))) |>
  pivot_wider(names_from = param, values_from = value) |>
  # unite("item", c("block", "trial_type")) |>
  ggplot(aes(x = d, y = a1, colour = trial_type, shape = block)) +
  facet_grid(vars(itemtype, invariance), vars(group)) +
  geom_point(size = 1) +
  # geom_hline(yintercept = 0, color = "darkgrey", linetype = "dotted") +
  # geom_crossbar(aes(ymin = value, ymax = value), position = "dodge", width = 0.5, middle.linewidth = 1) +
  scale_color_manual(values = rev(ptol_pal()(4))) +
  # scale_y_continuous(limits = ~range(.x, 0)) +
  labs(x = "Difficulty", y = "Discrimination", colour = "Item", shape = "Block") +
  theme(panel.border = element_rect())
ggsave("sds_diff_disc.png", width = 9.5, height = 6)

sds_data <- task_data_nested |> filter(item_task == "sds") |> unnest(data)

sds_props <- sds_data |>
  group_by(site, item_uid, item_group, item) |>
  summarise(prop_correct = mean(correct), n = n()) |>
  ungroup() |>
  mutate(param = "prop_correct") |>
  rename(group = site, block = item_group, trial_type = item, value = prop_correct)

sds_props |>
  # filter(!(block %in% c("dimensions", "same"))) |>
  ggplot(aes(y = value, x = block, colour = trial_type)) +
  facet_grid(vars(), vars(group)) +
  geom_hline(yintercept = 0, color = "darkgrey", linetype = "dotted") +
  geom_crossbar(aes(ymin = value, ymax = value), position = "dodge", width = 0.5, middle.linewidth = 1) +
  # scale_color_manual(values = rev(ptol_pal()(4))) +
  scale_color_manual(values = c(rev(ptol_pal()(4)), rev(ptol_pal()(6)))) +
  scale_y_continuous(limits = ~range(.x, 0)) +
  labs(x = "Block", y = "Proportion correct", colour = "Item") +
  theme(panel.border = element_rect())
ggsave("sds_props.png", width = 9, height = 4)


# turn json-ish response/distractors string into character vector
# e.g. "{'0': 'sm-blue-triangle', '1': 'lg-yellow-triangle'}" -> c("sm-blue-triangle", "lg-yellow-triangle")
parse_response <- \(resp) {
  if (is.na(resp) || str_trim(resp) == "") return(character(0))
  
  # convert single quotes to double quotes to parse as JSON
  json_str <- str_replace_all(resp, "'", '"')
  parsed <- fromJSON(json_str, simplifyVector = TRUE)
  parsed |> as.character() |> unname()
}

# 2match: sm-blue-triangle
# 3match/4match: med-green-circle-2-black / med-red-square-2 / med-blue-square-striped / med-red-star
# -2-black / -2 / -striped / ''

# infer dimensions for one stimulus
# e.g. med-red-star / med-red-star-2 / med-red-star-striped /  med-red-star-2-striped
code_stim <- \(stim, grp) {
  
  # extract number if present, other use default "1"
  num <- str_extract(stim, "\\d") |> discard(is.na)
  if (length(num) == 0) num <- "1"
  # extract background color if present, otherwise use default "white"
  bg <- str_extract(stim, "gray|black|striped") |> discard(is.na)
  if (length(bg) == 0) bg <- "white"
  
  # first 3 parts are always size/color/shape, then number and background from above
  att <- set_names(c(stim[1:3], num, bg), c("size", "color", "shape", "number", "background"))
  
  # use only relevant dimensions depending on item group
  # if (grp == "2match") return(att[c("size", "color", "shape")])
  # if (grp %in% c("3match", "4match")) return(att[c("color", "shape", "number", "background")])
}

# split vector of stimuli (e.g. med-green-circle-2-black) into parts and infer parts' dimensions
code_dims <- \(resp, grp) {
  resp |> str_split("-") |> map(\(s) code_stim(s, grp))
}

# given list of character vectors of named dimensions, return which dimensions match for any two items
# e.g. list(c(size = "lg", color = "yellow", shape = "triangle), c(size = "lg", color = "yellow", shape = "square)) -> c("size", "color")
# match_dims <- \(opts) {
#   opts |> transpose() |> map(unlist) |> map(n_distinct) |> keep(\(x) x < length(opts)) |> names() |> sort()
# }

# given list of character vectors of named dimensions,
# return how many pairs of items match on each dimension,
# excluding dimensions that all items match on
match_opts_dims <- \(opts) {
  opts |> transpose() |> map(unlist) |> map(base::table) |> discard(\(x) length(x) == 1) |>
    map(\(x) sum(choose(x, 2))) |> unlist()
}

match_resp_dims <- \(resp, opts_dims) {
  resp_t <- resp |> transpose() |> map(unlist)
  resp_t[names(opts_dims)] |> map(n_distinct) |> keep(\(x) x < length(resp)) |> names() |> sort()
}

# detect which task versions correspond to corpus v1 based on the dimensions prompt starting with "Touch" vs. "Choose"
sds_v1_versions <- sds_data |>
  filter(item_group == "dimensions") |>
  distinct(task_version, item_original) |>
  mutate(prompt = str_extract(item_original, "^[A-z]*")) |>
  distinct(task_version, prompt) |> View()
  filter(prompt == "Touch") |>
  pull(task_version)

sds_data |>
  distinct(site, dataset, task_version, run_id) |>
  mutate(v1 = task_version %in% sds_v1_versions) |>
  count(site, dataset, task_version, v1)
  

sds_data |>
  distinct(task_version, item_group, distractors) |>
  filter(item_group == "2match") |>
  count(item_group, task_version) |>
  filter(n == 1)
  
sds_data |>
  filter(item_group == "2match", task_version == "1.0.12") |> pull(run_id)

sds_data |>
  distinct(task_version, item_group, trial_number) |>
  group_by(task_version, item_group) |>
  summarise(t1 = min(trial_number), t2 = max(trial_number)) |>
  ungroup() |>
  count(item_group, t1, t2) |> filter(item_group == "2match")

sds_versions <- sds_data |>
  group_by(site, dataset, task_version) |>
  nest() |>
  ungroup() |>
  arrange(dataset, task_version)

# sm-red-tri

sds_match <- sds_data |>
  # filter to only 2match/3match/4match
  filter(str_detect(item_group, "match")) |>
  # use positions of "choice1" to infer a trial index grouping choices together
  group_by(run_id, item_group) |>
  arrange(timestamp, .by_group = TRUE) |>
  mutate(trial_index = if_else(item == "choice1", 1, 0)) |>
  mutate(trial_index = cumsum(trial_index)) |>
  ungroup() |>
  # remove rows with any responses that aren't two cards
  filter(str_count(response, ":") == 2) |>
  # remove trials if they have fewer rows than they should for their match group
  mutate(match_k = str_extract(item_group, "^.") |> as.numeric()) |>
  group_by(run_id, item_group, trial_index) |>
  filter(n() == unique(match_k)) |>
  ungroup() |>
  select(run_id, trial_index, item_group, match_k, item, resp = response, opts = distractors, correct, original_correct)

# parse response and options strings into vectors of stimuli
sds_opts <- sds_match |>
  mutate(resp_parsed = resp |> map(parse_response) |> map(sort),
         opts_parsed = opts |> map(parse_response) |> map(sort))

sds_coded <- sds_opts |>
  # code dimension values for each stimulus in response and options
  mutate(resp_coded = map2(resp_parsed, item_group, code_dims),
         opts_coded = map2(opts_parsed, item_group, code_dims))
  # code matching dimensions for response and options
  # mutate(resp_dims = map(resp_coded, match_dims),
  #        opts_dims = map(opts_coded, match_dims))

sds_dims <- sds_coded |>
  mutate(opts_dims = map(opts_coded, match_opts_dims),
         resp_dims = map2(resp_coded, opts_dims, match_resp_dims)) |>
  mutate(n_matches = map_int(opts_dims, sum))

# 3.8% of trials don't match
sds_dims |> count(n_matches == match_k)

# cc <- smc |> mutate(n_matches = map_int(opts_dims, \(o) sum(unlist(o))), dups = map_lgl(opts_dims, \(o) any(o >= 2))) |>
#   select(item_group, match_k, contains("opts"), n_matches, dups) |> distinct()
#   # select(match_k, n_r, n_o) |>
# cc |> count(match_k, n_matches, dups)
# 
# cc |> filter(match_k != n_matches | dups)
# cc |> filter(n_o == 10) |> distinct(opts)
# 
# # given list of character vectors of named dimensions, return which dimensions match are the same for all items
# const_dims <- \(opts) opts |> transpose() |> map(unlist) |> map(n_distinct) |> keep(\(x) x == 1) |> names()
# 
# smc <- sds_match_coded |>
#   mutate(ignore_dims = map(opts_coded, const_dims),
#          resp_coded = map2(resp_coded, ignore_dims, \(d, ig) d[!(d %in% ig)]),
#          opts_coded = map2(opts_coded, ignore_dims, \(d, ig) d[!(d %in% ig)])) |>
#   mutate(resp_dims = map(resp_coded, match_dims),
#          opts_dims = map(opts_coded, match_dims))

# opts <- smc |>
#   select(item_group, match_k, contains("opts"), ignore_dims) |>
#   distinct()
# 
# opts |>
#   mutate(n_ignore = map_int(ignore_dims, length)) |>
#   filter(item_group == "3match", n_ignore == 1) |>
#   slice(10)
#   # count(item_group, ignore_dims)

# mw <- sds_match_coded |>
#   mutate(n_match = map_int(opts_dims, length)) |>
#   filter(match_k != n_match)
# sds_match_coded |> distinct(item_group, opts_dims) |> arrange(item_group) |> deframe()

# collapse by trial
sds_match_trials <- sds_dims |>
  mutate(subtrial_match = map_int(resp_dims, length) > 0) |>
  group_by(run_id, item_group, match_k, n_matches, trial_index, opts_dims) |>
  summarise(across(everything(), list)) |>
  ungroup() |>
  filter(match_k == n_matches)

code_misses <- \(opts_dims, resp_dims, k) {
  trial_dims <- resp_dims |> unlist() |> unique()
  if (n_distinct(trial_dims) == k & all(trial_dims %in% opts_dims)) return(character())
  setdiff(opts_dims, trial_dims)
}

sds_match_misses <- sds_match_trials |>
  mutate(resp_dims = resp_dims |> map(unlist) |> map(unique),
         opts_dims = opts_dims |> map(\(od) od |> keep(\(x) x > 0) |> names())) |>
  mutate(missed_dims = map2(opts_dims, resp_dims, setdiff),
         n_miss = map_int(missed_dims, length)) |>
  # mutate(missed_dims = map2(opts_dims, resp_dims, code_misses)) |>
  mutate(subtrial_correct = map_lgl(subtrial_match, all),
         new_correct = n_miss == 0 & subtrial_correct)

# sds_match_misses <- sds_match_coded |>
#   group_by(run_id, item_group, trial_index, opts_dims) |>
#   summarise(choice = list(choice), choice_dims = list(choice_dims),
#             correct = list(correct), original_correct = list(original_correct)) |>
#   ungroup() |>
#   mutate(missed_dims = map2(opts_dims, resp_dims, setdiff))

sds_items <- sds_data |> select(task_version, item_group, distractors) |> distinct()
sds_opts |> left_join(run_data |> select(run_id, task_version)) |> select(task_version, item_group, opts) |> distinct() |> count(task_version, item_group) |>
  pivot_wider(names_from = item_group, values_from = n)

sds_comp <- sds_match_misses |>
  mutate(correct = map_lgl(correct, all),
         original_correct = map_lgl(original_correct, all))

sds_comp |> count(subtrial_correct, new_correct)
sds_comp |> count(subtrial_correct, n_miss == 0)

sds_comp |> filter(subtrial_correct, n_miss != 0)

sds_comp |> count(correct, new_correct)
sds_comp |> count(trial_original_correct, trial_new_correct)
sds_comp |> count(trial_original_correct, trial_correct)
sds_comp |> count(trial_original_correct, trial_correct, trial_new_correct)

sc <- sds_comp |> filter(trial_correct, !trial_new_correct) |> slice(1)

sds_comp |> filter(item_group == "3match")  |> filter(trial_correct, !trial_new_correct) |> nest(r = -c(item_group, opts, opts_coded))
sds_comp |> filter(item_group == "3match")  |> filter(!trial_correct, trial_new_correct) |> nest(r = -c(item_group, opts))


# TODO:
sds_comp |> filter(item_group == "3match")  |> filter(!trial_correct, trial_new_correct) |> nest(r = -c(item_group, opts)) |> pull(r) |> map(\(x) x |> distinct(opts_coded, opts_dims, resp_coded, resp_dims, missed_dims)) |> map(\(x) slice(x, 1)) |> bind_rows() |> slice(1)

sc <- sds_comp |> filter(trial_correct, !trial_new_correct) |> group_by(item_group, opts_dims, missed_dims) |> slice(1)

# sc <- sds_comp |> filter(trial_original_correct, !trial_new_correct) |> filter(item_group == "4match", map_int(missed_dims, length) == 3) |> slice(1)

misses_summary <- sds_2match_misses |> left_join(run_data |> select(site, run_id)) |>
  mutate(missed_dims = map_chr(missed_dims, \(s) paste(s, collapse = "\n"))) |>
  count(site, missed_dims) |>
  group_by(site) |>
  mutate(prop = n / sum(n)) |>
  arrange(desc(prop)) |>
  mutate(missed_dims = missed_dims |> as_factor() |> fct_recode("(none)" = ""))

ggplot(misses_summary, aes(x = missed_dims, y = prop)) +
  facet_wrap(vars(site)) +
  geom_col() +
  labs(x = "Missed dimensions in SDS 2match", y = "Proportion of trials")
ggsave("sds_2match_errors.png", width = 10, height = 3)

# sds_2match <- sds_data |>
#   filter(item_group == "2match") |>
#   group_by(run_id, item_group) |>
#   arrange(timestamp, .by_group = TRUE) |>
#   # mutate(trial_index = NA_integer_) |>
#   mutate(trial_index = if_else(item == "choice1", 1, 0)) |>
#   mutate(trial_index = cumsum(trial_index)) |>
#   ungroup() |>
#   select(site, run_id, trial_index, item, response, distractors)
# 
# sds_2match_coded <- sds_2match |>
#   mutate(choice = response |> map(parse_response) |> map(code_option),
#          options = distractors |> map(parse_response) |> map(code_option)) |>
#   mutate(choice_dims = map(choice, match_dims),
#          options_dims = map(options, match_dims))
# 
# sds_2match_misses <- sds_2match_coded |>
#   group_by(run_id, trial_index, options_dims) |>
#   summarise(choices = list(choice_dims)) |>
#   ungroup() |>
#   mutate(missed_dims = map2(options_dims, choices, setdiff))
# 
# misses_summary <- sds_2match_misses |> left_join(run_data |> select(site, run_id)) |>
#   mutate(missed_dims = map_chr(missed_dims, \(s) paste(s, collapse = "\n"))) |>
#   count(site, missed_dims) |>
#   group_by(site) |>
#   mutate(prop = n / sum(n)) |>
#   arrange(desc(prop)) |>
#   mutate(missed_dims = missed_dims |> as_factor() |> fct_recode("(none)" = ""))
# 
# ggplot(misses_summary, aes(x = missed_dims, y = prop)) +
#   facet_wrap(vars(site)) +
#   geom_col() +
#   labs(x = "Missed dimensions in SDS 2match", y = "Proportion of trials")
# ggsave("sds_2match_errors.png", width = 10, height = 3)
