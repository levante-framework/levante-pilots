library(tidyverse)
library(jsonlite)

task_data_nested <- read_rds(here("01_fetched_data/task_data_nested.rds"))
sds_data <- task_data_nested |> filter(item_task == "sds") |> unnest(data) |>
  filter(!str_detect(response, "mittel|rote|gelb|blau|grün")) |>
  filter(!(site == "pilot_western_ca" & timestamp < "2025-02-21"))

sds_indexed <- sds_data |>
  # filter to only 2match/3match/4match
  filter(str_detect(item_group, "match")) |>
  mutate(different = str_detect(item_original, "different")) |>
  group_by(run_id, item_group) |>
  arrange(timestamp, .by_group = TRUE) |>
  # use positions of "choice1" to infer a trial index grouping choices together
  mutate(trial_index = if_else(item == "choice1", 1, 0)) |>
  mutate(trial_index = cumsum(trial_index)) |>
  # use positions of "different" prompt to infer trial index
  mutate(trial_index_s = as.numeric(!different)) |>
  mutate(trial_index_s = cumsum(trial_index_s)) |>
  ungroup()

# sds_indexed |> filter(trial_index != trial_index_s) |> View()

sds_match <- sds_indexed |>
  # remove blocks that have any mis-indexed trials
  group_by(run_id, item_group) |>
  filter(all(trial_index == trial_index_s)) |>
  # remove trials if they have fewer (or too many) rows than they should
  # e.g. only 2 rows for 3match
  mutate(match_k = str_extract(item_group, "^.") |> as.numeric()) |>
  group_by(run_id, item_group, trial_index) |>
  filter(n() == unique(match_k)) |>
  # remove trials that don't have consistent response options for every response
  filter(n_distinct(distractors) == 1) |>
  ungroup() |>
  # remove rows with any response that isn't two cards
  # filter(str_count(response, ":") == 2) |>
  select(run_id, site, task_version, trial_index, item_group, match_k, trial_id,
         item, resp = response, opts = distractors, correct, original_correct)

# sds_match |> filter(trial_index != trial_index_s) |> View()
# sds_match |> count(site, task_version, trial_index == trial_index_s) |> count(site, task_version) |> View()
# 
# sds_match |> group_by(site, task_version, run_id, item_group, trial_index) |>
#   summarise(n_subtrials = n(), n_opts = n_distinct(opts)) |>
#   ungroup() |>
#   filter(n_opts != 1)
  # count(site, task_version, n_opts) |> View()
# filter(n_opts != 1)
# filter(item_group == "4match", n_opts == 4)


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

# # detect which task versions correspond to corpus v1 based on the dimensions prompt starting with "Touch" vs. "Choose"
# sds_v1_versions <- sds_data |>
#   filter(item_group == "dimensions") |>
#   distinct(task_version, item_original) |>
#   mutate(prompt = str_extract(item_original, "^[A-z]*")) |>
#   distinct(task_version, prompt) |>
#   filter(prompt == "Touch") |>
#   pull(task_version)

# sds_data |>
#   distinct(site, dataset, task_version, run_id) |>
#   mutate(v1 = task_version %in% sds_v1_versions) |>
#   count(site, dataset, task_version, v1)
# 
# sds_data |>
#   distinct(task_version, item_group, distractors) |>
#   filter(item_group == "2match") |>
#   count(item_group, task_version) |>
#   filter(n == 1)
#   
# sds_data |>
#   filter(item_group == "2match", task_version == "1.0.12") |> pull(run_id)
# 
# sds_data |>
#   distinct(task_version, item_group, trial_number) |>
#   group_by(task_version, item_group) |>
#   summarise(t1 = min(trial_number), t2 = max(trial_number)) |>
#   ungroup() |>
#   count(item_group, t1, t2) |> filter(item_group == "2match")
# 
# sds_versions <- sds_data |>
#   group_by(site, dataset, task_version) |>
#   nest() |>
#   ungroup() |>
#   arrange(dataset, task_version)

# sm-red-tri

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

# 2.5% of trials don't match
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

sds_correct <- sds_dims |>
  mutate(subtrial_match = map_int(resp_dims, length) > 0) |>
  select(run_id, item_group, trial_index, trial_id, resp, subtrial_match) |>
  nest(trials = c(trial_id, resp, subtrial_match)) |>
  mutate(new = map(trials, \(tr) map_lgl(1:nrow(tr), \(i) i == 1 | !(tr$resp[i] %in% tr$resp[1:(i-1)]))),
         correct = map2(trials, new, \(tr, ne) tr |> mutate(new = ne, correct = subtrial_match & new))) |>
  select(-new, -trials) |>
  unnest(correct) |>
  select(run_id, trial_id, correct)

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
  mutate(resp_dims_flat = resp_dims |> map(unlist) |> map(unique),
         opts_dims_flat = opts_dims |> map(\(od) od |> keep(\(x) x > 0) |> names())) |>
  mutate(missed_dims = map2(opts_dims_flat, resp_dims_flat, setdiff),
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

# sds_items <- sds_data |> select(task_version, item_group, distractors) |> distinct()
# sds_opts |> left_join(run_data |> select(run_id, task_version)) |> select(task_version, item_group, opts) |> distinct() |> count(task_version, item_group) |>
#   pivot_wider(names_from = item_group, values_from = n)

sds_comp <- sds_match_misses |>
  mutate(correct = map_lgl(correct, all),
         original_correct = map_lgl(original_correct, all))

# sds_comp |> count(subtrial_correct, new_correct)
sds_comp |> count(subtrial_correct, n_miss == 0)

# happens when two cards match in more than one dimension, so kid can not miss
# any dimensions but still have a response where nothing matches
sds_comp |> filter(!subtrial_correct, n_miss == 0)

# happens when kid picks the same pair multiple times or doesn't pick anything
sds_comp |> filter(subtrial_correct, n_miss != 0)


sds_comp |> filter(correct, !new_correct) |> slice(1)

sds_comp |> count(correct, new_correct)
sds_comp |> count(original_correct, new_correct)
sds_comp |> count(original_correct, correct)
sds_comp |> count(original_correct, correct, new_correct)

sc <- sds_comp |> filter(correct, !new_correct) |> slice(1)

sds_comp |> filter(item_group == "3match")  |> filter(trial_correct, !trial_new_correct) |> nest(r = -c(item_group, opts, opts_coded))
sds_comp |> filter(item_group == "3match")  |> filter(!trial_correct, trial_new_correct) |> nest(r = -c(item_group, opts))


# TODO:
sds_comp |> filter(item_group == "3match")  |> filter(!trial_correct, trial_new_correct) |> nest(r = -c(item_group, opts)) |> pull(r) |> map(\(x) x |> distinct(opts_coded, opts_dims, resp_coded, resp_dims, missed_dims)) |> map(\(x) slice(x, 1)) |> bind_rows() |> slice(1)

sc <- sds_comp |> filter(trial_correct, !trial_new_correct) |> group_by(item_group, opts_dims, missed_dims) |> slice(1)

# sc <- sds_comp |> filter(trial_original_correct, !trial_new_correct) |> filter(item_group == "4match", map_int(missed_dims, length) == 3) |> slice(1)

misses_summary <- sds_match_misses |>
  select(run_id, item_group, trial_index, missed_dims, n_miss, subtrial_correct, new_correct) |>
  left_join(run_data |> select(site, run_id)) |>
  mutate(missed_dims = map_chr(missed_dims, \(s) paste(s, collapse = ", "))) |>
  count(site, item_group, n_miss, missed_dims) |>
  group_by(site, item_group) |>
  mutate(prop = n / sum(n)) |>
  ungroup() |>
  arrange(n_miss, desc(prop)) |>
  mutate(missed_dims = missed_dims |> as_factor() |> fct_recode("(none)" = "")) |>
  mutate(dim_label = missed_dims |>
           str_replace("color", "col") |>
           str_replace("shape", "shp") |>
           str_replace("size", "sz") |>
           str_replace("number", "num") |>
           str_replace("background", "bg"))

misses_summary |> #filter(item_group == "3match") |>
ggplot(aes(x = n_miss, y = prop, fill = missed_dims)) +
  # facet_grid(vars(subtrial_correct), vars(site)) +
  facet_grid(vars(item_group), vars(site), scales = "free_y") +
  geom_col() +
  geom_text(aes(label = dim_label), color = "white", size = 2,
            position = position_stack(vjust = 0.5)) +
  scale_fill_viridis_d() +
  scale_y_continuous(expand = expansion()) +
  guides(fill = guide_legend(ncol = 1)) +
  labs(x = "Number of missed dimensions", y = "Proportion of trials",
       fill = "Missed dimensions") +
  theme(legend.key.height = unit(0.5, "lines"),
        legend.key.width = unit(1, "lines"))
ggsave("plots/sds_match_errors.png", width = 11, height = 6)
