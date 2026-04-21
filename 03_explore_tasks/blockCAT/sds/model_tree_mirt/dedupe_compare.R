dedupe_compare <- sds_tree |>
  mutate(item_uid = paste(node, item, sep = ".")) |>
  rename(correct = value) |>
  rlevante:::dedupe_items() |>
  rename(item_inst_unsort = item_inst) |>
  # arrange(group, run_id, item_group, trial_index, node) |>
  arrange(group, run_id, item_group) |>
  rlevante:::dedupe_items() |>
  rename(item_inst_sort = item_inst)

dedupe_compare |>
  count(item_uid, item_inst_sort, correct) |> count(item_uid, item_inst_sort) |> count(n)
dedupe_compare |>
  count(item_uid, item_inst_unsort, correct) |> count(item_uid, item_inst_unsort) |> count(n)

dedupe_compare |>
  count(item_uid, item_inst_sort, correct) |>
  group_by(item_uid, item_inst_sort) |>
  summarise(n_trials = sum(n), n_cat = n()) |>
  group_by(n_cat) |>
  summarise(total_resp = sum(n_trials))

dedupe_compare |>
  count(item_uid, item_inst_unsort, correct) |>
  group_by(item_uid, item_inst_unsort) |>
  summarise(n_trials = sum(n), n_cat = n()) |>
  group_by(n_cat) |>
  summarise(total_resp = sum(n_trials))

group_by(item_inst_sort) |>
  filter(n() != 2)
  count(item_inst_unsort == item_inst_sort)
