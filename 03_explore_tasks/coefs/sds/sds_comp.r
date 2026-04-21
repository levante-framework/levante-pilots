sds_comp <- trial_data_coded |> filter(item_task == "sds") |>
  left_join(run_data |> select(run_id, task_version)) |>
  mutate(matched = correct == original_correct)
# count(task_version, item_group, matched)

# sds_comp |>
#   pivot_wider(names_from = matched, values_from = n) |>
#   View()

sds_comp |>
  filter(task_version == "1.0.0-beta.35") |>
  filter(!matched) |>
  View()

sds_comp |>
  filter(item_group == "3match", run_id == "2wx4DFFCf2LS1lbdPMh5") |>
  arrange(trial_number) |>
  select(item, correct, original_correct, response, answer, distractors) |>
  View()





sds_compare <- trial_data_coded |>
  filter(item_task == "sds") |>
  mutate(correct_match = correct == original_correct) |>
  left_join(run_data |> select(run_id, task_version))

sds_mismatch <- sds_compare |>
  group_by(run_id, item_group) |>
  filter(any(!correct_match)) |>
  ungroup() |>
  select(dataset, run_id, trial_id, task_version, item_group, trial_index, item, item_original, distractors, response, contains("correct"))

write_csv(sds_mismatch, "sds_mismatch.csv")
