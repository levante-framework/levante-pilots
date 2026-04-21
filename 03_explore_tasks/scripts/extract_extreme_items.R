item_means <- task_data_nested |>
  filter(item_task == "vocab") |>
  unnest(data) |>
  group_by(item_task, item_group, item, language) |>
  summarise(item_mean = mean(correct),
            item_n = n()) |>
  arrange(language, item_mean, item)

trial_data_subset |>
  filter(item_task == "vocab") |>
  group_by(item_task, item_group, item, language) |>
  summarise(item_mean = mean(correct),
            item_n = n()) |>
  arrange(language, item_mean, item)

item_means |>
  filter(item_mean == 1 | item_mean == 0) |>
  write_csv("vocab_limit.csv")

trial_data_subset |>
  filter(item_task == "vocab") |>
  group_by(item_task, item_group, item, language) |>
  summarise(item_mean = mean(correct),
            item_n = n()) |>
  arrange(language, item_mean, item) |> filter(item == "squash")
  filter(item_mean == 1 | item_mean == 0)
