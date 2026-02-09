task_data_nested <- read_rds(here("01_fetched_data/task_data_nested.rds"))

hf <- task_data_nested |>
  filter(item_task == "hf") |>
  unnest(data) |>
  filter(!is.na(rt_numeric), rt_numeric > 200)

hf_rt <- hf |>
  mutate(rt_s = rt_numeric / 1000) |>
  group_by(dataset, run_id, item_group) |>
  summarise(rt_mean = mean(rt_s),
            rt_se = sd(rt_s) / sqrt(n()),
            rt_median = median(rt_s),
            under_1 = mean(rt_s <= 1),
            under_2 = mean(rt_s <= 2),
            under_3 = mean(rt_s <= 3)) |>
  ungroup()

hf_rt_props <- hf_rt |> select(dataset, run_id, item_group, contains("under")) |>
  pivot_longer(contains("under"), names_to = "cutoff", values_to = "prop") |>
  mutate(cutoff = cutoff |> str_remove("under_") |> paste("second"))

scores <- read_rds(here("02_scoring_outputs/scores/registry_scores.rds"))

hf_scores <- scores |>
  filter(item_task == "hf") |>
  select(run_id, ability = metric_value, ability_se = metric_se)

hf_compare <- inner_join(hf_rt_props, hf_scores)

# at what ability do prop_runs have prop_responses faster than cutoff
prop_responses <- 0.5
prop_runs <- 0.5
hf_compare |>
  filter(item_group == "heartsflowers")

ggplot(hf_compare |> filter(item_group == "heartsflowers"), aes(x = ability, y = prop)) +
  # facet_grid(vars(dataset), vars(cutoff)) +
  facet_wrap(vars(cutoff), labeller = label_both) +
  # geom_hline(aes(yintercept = rt), data = rt_cutoffs) +
  geom_point() +
  # geom_smooth() +
  labs(x = "Ability", y = "Proportion of mixed block responses faster than cutoff")

rt_cutoffs <- tibble(rt = c(1, 2, 3))

rt_compare_m <- inner_join(hf_rt, hf_scores) |> filter(item_group == "heartsflowers")

rt_vals <- c(0.2, 0.5, 1, 2, 3, 4, 10, 20, 25)
ggplot(rt_compare_m, aes(x = ability, y = log(rt_median))) +
  # facet_grid(vars(dataset), vars(cutoff)) +
  # facet_wrap(vars(cutoff), labeller = label_both) +
  # geom_hline(aes(yintercept = rt), colour = "lightgrey", data = rt_cutoffs) +
  # geom_point() +
  geom_point(alpha = 0.5, size = 0.5) +
  scale_y_continuous(breaks = log(rt_vals), labels = rt_vals) +
  geom_smooth(method = "lm") +
  labs(x = "Ability", y = "Median response time in mixed block (seconds)") +
  theme(panel.grid.major.y = element_line(color = "#EBEBEBFF"))
ggsave("hf_rt_medians.png", width = 10, height = 6)

hf_trial_compare <- hf |>
  filter(item_group == "heartsflowers") |>
  select(run_id, correct, original_correct, rt_numeric) |>
  inner_join(hf_scores) |>
  mutate(rt_s = rt_numeric / 1000)

ggplot(hf_trial_compare, aes(x = ability, y = log(rt_s))) +
  # facet_grid(vars(dataset), vars(cutoff)) +
  # facet_wrap(vars(cutoff), labeller = label_both) +
  # geom_hline(aes(yintercept = rt), colour = "lightgrey", data = rt_cutoffs) +
  # geom_hline(aes(yintercept = log(rt)), colour = "lightgrey", data = rt_cutoffs) +
  geom_point(alpha = 0.5, size = 0.5) +
  scale_y_continuous(breaks = log(rt_vals), labels = rt_vals) +
  labs(x = "Ability", y = "Trial-level response time in mixed block (seconds)") +
  theme(panel.grid.major.y = element_line(color = "#EBEBEBFF"))
  # geom_smooth()
ggsave("hf_rt_trials.png", width = 10, height = 6)

hf_trial_compare |>
  arrange(desc(ability)) |>
  mutate(ability_bin = cut(ability, breaks = c(-6, -2, 1, 4))) |>
  group_by(ability_bin) |>
  summarise(under_1 = mean(rt_s <= 1),
            under_2 = mean(rt_s <= 2),
            under_3 = mean(rt_s <= 3))
# count(ability > 2, rt_s < 1)
