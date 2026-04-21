mg_coefs <- best_coefs |>
  filter(task == "mg") |>
  distinct(task, item, difficulty) |>
  separate_wider_delim(cols = item, names = c("mg", "direction", "grid", "length"), delim = "_", cols_remove = FALSE) |>
  mutate(direction = direction |> fct_rev(), length = length |> fct_rev())

ggplot(mg_coefs, aes(x = difficulty, y = length, colour = grid)) +
  facet_wrap(vars(direction)) +
  geom_crossbar(aes(xmin = difficulty, xmax = difficulty), middle.linewidth = 0.5, width = 0.8) +
  scale_colour_ptol() +
  labs(x = "Difficulty", y = "Sequence length", colour = "Grid type") +
  theme(panel.grid.major.y = element_line(colour = "#EBEBEBFF"),
        legend.position = "top")
ggsave("mg_coefs.png", width = 7, height = 4)

mg_mod_rec <- mods_best |>
  filter(task == "mg") |>
  pull(mod_rec) |> pluck(1)

mg_mod <- model_from_record(mg_mod_rec)

mg_item_counts_correct <- mg_mod_rec@data |>
  colSums(na.rm = TRUE) |> enframe(name = "item", value = "n_correct")

mg_data <- mg_mod_rec@data
mg_item_counts_trial <- matrix(as.numeric(!is.na(mg_data)), ncol = ncol(mg_data), dimnames = list(NULL, colnames(mg_data))) |>
  colSums() |> enframe(name = "item", value = "n_trials")

mg_item_counts <- mg_item_counts_trial |> left_join(mg_item_counts_correct) |>
  mutate(item = item |> str_remove("-[0-9]+$")) |>
  group_by(item) |>
  summarise(n_correct = sum(n_correct), n_trials = sum(n_trials)) |>
  mutate(prop = n_correct / n_trials)

mg_compare <- mg_coefs |> left_join(mg_item_counts)

ggplot(mg_compare, aes(x = prop, y = difficulty, colour = grid)) +
  facet_wrap(vars(direction)) +
  geom_point() +
  scale_colour_ptol()
