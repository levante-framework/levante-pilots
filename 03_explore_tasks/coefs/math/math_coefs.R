math_coefs <- all_coefs |>
  filter(task == "math", model_set == "multigroup_site", itemtype == "rasch") |>
  mutate(#subset = subset |> fct_rev(),
         group = if_else(invariance == "scalar", "(all)", group),
         label = if_else(invariance == "scalar", glue("scalar – {str_replace(subset, '_', ' ')}"), glue("{invariance} ({group})"))) |>
  arrange(invariance, desc(subset)) |>
  mutate(label = fct_inorder(label)) |>
  mutate(difficulty = -d / a1) |>
  distinct() |>
  mutate(item = item |> str_remove("math_"),
         item_group = str_extract(item, "^.*?(?=_)") |> fct_inorder())

# ggplot(math_coefs, aes(x = d)) +
#   facet_wrap(vars(invariance, subset), labeller = label_both) +
#   geom_density(aes(colour = group)) +
#   scale_colour_ptol()
# ggsave("math_coefs_density.png", width = 10, height = 3)

# ggplot(math_coefs, aes(x = d)) +
#   facet_wrap(vars(invariance, subset), labeller = label_both) +
#   geom_histogram(aes(fill = group)) +
#   scale_fill_ptol()
# ggsave("math_coefs_hist.png", width = 10, height = 3)

prior <- tibble(difficulty = rnorm(100000, 0, 3), prior = "N(0, 3)")
ggplot(math_coefs, aes(x = difficulty)) +
  facet_wrap(vars(label), nrow = 1) +
  # facet_grid(vars(invariance), vars(subset, group),) +
  geom_histogram(aes(fill = group, y = after_stat(density)), binwidth = 0.5) +
  geom_density(aes(colour = prior), data = prior) +
  # scale_fill_ptol(guide = FALSE) +
  scale_fill_manual(guide = FALSE, values = c("darkgrey", site_pal)) +
  scale_colour_manual(values = "grey") +
  xlim(-8, 8) +
  labs(x = "Difficulty (larger = more difficult)", y = "", colour = "") +
  theme(strip.text = element_text(face = "plain", size = 8),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        legend.position = "bottom")
ggsave("math_coefs_hist.png", width = 11, height = 3.5)

# ggplot(math_coefs, aes(x = difficulty)) +
#   facet_grid(vars(item_group), vars(label)) +
#   # facet_grid(vars(invariance), vars(subset, group),) +
#   geom_histogram(aes(fill = group, y = after_stat(density))) +
#   geom_density(aes(colour = prior), data = prior) +
#   # scale_fill_ptol(guide = FALSE) +
#   scale_fill_manual(guide = FALSE, values = c("darkgrey", site_pal)) +
#   scale_colour_manual(values = "grey") +
#   xlim(-8, 8) +
#   labs(x = "Difficulty (larger = more difficult)", y = "", colour = "") +
#   theme(strip.text = element_text(face = "plain", size = 8),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.line.y = element_blank(),
#         legend.position = "bottom")
# ggsave("math_coefs_hist_grouped.png", width = 11, height = 11)



math_coefs_overlap <- math_coefs |>
  filter(subset == "overlap_items") |>
  mutate(item = item |> str_remove("math_"),
         item_group = str_extract(item, "^.*?(?=_)") |> fct_inorder()) |>
  arrange(group, desc(difficulty)) |>
  mutate(item = item |> fct_inorder())

ggplot(math_coefs_overlap, aes(x = difficulty, y = item)) +
  facet_grid(vars(item_group), vars(group), scales = "free_y", space = "free_y") +
  # geom_point(aes(colour = group)) +
  geom_crossbar(aes(colour = group, xmin = difficulty, xmax = difficulty), orientation = "y", position = "dodge", width = 0.5, middle.linewidth = 1) +
  scale_colour_manual(guide = FALSE, values = c("darkgrey", site_pal)) +
  labs(x = "Difficulty (larger = more difficult)", y = "", colour = "") +
  theme(panel.grid.major.y = element_line(colour = "#EBEBEBFF"))
ggsave("math_coefs_groups.png", width = 11, height = 20)

ggplot(math_coefs_overlap, aes(x = difficulty, y = item)) +
  # facet_grid(vars(item_group), vars(group), scales = "free_y", space = "free_y") +
  facet_grid(vars(item_group), scales = "free_y", space = "free_y") +
  # geom_point(aes(colour = group)) +
  geom_crossbar(aes(colour = group, xmin = difficulty, xmax = difficulty), middle.linewidth = 1) +
  # geom_crossbar(aes(colour = group, xmin = difficulty, xmax = difficulty), orientation = "y", position = "dodge", width = 0.5, middle.linewidth = 1) +
  scale_colour_manual(values = c("darkgrey", site_pal)) +
  labs(x = "Difficulty (larger = more difficult)", y = "", colour = "") +
  theme(panel.grid.major.y = element_line(colour = "#EBEBEBFF"),
        legend.position = "bottom")
ggsave("math_coefs_items.png", width = 8, height = 21)
