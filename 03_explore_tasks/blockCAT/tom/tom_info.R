library(here)
library(rlevante)

source("02_score_data/02_fit_irt/registry_helper.R")

tom_trials <- task_data_nested |>
  filter(item_task == "tom") |>
  unnest(data)

tom_items <- task_data_nested |>
  filter(item_task == "tom") |>
  unnest(data) |>
  distinct(item_uid, item_group, item, chance) |>
  mutate(item_type = str_remove(item, "_.$")) |>
  mutate(story_i = str_extract(item_uid, "(?<=tom_story)[0-9]+") |> as.numeric()) |>
  arrange(story_i) |>
  mutate(story = paste("story", story_i) |> fct_inorder(),
         item_group = item_group |> fct_inorder()) |>
  group_by(item_group) |>
  mutate(stories = paste(unique(story_i), collapse = ", ")) |>
  ungroup() |>
  mutate(group_label = glue("{item_group} ({stories})") |> fct_inorder())

tom_langs <- c("de", "en", "es")
lang_path <- \(lang) glue("02_scoring_outputs/model_registry/tom/by_language/{lang}/tom_rasch_f1.rds")

tom_modrec_lang <- tom_langs |> set_names() |> map(\(lang) read_rds(lang_path(lang)))
tom_mod_lang <- tom_modrec_lang |> map(model_from_record)

tom_modrec_scalar <- read_rds("02_scoring_outputs/model_registry/tom/multigroup_site/all_items/tom_rasch_f1_scalar.rds")
tom_mod_scalar <- model_from_record(tom_modrec_scalar)
tom_mod_group <- extract.mirt(tom_mod_scalar, "groupNames") |> set_names() |>
  map(\(group) extract.group(tom_mod_scalar, group))

tom_mods <- list(tom_mod_lang, tom_mod_group) |> list_flatten()

# mod <- tom_mods$de |> model_from_record()
theta <- seq(-5, 5, by = 0.01)

get_iteminfo_uni <- \(mod, theta) {
  extract.mirt(mod, "itemnames") |>
    map(\(item) {
      it <- extract.item(mod, item)
      tibble(theta = theta) |>
        mutate(item_inst = item,
               info = iteminfo(it, as.matrix(theta)))
    }) |>
    list_rbind() |>
    relocate(item_inst, .before = everything())
}

mod_info <- tom_mods |>
  map(partial(get_iteminfo_uni, theta = theta))

tom_info <- mod_info |> list_rbind(names_to = "subset") |>
  mutate(item_uid = item_inst |> str_remove("-[0-9]+$")) |>
  mutate(model_type = if_else(str_length(subset) == 2,
                              "by_language", "multigroup")) |>
  left_join(tom_items)

# ggplot(tom_info, aes(x = theta, y = info)) +
#   facet_grid(vars(item_group), vars(language)) +
#   geom_line(aes(group = item_uid, colour = item_type))

tom_info_story <- tom_info |>
  group_by(model_type, subset, group_label, item_group, story, story_i, theta) |>
  summarise(info = sum(info), n_items = n()) |>
  ungroup()

tom_info_story |>
  filter(model_type == "by_language") |>
ggplot(aes(x = theta, y = info)) +
  # facet_grid(vars(item_group), vars(language)) +
  facet_grid(vars(subset), vars(group_label)) +
  # facet_wrap(vars(language)) +
  geom_line(aes(group = story, colour = item_group)) +
  scale_colour_brewer(palette = "Spectral", guide = FALSE) +
  labs(x = "Theta", y = "Information (sum for story)")
ggsave("tom_info_language_grouped.png", width = 13, height = 6)

tom_info_story |>
  filter(model_type == "multigroup") |>
  filter(subset == subset[[1]]) |>
ggplot(aes(x = theta, y = info)) +
  # facet_grid(vars(item_group), vars(language)) +
  facet_grid(cols = vars(group_label)) +
  # facet_wrap(vars(language)) +
  geom_line(aes(group = story, colour = item_group)) +
  scale_colour_brewer(palette = "Spectral", guide = FALSE) +
  labs(x = "Theta", y = "Information (sum for story)")
ggsave("tom_info_scalar_grouped.png", width = 13, height = 3)

tom_info_story |>
  filter(model_type == "by_language") |>
ggplot(aes(x = theta, y = info)) +
  facet_wrap(vars(subset)) +
  geom_line(aes(group = story, colour = group_label)) +
  scale_colour_brewer(palette = "Spectral", name = "") +
  labs(x = "Theta", y = "Information (sum for story)")
ggsave("tom_info_language.png", width = 10, height = 3)

tom_info_story |>
  filter(model_type == "multigroup") |>
  filter(subset == subset[[1]]) |>
ggplot(aes(x = theta, y = info)) +
  # facet_wrap(vars(subset)) +
  geom_line(aes(group = story, colour = item_group)) +
  scale_colour_brewer(palette = "Spectral", guide = FALSE) +
  labs(x = "Theta", y = "Information (sum for story)")
ggsave("tom_info_scalar.png", width = 6, height = 4)

tom_info_max <- tom_info_story |>
  arrange(model_type, subset, theta) |>
  group_by(model_type, subset, theta) |>
  filter(info == max(info)) |>
  group_by(model_type, subset) |>
  mutate(story_cid = consecutive_id(story)) |>
  ungroup()

tom_info_max_labels <- tom_info_max |>
  group_by(model_type, subset, item_group, group_label, story_cid, story, story_i) |>
  summarise(theta = mean(theta),
            info = mean(info)) |>
  ungroup()

tom_info_max |>
  filter(model_type == "by_language") |>
ggplot(aes(x = theta, y = info, colour = group_label)) +
  facet_wrap(vars(subset)) +
  geom_line(aes(group = story_cid)) +
  geom_text(data = tom_info_max_labels |> filter(model_type == "by_language"),
            aes(label = story_i), size = 3, vjust = 0, nudge_y = 0.05,
            show.legend = FALSE) +
  scale_colour_brewer(palette = "Spectral", name = "", drop = FALSE) +
  labs(x = "Theta", y = "Information (sum for story)")
ggsave("tom_info_language_max.png", width = 10, height = 3)

tom_info_max |>
  filter(model_type == "multigroup") |>
  filter(subset == subset[[1]]) |>
  ggplot(aes(x = theta, y = info, colour = item_group)) +
  # facet_wrap(vars(subset)) +
  geom_line(aes(group = story_cid)) +
  geom_text(data = tom_info_max_labels |> filter(model_type == "multigroup") |> filter(subset == subset[[1]]),
            aes(label = story_i), size = 3, vjust = 0, show.legend = FALSE) +
  scale_colour_brewer(palette = "Spectral", name = "", drop = FALSE) +
  labs(x = "Theta", y = "Information (sum for story)")
ggsave("tom_info_scalar_max.png", width = 6, height = 4)

# irf <- \(theta, b) {
#   p <- exp(theta - b) / (1 + exp(theta - b))
#   p * (1 - p)
# }
# 
# ex_info <- expand_grid(theta = seq(-5, 5, length.out = 100),
#                        b = seq(-5, 5, by = 1)) |>
#   mutate(info = irf(theta, b))
# 
# ggplot(ex_info, aes(x = theta, y = info)) +
#   geom_line(aes(group = b))
# 
# irf(0, -3)
# irf(0, -2)
# 
# irf(0, 1)
# irf(0, 2)

tom_scores <- tom_mods |> map(scores) |> list_rbind(names_to = "language")

ggplot(tom_scores, aes(x = ability)) +
  facet_wrap(vars(language)) +
  geom_density()

ggplot(tom_scores, aes(x = language, y = ability)) +
  geom_boxplot()

tom_scores |>
  group_by(language) |>
  summarise(ability_mean = mean(ability),
            ability_sd = sd(ability))

get_coef <- \(mod){
  coef(mod, simplify = TRUE, IRTpars = TRUE)$items |>
    as_tibble(rownames = "item_inst") |>
    mutate(across(where(is.double), \(x) round(x, 3))) |>
    mutate(item_uid = item_inst |> str_remove("-[0-9]+$")) |>
    select(-item_inst) |>
    distinct()
}

mod_params <- tom_mods |> map(get_coef)

tom_params <- mod_params |> list_rbind(names_to = "subset") |>
  mutate(model_type = if_else(str_length(subset) == 2,
                              "by_language", "multigroup")) |>
  # mutate(item_uid = item_inst |> str_remove("-[0-9]+$")) |>
  distinct() |>
  left_join(tom_items)

multigroup_coef <- tom_params |>
  filter(model_type == "multigroup") |>
  filter(subset == subset[[1]]) |>
  group_by(item_group, story) |>
  mutate(story_b = mean(b),
         story_n = n()) |>
  group_by(item_group) |>
  mutate(group_b = mean(story_b),
         group_n = n()) |>
  ungroup() |>
  # arrange(desc(item_group), desc(story)) |>
  arrange(group_b, story_b) |>
  mutate(story = story |> fct_inorder(),
         item_group = item_group |> fct_inorder())

# multigroup_coef_summary <- multigroup_coef |>
#   summarise(b = mean(b), .by = c(item_group, story))

ggplot(multigroup_coef, aes(x = b, y = story, colour = item_type)) +
  facet_wrap(vars(item_group), scales = "free_y", space = "free_y", ncol = 1) +
  # geom_point() +
  geom_crossbar(aes(xmin = b, xmax = b), width = 0.5, middle.linewidth = 0.5) +
  # geom_crossbar(data = multigroup_coef_summary,
  #               aes(xmin = b, xmax = b), colour = "black",
  #               width = 0.75, middle.linewidth = 0.5) +
  scale_colour_brewer(palette = "Set1", name = "") +
  labs(x = "Difficulty", y = "") +
  theme(panel.grid.major.y = element_line(colour = "#EBEBEBFF"),
        legend.position = "bottom",
        strip.text = element_text(hjust = 0))
ggsave("tom_coef_scalar.png", width = 6.5, height = 8)


tom_param_info <- tom_params |>
  select(model_type, subset, item_uid, difficulty = b, g) |>
  left_join(tom_info |> select(model_type, subset, item_group, story, item_uid, theta, info))

ex_thetas <- seq(-1, 1, by = 1)
tom_param_info |>
  # filter(theta == 0) |>
  filter(theta %in% ex_thetas) |>
  mutate(theta_label = glue("θ = {theta}")) |>
  ggplot(aes(x = difficulty, y = info, colour = factor(g))) +
  facet_grid(vars(theta_label), vars(subset)) +
  geom_point() +
  # geom_line(aes(group = factor(g))) +
  geom_smooth(method = "gam") +
  scale_colour_brewer(palette = "Set1") +
  labs(x = "Item difficulty", y = "Item information",
       colour = "Chance")

tom_param_info_story <- tom_param_info |>
  group_by(language, item_group, story, theta) |>
  summarise(info = sum(info),
            mean_difficulty = mean(difficulty)) |>
  ungroup()

tom_param_info_story |>
  filter(theta %in% ex_thetas) |>
  mutate(theta_label = glue("θ = {theta}")) |>
ggplot(aes(x = mean_difficulty, y = info)) +
  facet_grid(vars(theta_label), vars(language)) +
  geom_point()
  # geom_line(aes(y = info, group = story))

tom_story_times <- tom_trials |>
  left_join(tom_items) |>
  arrange(run_id, timestamp) |>
  group_by(site, dataset, run_id, item_group, story) |>
  summarise(trials = n(),
            start = min(timestamp), end = max(timestamp)) |>
  ungroup() |>
  mutate(diff = difftime(end, start, units = "mins")) #|>
  # left_join(runs |> select(run_id, is_cat = adaptive, completed)) |>
  # filter(trials > 3, completed, diff < 120)

tom_story_times |>
  group_by(item_group) |>
  summarise(median_diff = median(diff)) |>
  ungroup() |>
  arrange(median_diff)
  # arrange(story)
