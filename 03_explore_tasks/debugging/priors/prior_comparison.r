all_coefs <- read_rds("02_scoring_outputs/all_coefs.rds")
all_coefs_noprior <- read_rds("02_scoring_outputs/all_coefs_noprior.rds")

coef_comp <- full_join(
  all_coefs |> select(task, group, model_set, subset, itemtype, invariance, item, a1_prior = a1, d_prior = d),
  all_coefs_noprior |> select(task, group, model_set, subset, itemtype, invariance, item, a1_noprior = a1, d_noprior = d),
  # by = c("task", "subset", "itemtype", "nfact", "file", "item")
)
# 
# anti_join(
#   all_coefs |> filter(model_set == "by_language") |> rename(a1_prior = a1, d_prior = d) |> distinct(item),
#   all_coefs_noprior |> filter(model_set == "by_language") |> rename(a1_noprior = a1, d_noprior = d) |> distinct(item),
# )
# 
# anti_join(
#   all_coefs_noprior |> filter(model_set == "by_language") |> rename(a1_noprior = a1, d_noprior = d) |> distinct(item),
#   all_coefs |> filter(model_set == "by_language") |> rename(a1_prior = a1, d_prior = d) |> distinct(item)
# )


coef_comp_tidy <- coef_comp |>
  pivot_longer(cols = contains("prior"), names_to = c("param", "prior"), names_sep = "_",
               values_to = "value") |>
  filter(value > -100) |>
  pivot_wider(names_from = prior, values_from = value) |>
  mutate(itemtype = itemtype |> fct_relevel("rasch"),
         param = param |> fct_relevel("d"),
         subset = subset |> fct_relevel("es"),
         group = group |> fct_relevel("pilot_uniandes_co")) |>
  filter(!(itemtype == "rasch" & param == "a1"))

tasks <- unique(coef_comp_tidy$task)
task_pal <- ptol_pal()(length(tasks)) |> set_names(tasks)

ggplot(coef_comp_tidy |> filter(model_set == "by_language"), aes(x = noprior, y = prior, color = task)) +
  facet_grid(cols = vars(itemtype, param), rows = vars(subset), scales = "free") +
  # facet_grid(rows = vars(task), cols = vars(itemtype), space = "free") +
  geom_hline(yintercept = 0, color = "grey", linetype = "dotted") +
  geom_vline(xintercept = 0, color = "grey", linetype = "dotted") +
  geom_abline(color = "grey", linetype = "dotted") +
  geom_point(size = 0.8, alpha = 0.5) +
  scale_color_manual(values = task_pal) +
  labs(x = "Parameter estimate without priors",
       y = "Parameter estimate with priors",
       color = "Task") +
  theme(panel.border = element_rect(color = "darkgrey"))
ggsave("prior_comparison_bylanguage.png", width = 10, height = 7)

coef_comp_bylang_task <- \(comp_task) {
  coef_comp_tidy |> filter(model_set == "by_language") |> filter(task == comp_task) |>
    ggplot(aes(x = noprior, y = prior, color = subset)) +
    facet_grid(cols = vars(itemtype, param), rows = vars(subset), scales = "free") +
    # facet_grid(rows = vars(task), cols = vars(itemtype), space = "free") +
    geom_hline(yintercept = 0, color = "grey", linetype = "dotted") +
    geom_vline(xintercept = 0, color = "grey", linetype = "dotted") +
    geom_abline(color = "grey", linetype = "dotted") +
    geom_point(size = 0.8, alpha = 0.5) +
    scale_colour_solarized() +
    # scale_color_manual(values = task_pal) +
    labs(x = "Parameter estimate without priors",
         y = "Parameter estimate with priors",
         color = "Language",
         caption = glue("Task: {comp_task}")) +
    theme(panel.border = element_rect(color = "darkgrey"),
          legend.position = "bottom")
  ggsave(glue("prior_comparison_bylanguage_{comp_task}.png"), width = 10, height = 7)
}

walk(tasks, coef_comp_bylang_task)

ggplot(coef_comp_tidy |> filter(model_set == "multigroup_site"), aes(x = noprior, y = prior, color = task)) +
  facet_grid(cols = vars(itemtype, param), rows = vars(invariance), scales = "free") +
  # facet_grid(rows = vars(task), cols = vars(itemtype), space = "free") +
  geom_hline(yintercept = 0, color = "grey", linetype = "dotted") +
  geom_vline(xintercept = 0, color = "grey", linetype = "dotted") +
  geom_abline(color = "grey", linetype = "dotted") +
  geom_point(size = 0.8, alpha = 0.5) +
  scale_color_ptol() +
  # scale_color_manual(values = task_pal) +
  labs(x = "Parameter estimate without priors",
       y = "Parameter estimate with priors",
       color = "Task") +
  theme(panel.border = element_rect(color = "darkgrey"))
ggsave("prior_comparison_multigroup.png", width = 10, height = 7)

coef_comp_multigroup_task <- \(comp_task) {
  coef_comp_tidy |> filter(model_set == "multigroup_site") |> filter(task == comp_task) |>
    ggplot(aes(x = noprior, y = prior, color = group)) +
    facet_grid(cols = vars(itemtype, param), rows = vars(invariance), scales = "free") +
    # facet_grid(rows = vars(task), cols = vars(itemtype), space = "free") +
    geom_hline(yintercept = 0, color = "grey", linetype = "dotted") +
    geom_vline(xintercept = 0, color = "grey", linetype = "dotted") +
    geom_abline(color = "grey", linetype = "dotted") +
    geom_point(size = 0.8, alpha = 0.5) +
    scale_color_solarized() +
    labs(x = "Parameter estimate without priors",
         y = "Parameter estimate with priors",
         color = "Site",
         caption = glue("Task: {comp_task}")) +
    theme(panel.border = element_rect(color = "darkgrey"),
          legend.position = "bottom")
  ggsave(glue("prior_comparison_multigroup_{comp_task}.png"), width = 10, height = 7)
}

walk(tasks, coef_comp_multigroup_task)
