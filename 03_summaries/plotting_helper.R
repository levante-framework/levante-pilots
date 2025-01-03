.font <- "Source Sans Pro"
theme_set(theme_bw(base_size = 14, base_family = .font))
theme_update(panel.grid = element_blank(),
             strip.background = element_blank(),
             legend.key = element_blank(),
             panel.border = element_blank(),
             axis.line = element_line(),
             strip.text = element_text(face = "bold"))


task_plot_pooled <- \(scores, ylab, nr = NULL, nc = NULL, y_axis = seq(6, 14, 2)) {
  ggplot(scores, aes(x = age, y = metric_value)) +
    ggh4x::facet_nested_wrap(vars(task_category, task_label),
                             nrow = nr, ncol = nc,
                             nest_line = element_line(), solo_line = TRUE,
                             axes = "x",
                             scales = "free_y") +
    geom_point(aes(colour = task_category), alpha = 0.5) +
    geom_smooth(aes(group = site), method = "gam", colour = "darkgrey", formula = y ~ s(x, bs = "re")) +
    scale_x_continuous(breaks = y_axis) +
    scale_colour_manual(values = task_pal) +
    labs(x = "Age (years)", y = ylab,
         caption = glue("Note: includes only tasks with at least {threshold_n} observations")) +
    guides(colour = "none")
}

task_plot_sites <- \(scores, ylab, nr = NULL, nc = NULL, y_axis = seq(6, 14, 2)) {
  ggplot(scores, aes(x = age, y = metric_value)) +
    ggh4x::facet_nested_wrap(vars(site_label, task_category, site_task_label),
                             nrow = nr, ncol = nc,
                             nest_line = element_line(), solo_line = TRUE,
                             axes = "x",
                             scales = "free_y") +
    geom_point(aes(colour = task_category), alpha = 0.5) +
    geom_smooth(aes(group = site), method = "gam", colour = "darkgrey", formula = y ~ s(x, bs = "re")) +
    scale_x_continuous(breaks = y_axis) +
    scale_colour_manual(values = task_pal) +
    labs(x = "Age (years)", y = ylab,
         caption = glue("Note: includes only tasks with at least {threshold_n} observations")) +
    guides(colour = "none")
}

task_categories <- tribble(
  ~task, ~task_category,
  "hearts-and-flowers", "executive function",
  "same-different-selection", "executive function",
  "memory-game", "executive function",
  "egma-math", "math",
  "matrix-reasoning", "reasoning",
  "mental-rotation", "spatial cognition",
  "trog", "language",
  "vocab", "language",
  "pa-es", "reading",
  "sre-es", "reading",
  "swr-es", "reading",
  "pa", "reading",
  "sre", "reading",
  "swr", "reading",
  "emotion-reasoning", "social cognition",
  "theory-of-mind", "social cognition",
  "hostile-attribution", "social cognition"
) |> mutate(task_category = task_category |> str_to_sentence() |> fct_inorder())

task_metrics <- tribble(
  ~task, ~metric_type,
  "hearts-and-flowers", "ability",
  "same-different-selection", "ability",
  "memory-game", "ability",
  "egma-math", "ability",
  "matrix-reasoning", "ability",
  "mental-rotation", "ability",
  "trog", "ability",
  "vocab", "ability",
  "pa-es", "prop_correct",
  "sre-es", "guessing_adjusted_number_correct",
  "swr-es", "ability",
  "pa", "prop_correct",
  "sre", "guessing_adjusted_number_correct",
  "swr", "prop_correct",
  "emotion-reasoning", "prop_correct",
  "theory-of-mind", "ability",
  "hostile-attribution", "prop_correct"
)