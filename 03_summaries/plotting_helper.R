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
    geom_point(aes(colour = task_category), alpha = 0.3) +
    geom_smooth(aes(group = site), method = "gam", colour = "darkgrey", formula = y ~ s(x, bs = "re")) +
    scale_x_continuous(breaks = y_axis) +
    scale_colour_manual(values = task_pal) +
    labs(x = "Age (years)", y = ylab,
         caption = glue("Note: includes only tasks with at least {threshold_n} observations")) +
    guides(colour = "none")
}


task_plot_comparative <- \(scores, ylab, nr = NULL, nc = NULL, y_axis = seq(6, 14, 2)) {
  ggplot(scores, aes(x = age, y = metric_value)) +
    ggh4x::facet_nested_wrap(vars(task_category, task_label),
                             nrow = nr, ncol = nc,
                             nest_line = element_line(), solo_line = TRUE,
                             axes = "x",
                             scales = "free_y") +
    geom_point(aes(colour = site), alpha = 0.3) +
    geom_smooth(aes(group = site, color = site), method = "gam", formula = y ~ s(x, bs = "re")) +
    scale_x_continuous(breaks = y_axis) +
    ggthemes::scale_colour_solarized() +
    labs(x = "Age (years)", y = ylab,
         caption = glue("Note: includes only tasks with at least {threshold_n} observations")) +
    theme(legend.position = "bottom")
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
    # scale_x_continuous(breaks = y_axis) +
    scale_colour_manual(values = task_pal) +
    labs(x = "Age (years)", y = ylab,
         caption = glue("Note: includes only tasks with at least {threshold_n} observations")) +
    guides(colour = "none")
}

task_plot_completeness <- \(scores, ylab, nr = NULL, nc = NULL, y_axis = seq(6, 14, 2)) {
  ggplot(scores, aes(x = age, y = metric_value)) +
    ggh4x::facet_nested_wrap(vars(site_label, task_category, site_task_label),
                             nrow = nr, ncol = nc,
                             nest_line = element_line(), solo_line = TRUE,
                             axes = "x",
                             scales = "free_y") +
    geom_point(aes(colour = task_category, shape = completed), alpha = 0.5) +
    geom_smooth(aes(group = site), method = "gam", colour = "darkgrey", formula = y ~ s(x, bs = "re")) +
    # geom_smooth(data = scores |> filter(completed),
    #             aes(group = site), method = "gam", colour = "darkgrey", formula = y ~ s(x, bs = "re")) +
    # scale_x_continuous(breaks = y_axis) +
    scale_colour_manual(values = task_pal) +
    scale_shape_manual(values = c(1, 16), guide = "none") +
    labs(x = "Age (years)", y = ylab) +
         # caption = glue("Note: includes only tasks with at least {threshold_n} observations")) +
    guides(colour = "none")
}
