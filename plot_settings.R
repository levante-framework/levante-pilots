library(ggplot2)
library(ggthemes)

.font <- "Source Sans 3"
sysfonts::font_add_google(.font)
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

theme_set(theme_bw(base_size = 14, base_family = .font))
theme_update(panel.grid = element_blank(),
             strip.background = element_blank(),
             legend.key = element_blank(),
             panel.border = element_blank(),
             axis.line = element_line(),
             strip.text = element_text(face = "bold"))

# set options for default continuous color and fill scales
options("ggplot2.continuous.colour" = viridis::scale_colour_viridis)
options("ggplot2.continuous.fill"   = viridis::scale_fill_viridis  )

# color palette for tasks: ptol
task_categories_vec <- c("Executive function", "Math", "Reasoning", "Spatial cognition", "Language", "Reading", "Social cognition")
task_pal <- ptol_pal()(length(task_categories_vec)) |> rlang::set_names(task_categories_vec)
.scale_colour_task <- \() scale_colour_manual(values = task_pal, ...)
.scale_color_task <- .scale_colour_task

# color palette for sites: custom subset of solarized
sites <- c("co_pilot", "de_pilot", "ca_pilot", "us_pilot")
site_pal <- solarized_pal()(length(sites) + 2)[c(1:2, 5:6)] |> rlang::set_names(sites)
.scale_colour_site <- \(...) scale_colour_manual(values = site_pal, ...)
.scale_color_site <- .scale_colour_site

# other contrasts: brewer Set1
.scale_color_default <- \(...) scale_colour_brewer(palette = "Set1", ...)

# specific element colors
pal <- list(
  grey = "#93a1a1",
  red  = RColorBrewer::brewer.pal(3, "Set1")[1],
  blue = RColorBrewer::brewer.pal(3, "Set1")[2]
)
