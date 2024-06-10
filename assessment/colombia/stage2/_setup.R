library(tidyverse)
library(glue)
library(ggforce)
library(ggthemes)

.font <- "Source Sans Pro"
theme_set(theme_bw(base_size = 14, base_family = .font))
theme_update(panel.grid = element_blank(),
             strip.background = element_blank(),
             legend.key = element_blank(),
             panel.border = element_blank(),
             axis.line = element_line(),
             strip.text = element_text(face = "bold"))
