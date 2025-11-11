library(redivis)
library(tidyverse)

registry_table <- redivis$organization("levante")$dataset("scoring:e97h")$table("model_registry")

regdir <- "02_scoring_outputs/model_registry"
regfiles <- list(name = list.files(regdir, recursive = TRUE),
                 path = list.files(regdir, recursive = TRUE, full.names = TRUE)) |>
  transpose()

registry_table$add_files(files = regfiles)
