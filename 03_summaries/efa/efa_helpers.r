library(tidyverse)
library(psych)
<<<<<<< HEAD
# library(corrplot)
library(here)
# library(reshape2)
# library(plyr)
=======
library(corrplot)
library(reshape2)
>>>>>>> f5ae04b8f3272136956ae4b40ce911c9a0f97ecb

col <- colorRampPalette(c("red", "white", "blue"))(200) # Red to white to blue



<<<<<<< HEAD

loadmax <- function(loadings, m_data_subset){
  loadings_df <- reshape2::melt(as.matrix(loadings[1:ncol(m_data_subset),]))
  colnames(loadings_df) = c('Item', 'Factor', 'Loading')

  loadings_df <- loadings_df %>%
    mutate(Factor = gsub("Factor", "F", Factor))
  loadings_df <- plyr::ddply(loadings_df, plyr::.(Item), function(x) {
    x$Loading[-which.max(abs(x$Loading))] <- 0
    x
  })
=======
loadmax <- function(loadings, m_data_subset) {
  # Melt the loadings into a long format
  loadings_df <- melt(as.matrix(loadings[1:ncol(m_data_subset), ]))
  colnames(loadings_df) <- c('Item', 'Factor', 'Loading')  # Set column names explicitly
  
  # Replace "Factor" with "F" in the Factor column
  loadings_df <- loadings_df |>
    mutate(Factor = gsub("Factor", "F", Factor))
  
  # Group by 'Item' and set non-maximum loadings to 0
  loadings_df <- loadings_df |>
    group_by(Item) |>
    mutate(Loading = ifelse(abs(Loading) != max(abs(Loading)), 0, Loading)) |>
    ungroup()
  
>>>>>>> f5ae04b8f3272136956ae4b40ce911c9a0f97ecb
  return(loadings_df)
}



plot_load <- function(loadings_df, title, lowlimit = -1, uplimit = 1){
    ggplot(loadings_df, aes(Factor,  Item, fill = Loading)) +
    geom_tile() +
    geom_text(aes(label = ifelse(Loading != 0, sprintf("%.2f", Loading), "")), 
              color = "black", vjust = 0.5, hjust = 0.5) + # Add non-zero loadings as text
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, 
                         limit = c(lowlimit, uplimit), 
                         space = "Lab", name="Loading") +
    theme_classic() +
    theme(axis.text.x = element_text(vjust = 0.5,
      hjust=1)) +
    labs(x = "", y = "Item", title = title) +
    coord_fixed(ratio = 1) 
}

