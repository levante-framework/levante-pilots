
library(psych)
library(corrplot)
library(reshape2)
library(plyr)

col <- colorRampPalette(c("red", "white", "blue"))(200) # Red to white to blue




loadmax <- function(loadings, m_data_subset){
  loadings_df <- melt(as.matrix(loadings[1:ncol(m_data_subset),]))
  colnames(loadings_df) = c('Item', 'Factor', 'Loading')

  loadings_df <- loadings_df %>%
    mutate(Factor = gsub("Factor", "F", Factor))
  loadings_df <- plyr::ddply(loadings_df, .(Item), function(x) {
    x$Loading[-which.max(abs(x$Loading))] <- 0
    x
  })
  return(loadings_df)
}




plot_load <- function(loadings_df, title){
    ggplot(loadings_df, aes(Factor,  Item, fill = Loading)) +
    geom_tile() +
    geom_text(aes(label = ifelse(Loading != 0, sprintf("%.2f", Loading), "")), 
              color = "black", vjust = 0.5, hjust = 0.5) + # Add non-zero loadings as text
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name="Loading") +
    theme_classic() +
    theme(axis.text.x = element_text(vjust = 0.5,
      hjust=1)) +
    labs(x = "", y = "Item", title = title) +
    coord_fixed(ratio = 1) 
}

