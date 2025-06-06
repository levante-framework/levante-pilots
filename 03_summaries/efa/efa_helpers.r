
library(psych)
library(corrplot)
library(reshape2)
library(patchwork) 
library(gtools)  # natural sorting

# col <- colorRampPalette(c(pal$red, "white", pal$blue))(200) # Red to white to blue

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
  
  return(loadings_df)
}



plot_load <- function(loadings_df, title, lowlimit = -1, uplimit = 1.2){
   # order items to make the y-axis consistent across sites
  loadings_df <- loadings_df %>%
    mutate(Item = factor(Item, levels = gtools::mixedsort(as.character(unique(Item))))) %>%
    arrange(Item)
  
  
  
    p <- ggplot(loadings_df, aes(Factor,  Item, fill = Loading)) +
    geom_tile() +
    geom_text(aes(label = ifelse(Loading != 0, sprintf("%.2f", Loading), "")), 
              color = "black", vjust = 0.5, hjust = 0.5) + # Add non-zero loadings as text
    scale_fill_gradient2(low = pal$blue, high = pal$red, mid = "white", midpoint = 0, 
                         limit = c(lowlimit, uplimit), 
                         space = "Lab", name="Loading") +
    theme_classic() +
    theme(axis.text.x = element_text(vjust = 0.5,
      hjust=1)) +
    labs(x = "", y = "Item", title = title) +
    coord_fixed(ratio = 1) 
    
    return(p)
    print(p)
}


