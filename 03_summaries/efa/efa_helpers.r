
library(psych)
library(corrplot)
library(reshape2)
library(patchwork) 
library(gtools)  # natural sorting

# col <- colorRampPalette(c(pal$red, "white", pal$blue))(200) # Red to white to blue

loadmax <- function(loadings, m_data_subset, threshold = -Inf) {
  loadings_df <- melt(as.matrix(loadings[1:ncol(m_data_subset), ]))
  colnames(loadings_df) <- c('Item', 'Factor', 'Loading')
  
  loadings_df <- loadings_df %>%
    mutate(Factor = gsub("Factor", "F", Factor)) %>%
    group_by(Item) %>%
    mutate(
      abs_loading = abs(Loading),
      max_abs_loading = max(abs_loading),
      Loading = ifelse(max_abs_loading < threshold, 0,
                       ifelse(abs_loading == max_abs_loading, Loading, 0))
    ) %>%
    select(-abs_loading, -max_abs_loading) %>%
    ungroup()
  
  # === Compute Cronbach's alpha ===
  alpha_info <- ltm::cronbach.alpha(m_data_subset, na.rm = TRUE)
  alpha_total <- alpha_info$alpha
  
  # Alpha if item deleted
  alpha_deleted <- sapply(colnames(m_data_subset), function(item) {
    ltm::cronbach.alpha(m_data_subset[, colnames(m_data_subset) != item], na.rm = TRUE)$alpha
  })
  
  # Add to loadings_df
  alpha_df <- data.frame(Item = names(alpha_deleted), Alpha_if_deleted = round(alpha_deleted, 2))
  loadings_df <- loadings_df %>%
    left_join(alpha_df, by = "Item") 
  
  attr(loadings_df, "alpha_total") <- round(alpha_total, 2)
  loadings_df <- loadings_df %>%
    mutate(Item = factor(Item, levels = gtools::mixedsort(as.character(unique(Item))))) %>%
    arrange(Item)
  return(loadings_df)
}



plot_load <- function(loadings_df, plot_title, lowlimit = -1, uplimit = 1.2){
  # order items to make the y-axis consistent across sites

  
  
  
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
    labs(x = "", y = "Item", title = plot_title) +
    coord_fixed(ratio = 1) 
  
  return(p)
  print(p)
}


## for caregiver survey

run_efa_plot <- function(caregiver_tidy, formi, remove_items=NULL, sites, nfactors, plot_title=NULL) {
  plot_list <- list()
  
  for (sitei in sites) {
    row_index <- which(caregiver_tidy$form_construct == formi & caregiver_tidy$site == sitei)
    
    if (length(row_index) == 0) {
      cat(sitei, ": no data\n")
      next
    }
    if (is.null(plot_title)){
      plot_title = formi
    }
    
    subconstruct <- caregiver_tidy$df_scaled[[row_index]]
    subconstruct <- subconstruct[, !colnames(subconstruct) %in% remove_items]
    
    ids <- caregiver_tidy$df_vals[[row_index]] %>%
      select(user_id, child_id)
    
    # EFA
    EFAresult <- factanal(~ ., data = subconstruct,
                          factors = nfactors[sitei],
                          scores = "regression",
                          rotation = "promax",
                          na.action = na.exclude)
    
    loadings_df <- loadmax(EFAresult$loadings, subconstruct)
    
    # plots
    p <- plot_load(loadings_df, sitei) +
      theme(legend.position = "none")
    plot_list[[sitei]] <- p
    
    # save factor scores
    scores_df <- as.data.frame(EFAresult$scores)
    colnames(scores_df) <- paste0(formi, 1:ncol(scores_df))
    caregiver_tidy$fscore[[row_index]] <- bind_cols(ids, scores_df)
  }
  
  # combine
  combined_plot <- wrap_plots(plot_list, nrow = 1) +
    plot_annotation(
      title = plot_title,
      theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))
    )
  print(combined_plot)
  
  return(list(
    plot = combined_plot,
    data = caregiver_tidy
  ))
}



