
library(psych)
library(corrplot)
library(reshape2)
library(patchwork) 
library(gtools)  # natural sorting

# col <- colorRampPalette(c(pal$red, "white", pal$blue))(200) # Red to white to blue
loadmax <- function(loadings, m_data_subset, threshold = -Inf, diff_tol = 0.1) {
  
  loadings_df <- melt(as.matrix(loadings[1:ncol(m_data_subset), ]))
  colnames(loadings_df) <- c("Item", "Factor", "Loading")
  
  loadings_df <- loadings_df %>%
    mutate(Factor = gsub("Factor", "F", Factor)) %>%
    group_by(Item) %>%
    mutate(
      abs_loading = abs(Loading),
      max_abs_loading = max(abs_loading),
      keep_flag =
        max_abs_loading >= threshold &
        (abs_loading == max_abs_loading |
           abs(max_abs_loading - abs_loading) < diff_tol),
      Loading = ifelse(keep_flag, Loading, 0)
    ) %>%
    select(-abs_loading, -max_abs_loading, -keep_flag) %>%
    ungroup()
  
  ## Cronbach alpha
  alpha_info <- ltm::cronbach.alpha(m_data_subset, na.rm = TRUE)
  alpha_total <- alpha_info$alpha
  
  alpha_deleted <- sapply(colnames(m_data_subset), function(item) {
    ltm::cronbach.alpha(
      m_data_subset[, colnames(m_data_subset) != item],
      na.rm = TRUE
    )$alpha
  })
  
  alpha_df <- data.frame(
    Item = names(alpha_deleted),
    Alpha_if_deleted = round(alpha_deleted, 2)
  )
  
  loadings_df <- loadings_df %>%
    left_join(alpha_df, by = "Item") %>%
    mutate(
      Item = factor(
        Item,
        levels = gtools::mixedsort(as.character(unique(Item)))
      )
    ) %>%
    arrange(Item)
  
  attr(loadings_df, "alpha_total") <- round(alpha_total, 2)
  
  return(loadings_df)
}

# loadmax <- function(loadings, m_data_subset, threshold = -Inf) {
#   loadings_df <- melt(as.matrix(loadings[1:ncol(m_data_subset), ]))
#   colnames(loadings_df) <- c('Item', 'Factor', 'Loading')
#   
#   loadings_df <- loadings_df %>%
#     mutate(Factor = gsub("Factor", "F", Factor)) %>%
#     group_by(Item) %>%
#     mutate(
#       abs_loading = abs(Loading),
#       max_abs_loading = max(abs_loading),
#       Loading = ifelse(max_abs_loading < threshold, 0,
#                        ifelse(abs_loading == max_abs_loading, Loading, 0))
#     ) %>%
#     select(-abs_loading, -max_abs_loading) %>%
#     ungroup()
#   
#   # === Compute Cronbach's alpha ===
#   alpha_info <- ltm::cronbach.alpha(m_data_subset, na.rm = TRUE)
#   alpha_total <- alpha_info$alpha
#   
#   # Alpha if item deleted
#   alpha_deleted <- sapply(colnames(m_data_subset), function(item) {
#     ltm::cronbach.alpha(m_data_subset[, colnames(m_data_subset) != item], na.rm = TRUE)$alpha
#   })
#   
#   # Add to loadings_df
#   alpha_df <- data.frame(Item = names(alpha_deleted), Alpha_if_deleted = round(alpha_deleted, 2))
#   loadings_df <- loadings_df %>%
#     left_join(alpha_df, by = "Item") 
#   
#   attr(loadings_df, "alpha_total") <- round(alpha_total, 2)
#   loadings_df <- loadings_df %>%
#     mutate(Item = factor(Item, levels = gtools::mixedsort(as.character(unique(Item))))) %>%
#     arrange(Item)
#   return(loadings_df)
# }



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
  
  if (is.null(plot_title)) {
    plot_title <- formi
  }
  
  keep_y_axis <- TRUE   
  
  for (sitei in sites) {
    
    row_df <- caregiver_tidy %>%
      filter(form_construct == formi, site == sitei)
    
    if (nrow(row_df) == 0) {
      cat(sitei, ": no data\n")
      next
    }
    
    ## scaled data
    subconstruct <- row_df$df_scaled[[1]]
    subconstruct <- subconstruct[, !colnames(subconstruct) %in% remove_items]
    
    ## ids
    ids <- row_df$df_vals[[1]] %>%
      select(respondent_id, child_id)
    
    ## item-level metadata
    item_info <- row_df$data[[1]] %>%
      select(variable, form_subconstruct) %>%
      distinct() %>%
      rename(Item = variable)
    
    ## EFA
    EFAresult <- factanal(
      ~ .,
      data = subconstruct,
      factors = nfactors[sitei],
      scores = "regression",
      rotation = "promax",
      na.action = na.exclude
    )
    
    loadings_df <- loadmax(EFAresult$loadings, subconstruct) %>%
      left_join(item_info, by = "Item")
    
    p <- plot_load(loadings_df, sitei) +
      theme(legend.position = "none")
    
    ## ===== only keep y-axis for the first plot =====
    ## ===== only keep subconstruct info for the last plot =====
    is_last_plot <- identical(sitei, tail(sites, 1))
    
    sub_df <- loadings_df %>%
      distinct(Item, form_subconstruct)
    ## ===== y axis logic =====
    if (is_last_plot) {
      
      ## last plot: hide item y axis, but add subconstruct labels
      p <- p +
        theme(
          axis.text.y  = element_blank(),
          axis.title.y = element_blank()
        )

      
      p <- p +
        geom_text(
          data = sub_df,
          aes(
            x = Inf,
            y = Item,
            label = form_subconstruct
          ),
          inherit.aes = FALSE,
          hjust = -0.05,
          size = 3
        ) +
        coord_cartesian(clip = "off") +
        theme(
          plot.margin = margin(r = 50)
        )
      
      
    } else if (keep_y_axis) {
      
      ## first plot: keep item y axis
      keep_y_axis <- FALSE
      p = p +
        geom_text(
          data = sub_df,
          aes(x = Inf, y = Item, label = ""),
          inherit.aes = FALSE
        ) +
        coord_cartesian(clip = "off") +
        theme(plot.margin = margin(r = 50))
      
    } else {
      
      ## middle plots: hide item y axis
      p <- p +
        geom_text(
          data = sub_df,
          aes(x = Inf, y = Item, label = ""),
          inherit.aes = FALSE
        ) +
        coord_cartesian(clip = "off") +
        theme(plot.margin = margin(r = 50))+
        theme(
          axis.text.y  = element_blank(),
          axis.title.y = element_blank()
        )
    }

    plot_list[[sitei]] <- p
    
    ## save factor scores
    scores_df <- as.data.frame(EFAresult$scores)
    colnames(scores_df) <- paste0(formi, 1:ncol(scores_df))
    
    caregiver_tidy$fscore[
      caregiver_tidy$form_construct == formi &
        caregiver_tidy$site == sitei
    ] <- list(bind_cols(ids, scores_df))
  }
  
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

# run_efa_plot <- function(caregiver_tidy, formi, remove_items=NULL, sites, nfactors, plot_title=NULL) {
#   plot_list <- list()
#   
#   for (sitei in sites) {
#     row_index <- which(caregiver_tidy$form_construct == formi & caregiver_tidy$site == sitei)
#     
#     if (length(row_index) == 0) {
#       cat(sitei, ": no data\n")
#       next
#     }
#     if (is.null(plot_title)){
#       plot_title = formi
#     }
#     
#     subconstruct <- caregiver_tidy$df_scaled[[row_index]]
#     subconstruct <- subconstruct[, !colnames(subconstruct) %in% remove_items]
#     
#     ids <- caregiver_tidy$df_vals[[row_index]] %>%
#       select(respondent_id, child_id)
#     
#     # EFA
#     EFAresult <- factanal(~ ., data = subconstruct,
#                           factors = nfactors[sitei],
#                           scores = "regression",
#                           rotation = "promax",
#                           na.action = na.exclude)
#     
#     loadings_df <- loadmax(EFAresult$loadings, subconstruct)
#     
#     # plots
#     p <- plot_load(loadings_df, sitei) +
#       theme(legend.position = "none")
#     plot_list[[sitei]] <- p
#     
#     # save factor scores
#     scores_df <- as.data.frame(EFAresult$scores)
#     colnames(scores_df) <- paste0(formi, 1:ncol(scores_df))
#     caregiver_tidy$fscore[[row_index]] <- bind_cols(ids, scores_df)
#   }
#   
#   # combine
#   combined_plot <- wrap_plots(plot_list, nrow = 1) +
#     plot_annotation(
#       title = plot_title,
#       theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))
#     )
#   print(combined_plot)
#   
#   return(list(
#     plot = combined_plot,
#     data = caregiver_tidy
#   ))
# }



