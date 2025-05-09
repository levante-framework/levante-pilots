clean_attributes <- function(attributes) {
  non_white_backgrounds <- c("gray", "striped", "black")
  
  if (!any(attributes %in% non_white_backgrounds)) {
    attributes <- c(attributes, "white")
  }
  
  is_numeric <- suppressWarnings(!is.na(as.numeric(attributes)))
  if (!any(is_numeric)) {
    attributes <- append(attributes, "1", after = 3)
  }
  
  attributes
}

shared_trait <- function(selections, ignore_dims) {
  dimension_indices <- c(size = 1, color = 2, shape = 3, number = 4, bgcolor = 5)
  sets <- vector("list", length(dimension_indices))
  names(sets) <- names(dimension_indices)
  
  for (dim in names(dimension_indices)) {
    if (!(dim %in% ignore_dims)) {
      sets[[dim]] <- character(0)
    }
  }
  
  for (sel in selections) {
    attributes <- clean_attributes(strsplit(sel, "-")[[1]])
    for (dim in names(sets)) {
      index <- dimension_indices[[dim]]
      sets[[dim]] <- c(sets[[dim]], attributes[index])
    }
  }
  
  # check if any non-ignored dimension has all the same value
  any(sapply(sets, function(vals) length(unique(vals)) == 1))
}


has_new_selection <- function(selections, previous_selections) {
  if (length(previous_selections) == 0) {
    return(TRUE)
  }
  
  for (prev in previous_selections) {
    if (all(selections == prev) || all(rev(selections) == prev)) {
      return(FALSE)
    }
  }
  TRUE
}


compare_selections <- function(selections, previous_selections, ignore_dims) {
  shared_trait(selections, ignore_dims) &&
    has_new_selection(selections, previous_selections)
}



example_usage <- function() {
  stim_trial_type <- "something-same-2"
  ignore_dims <- switch(stim_trial_type,
                        "something-same-2" = c("number", "bgcolor"),
                        "2-match" = c("number", "bgcolor"),
                        "3-match" = c("size"),
                        "4-match" = c("size"),
                        character(0))
  
  # selectedCards is a character vector of 2 strings like "sm-red-circle-1-gray"
  selectedCards <- c("sm-red-circle-1-gray", "sm-red-circle-1-gray")
  previousSelections <- list(c("sm-red-circle-1-gray", "sm-red-circle-1-gray"))
  
  isCorrect <- compare_selections(selectedCards, previousSelections, ignore_dims) # F
  
  selectedCards <- c("sm-red-circle-1-gray", "sm-red-circle-1-striped")
  previousSelections <- list(c("sm-red-circle-1-gray", "sm-red-circle-1-gray"))  # only duplicates so far
  stim_trial_type <- "something-same-2"
  ignore_dims <- c("number", "bgcolor")
  isCorrect <- compare_selections(selectedCards, previousSelections, ignore_dims) # T
}