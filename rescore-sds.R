library(jsonlite)

dimension_indices <- c(size = 1, color = 2, shape = 3, number = 4, bgcolor = 5)

# Utility: parse response into vector of card strings
# Convert pseudo-JSON string into character vector of selections
parse_response <- function(resp) {
  if (is.na(resp) || str_trim(resp) == "") return(character(0))
  
  # Convert single quotes to double quotes to parse as JSON
  json_str <- str_replace_all(resp, "'", '"')
  parsed <- fromJSON(json_str, simplifyVector = TRUE)
  unname(as.character(parsed))
}

# adds back missing defaults (bgcolor=white; number=1)
clean_attributes <- function(attributes) {
  non_white_backgrounds <- c("gray", "striped", "black")
  
  if (!any(attributes %in% non_white_backgrounds)) {
    attributes <- c(attributes, "white") # default bgcolor
  }
  
  is_numeric <- suppressWarnings(!is.na(as.numeric(attributes)))
  if (!any(is_numeric)) {
    attributes <- append(attributes, "1", after = 3)
  }
  
  attributes
}


shared_trait <- function(selections, ignore_dims) {
  sets <- vector("list", length(dimension_indices))
  names(sets) <- names(dimension_indices)
  
  for (dim in names(sets)) {
    if (!(dim %in% ignore_dims)) {
      sets[[dim]] <- character(0)
    }
  }
  
  for (sel in selections) {
    attributes <- clean_attributes(str_split(sel, "-")[[1]])
    for (dim in names(sets)) {
      if (!(dim %in% ignore_dims)) {
        index <- dimension_indices[[dim]]
        sets[[dim]] <- c(sets[[dim]], attributes[index])
      }
    }
  }
  
  any(sapply(sets, function(vals) {
    !is.null(vals) & length(unique(vals)) == 1
    }))
}


has_new_selection <- function(selections, previousSelections) {
  
  if (length(previousSelections) == 0) return(TRUE)
  
  # check if any previous selection has exactly the same elements (regardless of order)
  !any(sapply(previousSelections, function(prev) {
    setequal(selections, prev)
  }))
}


compare_selections <- function(selections, previous_selections, ignore_dims) {
  shared_trait(selections, ignore_dims) &&
    has_new_selection(selections, previous_selections)
}



example_usage <- function() {
  stim_trial_type <- "2match"
  ignore_dims <- switch(stim_trial_type,
                        "2match" = c("number", "bgcolor"),
                        "3match" = c("size"),
                        "4match" = c("size"),
                        character(0))
  
  clean_attributes(strsplit("med-red-circle", "-")[[1]])
  # "med" red" "circle" "white" "1"
  clean_attributes(strsplit("sm-red-circle-2-gray", "-")[[1]])
  # "sm"     "red"    "circle" "2"      "gray" 
  
  clean_attributes(strsplit('med-yellow-square-gray', "-")[[1]])
  clean_attributes(strsplit('med-blue-triangle-3-gray', "-")[[1]])
  
  # selectedCards is a character vector of 2 strings like "sm-red-circle-1-gray"
  selectedCards <- c("med-blue-square", "sm-red-circle")
  previousSelections <- list(c("sm-red-circle", "sm-blue-triangle"))
  has_new_selection(selectedCards, previousSelections)
  shared_trait(selectedCards, ignore_dims) 
  isCorrect <- compare_selections(selectedCards, previousSelections, ignore_dims) # F
  
  selectedCards <- c("sm-red-circle-1-gray", "sm-blue-triangle-1-striped")
  previousSelections <- list(c("sm-red-circle-1-gray", "sm-red-circle-1-gray"))  # only duplicates so far
  stim_trial_type <- "2match"
  ignore_dims <- c("number", "bgcolor")
  isCorrect <- compare_selections(selectedCards, previousSelections, ignore_dims) # T
  
  selected <- c("sm-red-circle", "med-red-square-1-white")
  previous <- list()
  ignore_dims <- c("number", "bgcolor")
  
  compare_selections(selected, previous, ignore_dims)
  
  previous <- list(selected)
  # same as previous selection — should now return FALSE
  compare_selections(selected, previous, ignore_dims)
  
  selectedCards <- c("sm-blue-triangle", "lg-yellow-triangle")
  previousSelections <- list(c("sm-blue-triangle", "lg-yellow-triangle"))
  shared_trait(selectedCards, ignore_dims) # TRUE
  compare_selections(selectedCards, previousSelections, ignore_dims) # TRUE
  has_new_selection(selectedCards, previousSelections) # FALSE
  
  shared_trait(c("sm-blue-triangle", "sm-yellow-square"), c("size"))
  
  shared_trait(c("med-blue-square", "sm-red-circle"), c("number", "bgcolor")) 
  
  compare_selections(c("sm-blue-triangle", "sm-yellow-triangle"), 
                     list(c("sm-blue-triangle", "sm-yellow-triangle")), ignore_dims) 
  
  
}