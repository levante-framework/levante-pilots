library(tidyverse)
library(labelled)

######## MinBlur: Specify Algorithm ########  

## Algorithm inputs:
## df_pop = relevant population-level demographics (joint distributions). NOTE: please ensure
##    that the variable NAMES and CLASSES in "df_pop" match relevant "df" variable names. Otherwise,
##    the algorithm will not know how to link these data.
## df = dataset
## group_vars = all quasi-identifiers
## blur_vars = quasi-identifiers that *can* be blurred. NOTES: (1) order variables from least 
##    relevant to research question (blurred first) to most relevant (blurred last); (2) you do NOT
##    have to blur all variables
## k = requested level of k-anonymity

## Algorithm outputs:
## 1. ['K_Anon'] De-identified dataset (subset of data that satisfies requested level of k-anon.)
## 2. ['Less_K'] Secondary dataset (subset of data that did not satisfy requested level of k-anon, 
##    and therefore was supressed from (1))
## 3. ['Perc_K_Anon'] The percentage of the original dataset that could be de-identified. If 100%
##    could be de-identified, then the number of rows in 'K_Anon' should match 'df.'

MinBlur <- function(df_pop, df, group_vars, blur_vars, k) {
  '%!in%' <- function(x,y)!('%in%'(x,y))
  df <- unlabelled(df)
  df<- data.frame(lapply(df, as.character))
  df_pop$quasi_vars <- apply(df_pop[group_vars] , 1 , paste , collapse = "-" )
  df$quasi_vars <- apply(df[group_vars] , 1 , paste , collapse = "-" )
  group_vars <-  names(df[group_vars])
  group_vars <- lapply(group_vars, as.symbol)
  df_kpop <- df_pop %>%
    group_by(.dots=group_vars) %>% mutate(N = n()) 
  df_og_clean <- df_kpop %>% filter(N >= k)
  df_k <- df %>%
    group_by(.dots=group_vars) %>% mutate(N = n())
  df_clean <- df_k %>% filter(quasi_vars %in% df_og_clean$quasi_vars)
  df_id <- df_k %>% filter(quasi_vars %!in% df_og_clean$quasi_vars)
  for (i in 1:length(blur_vars)) {
    df_id[,blur_vars[i]] <- NA
    df_id <- df_id %>%
      group_by(.dots=group_vars) %>% mutate(N = n())
    df_clean2 <- df_id %>% filter(N >= k)
    df_clean2 <- data.frame(lapply(df_clean2, as.character))
    df_clean <- data.frame(lapply(df_clean, as.character))
    df_id <- df_id %>%  filter(N < k)
    df_clean <- full_join(df_clean, df_clean2)
  }
  clean_rate <- dim(df_clean)[1]/(dim(df_clean)[1]+dim(df_id)[1])
  clean_rate <- round(clean_rate*100, 2)
  list <- list("K_Anon" = df_clean, "Less_K" = df_id, "Perc_K_Anon" = clean_rate)
  return(list)
}
