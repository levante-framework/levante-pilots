
## Responsible data sharing: 
## Identifying and remedying possible re-identification of human participants 
## [AUTHOR NAMES REDACTED]

if (!require("pacman")) {install.packages("pacman"); require(pacman)}
p_load(tidyverse, labelled,
       install = TRUE)

## Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

######## SPECIFY FUNCTIONS RELEVANT TO EVALUATION:  ########  

return_k <- function(df, group_vars, blur_vars, k) {
  df <- unlabelled(df)
  df <- data.frame(lapply(df, as.character))
  group_vars <-  names(df[group_vars])
  group_vars <- lapply(group_vars, as.symbol)
  df_k <- df %>%
    group_by(.dots=group_vars) %>% mutate(N = n())
  df_clean <- df_k %>% filter(N >= k)
  df_id <- df_k %>% filter(N < k)
  for (i in 1:length(blur_vars)) {
    print(i)
    print(dim(df_id)[1])
    print(dim(df_clean)[1])
    df_id[,blur_vars[i]] <- "BLURRED"
    df_id <- df_id %>%
      group_by(.dots=group_vars) %>% mutate(N = n())
    df_clean2 <- df_id %>% filter(N >= k)  
    df_id <- df_id %>%  filter(N < k)
    df_clean2 <- data.frame(lapply(df_clean2, as.character)) 
    df_clean <- data.frame(lapply(df_clean, as.character)) 
    df_clean <- full_join(df_clean, df_clean2)
  }
  clean_rate <- dim(df_clean)[1]/(dim(df_clean)[1]+dim(df_id)[1]) 
  clean_rate <- round(clean_rate*100, 2)
  list <- list("K_Anon" = df_clean, "Less_K" = df_id, "Perc_K_Anon" = clean_rate)
  return(list)
}

'%!in%' <- function(x,y)!('%in%'(x,y))

compare_dist <- function(df_original, df_blur, blurred) {
  df_original <- unlabelled(df_original)
  table_og <- NULL
  hist_og <- NULL
  table_blur <- NULL
  hist_blur <- NULL
  for (i in 1:length(blurred)) {
    print(i)
    table_og[[i]] <- table(df_original[,blurred[i]])
    table_blur[[i]] <- table(df_blur[,blurred[i]])
    par(mfrow=c(2,1))
    hist_og[[i]] <- barplot(prop.table(table(df_original[,blurred[i]])), 
                            main = paste("Original Distribution:", blurred[i], sep = " "),
                            sub = paste("Unique levels:", n_distinct(df_original[,blurred[i]])))
    hist_blur[[i]] <- barplot(prop.table(table(df_blur[,blurred[i]])), 
                              main = paste("Blurred Distribution:", blurred[i], sep = " "),
                              sub = paste("Unique levels:", n_distinct(df_blur[,blurred[i]])))
    par(mfrow=c(1,1))
  }
  list <- list("Table_Original" = table_og, "Dist_Original" = hist_og, 
               "Table_Blur" = table_blur, "Dist_Blur" = hist_blur)
  return(list)
}

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

######## MinBlur: Example Use Case ########  

## Note: This corresponds to case #2

## Step 0 -- Create synthetic datasets

set.seed(1838)

## Population Data ("df_pop")
p_age2_pop <- sample(c(17:22), 4800, replace = TRUE, prob = c(.03, .21, .26, .25, .23, .02))
p_ethnicity2_pop <- sample(c("White", "Black", "East Asian", "South Asian", "Hispanic"),
                           4800, replace = TRUE, 
                           prob = c(.55, .1, .15, 4800, .1 ))
p_gender2_pop <- sample(c("Man", "Woman", "Open1", "Open2", "Open3", "Open4", "Open5"), 4800, replace = TRUE, 
                        prob = c(.46, .48, rep(0.01, 5)))
case2_pop <- data.frame(p_age2_pop, p_ethnicity2_pop, p_gender2_pop)
colnames(case2_pop) <- c("p_age2", "p_ethnicity2", "p_gender2")
head(case2_pop)

## Data ("df")
p_id2 <- paste("Subj", 1:70, sep = "")
p_age2 <- sample(c(17:22), 70, replace = TRUE, prob = c(.05, .20, .25, .25, .20, .05))
p_ethnicity2 <- sample(c("White", "Black", "East Asian", "South Asian", "Hispanic"),
                       70, replace = TRUE, 
                       prob = c(.55, .1, .15, .15, .1 ))
p_gender2 <- sample(c("Man", "Woman", "Open1", "Open2", "Open3", "Open4", "Open5"),70, replace = TRUE, 
                    prob = c(.45, .55, rep(0.01, 5)))
case2 <- data.frame(p_id2, p_age2, p_ethnicity2, p_gender2)
head(case2)

## *** Step 1 (optional) *** -- Calculate k-anonymity *before* using algorithm

## Calculate k-anonymity using population data
case2_pop %>% group_by(p_age2, p_ethnicity2, p_gender2) %>%
  summarise(k_anon = n()) %>%
  group_by(k_anon) %>% tally()

## Calculate k-anonymity using dataset
case2 %>% group_by(p_age2, p_ethnicity2, p_gender2) %>%
  summarise(k_anon = n()) %>%
  group_by(k_anon) %>% tally()

## *** Step 2 *** -- Apply MinBlur

case2_k <- MinBlur(case2_pop, case2, group_vars = c("p_age2", "p_ethnicity2", "p_gender2"), 
                   blur_vars = c("p_age2", "p_ethnicity2", "p_gender2"), 
                   k = 3)
## Note: If you get the following error, please ignore; it should not thrown:
## -- "The `.dots` argument of `group_by()` is deprecated as of dplyr 1.0.0."
## https://github.com/rstudio/gt/issues/709

## Output 1: ['K_Anon'] De-identified dataset (subset of data that satisfies requested level of k-anon.)
case2_k$K_Anon

## Output 2: ['Less_K'] Secondary dataset (subset of data that did not satisfy requested level of k-anon, 
##    and therefore was supressed from (1))
case2_k$Less_K

## Output 3: ['Perc_K_Anon'] The percentage of the original dataset that could be de-identified. If 100%
##    could be de-identified, then the number of rows in 'K_Anon' should match 'df.'
case2_k$Perc_K_Anon

## *** Step 3 *** -- Evaluate data fidelity
## >> Consider: 1. How did the distribution of quasi-identifiers change? 
##                 Were any variable levels removed?
compare_dist(df_original = case2, 
             df_blur = case2_k$K_Anon, 
             blurred = c("p_age2", "p_ethnicity2", "p_gender2"))

## >> Consider: 2. Did the predictive power of any key quasi-identifiers change?
##                 Were any variable levels removed?

## *** Step 4 (optional) *** -- Re-specify MinBlur if data fidelity is not high

######## MinBlurLite: Specify Algorithm ######## 

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

MinBlurLite <- function(df, group_vars, blur_vars, k) {
  df <- unlabelled(df)
  df<- data.frame(lapply(df, as.character))
  group_vars <-  names(df[group_vars])
  group_vars <- lapply(group_vars, as.symbol)
  df_k <- df %>%
    group_by(.dots=group_vars) %>% mutate(N = n()) 
  df_clean <- df_k %>% filter(N >= k)
  df_id <- df_k %>% filter(N < k) 
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

######## MinBlurLite: Example Use Case ########  

## Note: This corresponds partially to case #2

## Step 0 -- Create synthetic datasets

set.seed(1838)

## Data ("df")
p_id2 <- paste("Subj", 1:70, sep = "")
p_age2 <- sample(c(17:22), 70, replace = TRUE, prob = c(.05, .20, .25, .25, .20, .05))
p_ethnicity2 <- sample(c("White", "Black", "East Asian", "South Asian", "Hispanic"),
                       70, replace = TRUE, 
                       prob = c(.55, .1, .15, .15, .1 ))
p_gender2 <- sample(c("Man", "Woman", "Open1", "Open2", "Open3", "Open4", "Open5"),70, replace = TRUE, 
                    prob = c(.45, .55, rep(0.01, 5)))

case2 <- data.frame(p_id2, p_age2, p_ethnicity2, p_gender2)
case2

## *** Step 1 (optional) *** -- Calculate k-anonymity *before* using algorithm

## Calculate k-anonymity using population data
case2_pop %>% group_by(p_age2, p_ethnicity2, p_gender2) %>%
  summarise(k_anon = n()) %>%
  group_by(k_anon) %>% tally()

## Calculate k-anonymity using dataset
case2 %>% group_by(p_age2, p_ethnicity2, p_gender2) %>%
  summarise(k_anon = n()) %>%
  group_by(k_anon) %>% tally()

## *** Step 2 *** -- Apply MinBlurLite
case2_k <- MinBlurLite(case2, group_vars = c("p_age2", "p_ethnicity2", "p_gender2"), 
                       blur_vars = c("p_age2", "p_ethnicity2", "p_gender2"), 
                       k = 3)

## Note: If you get the following error, please ignore; it should not thrown:
## -- "The `.dots` argument of `group_by()` is deprecated as of dplyr 1.0.0."
## https://github.com/rstudio/gt/issues/709

## Output 1: ['K_Anon'] De-identified dataset (subset of data that satisfies requested level of k-anon.)
case2_k$K_Anon

## Output 2: ['Less_K'] Secondary dataset (subset of data that did not satisfy requested level of k-anon, 
##    and therefore was supressed from (1))
case2_k$Less_K

## Output 3: ['Perc_K_Anon'] The percentage of the original dataset that could be de-identified. If 100%
##    could be de-identified, then the number of rows in 'K_Anon' should match 'df.'
case2_k$Perc_K_Anon

## *** Step 3 *** -- Evaluate data fidelity
## >> Consider: 1. How did the distribution of quasi-identifiers change? 
##                 Were any variable levels removed?
compare_dist(df_original = case2, 
             df_blur = case2_k$K_Anon, 
             blurred = c("p_age2", "p_ethnicity2", "p_gender2"))

## >> Consider: 2. Did the predictive power of any key quasi-identifiers change?
##                 Were any variable levels removed?

## *** Step 4 (optional) *** -- Re-specify MinBlurLite if data fidelity is not high

