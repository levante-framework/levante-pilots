# Fit all candidate SDS scoring models for sds_scoring_report.qmd
# ------------------------------------------------------------------
# Written by Claude Code (Anthropic), directed by Mike Frank, July 2026.
# Run from the levante-pilots repo root (or let here::here resolve it).
# All fits are cached in model_tree_mirt/report_cache/ -- delete the cache to refit.
#
# Model naming: {structure}{floors}_{itemtype}
#   structure: m0 = items are block x choice (production-style)
#              m1 = items are block x choice x status
#              t1 = IRTree pseudo-items (match + conditional new nodes), 1 factor
#              t2 = IRTree pseudo-items, 2 factors (match, new)
#   floors: (none) = theory-derived random-pair guessing floors
#           c = group-mean principled floors (m0c only)
#           z = no guessing floor (guess = 0)
#   m0 uses the corpus metadata chance values (.5/.75/0...) that production uses
#   mg_* = multigroup scalar by site versions

suppressMessages({
  library(dplyr); library(tidyr); library(readr); library(purrr)
  library(stringr); library(glue); library(mirt); library(tibble); library(jsonlite)
})

sds_dir <- here::here("03_explore_tasks/blockCAT/sds")
cache_dir <- file.path(sds_dir, "model_tree_mirt/report_cache")
dir.create(cache_dir, showWarnings = FALSE)

# ---- helpers (mirroring levantemodels) --------------------------------------
dedupe_items <- function(df, item_sep = "-") {
  df |> group_by(run_id, item_uid) |>
    mutate(instance = seq_along(item_uid)) |> ungroup() |>
    mutate(item_inst = as.character(glue("{item_uid}{item_sep}{instance}")))
}
remove_no_var_items <- function(df) {
  df |> group_by(item_inst) |> filter(n_distinct(correct) > 1) |> ungroup()
}
to_mirt_shape <- function(df) {
  df |> mutate(correct = as.numeric(correct)) |>
    select(run_id, item_inst, correct) |>
    pivot_wider(names_from = item_inst, values_from = correct) |>
    column_to_rownames("run_id")
}
paste_c <- \(...) paste(..., collapse = ",")

# model string: single factor or by-node factors, instance constraints, priors
gen_model_str <- function(df, df_prepped, item_type, priors = NULL, by_node = FALSE) {
  items <- df |> pull(item_uid) |> unique()
  if (by_node) {
    nodes <- df |> pull(node) |> unique()
    factors <- nodes |> set_names() |> map(\(nd) {
      idx <- which(str_detect(colnames(df_prepped), glue("^{nd}\\.")))
      paste(idx, collapse = ",")
    })
  } else {
    factors <- list(glue("1-{ncol(df_prepped)}")) |> set_names("F1")
  }
  slopes <- paste0("a", 1:length(factors)) |> set_names(names(factors))
  item_params <- items |> set_names() |> map(\(iu) {
    params <- "d"
    if (item_type != "Rasch") {
      if (by_node) {
        nd <- str_extract(iu, "^[A-Za-z]+(?=\\.)")
        params <- c(params, slopes[[nd]])
      } else params <- c(params, slopes[["F1"]])
    }
    params
  })
  constraints <- items |> map(\(iu) {
    prefixes <- str_sub(colnames(df_prepped), 1, str_length(iu) + 1)
    matched_idx <- which(prefixes == paste0(iu, "-"))
    if (length(matched_idx) > 1)
      map_chr(item_params[[iu]], \(p) glue("({paste_c(matched_idx)},{p})")) |> paste_c()
  }) |> compact() |> paste_c()
  constraint <- if (str_length(constraints) > 1) paste0("CONSTRAIN=", constraints) else ""
  prior_terms <- priors |>
    imap(\(pr, param) glue("(1-{ncol(df_prepped)},{paste(c(param, pr), collapse = ',')})"))
  prior <- if (length(prior_terms) > 0) glue("PRIOR={paste(prior_terms, collapse = ',')}") else ""
  factor_str <- imap_chr(factors, \(idx, nm) glue("{nm} = {idx}")) |> unname()
  paste(c(factor_str, constraint, prior), collapse = "\n")
}

prep <- function(df) {
  df_f <- df |> dedupe_items() |> remove_no_var_items()
  df_w <- to_mirt_shape(df_f)
  # align guess to columns BY NAME (fscores matches by position -- never trust order)
  guess_map <- df_f |> distinct(item_inst, chance) |> deframe()
  guess <- unname(guess_map[colnames(df_w)])
  list(filtered = df_f, wide = df_w, guess = guess)
}

fit_cached <- function(name, expr) {
  f <- file.path(cache_dir, paste0(name, ".rds"))
  if (file.exists(f)) return(read_rds(f))
  message("fitting ", name, " ...")
  mod <- force(expr)
  write_rds(mod, f, compress = "gz")
  mod
}

fit_uni <- function(dat, itemtype, priors, by_node = FALSE, name) {
  p <- prep(dat)
  ms <- gen_model_str(p$filtered, p$wide, itemtype, priors, by_node)
  mod <- fit_cached(name, mirt(data = p$wide, itemtype = itemtype,
                               model = mirt.model(ms), guess = p$guess,
                               SE = FALSE, verbose = FALSE,
                               technical = list(NCYCLES = 5000)))
  list(mod = mod, prep = p)
}

extract_results <- function(fit, name) {
  mod <- fit$mod
  nf <- extract.mirt(mod, "nfact")
  fs <- fscores(mod, method = "EAP", full.scores.SE = TRUE, verbose = FALSE)
  sc <- as_tibble(fs) |> mutate(run_id = rownames(fit$prep$wide), .before = 1)
  fnames <- colnames(fs)[1:nf]
  emp_rxx <- map_dbl(fnames, \(f) {
    th <- fs[, f]; se <- fs[, paste0("SE_", f)]
    var(th) / (var(th) + mean(se^2))
  }) |> set_names(fnames)
  mrxx <- if (nf == 1) tryCatch(marginal_rxx(mod), error = \(e) NA) else NA
  tibble(model = name, nfact = nf,
         converged = extract.mirt(mod, "converged"),
         AIC = extract.mirt(mod, "AIC"), BIC = extract.mirt(mod, "BIC"),
         npar = extract.mirt(mod, "nest"),
         factor = fnames, emp_rxx = unname(emp_rxx), marg_rxx = mrxx) |>
    mutate(scores = list(sc))
}

# ---- data -------------------------------------------------------------------
sds_trials <- read_rds(file.path(sds_dir, "data/trials_sds_coded.rds"))

sds_sub <- sds_trials |>
  mutate(match_k = as.integer(str_sub(item_group, 1, 1)),
         prev_m = as.integer(str_extract(item_status, "[0-9](?=m)")),
         n_pairs = match_k * (match_k + 1) / 2,
         correct = match & new,
         chance_correct = (match_k - prev_m) / n_pairs,
         site = group |> str_remove("_bogota$|_rural$|_main$"))

# corpus chance values that production metadata carries for these items
corpus_chance <- tribble(
  ~item_uid, ~chance_corpus,
  "sds_2match_choice1", 0.5,
  "sds_2match_choice2", 0.75,
  "sds_3match_choice1", 0, "sds_3match_choice2", 0, "sds_3match_choice3", 0,
  "sds_4match_choice1", 0, "sds_4match_choice2", 0, "sds_4match_choice3", 0,
  "sds_4match_choice4", 0)

data_m0 <- sds_sub |>
  mutate(item_uid = glue("sds_{item_group}_{item_choice}") |> as.character()) |>
  left_join(corpus_chance, by = "item_uid") |>
  mutate(chance = chance_corpus) |>
  select(site, run_id, item_uid, correct, chance)
data_m0c <- data_m0 |>
  left_join(sds_sub |>
              mutate(item_uid = glue("sds_{item_group}_{item_choice}") |> as.character()) |>
              summarise(chance_mean = mean(chance_correct), .by = item_uid),
            by = "item_uid") |>
  mutate(chance = chance_mean) |> select(-chance_mean)
data_m0z <- data_m0 |> mutate(chance = 0)
data_m1 <- sds_sub |>
  mutate(item_uid = glue("sds_{item_group}_{item_choice}_{item_status}") |> as.character(),
         chance = chance_correct) |>
  select(site, run_id, item_uid, correct, chance)
data_m1z <- data_m1 |> mutate(chance = 0)

# tree long data (mapping sequential pair choices onto match / new-given-match /
# new-given-non-match nodes; see sds_tree.qmd)
sds_tree <- sds_trials |>
  mutate(value_newM = if_else(match, new, NA),
         value_newN = if_else(!match, new, NA)) |>
  rename(value_match = match) |>
  select(-new, -chance_new) |>
  pivot_longer(cols = c(starts_with("chance"), starts_with("value")),
               names_to = c(".value", "node"), names_sep = "_") |>
  filter(!is.na(value)) |>
  # drop deterministic nodes: guaranteed-new (chance 1) and impossible-new (chance 0)
  filter(chance != 1, chance != 0) |>
  mutate(item = glue("{item_group}_{item_status}") |> as.character(),
         site = group |> str_remove("_bogota$|_rural$|_main$")) |>
  arrange(group, run_id, item_group, trial_index, node)
write_rds(sds_tree, file.path(cache_dir, "sds_tree.rds"), compress = "gz")

data_t1 <- sds_tree |>
  mutate(item_uid = paste(node, item, sep = ".")) |>
  rename(correct = value) |>
  select(site, run_id, node, item_uid, correct, chance)
data_t1z <- data_t1 |> mutate(chance = 0)
# 2-node: one "new" factor over newM+newN pseudo-items (which stay distinct items)
data_t2 <- data_t1 |>
  mutate(item_uid = paste0(if_else(node == "match", "match", "new"), ".",
                           str_remove(item_uid, "^[A-Za-z]+\\."),
                           if_else(node == "newN", "_nn", "")),
         node = if_else(node == "match", "match", "new"))
data_t2z <- data_t2 |> mutate(chance = 0)

# ---- pooled fits ------------------------------------------------------------
priors_rasch  <- list(d = c("norm", 0, 3))
priors_2pl_1f <- list(d = c("norm", 0, 3), a1 = c("norm", 1, 0.3))
priors_2pl_2f <- list(d = c("norm", 0, 3), a1 = c("norm", 1, 0.3), a2 = c("norm", 1, 0.3))

fits <- list(
  m0_rasch  = fit_uni(data_m0,  "Rasch", priors_rasch,  FALSE, "m0_rasch"),
  m0_2pl    = fit_uni(data_m0,  "2PL",   priors_2pl_1f, FALSE, "m0_2pl"),
  m0c_rasch = fit_uni(data_m0c, "Rasch", priors_rasch,  FALSE, "m0c_rasch"),
  m0z_rasch = fit_uni(data_m0z, "Rasch", priors_rasch,  FALSE, "m0z_rasch"),
  m1_rasch  = fit_uni(data_m1,  "Rasch", priors_rasch,  FALSE, "m1_rasch"),
  m1_2pl    = fit_uni(data_m1,  "2PL",   priors_2pl_1f, FALSE, "m1_2pl"),
  m1z_rasch = fit_uni(data_m1z, "Rasch", priors_rasch,  FALSE, "m1z_rasch"),
  m1z_2pl   = fit_uni(data_m1z, "2PL",   priors_2pl_1f, FALSE, "m1z_2pl"),
  t1_rasch  = fit_uni(data_t1,  "Rasch", priors_rasch,  FALSE, "t1_rasch"),
  t1_2pl    = fit_uni(data_t1,  "2PL",   priors_2pl_1f, FALSE, "t1_2pl"),
  t1z_rasch = fit_uni(data_t1z, "Rasch", priors_rasch,  FALSE, "t1z_rasch"),
  t1z_2pl   = fit_uni(data_t1z, "2PL",   priors_2pl_1f, FALSE, "t1z_2pl"),
  t2_rasch  = fit_uni(data_t2,  "Rasch", priors_rasch,  TRUE,  "t2_rasch"),
  t2_2pl    = fit_uni(data_t2,  "2PL",   priors_2pl_2f, TRUE,  "t2_2pl"),
  t2z_rasch = fit_uni(data_t2z, "Rasch", priors_rasch,  TRUE,  "t2z_rasch"))

results_pooled <- imap(fits, extract_results) |> list_rbind()
write_rds(results_pooled, file.path(cache_dir, "results_pooled.rds"), compress = "gz")

# ---- multigroup scalar fits (by site), production-style ---------------------
invariance_scalar <- c("free_means", "free_var", "intercepts", "slopes")

fit_mg <- function(dat, itemtype, priors, name) {
  f <- file.path(cache_dir, paste0(name, ".rds"))
  df_f <- dat |> dedupe_items() |>
    group_by(item_inst) |> filter(n_distinct(correct) > 1) |>
    group_by(item_inst, site) |> mutate(ncat_g = n_distinct(correct)) |>
    group_by(item_inst) |> filter(n_distinct(ncat_g) == 1, all(ncat_g > 1)) |> ungroup()
  df_w <- df_f |> mutate(correct = as.numeric(correct)) |>
    select(run_id, site, item_inst, correct) |>
    pivot_wider(names_from = item_inst, values_from = correct) |>
    column_to_rownames("run_id")
  groups <- df_w$site
  df_w2 <- df_w |> select(-site)
  guess_map <- df_f |> distinct(item_inst, chance) |> deframe()
  guess <- unname(guess_map[colnames(df_w2)])
  ms <- gen_model_str(df_f, df_w2, itemtype, priors, by_node = FALSE)
  if (file.exists(f)) mod <- read_rds(f)
  else {
    message("fitting ", name, " ...")
    mod <- multipleGroup(data = df_w2, itemtype = itemtype,
                         model = mirt.model(ms), group = groups,
                         invariance = invariance_scalar, guess = guess,
                         verbose = FALSE, technical = list(NCYCLES = 5000))
    write_rds(mod, f, compress = "gz")
  }
  fs <- fscores(mod, method = "EAP", full.scores.SE = TRUE, verbose = FALSE)
  sc <- as_tibble(fs) |> mutate(run_id = rownames(df_w2), .before = 1)
  emp <- var(fs[, 1]) / (var(fs[, 1]) + mean(fs[, 2]^2))
  tibble(model = name, nfact = 1, converged = extract.mirt(mod, "converged"),
         factor = "F1", emp_rxx = emp, scores = list(sc))
}

results_mg <- bind_rows(
  fit_mg(data_m1z, "Rasch", priors_rasch,  "mg_m1z_rasch_scalar"),
  fit_mg(data_m1z, "2PL",   priors_2pl_1f, "mg_m1z_2pl_scalar"),
  fit_mg(data_t1z, "Rasch", priors_rasch,  "mg_t1z_rasch_scalar"))
write_rds(results_mg, file.path(cache_dir, "results_mg.rds"), compress = "gz")

# ---- production recode bug: order-variant repeat coding ---------------------
# levantemodels::recode_sds() codes a response "new" iff its raw response string
# hasn't occurred earlier in the trial; the exploration recode sorts the parsed
# cards first. Reconstruct both codings and quantify + refit.
raw <- read_rds(file.path(sds_dir, "data/trials_sds.rds"))
parse_response <- function(resp) {
  if (is.na(resp) || str_trim(resp) == "") return(character(0))
  fromJSON(str_replace_all(resp, "'", '"'), simplifyVector = TRUE) |>
    as.character() |> unname()
}
cmp <- raw |> filter(item_task == "sds", str_detect(item_group, "match")) |>
  filter(response != "{}", str_count(response, ":") == 2) |>
  group_by(run_id, item_group) |> arrange(timestamp, .by_group = TRUE) |>
  mutate(trial_index = cumsum(item == "choice1")) |> ungroup() |>
  filter(trial_index > 0) |>
  group_by(run_id, item_group, trial_index) |>
  mutate(resp_norm = map_chr(response, \(r) paste(sort(parse_response(r)), collapse = " ")),
         new_raw  = map_lgl(row_number(), \(i) i == 1 || !(response[i] %in% response[1:(i-1)])),
         new_sort = map_lgl(row_number(), \(i) i == 1 || !(resp_norm[i] %in% resp_norm[1:(i-1)])),
         choice_i = row_number()) |>
  ungroup() |>
  inner_join(sds_trials |>
               mutate(choice_i = as.integer(str_extract(item_choice, "[0-9]"))) |>
               select(run_id, item_group, trial_index, choice_i, match, new),
             by = c("run_id", "item_group", "trial_index", "choice_i"))

data_m0bug <- cmp |>
  mutate(correct = match & new_raw,
         item_uid = glue("sds_{item_group}_choice{choice_i}") |> as.character(),
         chance = 0) |>
  select(run_id, item_uid, correct, chance)
fit_bug <- fit_uni(data_m0bug, "Rasch", priors_rasch, FALSE, "m0bug_rasch")
sc_bug <- fscores(fit_bug$mod, method = "EAP", verbose = FALSE)
bug_scores <- tibble(run_id = rownames(fit_bug$prep$wide), theta_bug = sc_bug[, 1]) |>
  inner_join(results_pooled |> filter(model == "m0z_rasch") |> pull(scores) |>
               pluck(1) |> select(run_id, theta_fix = F1),
             by = "run_id")
write_rds(list(cmp = cmp |> select(run_id, dataset, item_group, trial_index,
                                   choice_i, match, new_raw, new_sort),
               bug_scores = bug_scores),
          file.path(cache_dir, "bug_impact.rds"), compress = "gz")

message("done: all fits cached in ", cache_dir)
