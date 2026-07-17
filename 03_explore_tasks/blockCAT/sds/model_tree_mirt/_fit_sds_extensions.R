# Extension fits for sds_scoring_report.qmd: correlated-factor trees, 3-node
# tree, feature-signature items, feature-knowledge ("gate") items, and the
# multigroup invariance ladder.
# ------------------------------------------------------------------
# Written by Claude Code (Anthropic), directed by Mike Frank, July 2026.
# Run after (or it will source) _fit_sds_report_models.R. Cached in report_cache/.

suppressMessages({
  library(dplyr); library(tidyr); library(readr); library(purrr)
  library(stringr); library(glue); library(mirt); library(tibble)
})

sds_dir <- here::here("03_explore_tasks/blockCAT/sds")
cache_dir <- file.path(sds_dir, "model_tree_mirt/report_cache")
if (!file.exists(file.path(cache_dir, "results_pooled.rds"))) {
  source(file.path(sds_dir, "model_tree_mirt/_fit_sds_report_models.R"))
}

# ---- helpers (as in _fit_sds_report_models.R, plus COV support) -------------
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

gen_model_str <- function(df, df_prepped, item_type, priors = NULL,
                          by_node = FALSE, free_cov = FALSE) {
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
  cov_str <- if (free_cov && length(factors) > 1) {
    pairs <- combn(names(factors), 2)
    paste0("COV = ", paste(map_chr(1:ncol(pairs), \(j) paste(pairs[, j], collapse = "*")),
                           collapse = ", "))
  } else ""
  factor_str <- imap_chr(factors, \(idx, nm) glue("{nm} = {idx}")) |> unname()
  paste(c(factor_str, constraint, cov_str, prior), collapse = "\n")
}

prep <- function(df) {
  df_f <- df |> dedupe_items() |> remove_no_var_items()
  df_w <- to_mirt_shape(df_f)
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
fit_uni <- function(dat, itemtype, priors, by_node = FALSE, name, free_cov = FALSE) {
  p <- prep(dat)
  ms <- gen_model_str(p$filtered, p$wide, itemtype, priors, by_node, free_cov)
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
sds_tree <- read_rds(file.path(cache_dir, "sds_tree.rds"))
raw <- read_rds(file.path(sds_dir, "data/trials_sds.rds"))

data_m1z <- sds_trials |>
  mutate(correct = match & new,
         item_uid = glue("sds_{item_group}_{item_choice}_{item_status}") |> as.character(),
         chance = 0,
         site = group |> str_remove("_bogota$|_rural$|_main$")) |>
  select(site, run_id, item_uid, correct, chance)
data_m0z <- sds_trials |>
  mutate(correct = match & new,
         item_uid = glue("sds_{item_group}_{item_choice}") |> as.character(),
         chance = 0,
         site = group |> str_remove("_bogota$|_rural$|_main$")) |>
  select(site, run_id, item_uid, correct, chance)
data_t1z <- sds_tree |>
  mutate(item_uid = paste(node, item, sep = "."), chance = 0) |>
  rename(correct = value) |>
  select(run_id, node, item_uid, correct, chance)
data_t2z <- data_t1z |>
  mutate(item_uid = paste0(if_else(node == "match", "match", "new"), ".",
                           str_remove(item_uid, "^[A-Za-z]+\\."),
                           if_else(node == "newN", "_nn", "")),
         node = if_else(node == "match", "match", "new"))

# feature signature of each card set: which dimensions contain matching pairs
decode_card <- function(card) {
  c(size = str_extract(card, "^[SML]"), color = str_sub(card, 2, 2),
    shape = str_sub(card, 3, 3), number = str_extract(card, "[0-9]"),
    bg = str_sub(card, -1, -1))
}
stim_sig <- function(stim) {
  m <- do.call(rbind, map(str_split_1(stim, "-"), decode_card))
  matched <- map_chr(colnames(m), \(d) {
    tab <- table(m[, d])
    if (length(tab) > 1 && sum(choose(tab, 2)) > 0) d else NA_character_
  })
  paste(sort(matched[!is.na(matched)]), collapse = "+")
}
sigs <- sds_trials |> distinct(item_stimulus) |>
  mutate(sig = map_chr(item_stimulus, stim_sig))

data_m1z_sig <- sds_trials |>
  left_join(sigs, by = "item_stimulus") |>
  mutate(correct = match & new,
         item_uid = glue("sds_{item_group}_{item_choice}_{item_status}_{sig}") |> as.character(),
         chance = 0,
         site = group |> str_remove("_bogota$|_rural$|_main$")) |>
  select(site, run_id, item_uid, correct, chance)

# gate items: same + dimensions blocks (released correctness, corpus floors),
# restricted to the runs in the match-block data
data_gate <- raw |>
  filter(item_group %in% c("same", "dimensions"),
         run_id %in% unique(sds_trials$run_id)) |>
  mutate(chance = replace_na(chance, 0),
         site = dataset |> str_remove("_bogota$|_rural$|_main$")) |>
  select(site, run_id, item_uid, correct, chance) |>
  filter(!is.na(correct))
data_m1z_gate <- bind_rows(data_m1z, data_gate)

# ---- fits -------------------------------------------------------------------
priors_rasch  <- list(d = c("norm", 0, 3))
priors_2pl <- list(d = c("norm", 0, 3), a1 = c("norm", 1, 0.3))

fits_ext <- list(
  # correlated-factor trees (no floors)
  t2zc_rasch = fit_uni(data_t2z, "Rasch", priors_rasch, TRUE, "t2zc_rasch", free_cov = TRUE),
  t3z_rasch  = fit_uni(data_t1z, "Rasch", priors_rasch, TRUE, "t3z_rasch",  free_cov = TRUE),
  # feature-signature items
  m1z_sig_rasch = fit_uni(data_m1z_sig, "Rasch", priors_rasch, FALSE, "m1z_sig_rasch"),
  # gate (feature-knowledge) items added
  m1z_gate_rasch = fit_uni(data_m1z_gate, "Rasch", priors_rasch, FALSE, "m1z_gate_rasch"))

results_ext <- imap(fits_ext, extract_results) |> list_rbind()

# factor correlations for the correlated trees
factor_cors <- map(c("t2zc_rasch", "t3z_rasch"), \(nm) {
  mod <- fits_ext[[nm]]$mod
  cv <- coef(mod, simplify = TRUE)$cov
  tibble(model = nm, cov = list(cov2cor(cv)))
}) |> list_rbind()

write_rds(results_ext, file.path(cache_dir, "results_ext.rds"), compress = "gz")
write_rds(factor_cors, file.path(cache_dir, "factor_cors.rds"), compress = "gz")

# ---- multigroup invariance ladder + sensitivity ------------------------------
invariances <- list(
  configural = "",
  metric = c("slopes"),
  scalar = c("free_means", "free_var", "intercepts", "slopes"))

fit_mg <- function(dat, itemtype, priors, name, invariance) {
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
                         invariance = invariances[[invariance]], guess = guess,
                         verbose = FALSE, technical = list(NCYCLES = 5000))
    write_rds(mod, f, compress = "gz")
  }
  fs <- fscores(mod, method = "EAP", full.scores.SE = TRUE, verbose = FALSE)
  sc <- as_tibble(fs) |> mutate(run_id = rownames(df_w2), .before = 1)
  emp <- var(fs[, 1]) / (var(fs[, 1]) + mean(fs[, 2]^2))
  tibble(model = name, invariance = invariance, itemtype = itemtype, nfact = 1,
         converged = extract.mirt(mod, "converged"),
         AIC = extract.mirt(mod, "AIC"), BIC = extract.mirt(mod, "BIC"),
         npar = extract.mirt(mod, "nest"), logLik = extract.mirt(mod, "logLik"),
         factor = "F1", emp_rxx = emp, scores = list(sc))
}

results_ladder <- bind_rows(
  fit_mg(data_m1z, "Rasch", priors_rasch, "mg_m1z_rasch_config", "configural"),
  fit_mg(data_m1z, "Rasch", priors_rasch, "mg_m1z_rasch_scalar2", "scalar"),
  fit_mg(data_m1z, "2PL", priors_2pl, "mg_m1z_2pl_config", "configural"),
  fit_mg(data_m1z, "2PL", priors_2pl, "mg_m1z_2pl_metric", "metric"),
  fit_mg(data_m1z, "2PL", priors_2pl, "mg_m1z_2pl_scalar2", "scalar"),
  # multigroup-sensitivity: does the mg setting change the structure comparison?
  fit_mg(data_m0z, "Rasch", priors_rasch, "mg_m0z_rasch_scalar", "scalar"))
write_rds(results_ladder, file.path(cache_dir, "results_ladder.rds"), compress = "gz")

# gate-augmented champion in production configuration
results_gate_mg <- bind_rows(
  fit_mg(data_m1z_gate, "Rasch", priors_rasch, "mg_m1z_gate_rasch_config", "configural"),
  fit_mg(data_m1z_gate, "Rasch", priors_rasch, "mg_m1z_gate_rasch_scalar", "scalar"))
write_rds(results_gate_mg, file.path(cache_dir, "results_gate_mg.rds"), compress = "gz")

print(results_ext |> select(-scores))
print(results_ladder |> select(-scores))
print(results_gate_mg |> select(-scores))
message("done")
