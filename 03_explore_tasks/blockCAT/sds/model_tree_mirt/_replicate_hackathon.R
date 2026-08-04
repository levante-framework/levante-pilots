# Replication of the key SDS scoring comparisons on the unified hackathon
# dataset (levante_data_pilots_hackathon:fpx0:v1_1), fetched with the new
# `levante` package (remotes::install_github("levante-framework/levante-r")).
# ------------------------------------------------------------------
# Written by Claude Code (Anthropic), directed by Mike Frank, July 2026.
# The recode here is the AUDITED independent implementation from
# _audit_recode.R (verified to agree 100% with _recode_sds.R on the primary
# data), so this doubles as an end-to-end replication of recode + models.
# Fits cached in report_cache/ with the _hack suffix.

suppressMessages({
  library(dplyr); library(tidyr); library(readr); library(purrr)
  library(stringr); library(glue); library(mirt); library(tibble); library(boot)
})

sds_dir <- here::here("03_explore_tasks/blockCAT/sds")
cache_dir <- file.path(sds_dir, "model_tree_mirt/report_cache")

# fetch if not present (requires Redivis auth)
if (!file.exists(file.path(sds_dir, "data/trials_sds_hackathon.rds"))) {
  library(levante)
  trials <- get_trials("levante_data_pilots_hackathon:fpx0", version = "v1_1")
  write_rds(trials |> filter(item_task == "sds"),
            file.path(sds_dir, "data/trials_sds_hackathon.rds"), compress = "gz")
  scores <- get_scores("levante_data_pilots_hackathon:fpx0", version = "v1_1")
  write_rds(scores |> filter(task_code == "sds"),
            file.path(sds_dir, "data/runs_sds_hackathon.rds"), compress = "gz")
}
raw_h <- read_rds(file.path(sds_dir, "data/trials_sds_hackathon.rds"))
runs_h <- read_rds(file.path(sds_dir, "data/runs_sds_hackathon.rds"))

# ---- recode (audited independent implementation) ----------------------------
extract_tokens <- \(s) str_match_all(s, "'[0-9]+': '([^']*)'")[[1]][, 2]
decode_stim <- function(stim) {
  parts <- str_split_1(stim, "-")
  num <- parts[str_detect(parts, "^[0-9]$")]
  tex <- parts[parts %in% c("gray", "black", "striped")]
  tibble(size = parts[1], color = parts[2], shape = parts[3],
         number = if (length(num)) num else "1",
         texture = if (length(tex)) tex else "white")
}
code_trial <- function(resps, opts_str) {
  opts <- sort(extract_tokens(opts_str))
  opts_attrs <- map(opts, decode_stim) |> list_rbind()
  varying <- names(opts_attrs)[map_lgl(opts_attrs, \(v) n_distinct(v) > 1)]
  pair_norm <- map_chr(resps, \(r) paste(sort(extract_tokens(r)), collapse = "|"))
  match_i <- map_lgl(resps, \(r) {
    cards <- sort(extract_tokens(r))
    if (length(cards) != 2) return(NA)
    a <- decode_stim(cards[1]); b <- decode_stim(cards[2])
    any(map_lgl(varying, \(d) a[[d]] == b[[d]]))
  })
  new_i <- map_lgl(seq_along(pair_norm), \(i) i == 1 || !(pair_norm[i] %in% pair_norm[1:(i - 1)]))
  n_match_pairs <- sum(map_dbl(varying, \(d) sum(choose(table(opts_attrs[[d]]), 2))))
  tibble(choice_i = seq_along(resps), match = match_i, new = new_i,
         n_matches = n_match_pairs)
}

coded_h_file <- file.path(cache_dir, "coded_hackathon.rds")
if (file.exists(coded_h_file)) coded_h <- read_rds(coded_h_file) else {
  coded_h <- raw_h |>
    filter(str_detect(item_group, "match")) |>
    filter(!str_detect(response, "mittel|rote|gelb|blau|grün")) |>
    filter(!(dataset == "pilot_western_ca_main" & timestamp < "2025-02-21")) |>
    # index trials BEFORE dropping invalid responses (see _audit_recode.R)
    group_by(run_id, item_group) |> arrange(timestamp, .by_group = TRUE) |>
    mutate(trial_index = cumsum(item == "choice1")) |> ungroup() |>
    filter(trial_index > 0) |>
    group_by(run_id, item_group, trial_index) |>
    filter(!any(response == "{}"), !any(str_count(response, ":") != 2)) |>
    mutate(match_k = as.integer(str_sub(item_group, 1, 1))) |>
    filter(n() == unique(match_k), n_distinct(distractors) == 1) |>
    arrange(timestamp, .by_group = TRUE) |>
    summarise(dataset = dataset[1], match_k = match_k[1],
              coded = list(code_trial(response, distractors[1])), .groups = "drop") |>
    unnest(coded) |>
    filter(match_k == n_matches) |>
    group_by(run_id, item_group, trial_index) |>
    mutate(prev_m = lag(cumsum(match & new), default = 0),
           prev_n = lag(cumsum(!match & new), default = 0)) |>
    ungroup() |>
    mutate(item_status = paste0(prev_m, "m", prev_n, "n"),
           item_choice = paste0("choice", choice_i),
           n_pairs = match_k * (match_k + 1) / 2,
           chance_correct = (match_k - prev_m) / n_pairs,
           correct = match & new)
  write_rds(coded_h, coded_h_file, compress = "gz")
}
message("hackathon coded: ", nrow(coded_h), " selections, ",
        n_distinct(coded_h$run_id), " runs")

# ---- model datasets ----------------------------------------------------------
corpus_chance <- tribble(
  ~item_uid, ~chance_corpus,
  "sds_2match_choice1", 0.5, "sds_2match_choice2", 0.75,
  "sds_3match_choice1", 0, "sds_3match_choice2", 0, "sds_3match_choice3", 0,
  "sds_4match_choice1", 0, "sds_4match_choice2", 0, "sds_4match_choice3", 0,
  "sds_4match_choice4", 0)

data_m0_h <- coded_h |>
  mutate(item_uid = glue("sds_{item_group}_{item_choice}") |> as.character()) |>
  left_join(corpus_chance, by = "item_uid") |>
  mutate(chance = chance_corpus) |>
  select(run_id, item_uid, correct, chance)
data_m0z_h <- data_m0_h |> mutate(chance = 0)
data_m1_h <- coded_h |>
  mutate(item_uid = glue("sds_{item_group}_{item_choice}_{item_status}") |> as.character(),
         chance = chance_correct) |>
  select(run_id, item_uid, correct, chance)
data_m1z_h <- data_m1_h |> mutate(chance = 0)
data_gate_h <- raw_h |>
  filter(item_group %in% c("same", "dimensions"), run_id %in% unique(coded_h$run_id)) |>
  mutate(chance = replace_na(chance, 0)) |>
  select(run_id, item_uid, correct, chance) |> filter(!is.na(correct))
data_m1z_gate_h <- bind_rows(data_m1z_h, data_gate_h)

# tree (2 correlated factors, no floors): match node + new nodes
tree_h <- coded_h |>
  mutate(value_match = match,
         value_new = if_else(rep(TRUE, n()), new, NA),
         chance_newM = 1 - prev_m / match_k,
         chance_newN = 1 - prev_n / (n_pairs - match_k)) |>
  (\(d) bind_rows(
    d |> mutate(node = "match", value = match,
                item_uid = glue("match.{item_group}_{item_status}") |> as.character()),
    d |> filter(match, chance_newM < 1, chance_newM > 0) |>
      mutate(node = "new", value = new,
             item_uid = glue("new.{item_group}_{item_status}") |> as.character()),
    d |> filter(!match, chance_newN < 1, chance_newN > 0) |>
      mutate(node = "new", value = new,
             item_uid = glue("new.{item_group}_{item_status}_nn") |> as.character())))() |>
  mutate(correct = value, chance = 0) |>
  select(run_id, node, item_uid, correct, chance)

# ---- fitting machinery (as in _fit_sds_extensions.R) -------------------------
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
  } else factors <- list(glue("1-{ncol(df_prepped)}")) |> set_names("F1")
  constraints <- items |> map(\(iu) {
    prefixes <- str_sub(colnames(df_prepped), 1, str_length(iu) + 1)
    matched_idx <- which(prefixes == paste0(iu, "-"))
    if (length(matched_idx) > 1) glue("({paste_c(matched_idx)},d)")
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
fit_uni <- function(dat, name, by_node = FALSE, free_cov = FALSE) {
  df_f <- dat |> dedupe_items() |> remove_no_var_items()
  df_w <- to_mirt_shape(df_f)
  guess_map <- df_f |> distinct(item_inst, chance) |> deframe()
  guess <- unname(guess_map[colnames(df_w)])
  f <- file.path(cache_dir, paste0(name, ".rds"))
  ms <- gen_model_str(df_f, df_w, "Rasch", list(d = c("norm", 0, 3)), by_node, free_cov)
  if (file.exists(f)) mod <- read_rds(f) else {
    message("fitting ", name, " ...")
    mod <- mirt(data = df_w, itemtype = "Rasch", model = mirt.model(ms),
                guess = guess, SE = FALSE, verbose = FALSE,
                technical = list(NCYCLES = 5000))
    write_rds(mod, f, compress = "gz")
  }
  nf <- extract.mirt(mod, "nfact")
  fs <- fscores(mod, method = "EAP", full.scores.SE = TRUE, verbose = FALSE)
  sc <- as_tibble(fs) |> mutate(run_id = rownames(df_w), .before = 1)
  fnames <- colnames(fs)[1:nf]
  emp <- map_dbl(fnames, \(fn) {
    th <- fs[, fn]; se <- fs[, paste0("SE_", fn)]
    var(th) / (var(th) + mean(se^2))
  })
  tibble(model = name, factor = fnames, converged = extract.mirt(mod, "converged"),
         BIC = extract.mirt(mod, "BIC"), emp_rxx = emp) |>
    mutate(scores = list(sc))
}

results_hack <- bind_rows(
  fit_uni(data_m0_h,       "hack_m0_rasch"),
  fit_uni(data_m0z_h,      "hack_m0z_rasch"),
  fit_uni(data_m1_h,       "hack_m1_rasch"),
  fit_uni(data_m1z_h,      "hack_m1z_rasch"),
  fit_uni(data_m1z_gate_h, "hack_m1z_gate_rasch"),
  fit_uni(tree_h,          "hack_t2zc_rasch", by_node = TRUE, free_cov = TRUE))

# ---- evaluation ---------------------------------------------------------------
rt_h <- raw_h |> group_by(run_id) |> summarise(t_start = min(timestamp))
meta_h <- runs_h |> select(run_id, user_id, dataset, age) |> left_join(rt_h, by = "run_id")
sc_h <- results_hack |> select(model, factor, scores) |>
  pmap(\(model, factor, scores) scores |> select(run_id, value = all_of(factor)) |>
         mutate(model = model, factor = factor)) |>
  list_rbind() |> left_join(meta_h, by = "run_id")
age_h <- sc_h |> group_by(model, factor) |>
  summarise(r_age = cor(value, age, use = "complete.obs"), .groups = "drop")
retest_h <- sc_h |> filter(dataset == "pilot_mpieva_de_main") |>
  group_by(model, factor, user_id) |> filter(n() == 2) |>
  arrange(t_start, .by_group = TRUE) |>
  summarise(t1 = value[1], t2 = value[2], .groups = "drop") |>
  group_by(model, factor) |>
  summarise(r_retest = cor(t1, t2), n_retest = n(), .groups = "drop")
eval_hack <- results_hack |> select(model, factor, converged, BIC, emp_rxx) |>
  left_join(age_h, by = c("model", "factor")) |>
  left_join(retest_h, by = c("model", "factor"))
write_rds(list(eval = eval_hack, coded_n = nrow(coded_h),
               runs_n = n_distinct(coded_h$run_id)),
          file.path(cache_dir, "results_hack.rds"), compress = "gz")
print(eval_hack)
