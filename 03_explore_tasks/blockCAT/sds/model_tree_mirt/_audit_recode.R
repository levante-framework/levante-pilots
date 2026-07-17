# Independent audit of _recode_sds.R correctness coding
# ------------------------------------------------------------------
# Written by Claude Code (Anthropic), directed by Mike Frank, July 2026.
# Reimplements the SDS match/new/correct coding from raw trials with
# deliberately different code (regex parsing instead of JSON, direct
# attribute-table matching instead of the transpose/table helpers), then
# compares against data/trials_sds_coded.rds cell by cell. Also validates
# the stimulus grammar, the status counts, and the chance formulas
# (by Monte Carlo). Results cached in report_cache/audit.rds.

suppressMessages({
  library(dplyr); library(tidyr); library(readr); library(purrr)
  library(stringr); library(tibble)
})

sds_dir <- here::here("03_explore_tasks/blockCAT/sds")
cache_dir <- file.path(sds_dir, "model_tree_mirt/report_cache")

raw <- read_rds(file.path(sds_dir, "data/trials_sds.rds"))
coded <- read_rds(file.path(sds_dir, "data/trials_sds_coded.rds"))

# ---- 1. stimulus grammar validation -----------------------------------------
# every stimulus token in every match-block response/distractor should parse as
# size-color-shape[-number][-texture]
stim_regex <- "^(sm|med|lg)-(yellow|blue|red|green)-(triangle|circle|square|star)(-[0-9])?(-(gray|black|striped))?$"
extract_tokens <- \(s) str_match_all(s, "'[0-9]+': '([^']*)'")[[1]][, 2]

match_raw <- raw |> filter(item_task == "sds", str_detect(item_group, "match"))
all_tokens <- c(unlist(map(match_raw$response, extract_tokens)),
                unlist(map(match_raw$distractors, extract_tokens)))
bad_tokens <- unique(all_tokens[!str_detect(all_tokens, stim_regex)])
grammar_check <- list(n_tokens = length(all_tokens), bad_tokens = bad_tokens)

# ---- 2. independent recompute of match / new / correct ----------------------
# attribute coder: positional size/color/shape, then optional number + texture
decode_stim <- function(stim) {
  parts <- str_split_1(stim, "-")
  num <- parts[str_detect(parts, "^[0-9]$")]
  tex <- parts[parts %in% c("gray", "black", "striped")]
  tibble(size = parts[1], color = parts[2], shape = parts[3],
         number = if (length(num)) num else "1",
         texture = if (length(tex)) tex else "white")
}

# a selected pair "matches" iff the two cards share a value on >= 1 dimension
# that is NOT constant across the whole card set
audit_trial <- function(resps, opts_str) {
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
  # matching pairs in the card set, per varying dimension
  n_match_pairs <- sum(map_dbl(varying, \(d) sum(choose(table(opts_attrs[[d]]), 2))))
  tibble(choice_i = seq_along(resps), match_a = match_i, new_a = new_i,
         n_matches_a = n_match_pairs)
}

# NB: index trials BEFORE dropping invalid responses (as the local recode
# does), then drop whole trials containing them — filtering rows first would
# shift trial indices whenever a dropped trial contains a choice1 row
audited <- match_raw |>
  group_by(run_id, item_group) |> arrange(timestamp, .by_group = TRUE) |>
  mutate(trial_index = cumsum(item == "choice1")) |> ungroup() |>
  filter(trial_index > 0) |>
  group_by(run_id, item_group, trial_index) |>
  filter(!any(response == "{}"), !any(str_count(response, ":") != 2)) |>
  filter(n_distinct(distractors) == 1) |>
  arrange(timestamp, .by_group = TRUE) |>
  summarise(audit = list(audit_trial(response, distractors[1])), .groups = "drop") |>
  unnest(audit)

# independent status counts: distinct previously-selected matching/non-matching pairs
audited <- audited |>
  group_by(run_id, item_group, trial_index) |>
  mutate(prev_m_a = lag(cumsum(match_a & new_a), default = 0),
         prev_n_a = lag(cumsum(!match_a & new_a), default = 0)) |>
  ungroup()

cmp <- coded |>
  mutate(choice_i = as.integer(str_extract(item_choice, "[0-9]")),
         prev_m = as.integer(str_extract(item_status, "[0-9](?=m)")),
         prev_n = as.integer(str_extract(item_status, "[0-9](?=n)"))) |>
  inner_join(audited, by = c("run_id", "item_group", "trial_index", "choice_i"))

agreement <- cmp |>
  summarise(n = n(),
            match_agree = mean(match == match_a),
            new_agree = mean(new == new_a),
            correct_agree = mean((match & new) == (match_a & new_a)),
            status_m_agree = mean(prev_m == prev_m_a),
            status_n_agree = mean(prev_n == prev_n_a),
            n_matches_agree = mean(as.integer(str_sub(item_group, 1, 1)) == n_matches_a),
            coverage = n() / nrow(coded))

# ---- 3. Monte Carlo validation of the chance formulas -----------------------
# guessing model: choose uniformly among all C(k+1, 2) pairs, with replacement,
# k selections per trial; check P(match), P(new | match, status), P(new | non-match, status)
mc_chance <- function(k, n_sim = 200000) {
  n_cards <- k + 1
  pairs <- combn(n_cards, 2)
  n_pairs <- ncol(pairs)
  # matching pairs: in SDS sets, each of the k matching pairs is distinct; label
  # pairs 1..k as matches (which pairs they are doesn't matter by symmetry)
  is_match <- c(rep(TRUE, k), rep(FALSE, n_pairs - k))
  sims <- map(1:n_sim, \(s) sample(n_pairs, k, replace = TRUE))
  recs <- map(sims, \(sel) {
    prev <- integer(0)
    map(seq_along(sel), \(i) {
      p <- sel[i]
      pm <- sum(is_match[unique(prev[prev %in% which(is_match)])] |> length(), na.rm = TRUE)
      prev_uniq <- unique(prev)
      out <- tibble(choice = i, match = is_match[p], new = !(p %in% prev),
                    prev_m = sum(is_match[prev_uniq]), prev_n = sum(!is_match[prev_uniq]))
      prev <<- c(prev, p)
      out
    }) |> list_rbind()
  }) |> list_rbind()
  recs |> mutate(status = paste0(prev_m, "m", prev_n, "n")) |>
    group_by(status) |>
    summarise(p_match_mc = mean(match),
              p_newM_mc = mean(new[match]),
              p_newN_mc = mean(new[!match]),
              n_mc = n(), .groups = "drop") |>
    mutate(match_k = k,
           n_pairs = n_pairs,
           prev_m = as.integer(str_extract(status, "[0-9](?=m)")),
           prev_n = as.integer(str_extract(status, "[0-9](?=n)")),
           p_match_formula = k / n_pairs,
           p_newM_formula = 1 - prev_m / k,
           p_newN_formula = 1 - prev_n / (n_pairs - k))
}
set.seed(1)
mc <- map(2:4, \(k) mc_chance(k, 50000)) |> list_rbind()

write_rds(list(grammar = grammar_check, agreement = agreement,
               cmp_disagreements = cmp |> filter(match != match_a | new != new_a |
                                                   prev_m != prev_m_a | prev_n != prev_n_a),
               mc = mc),
          file.path(cache_dir, "audit.rds"), compress = "gz")

print(grammar_check)
print(agreement)
print(mc |> select(match_k, status, starts_with("p_")) |> arrange(match_k, status), n = 30)
