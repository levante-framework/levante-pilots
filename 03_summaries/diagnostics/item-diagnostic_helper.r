
# ---------- helpers ----------
pbis_corrected <- function(mat){
  pbs <- numeric(ncol(mat))
  for(j in seq_len(ncol(mat))){
    xj   <- mat[, j]
    totj <- rowSums(mat[, -j, drop = FALSE], na.rm = TRUE)
    pbs[j] <- suppressWarnings(cor(xj, totj, use = "pairwise.complete.obs"))
  }
  setNames(pbs, colnames(mat))
}

normalize_fit <- function(x){
  if (is.null(x)) return(tibble())
  df <- tryCatch(as.data.frame(x), error = function(e) NULL)
  if (is.null(df)) return(tibble())
  if (!"item" %in% names(df)) {
    rn <- rownames(df)
    df$item <- if (!is.null(rn)) rn else paste0("item_", seq_len(nrow(df)))
    rownames(df) <- NULL
  }
  as_tibble(df)
}

get_infit <- function(mod){
  out <- tryCatch(mirt::itemfit(mod, na.rm = TRUE, fit_stats = "infit"), error = function(e) NULL)
  out <- normalize_fit(out)
  if (!nrow(out)) out <- tibble(item = character())
  if (!"infit"   %in% names(out)) out$infit    <- NA_real_
  if (!"z.infit" %in% names(out)) out$`z.infit` <- NA_real_
  out %>% select(item, infit, `z.infit`)
}

get_sx2 <- function(mod){
  out <- tryCatch(mirt::itemfit(mod, na.rm = TRUE), error = function(e) NULL)
  out <- normalize_fit(out)
  if (!nrow(out)) out <- tibble(item = character())
  for (nm in c("S_X2","df.S_X2","RMSEA.S_X2","p.S_X2")) {
    if (!nm %in% names(out)) out[[nm]] <- NA_real_
  }
  out %>% select(item, S_X2, df.S_X2, RMSEA.S_X2, p.S_X2)
}

get_n_eff <- function(dat) sum(stats::complete.cases(dat))

# Model-level fit pulled from mirt object
get_model_fit <- function(mod){
  tibble(
    AIC    = tryCatch(mirt::extract.mirt(mod, "AIC"),    error = \(e) NA_real_),
    BIC    = tryCatch(mirt::extract.mirt(mod, "BIC"),    error = \(e) NA_real_),
    SABIC  = tryCatch(mirt::extract.mirt(mod, "SABIC"),  error = \(e) NA_real_),
    logLik = tryCatch(as.numeric(mirt::extract.mirt(mod, "logLik")),
                      error = \(e) NA_real_),
    npar   = tryCatch(as.numeric(mirt::extract.mirt(mod, "npar")),
                      error = \(e) NA_real_)
  )
}

# ---------- main per-row summarizer ----------
summarize_item_fit_with_meta <- function(task, model_set, subset, mod_rec, nfact, itemtype, invariance, mod){
  dat <- mod@Data$data
  
  # classical
  p_correct <- colMeans(dat, na.rm = TRUE)
  pbis      <- tryCatch(pbis_corrected(dat),
                        error = function(e) rep(NA_real_, ncol(dat)) |> setNames(colnames(dat)))
  classical <- tibble(
    item           = names(p_correct),
    p_correct      = unname(p_correct),
    point_biserial = unname(pbis)
  )
  
  # IRT
  infit_tbl <- get_infit(mod)
  sx2_tbl   <- get_sx2(mod)
  
  # align to classical item set (prevents join errors)
  infit_tbl <- right_join(infit_tbl, classical %>% select(item), by = "item")
  sx2_tbl   <- right_join(sx2_tbl,   classical %>% select(item), by = "item")
  
  n_eff_task   <- get_n_eff(dat)
  n_items_task <- ncol(dat)
  
  model_fit <- get_model_fit(mod)
  
  # merge & append metadata + model-level fit
  classical %>%
    left_join(infit_tbl, by = "item") %>%
    left_join(sx2_tbl,   by = "item") %>%
    mutate(
      task        = task,
      model_set   = model_set,
      subset      = subset,
      nfact       = nfact,
      itemtype    = itemtype,
      invariance  = invariance,
      n_eff_task  = n_eff_task,
      n_items     = n_items_task,
      AIC         = model_fit$AIC,
      BIC         = model_fit$BIC,
      SABIC       = model_fit$SABIC,
      logLik      = model_fit$logLik,
      npar        = model_fit$npar
    ) %>%
    relocate(task, model_set, subset, itemtype, invariance, nfact,
             n_eff_task, n_items, item)
}