# class definition
ModelRecord <- setClass(
  "ModelRecord",
  slots = c(
    model_class   = "character",  # "SingleGroupClass", "MultipleGroupClass"
    model_vals    = "data.frame", # mod2vals() output
    itemtype      = "character",  # "Rasch", "2PL", "3PL"
    # model         = "mirt.model",
    method        = "character",
    data          = "matrix",
    factors       = "character",
    nfact         = "numeric",
    invariance    = "character",
    items         = "character",
    runs          = "character",
    group_names   = "character",
    groups        = "character",
    scores        = "tbl_df",
    fit           = "list"
  )
)

# constructor helper
modelrecord <- \(mod, row_ids) {
  new("ModelRecord",
      model_class = as.character(class(mod)),
      model_vals = as.data.frame(mod2values(mod)), # actually mirt_df but S4 doesn't like that
      itemtype = unique(extract.mirt(mod, "itemtype")),
      # model = extract.mirt(mod, "model"),
      method = unique(extract.mirt(mod, "method")),
      data = extract.mirt(mod, "data"),
      factors = extract.mirt(mod, "factorNames"),
      nfact = extract.mirt(mod, "nfact"),
      invariance = extract.mirt(mod, "invariance"),
      items = extract.mirt(mod, "itemnames"),
      runs = row_ids,
      group_names = extract.mirt(mod, "groupNames"),
      groups = as.character(extract.mirt(mod, "group")),
      scores = extract_scores(mod, row_ids),
      fit = mod@Fit
  )
}

# helper to extract and tidy scores
extract_scores <- \(mod, row_ids) {
  fscores(mod, method = "EAP", full.scores.SE = TRUE) |>
    as_tibble() |>
    rename(ability = F1, se = SE_F1) |>
    mutate(run_id = row_ids, .before = everything())
}

# method for show generic
setMethod("show", "ModelRecord", \(object) {
  cat(glue("{is(object)[[1]]} of a {object@model_class} model ({object@nfact} factor {object@itemtype})"))
})

# method for AIC generic
setMethod("AIC", "ModelRecord", \(object) {
  object@fit$AIC
})

# method for BIC generic
setMethod("BIC", "ModelRecord", \(object) {
  object@fit$BIC
})

# method for logLik generic
setMethod("logLik", "ModelRecord", \(object) {
  object@fit$logLik
})

# accessor functions for slots
model_class <- \(object) object@model_class
model_vals <- \(object) object@model_vals
items <- \(object) object@items
scores <- \(object) object@scores
tabdata <- \(object) object@tabdata
