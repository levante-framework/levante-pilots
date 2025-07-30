# class definition
ModelRecord <- setClass(
  "ModelRecord",
  slots = c(
    model_class   = "character",  # "SingleGroupClass", "MultipleGroupClass"
    model_vals    = "data.frame", # mod2vals() output
    itemtype      = "character",  # "Rasch", "2PL", "3PL"
    # model         = "mirt.model",
    method        = "character",
    tabdata       = "matrix",
    factors       = "character",
    nfact         = "numeric",
    invariance    = "character",
    loglikelihood = "numeric",
    bic           = "numeric",
    items         = "character",
    runs          = "character",
    groups        = "character"
  )
)

# constructor helper
modelrecord <- \(mod, row_ids) {
  new("ModelRecord",
      model_class = as.character(class(mod)),
      model_vals = as.data.frame(mod2values(mod)), # actually mirt_df but S4 doesn't like that
      itemtype = unique(extract.mirt(mod, "itemtype")),
      # model = unique(extract.mirt(mod, "model")),
      method = unique(extract.mirt(mod, "method")),
      tabdata = unique(extract.mirt(mod, "tabdata")),
      factors = extract.mirt(mod, "factorNames"),
      nfact = extract.mirt(mod, "nfact"),
      invariance = extract.mirt(mod, "invariance"),
      loglikelihood = extract.mirt(mod, "logLik"),
      bic = extract.mirt(mod, "BIC"),
      items = extract.mirt(mod, "itemnames"),
      runs = row_ids,
      groups = extract.mirt(mod, "groupNames")
  )
}

# method for show generic
setMethod("show", "ModelRecord", \(object) {
  cat(glue("{is(object)[[1]]} of a {object@model_class} model ({object@nfact} factor {object@itemtype})"))
})
