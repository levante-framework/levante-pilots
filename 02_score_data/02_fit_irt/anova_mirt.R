# modified version of anova for SingleGroupClass and MultipleGroupClass that can
# handle a list of models being passed in (original version can't due to how it
# tries to get model object names, see commented out lines)

anova_mirt_wrapper <- \(mods) {
  rlang::exec(anova_mirt, !!!mods)
}

anova_mirt <- function(object, object2, ...,
                       bounded = FALSE, mix = 0.5, frame = 1, verbose = FALSE){
  # if(frame > 1){
  #   nms1 <- deparse(substitute(object, env = parent.frame(frame+1)))
  #   nms2 <- deparse(substitute(object2, env = parent.frame(frame)))
  # } else {
  #   nms1 <- deparse(substitute(object, env = parent.frame()))
  #   # nms2 <- deparse(substitute(object2, env = parent.frame()))
  #   nms2 <- deparse(substitute(object2, env = environment()))
  # }
  dots <- list(...)
  if(length(dots)){
    # nms3 <- if(frame > 1)
    #   deparse(substitute(list(...), env = parent.frame(frame)))
    # # else deparse(substitute(list(...), env = parent.frame()))
    # else deparse(substitute(list(...), env = environment()))
    # print(nms3)
    # nms3 <- gsub("list\\(", "", nms3)
    # nms3 <- gsub(")", "", nms3)
    # nms3 <- strsplit(nms3, ", ")[[1]]
    dots <- c(object, object2, dots)
    ret <- vector('list', length(dots)-1L)
    for(i in 1L:length(ret)){
      ret[[i]] <- anova_mirt(dots[[i]], dots[[i+1L]], bounded=bounded,
                             mix=mix)
      if(i > 1L)
        ret[[i]] <- ret[[i]][2L, ]
    }
    ret <- do.call(rbind, ret)
    # rownames(ret) <- c(nms1, nms2, nms3)
    return(ret)
  }
  if(missing(object2)){
    hasPriors <- object@Fit$logPrior != 0
    ret <- data.frame(AIC = object@Fit$AIC,
                      SABIC = object@Fit$SABIC,
                      HQ = object@Fit$HQ,
                      BIC = object@Fit$BIC,
                      logLik = object@Fit$logLik)
    if(hasPriors)
      ret$logPost = object@Fit$logPrior + object@Fit$logLik
    ret <- as.mirt_df(ret)
    # rownames(ret) <- nms1
    return(ret)
  }
  df <- object@Fit$df - object2@Fit$df
  ret <- data.frame(AIC = c(object@Fit$AIC, object2@Fit$AIC),
                    SABIC = c(object@Fit$SABIC, object2@Fit$SABIC),
                    HQ = c(object@Fit$HQ, object2@Fit$HQ),
                    BIC = c(object@Fit$BIC, object2@Fit$BIC),
                    logLik = c(object@Fit$logLik, object2@Fit$logLik))
  if(any(object2@Fit$logPrior != 0 || object@Fit$logPrior != 0)){
    ret$logPost = c(object@Fit$logLik + object@Fit$logPrior,
                    object2@Fit$logLik + object2@Fit$logPrior)
    ret$df <- c(NA, abs(df))
  } else {
    X2 <- 2*object2@Fit$logLik - 2*object@Fit$logLik
    ret$X2 <- c(NA, X2)
    ret$df <- c(NA, df)
    ret$p <- c(NA, 1 - pchisq(X2,abs(df)))
    if(bounded)
      ret$p[2L] <- 1 - mixX2(X2, df=abs(df), mix=mix)
    ret$p[ret$X2 < 0] <- NaN
    ret$p[ret$df <= 0] <- NaN
  }
  # rownames(ret) <- c(nms1, nms2)
  ret <- as.mirt_df(ret)
  ret
}

as.mirt_df <- function(df){
  class(df) <- c('mirt_df', class(df))
  df
}
