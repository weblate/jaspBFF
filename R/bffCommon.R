#' @import jaspBase

.bffGetR                     <- function(options, singleStudy = FALSE) {
  if (options[["priorR"]] == "automatic" && singleStudy)
    return(1)
  else if (options[["priorR"]] == "automatic" && !singleStudy)
    return(NULL)
  else
    return(options[["priorRManualValue"]])
}
.bffGetAlternativeHypothesis <- function(options) {
  return(switch(
    options[["alternativeHypothesis"]],
    "equal"    = "two.sided",
    "greater"  = "greater",
    "less"     = "less"
  ))
}
.bffGetBFTitle               <- function(options, maximum = TRUE) {

  bfType     <- options[["bayesFactorType"]]
  hypothesis <- options[["alternativeHypothesis"]]

  if (bfType == "BF10") {
    if (hypothesis == "equal") {
      bfTitle <- "BF\u2081\u2080"
    } else if (hypothesis == "greater") {
      bfTitle <- "BF\u208A\u2080"
    } else {
      bfTitle <- "BF\u208B\u2080"
    }
    if (maximum)
      bfTitle <- paste0("max(", bfTitle, ")")
  } else if (bfType == "LogBF10") {
    if (hypothesis == "equal") {
      bfTitle <- "\u0042\u0046\u2081\u2080"
    } else if (hypothesis == "greater") {
      bfTitle <- "\u0042\u0046\u208A\u2080"
    } else {
      bfTitle <- "\u0042\u0046\u208B\u2080"
    }
    if (maximum)
      bfTitle <- paste0("max[", bfTitle, "]")
    bfTitle <- paste0("Log(", bfTitle, ")")
  } else if (bfType == "BF01") {
    if (hypothesis == "equal") {
      bfTitle <- "BF\u2080\u2081"
    } else if (hypothesis == "greater") {
      bfTitle <- "BF\u2080\u208A"
    } else {
      bfTitle <- "BF\u2080\u208B"
    }
    if (maximum)
      bfTitle <- paste0("min(", bfTitle, ")")
  }
  return(bfTitle)
}
.bffSummaryTable             <- function(jaspResults, dataset, options, title, dependencies, ready) {

  if (!is.null(jaspResults[["summaryTable"]]))
    return()

  summaryTable <- createJaspTable(title = title)
  summaryTable$dependOn(c(dependencies, "bayesFactorType"))
  summaryTable$position <- 1
  jaspResults[["summaryTable"]] <- summaryTable

  # create empty table
  bfTitle <- .bffGetBFTitle(options)
  summaryTable$addColumnInfo(name = "n",        title = gettext("# Estimates"), type = "integer")
  summaryTable$addColumnInfo(name = "bf",       title = bfTitle,                 type = "number")
  summaryTable$addColumnInfo(name = "atOmega",  title = gettext("At \U03C9"),    type = "number")

  if (!ready)
    return()

  fit <- jaspResults[["fit"]]$object

  if (isTryError(fit))
    summaryTable$setError(fit)

  summaryTable$addRows(list(
    n       = nrow(dataset),
    bf      = .recodeBFtype(bfOld = fit[["log_bf"]], newBFtype = options[["bayesFactorType"]], oldBFtype = "LogBF10"),
    atOmega = options[["omega"]]
  ))

  if (options[["priorR"]] == "automatic")
    summaryTable$addFootnote(gettextf("The Bayes factor function was estimated with r = %1$.2f.", fit[["r"]]))

  return()
}
.bffBayesFactorOmegaTable    <- function(jaspResults, dataset, options, title, dependencies, ready) {

  if (!is.null(jaspResults[["omegaTable"]]))
    return()

  omegaTable <- createJaspTable(title = title)
  omegaTable$dependOn(c(dependencies, "bayesFactorType", "bayesFactorAtOmega", "bayesFactorAtOmegaValue"))
  omegaTable$position <- 4
  jaspResults[["omegaTable"]] <- omegaTable

  # create empty table
  bfTitle <- .bffGetBFTitle(options, maximum = FALSE)
  omegaTable$addColumnInfo(name = "n",        title = gettext("# Estimates"), type = "integer")
  omegaTable$addColumnInfo(name = "bf",       title = bfTitle,                 type = "number")
  omegaTable$addColumnInfo(name = "atOmega",  title = gettext("At \U03C9"),    type = "number")

  if (!ready)
    return()

  fit <- jaspResults[["fitOmega"]]$object

  if (isTryError(fit))
    omegaTable$setError(fit)

  omegaTable$addRows(list(
    n       = nrow(dataset),
    bf      = .recodeBFtype(bfOld = fit[["log_bf"]], newBFtype = options[["bayesFactorType"]], oldBFtype = "LogBF10"),
    atOmega = options[["omega"]]
  ))

  if (options[["priorR"]] == "automatic")
    omegaTable$addFootnote(gettextf("The Bayes factor function was estimated with r = %1$.2f.", fit[["r"]]))

  return()
}
.bffBayesFactorFunctionPlot  <- function(jaspResults, dataset, options, title, dependencies, ready) {

  if (!is.null(jaspResults[["bayesFactorFunctionPlot"]]))
    return()

  bayesFactorFunctionPlot <- createJaspPlot(title = title)
  bayesFactorFunctionPlot$dependOn(c(dependencies, "plotBayesFactorFunction", "plotBayesFactorFunctionAdditionalInfo"))
  bayesFactorFunctionPlot$position <- 3
  jaspResults[["bayesFactorFunctionPlot"]] <- bayesFactorFunctionPlot

  # create empty table
  if (!ready)
    return()

  fit <- jaspResults[["fit"]]$object

  if (isTryError(fit))
    bayesFactorFunctionPlot$setError(fit)

  tempPlot <- plot(fit)

  if (isTryError(fitPlot))
    bayesFactorFunctionPlot$setError(fitPlot)

  bayesFactorFunctionPlot$plotObject <- tempPlot + jaspGraphs::themeJaspRaw() + jaspGraphs::geom_rangeframe()

  return()
}
.bffPriorAndPosteriorPlot    <- function(jaspResults, dataset, options, title, dependencies, ready) {

  if (!is.null(jaspResults[["priorAndPosteriorPlot"]]))
    return()

  priorAndPosteriorPlot <- createJaspPlot(title = title)
  priorAndPosteriorPlot$dependOn(c(dependencies, "plotPriorAndPosterior", "plotBayesFactorFunctionAdditionalInfo"))
  priorAndPosteriorPlot$position <- 3
  jaspResults[["priorAndPosteriorPlot"]] <- priorAndPosteriorPlot

  # create empty table
  if (!ready)
    return()

  fit <- jaspResults[["fit"]]$object

  if (isTryError(fit))
    priorAndPosteriorPlot$setError(fit)

  tempPlot <- posterior_plot(fit, prior = TRUE)

  if (isTryError(fitPlot))
    priorAndPosteriorPlot$setError(fitPlot)

  priorAndPosteriorPlot$plotObject <- tempPlot + jaspGraphs::themeJaspRaw() + jaspGraphs::geom_rangeframe()

  return()
}
