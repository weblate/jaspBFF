#' @import jaspBase

# `options[["test"]]` contains information for dispatching in the generic analysis
# "ISTT" = Independent samples t-test

bffAnalysis <- function(jaspResults, dataset, options, test) {

  saveRDS(options, file = "C:/JASP/options.RDS")
  saveRDS(dataset, file = "C:/JASP/dataset.RDS")

  # load data and fit Bayes factor function
  if (.bffReady(options)) {
    dataset <- .bffReadDataset(dataset, options)
    .bffFitBFF(jaspResults, dataset, options)
  }

  # default summary table
  .bffSummaryTable(jaspResults, dataset, options)

  # compute Bayes factor at specified omega
  if (options[["bayesFactorAtOmega"]]) {
    if (.bffReady(options))
      .bffFitBFFOmega(jaspResults, dataset, options)
    .bffBayesFactorOmegaTable(jaspResults, dataset, options)
  }

  # Bayes factor function plot
  if (options[["plotBayesFactorFunction"]])
    .bffBayesFactorFunctionPlot(jaspResults, dataset, options)

  # prior and posterior plot
  if (options[["plotPriorAndPosterior"]])
    .bffPriorAndPosteriorPlot(jaspResults, dataset, options)
}


.bffGetDependencies          <- function(options) {

  dependenciesGlobal <- c("alternativeHypothesis", "priorR", "priorRManualValue")
  dependenciesTest   <- switch(
    options[["test"]],
    "ISTT" = c("tStatistic", "sampleSizeGroup1", "sampleSizeGroup2")
  )

  return(c(dependenciesGlobal, dependenciesTest))
}
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
.bffFitBFF                   <- function(jaspResults, dataset, options) {

  if(!is.null(jaspResults[["fit"]]))
    return()

  fitContainer <- createJaspState()
  fitContainer$dependOn(.bffGetDependencies(options))
  jaspResults[["fit"]] <- fitContainer

  if (options[["test"]] == "ISTT")
    fit <- try(t_test_BFF(
      t_stat      = dataset[[options[["tStatistic"]]]],
      one_sample  = FALSE,
      alternative = .bffGetAlternativeHypothesis(options),
      n1          = dataset[[options[["sampleSizeGroup1"]]]],
      n2          = dataset[[options[["sampleSizeGroup2"]]]],
      r           = .bffGetR(options, singleStudy = nrow(dataset) == 1),
      omega       = NULL))

  # TODO: quick fix
  fit$omega <- 0.2

  jaspResults[["fit"]]$object <- fit

  return()
}
.bffFitBFFOmega              <- function(jaspResults, dataset, options) {

  if(!is.null(jaspResults[["fitOmega"]]))
    return()

  fitOmegaContainer <- createJaspState()
  fitOmegaContainer$dependOn(c(.bffGetDependencies(options), "bayesFactorAtOmegaValue"))
  jaspResults[["fitOmega"]] <- fitOmegaContainer

  if (options[["test"]] == "ISTT")
    fitOmega <- try(t_test_BFF(
      t_stat      = dataset[[options[["tStatistic"]]]],
      one_sample  = FALSE,
      alternative = .bffGetAlternativeHypothesis(options),
      n1          = dataset[[options[["sampleSizeGroup1"]]]],
      n2          = dataset[[options[["sampleSizeGroup2"]]]],
      r           = jaspResults[["fit"]]$object[["r"]],
      omega       = options[["bayesFactorAtOmegaValue"]]))

  jaspResults[["fitOmega"]]$object <- fitOmega

  return()
}
.bffReady                    <- function(options) {
  switch(
    options[["test"]],
    "ISTT" =  options[["tStatistic"]] != "" &&
      options[["sampleSizeGroup1"]] != ""  &&
      options[["sampleSizeGroup2"]] != ""
  )
}
.bffReadDataset              <- function(dataset, options) {

  if (!is.null(dataset))
    return(dataset)

  # load the data
  dataset <- .readDataSetToEnd(
    columns.as.numeric = switch(
      options[["test"]],
      "ISTT" = c(options[["tStatistic"]], options[["sampleSizeGroup1"]], options[["sampleSizeGroup2"]])
    )
  )

  # clean from NAs
  dataset <- na.omit(dataset)

  # check of errors
  .hasErrors(
    dataset                      = dataset,
    type                         = c("negativeValues"),
    negativeValues.target        = switch(
      options[["test"]],
      "ISTT" = c(options[["sampleSizeGroup1"]], options[["sampleSizeGroup2"]])
    ),
    exitAnalysisIfErrors         = TRUE
  )
  .hasErrors(
    dataset                      = dataset,
    type                         = c("infinity"),
    all.target                   = switch(
      options[["test"]],
      "ISTT" = c(options[["tStatistic"]], options[["sampleSizeGroup1"]], options[["sampleSizeGroup2"]])
    ),
    exitAnalysisIfErrors         = TRUE
  )

  return(dataset)
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
.bffSummaryTable             <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["summaryTable"]]))
    return()

  summaryTable <- createJaspTable(title = switch(
    options[["test"]],
    "ISTT" = gettext("Two-Sample T-Test Bayes Factor Function Summary Table")
  ))
  summaryTable$dependOn(c(.bffGetDependencies(options), "bayesFactorType"))
  summaryTable$position <- 1
  jaspResults[["summaryTable"]] <- summaryTable

  # create empty table
  bfTitle <- .bffGetBFTitle(options)
  summaryTable$addColumnInfo(name = "n",        title = gettext("# Estimates"), type = "integer")
  summaryTable$addColumnInfo(name = "bf",       title = bfTitle,                 type = "number")
  summaryTable$addColumnInfo(name = "atOmega",  title = gettext("At \U03C9"),    type = "number")

  if (!.bffReady(options))
    return()

  fit <- jaspResults[["fit"]]$object

  if (isTryError(fit))
    summaryTable$setError(fit)

  summaryTable$addRows(list(
    n       = nrow(dataset),
    bf      = .recodeBFtype(bfOld = fit[["log_bf"]], newBFtype = options[["bayesFactorType"]], oldBFtype = "LogBF10"),
    atOmega = fit[["omega"]]
  ))

  if (options[["priorR"]] == "automatic")
    summaryTable$addFootnote(gettextf("The Bayes factor function was estimated with r = %1$.2f.", fit[["r"]]))

  return()
}
.bffBayesFactorOmegaTable    <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["omegaTable"]]))
    return()

  omegaTable <- createJaspTable(title = switch(
    options[["test"]],
    "ISTT" = gettext("Two-Sample T-Test Bayes Factor")
  ))
  omegaTable$dependOn(c(
    .bffGetDependencies(options),
    "bayesFactorType", "bayesFactorAtOmega", "bayesFactorAtOmegaValue"))
  omegaTable$position <- 2
  jaspResults[["omegaTable"]] <- omegaTable

  # create empty table
  bfTitle <- .bffGetBFTitle(options, maximum = FALSE)
  omegaTable$addColumnInfo(name = "n",        title = gettext("# Estimates"), type = "integer")
  omegaTable$addColumnInfo(name = "bf",       title = bfTitle,                 type = "number")
  omegaTable$addColumnInfo(name = "atOmega",  title = gettext("At \U03C9"),    type = "number")

  if (!.bffReady(options))
    return()

  fit <- jaspResults[["fitOmega"]]$object

  if (isTryError(fit))
    omegaTable$setError(fit)

  omegaTable$addRows(list(
    n       = nrow(dataset),
    bf      = .recodeBFtype(bfOld = fit[["log_bf"]], newBFtype = options[["bayesFactorType"]], oldBFtype = "LogBF10"),
    atOmega = options[["bayesFactorAtOmegaValue"]]
  ))

  if (options[["priorR"]] == "automatic")
    omegaTable$addFootnote(gettextf("The Bayes factor function was estimated with r = %1$.2f.", fit[["r"]]))

  return()
}
.bffBayesFactorFunctionPlot  <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["bayesFactorFunctionPlot"]]))
    return()

  bayesFactorFunctionPlot <- createJaspPlot(title = switch(
    options[["test"]],
    "ISTT" = gettext("Two-Sample T-Test Bayes Factor Function")
  ))
  bayesFactorFunctionPlot$dependOn(c(
    .bffGetDependencies(options),
    "plotBayesFactorFunction", "plotBayesFactorFunctionAdditionalInfo"))
  bayesFactorFunctionPlot$position <- 3
  jaspResults[["bayesFactorFunctionPlot"]] <- bayesFactorFunctionPlot

  # create empty plot
  if (!.bffReady(options))
    return()

  fit <- jaspResults[["fit"]]$object

  if (isTryError(fit))
    bayesFactorFunctionPlot$setError(fit)

  tempPlot <- plot.BFF(fit)

  if (isTryError(tempPlot))
    bayesFactorFunctionPlot$setError(tempPlot)

  bayesFactorFunctionPlot$plotObject <- tempPlot #+ jaspGraphs::themeJaspRaw() + jaspGraphs::geom_rangeframe()

  return()
}
.bffPriorAndPosteriorPlot    <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["priorAndPosteriorPlot"]]))
    return()

  priorAndPosteriorPlot <- createJaspPlot(title = switch(
    options[["test"]],
    "ISTT" = gettext("Prior and Posterior Plot")
  ))
  priorAndPosteriorPlot$dependOn(c(
    .bffGetDependencies(options),
    "plotPriorAndPosterior", "plotBayesFactorFunctionAdditionalInfo"))
  priorAndPosteriorPlot$position <- 4
  jaspResults[["priorAndPosteriorPlot"]] <- priorAndPosteriorPlot

  # create empty table
  if (!.bffReady(options))
    return()

  fit <- jaspResults[["fit"]]$object

  if (isTryError(fit))
    priorAndPosteriorPlot$setError(fit)

  tempPlot <- posterior_plot(fit, prior = TRUE)

  if (isTryError(tempPlot))
    priorAndPosteriorPlot$setError(tempPlot)

  priorAndPosteriorPlot$plotObject <- tempPlot + jaspGraphs::themeJaspRaw() + jaspGraphs::geom_rangeframe()

  return()
}
