#
# Copyright (C) 2013-2018 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

# ISTT = independent samples t-test

#' @export
bffIndependentSamplesTTest <- function(jaspResults, dataset, options) {

  saveRDS(options, file = "C:/JASP/options.RDS")
  saveRDS(dataset, file = "C:/JASP/dataset.RDS")

  if (.bffISTTReady(options)) {
    dataset <- .bffISTTReadDataset(dataset, options)
    .bffISTTFitBFF(jaspResults, dataset, options)
  }

  .bffISTTSummaryTable(jaspResults, dataset, options)

  if (options[["bayesFactorAtOmega"]]) {
    if (.bffISTTReady(options))
      .bffISTTFitBFFOmega(jaspResults, dataset, options)
    .bffISTTBayesFactorOmegaTable(jaspResults, dataset, options)
  }

  if (options[["plotBayesFactorFunction"]])
    .bffISTTBayesFactorFunctionPlot(jaspResults, dataset, options)

  if (options[["plotPriorAndPosterior"]])
    .bffISTTPriorAndPosteriorPlot(jaspResults, dataset, options)
}


.bffISTTDependencies <- c(
  "tStatistic", "sampleSizeGroup1", "sampleSizeGroup2",
  "alternativeHypothesis", "priorR", "priorRManualValue"
)

.bffISTTReady                   <- function(options) {
  return(
    options[["tStatistic"]] != ""
    && options[["sampleSizeGroup1"]] != ""
    && options[["sampleSizeGroup2"]] != ""
  )
}
.bffISTTReadDataset             <- function(dataset, options) {

  if (!is.null(dataset))
    return(dataset)

  # load the data
  dataset <- .readDataSetToEnd(
    columns.as.numeric = c(options[["tStatistic"]], options[["sampleSizeGroup1"]], options[["sampleSizeGroup2"]])
  )

  # clean from NAs
  dataset <- na.omit(dataset)

  # check of errors
  .hasErrors(
    dataset                      = dataset,
    type                         = c("negativeValues"),
    negativeValues.target        = c(options[["sampleSizeGroup1"]], options[["sampleSizeGroup2"]]),
    exitAnalysisIfErrors         = TRUE
  )
  .hasErrors(
    dataset                      = dataset,
    type                         = c("infinity"),
    all.target                   = c(options[["tStatistic"]], options[["sampleSizeGroup1"]], options[["sampleSizeGroup2"]]),
    exitAnalysisIfErrors         = TRUE
  )

  return(dataset)
}
.bffISTTFitBFF                  <- function(jaspResults, dataset, options) {

  if(!is.null(jaspResults[["fit"]]))
    return()

  fitContainer <- createJaspState()
  fitContainer$dependOn(.bffISTTDependencies)
  jaspResults[["fit"]] <- fitContainer

  fit <- try(t_test_BFF(
    t_stat      = dataset[[options[["tStatistic"]]]],
    one_sample  = FALSE,
    alternative = .bffGetAlternativeHypothesis(options),
    n1          = dataset[[options[["sampleSizeGroup1"]]]],
    n2          = dataset[[options[["sampleSizeGroup2"]]]],
    r           = .bffGetR(options, singleStudy = nrow(dataset) == 1),
    omega       = NULL))

  jaspResults[["fit"]]$object <- fit

  return()
}
.bffISTTFitBFFOmega             <- function(jaspResults, dataset, options) {

  if(!is.null(jaspResults[["fitOmega"]]))
    return()

  fitOmegaContainer <- createJaspState()
  fitOmegaContainer$dependOn(c(.bffISTTDependencies, "bayesFactorAtOmegaValue"))
  jaspResults[["fitOmega"]] <- fitOmegaContainer

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
.bffISTTSummaryTable            <- function(jaspResults, dataset, options) {
  .bffSummaryTable(
    jaspResults  = jaspResults,
    dataset      = dataset,
    options      = options,
    title        = gettext("Two-Sample T-Test Bayes Factor Function Summary Table"),
    dependencies = .bffISTTDependencies,
    ready        = .bffISTTReady(options))
}
.bffISTTBayesFactorFunctionPlot <- function(jaspResults, dataset, options) {
  .bffBayesFactorFunctionPlot(
    jaspResults  = jaspResults,
    dataset      = dataset,
    options      = options,
    title        = gettext("Two-Sample T-Test Bayes Factor Function"),
    dependencies = .bffISTTDependencies,
    ready        = .bffISTTReady(options))
}
.bffISTTPriorAndPosteriorPlot   <- function(jaspResults, dataset, options) {
  .bffPriorAndPosteriorPlot(
    jaspResults  = jaspResults,
    dataset      = dataset,
    options      = options,
    title        = gettext("Prior and Posterior Plot"),
    dependencies = .bffISTTDependencies,
    ready        = .bffISTTReady(options))
}
.bffISTTBayesFactorOmegaTable   <- function(jaspResults, dataset, options) {
  .bffBayesFactorOmegaTable(
    jaspResults  = jaspResults,
    dataset      = dataset,
    options      = options,
    title        = gettext("Two-Sample T-Test Bayes Factor"),
    dependencies = .bffISTTDependencies,
    ready        = .bffISTTReady(options))
}
