#' Creates a new Config object.
#'
#' The config object holds global constants.
#'
#' @param basedir a directory that contains subdirectories for each
#' participant
#' @param pptprefix the prefix for participant subdirectories before the
#' participant's ID
#' @param indexfile the name of the CSV file containing participant IDs
#' and groups
#' @param tests a list with names corresponding to the names of cognitive
#' test files, and character vector columns corresponding to the names of
#' columns that should be analyzed
#' @export
Config <- function(
    basedir,
    pptprefix = "MRH152_",
    indexfile = "index.csv",
    tests = DefaultTestConfigs()) {
  stopifnot(is.character(basedir))
  stopifnot(is.character(pptprefix))
  stopifnot(is.character(indexfile))
  structure(
    list(
      basedir = basedir,
      pptprefix = pptprefix,
      indexfile = indexfile,
      tests = tests
    ),
    class = "becophd.Config"
  )
}

DefaultTestConfigs <- function() {
  columns <- list(
    digitspanvisual = c("bML", "fML"),
    listlearningtask = c("recallScore", "delayedRecallScore"),
    probabilisticsrt_ro = c("propCorrect_seqA", "meanCorrRT_seqA", "propCorrect_seqB", "meanCorrRT_seqB"),
    prospectivememorytask_versiona = c("Hit", "Miss", "FA", "CR")
    # TODO - add "sart" once we can deal with missing files.
  )
}
