#' Reads inquisit data from one file.
#'
#' @param file the name of the file
#' @return the data frame
#' @keywords internal
ReadInquisit <- function(file) {
  utils::read.delim(file, header = TRUE, na.strings = c("not run", "nan", "NA"))
}

#' Gets the name of the inquisit summary file.
#'
#' @return a glob pattern to match the file name
#' @keywords internal
InquisitSummaryPattern <- function(id, testname, index) {
  dir <- index[id, ]$dir
  if (is.na(dir)) {
    stop(paste("missing dir in index for participant: ", id))
  }
  file.path(
    dir,
    "cogbat",
    paste0(testname, "_summary_*.iqdat")
  )
}

#' Matches a glob against a single file, or throws an error.
#' @keywords internal
MatchFile <- function(pattern) {
  f <- Sys.glob(pattern)
  if (length(f) != 1L) {
    stop("no file matching pattern: ", pattern)
  }
  f
}

#' Reads one summary file.
#'
#' @return a data frame with a single row
#' @keywords internal
ReadInquisitSummary <- function(id, testname, index) {
  df <- InquisitSummaryPattern(id, testname, index) |>
    MatchFile() |>
    ReadInquisit()
  # There should only be one row in a summary file.
  row.names(df) <- id
  df
}

#' Reads the summary results for all participants.
#'
#' @param testname character the test name as it appears in the file name
#' @param index data frame (as created by [ReadIndex()])
#' @return data frame with one row for each participant
#' @export
ReadAllInquisitSummaries <- function(testname, index) {
  results <- purrr::map(
    index$ID,
    # Emit a warning if file is missing, and return a NULL row.
    \(id) tryCatch(
      ReadInquisitSummary(id, testname, index),
      error = \(cond) {
        warning(
          paste(
            "Missing data for test:", testname,
            "participant:", id,
            "\n", cond
          )
        )
        NULL
      }
    )
  )
  # Replace NULL results with a row of NAs; we need to infer the columns
  # from the successful rows.
  successes <- purrr::compact(results)
  if (length(successes) == 0L) {
    stop(paste("No data for test:", testname))
  }
  cols <- colnames(successes[[1]])
  na_row <- rep(NA, length(cols)) |>
    t() |>
    as.data.frame() |>
    stats::setNames(cols)
  purrr::map(results, \(x) if (is.null(x)) na_row else x) |>
    purrr::list_rbind() |>
    cbind(index)
}

#' Reads all the summaries for all participants.
#'
#' @param config object (as created by [Config()])
#' @param index data frame (as created by [ReadIndex()])
#' @return list of data frames, named according to the test, each containing
#' one row per participant
#' @export
ReadAllConfiguredSummaries <- function(config, index) {
  testnames <- names(config$tests)
  # map will preserve names, so name the tests by themselves.
  names(testnames) <- testnames
  testnames |>
    purrr::map(\(testname) ReadAllInquisitSummaries(testname, index))
}

#' Writes box plots for each of the configured columns.
#'
#' @param config object (as created by [Config()])
#' @param index data frame (as created by [ReadIndex()])
#' @param dfs list of data frames (as created by
#' [ReadAllConfiguredSummaries()])
#' @export
PlotAllConfiguredSummaries <- function(config, index, dfs) {
  for (name in names(config$tests)) {
    df <- dfs[[name]]
    if (is.null(df)) {
      stop("Could not find data frame: ", name)
    }
    columns <- config$tests[[name]]
    for (column in columns) {
      if (is.null(df[[column]])) {
        warning("Could not find column: '", column, "' in data frame ", name)
      } else {
        print(graphics::plot(df[[column]] ~ df$Group, ylab = paste(name, column)))
      }
    }
  }
}
