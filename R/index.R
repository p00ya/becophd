#' Reads the index table.
#'
#' The table should be named index.csv, and should have ID, Age and Group columns.
#'
#' @param config an object created with [Config()]
#' @export
ReadIndex <- function(config) {
  file <- file.path(config$basedir, "index.csv")
  df <- utils::read.csv(
    file,
    colClasses = c(ID = "character", Age = "integer", Group = "factor"),
  )
  df$dir <- ParticipantDirectory(config, df$ID)
  row.names(df) <- df$ID
  df
}

#' Returns the participant directory.
#'
#' @param config an object created with [Config()]
#' @param id the participant ID
#' @return a length-one character vector
#' @keywords internal
ParticipantDirectory <- function(config, id) {
  file.path(config$basedir, paste0(config$pptprefix, id))
}
