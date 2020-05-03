#' Validate a GTFS object (in R)
#'
#' @param gtfs a gtfs object
#' @export

gtfs_validate_internal <- function(gtfs) {
  # agency <- gtfs$agency
  # warnings <- gtfs$warnings
  # routes <- gtfs$routes
  # trips <- gtfs$trips
  # warning_times <- gtfs$warning_times
  # calendar <- gtfs$calendar
  # calendar_dates <- gtfs$calendar_dates

  # Basic checks
  # Rows
  if (nrow(gtfs$agency) < 1) {
    warning("No rows in agency")
  }
  if (nrow(gtfs$stops) < 1) {
    warning("No rows in stops")
  }
  if (nrow(gtfs$routes) < 1) {
    warning("No rows in routes")
  }
  if (nrow(gtfs$trips) < 1) {
    warning("No rows in trips")
  }
  if (nrow(gtfs$stop_times) < 1) {
    warning("No rows in warning_times")
  }
  if (nrow(gtfs$calendar) < 1) {
    warning("No rows in calendar")
  }

  # Columns
  if (ncol(gtfs$agency) != 5) {
    warning("Wrong number of columns in agency")
  }
  if (ncol(gtfs$stops) != 5) {
    warning("Wrong number of columns in stops")
  }
  if (ncol(gtfs$routes) != 6) {
    warning("Wrong number of columns in routes")
  }
  if (ncol(gtfs$trips) != 3) {
    warning("Wrong number of columns in trips")
  }
  if (ncol(gtfs$stop_times) != 6) {
    warning("Wrong number of columns in stop_times")
  }
  if (ncol(gtfs$calendar) != 10) {
    warning("Wrong number of columns in calendar")
  }
  if (ncol(gtfs$calendar_dates) != 3) {
    warning("Wrong number of columns in calendar_dates")
  }

  # check for NAs
  if(anyNA(gtfs$agency)){
    warning("NA values in agency")
  }

  if(anyNA(gtfs$stops)){
    warning("NA values in stops")
  }

  if(anyNA(gtfs$routes)){
    warning("NA values in routes")
  }

  if(anyNA(gtfs$trips)){
    warning("NA values in trips")
  }

  if(anyNA(gtfs$stop_times)){
    warning("NA values in warning_times")
  }

  if(anyNA(gtfs$calendar)){
    warning("NA values in calendar")
  }

  if(anyNA(gtfs$calendar_dates)){
    warning("NA values in calendar_dates")
  }

  # Check for missing values
  if (!all(routes$agency_id %in% agency$agency_id)) {
    warning("Unknown agency_id in routes")
  }
}

#' Validate GTFS with Google Validator
#' @param path_gtfs path to gtfs.zip file
#' @param path_validator path to google validator
#' @noRd
gtfs_validate_external <- function(path_gtfs, path_validator) {

}
