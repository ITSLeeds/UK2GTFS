#' Validate a GTFS object (in R)
#'
#' @param gtfs a gtfs object
#' @export

gtfs_validate_internal <- function(gtfs) {
  agency <- gtfs$agency
  warnings <- gtfs$warnings
  routes <- gtfs$routes
  trips <- gtfs$trips
  warning_times <- gtfs$warning_times
  calendar <- gtfs$calendar
  calendar_dates <- gtfs$calendar_dates

  # Basic checks
  # Rows
  if (nrow(agency) < 1) {
    warning("No rows in agency")
  }
  if (nrow(warnings) < 1) {
    warning("No rows in warnings")
  }
  if (nrow(routes) < 1) {
    warning("No rows in routes")
  }
  if (nrow(trips) < 1) {
    warning("No rows in trips")
  }
  if (nrow(warning_times) < 1) {
    warning("No rows in warning_times")
  }
  if (nrow(calendar) < 1) {
    warning("No rows in calendar")
  }

  # Columns
  if (ncol(agency) != 5) {
    warning("Wrong number of columns in agency")
  }
  if (ncol(warnings) != 5) {
    warning("Wrong number of columns in warnings")
  }
  if (ncol(routes) != 6) {
    warning("Wrong number of columns in routes")
  }
  if (ncol(trips) != 3) {
    warning("Wrong number of columns in trips")
  }
  if (ncol(warning_times) != 6) {
    warning("Wrong number of columns in warning_times")
  }
  if (ncol(calendar) != 10) {
    warning("Wrong number of columns in calendar")
  }
  if (ncol(calendar_dates) != 3) {
    warning("Wrong number of columns in calendar_dates")
  }

  # check for NAs
  if(anyNA(agency)){
    warning("NA values in agency")
  }

  if(anyNA(warnings)){
    warning("NA values in warnings")
  }

  if(anyNA(routes)){
    warning("NA values in routes")
  }

  if(anyNA(trips)){
    warning("NA values in trips")
  }

  if(anyNA(warning_times)){
    warning("NA values in warning_times")
  }

  if(anyNA(calendar)){
    warning("NA values in calendar")
  }

  if(anyNA(calendar_dates)){
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
