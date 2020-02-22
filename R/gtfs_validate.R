#' Validate a GTFS object (in R)
#'
#' @param gtfs a gtfs object
#' @export

gtfs_validate_internal <- function(gtfs) {
  agency <- gtfs$agency
  stops <- gtfs$stops
  routes <- gtfs$routes
  trips <- gtfs$trips
  stop_times <- gtfs$stop_times
  calendar <- gtfs$calendar
  calendar_dates <- gtfs$calendar_dates

  # Basic checks
  # Rows
  if (nrow(agency) < 1) {
    stop("No rows in agency")
  }
  if (nrow(stops) < 1) {
    stop("No rows in stops")
  }
  if (nrow(routes) < 1) {
    stop("No rows in routes")
  }
  if (nrow(trips) < 1) {
    stop("No rows in trips")
  }
  if (nrow(stop_times) < 1) {
    stop("No rows in stop_times")
  }
  if (nrow(calendar) < 1) {
    stop("No rows in calendar")
  }

  # Columns
  if (ncol(agency) != 5) {
    stop("Wrong number of columns in agency")
  }
  if (ncol(stops) != 5) {
    stop("Wrong number of columns in stops")
  }
  if (ncol(routes) != 6) {
    stop("Wrong number of columns in routes")
  }
  if (ncol(trips) != 3) {
    stop("Wrong number of columns in trips")
  }
  if (ncol(stop_times) != 6) {
    stop("Wrong number of columns in stop_times")
  }
  if (ncol(calendar) != 10) {
    stop("Wrong number of columns in calendar")
  }
  if (ncol(calendar_dates) != 3) {
    stop("Wrong number of columns in calendar_dates")
  }



  if (!all(routes$agency_id %in% agency$agency_id)) {
    stop("Unknown agency_id in routes")
  }
}

#' Validate GTFS with Google Validator
#' @param path_gtfs path to gtfs.zip file
#' @param path_validator path to google validator
#' @export
gtfs_validate_external <- function(path_gtfs, path_validator) {

}
