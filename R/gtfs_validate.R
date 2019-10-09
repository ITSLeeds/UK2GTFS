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

  # Check speeds
  # speed <- dplyr::left_join(stop_times, stops, by = "stop_id")
  # # speed <- speed[,c("departure_time","stop_sequence","stop_lon","stop_lat")]
  # speed$stop_lon <- as.numeric(speed$stop_lon)
  # speed$stop_lat <- as.numeric(speed$stop_lat)
  # speed$stop_sequence <- as.numeric(speed$stop_sequence)
  #
  # speed$departure_time <- lubridate::hms(speed$departure_time)
  # travel_time <- speed$departure_time[seq(2, length(speed$departure_time))] -
  #   speed$departure_time[seq(1, length(speed$departure_time) - 1)]
  # travel_time <- c(0, as.numeric(travel_time))
  # travel_time[travel_time == 0] <- 20
  # speed$travel_time <- travel_time
  #
  # from <- matrix(c(
  #   speed$stop_lon[seq(1, length(speed$stop_lon) - 1)],
  #   speed$stop_lat[seq(1, length(speed$stop_lon) - 1)]
  # ),
  # ncol = 2
  # )
  #
  # to <- matrix(c(
  #   speed$stop_lon[seq(2, length(speed$stop_lon))],
  #   speed$stop_lat[seq(2, length(speed$stop_lon))]
  # ),
  # ncol = 2
  # )
  #
  # colnames(from) <- c("X", "Y")
  # colnames(to) <- c("X", "Y")
  # # points <- matrix(c(speed$stop_lon,
  # #                    speed$stop_lat),
  # #                  ncol = 2)
  #
  # # speed$distance  <- c(0,geodist::geodist(points, sequential = TRUE))
  # speed$distance <- c(0, geodist::geodist(from, to, paired = TRUE))
  # # speed$distance3 <- geodist::geodist(points, sequential = TRUE, pad = TRUE)
  #
  #
  # speed$speed <- round(speed$distance / speed$travel_time, 0)
  # speed$speed <- ifelse(speed$stop_sequence > c(0, speed$stop_sequence[seq(1, length(speed$departure_time) - 1)]),
  #   speed$speed, NA
  # )
  # speed$speed[1] <- NA
  # speed <- speed[!is.na(speed$speed), ]
  # speed <- speed[speed$speed > 30, ]
  # if (nrow(speed) > 0) {
  #   warning(paste0(length(speed$speed[speed$speed <= 100]), " too fast trips (> 30m/s) and ", length(speed$speed[speed$speed > 100]), " crazy fast trips (> 100 m/s)"))
  # }
}

#' Validate GTFS with Google Validator
#' @param path_gtfs path to gtfs.zip file
#' @param path_validator path to google validator
#' @export
gtfs_validate_external <- function(path_gtfs, path_validator) {

}
