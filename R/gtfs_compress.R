#' Reduce file size of a GTFS object
#'
#' @param gtfs agtfs objects
#' @export
gtfs_compress <- function(gtfs) {
  agency <- gtfs$agency
  stops <- gtfs$stops
  routes <- gtfs$routes
  trips <- gtfs$trips
  stop_times <- gtfs$stop_times
  calendar <- gtfs$calendar
  calendar_dates <- gtfs$calendar_dates

  # Simplify stop_ids
  stop_id <- unique(stops$stop_id)
  stops$stop_id <- as.integer(factor(stops$stop_id, levels = stop_id))
  stop_times$stop_id <- as.integer(factor(stop_times$stop_id, levels = stop_id))

  # Simplify trip_ids
  trip_id <- unique(trips$trip_id)
  trips$trip_id <- as.integer(factor(trips$trip_id, levels = trip_id))
  stop_times$trip_id <- as.integer(factor(stop_times$trip_id, levels = trip_id))

  # Simplify route_ids
  route_id <- unique(routes$route_id)
  routes$route_id <- as.integer(factor(routes$route_id, levels = route_id))
  trips$route_id <- as.integer(factor(trips$route_id, levels = route_id))

  gtfs$stops <- stops
  gtfs$routes <- routes
  gtfs$trips <- trips
  gtfs$stop_times <- stop_times

  return(gtfs)
}
