#' Plots a gtfs trip
#' @param gtfs a gtfs object
#' @param trip_id a trip_id
#' @export

plot_trip <- function(gtfs, trip_id) {
  stops <- gtfs$stops
  stop_times <- gtfs$stop_times
  stop_times <- stop_times[stop_times$trip_id == trip_id, ]

  stops <- sf::st_as_sf(stops, coords = c("stop_lon", "stop_lat"))
  sf::st_crs(stops) <- 4326

  stop_times <- dplyr::left_join(stop_times, stops, by = "stop_id")
  stop_times <- sf::st_sf(stop_times)
  stop_times$stop_sequence <- as.numeric(stop_times$stop_sequence)
  tmap::qtm(stop_times, dots.col = "stop_sequence")
}
