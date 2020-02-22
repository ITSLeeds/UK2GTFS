#' Split a gtfs file by location
#'
#' @param gtfs a gtfs object
#' @param bounds an sf object
#' @export
gtfs_subset <- function(gtfs, bounds) {
  stops <- gtfs$stops
  stop_times <- gtfs$stop_times

  if (nrow(bounds) > 1) {
    message("Multiple geometrys offered, using total area of all geometries")
    bounds <- sf::st_combine(bounds)
    suppressWarnings(bounds <- sf::st_buffer(bounds, 0.001))
  }

  # bbox <- sf::st_bbox(bounds)
  stops_inc <- stops[!is.na(stops$stop_lon), ]
  stops_inc$stop_lon <- as.numeric(stops_inc$stop_lon)
  stops_inc$stop_lat <- as.numeric(stops_inc$stop_lat)

  stops_inc <- sf::st_as_sf(stops_inc, coords = c("stop_lon", "stop_lat"), crs = 4326)
  suppressWarnings(stops_inc <- stops_inc[bounds, ])
  stops_inc <- unique(stops_inc$stop_id)

  gtfs$stops <- gtfs$stops[gtfs$stops$stop_id %in% stops_inc, ]
  gtfs$stop_times <- gtfs$stop_times[gtfs$stop_times$stop_id %in% stops_inc, ]
  # Check for single stop trips
  n_stops <- table(gtfs$stop_times$trip_id)
  single_stops <- as.integer(names(n_stops[n_stops == 1]))
  gtfs$stop_times <- gtfs$stop_times[!gtfs$stop_times$trip_id %in% single_stops, ]

  # Check for any unused stops
  gtfs$stops <- gtfs$stops[gtfs$stops$stop_id %in% unique(gtfs$stop_times$stop_id), ]

  gtfs$trips <- gtfs$trips[gtfs$trips$trip_id %in% unique(gtfs$stop_times$trip_id), ]
  gtfs$routes <- gtfs$routes[gtfs$routes$route_id %in% unique(gtfs$trips$route_id), ]
  gtfs$calendar <- gtfs$calendar[gtfs$calendar$service_id %in% unique(gtfs$trips$service_id), ]
  gtfs$calendar_dates <- gtfs$calendar_dates[gtfs$calendar_dates$service_id %in% unique(gtfs$trips$service_id), ]
  gtfs$agency <- gtfs$agency[gtfs$agency$agency_id %in% unique(gtfs$routes$agency_id), ]



  return(gtfs)
}
