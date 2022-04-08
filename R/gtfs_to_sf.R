#' Make stops into an SF object
#'
#' Make stops.txt into a sf data frame for plotting
#'
#' @param gtfs a gtfs object
#' @return sf data frame of points
#' @export
gtfs_stops_sf <- function(gtfs){
  stops <- gtfs$stops

  if(class(stops$stop_lon) != "numeric"){
    message("Converting stop_lon to numeric")
    stops$stop_lon <- as.numeric(stops$stop_lon)
  }

  if(class(stops$stop_lat) != "numeric"){
    message("Converting stop_lat to numeric")
    stops$stop_lat <- as.numeric(stops$stop_lat)
  }

  if(anyNA(stops$stop_lon) | anyNA(stops$stop_lon)){
    message("Stops with missing lat/lng removed")
    stops <- stops[!is.na(stops$stop_lon),]
    stops <- stops[!is.na(stops$stop_lat),]
  }

  stops <- sf::st_as_sf(stops, coords = c("stop_lon","stop_lat"), crs = 4326)

  return(stops)

}

#' Make trips into an SF object
#'
#' Make trips.txt into a sf data frame for plotting
#'
#' @param gtfs a gtfs object
#' @return sf data frame of lines
#' @export
gtfs_trips_sf <- function(gtfs){
  stop_times <- gtfs$stop_times
  stops <- gtfs$stops
  stops <- stops[,c("stop_id","stop_lon","stop_lat")]

  if(class(stops$stop_lon) != "numeric"){
    stops$stop_lon <- as.numeric(stops$stop_lon)
  }

  if(class(stops$stop_lat) != "numeric"){
    stops$stop_lat <- as.numeric(stops$stop_lat)
  }

  if(anyNA(stops$stop_lon) | anyNA(stops$stop_lat)){
    message("Stops with missing lat/lng removed")
    stops <- stops[!is.na(stops$stop_lon),]
    stops <- stops[!is.na(stops$stop_lat),]
  }

  stop_times <- dplyr::left_join(stop_times, stops, by = "stop_id")

  stop_times <- dplyr::group_by(stop_times, trip_id)
  stop_times <- dplyr::group_split(stop_times)
  stop_times <- lapply(stop_times, df2line)
  stop_times <- dplyr::bind_rows(stop_times)

  return(stop_times)

}


#' Make routes into an SF object
#'
#' Make similar to `gtfs_trips_sf` expect one trip is chosen to represent each route
#'
#' @param gtfs a gtfs object
#' @return sf data frame of lines
#' @export
gtfs_routes_sf <- function(gtfs){

  # Select one trip per route
  gtfs$trips <- gtfs$trips[!duplicated(gtfs$trips$route_id),]
  gtfs$stop_times <- gtfs$stop_times[gtfs$stop_times$trip_id %in% gtfs$trips$trip_id,]

  stop_times <- gtfs$stop_times
  stops <- gtfs$stops
  stops <- stops[,c("stop_id","stop_lon","stop_lat")]

  if(class(stops$stop_lon) != "numeric"){
    stops$stop_lon <- as.numeric(stops$stop_lon)
  }

  if(class(stops$stop_lat) != "numeric"){
    stops$stop_lat <- as.numeric(stops$stop_lat)
  }

  if(anyNA(stops$stop_lon) | anyNA(stops$stop_lat)){
    message("Stops with missing lat/lng removed")
    stops <- stops[!is.na(stops$stop_lon),]
    stops <- stops[!is.na(stops$stop_lat),]
  }

  stop_times <- dplyr::left_join(stop_times, stops, by = "stop_id")

  stop_times <- dplyr::group_by(stop_times, trip_id)
  stop_times <- dplyr::group_split(stop_times)
  stop_times <- lapply(stop_times, df2line)
  stop_times <- dplyr::bind_rows(stop_times)

  stop_times <- dplyr::left_join(stop_times, gtfs$trips, by = "trip_id")
  stop_times <- dplyr::left_join(stop_times, gtfs$routes, by = "route_id")
  stop_times <- dplyr::left_join(stop_times, gtfs$agency, by = "agency_id")

  stop_times <- stop_times[,c("route_id","agency_id","agency_name","route_short_name","route_long_name","route_desc","route_type")]

  return(stop_times)

}

#' COnvert data frame of points to linestring
#' @param x a sf data frame of points
#' @return sf data frame of lines
#' @noRd
df2line <- function(x){
  geom <- as.matrix(x[,c("stop_lon","stop_lat")])
  geom <- sf::st_linestring(geom)
  res <- data.frame(trip_id = x$trip_id[1],
                    geometry = sf::st_sfc(list(geom)))
  res <- sf::st_as_sf(res, crs = 4326)
  return(res)
}
