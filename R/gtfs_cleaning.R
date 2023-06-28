# Removing problems functions

#' Split a GTFS object based on trip_ids
#'
#' @param gtfs gtfs list
#' @param trip_ids a vector of trips ids
#' @export

gtfs_split_ids <- function(gtfs, trip_ids) {
  agency <- gtfs$agency
  stops <- gtfs$stops
  routes <- gtfs$routes
  trips <- gtfs$trips
  stop_times <- gtfs$stop_times
  calendar <- gtfs$calendar
  calendar_dates <- gtfs$calendar_dates


  trips_true <- trips[trips$trip_id %in% trip_ids, ]
  trips_false <- trips[!trips$trip_id %in% trip_ids, ]

  stop_times_true <- stop_times[stop_times$trip_id %in% trips_true$trip_id, ]
  stop_times_false <- stop_times[stop_times$trip_id %in% trips_false$trip_id, ]

  routes_true <- routes[routes$route_id %in% trips_true$route_id, ]
  routes_false <- routes[routes$route_id %in% trips_false$route_id, ]

  calendar_true <- calendar[calendar$service_id %in% trips_true$service_id, ]
  calendar_false <- calendar[calendar$service_id %in% trips_false$service_id, ]

  calendar_dates_true <- calendar_dates[calendar_dates$service_id %in% trips_true$service_id, ]
  calendar_dates_false <- calendar_dates[calendar_dates$service_id %in% trips_false$service_id, ]

  stops_true <- stops[stops$stop_id %in% stop_times_true$stop_id, ]
  stops_false <- stops[stops$stop_id %in% stop_times_false$stop_id, ]

  agency_true <- agency[agency$agency_id %in% routes_true$agency_id, ]
  agency_false <- agency[agency$agency_id %in% routes_false$agency_id, ]

  gtfs_true <- list(agency_true, stops_true, routes_true, trips_true, stop_times_true, calendar_true, calendar_dates_true)
  gtfs_false <- list(agency_false, stops_false, routes_false, trips_false, stop_times_false, calendar_false, calendar_dates_false)

  names(gtfs_true) <- c("agency", "stops", "routes", "trips", "stop_times", "calendar", "calendar_dates")
  names(gtfs_false) <- c("agency", "stops", "routes", "trips", "stop_times", "calendar", "calendar_dates")

  result <- list(gtfs_true, gtfs_false)
  names(result) <- c("true", "false")
  return(result)
}

#' Find fast trips
#' @description Fast trips can identify problems with the input data or
#'   conversion process. This function returns trip_ids for trips that exceed
#'   `maxspeed`.
#' @param gtfs list of gtfs tables
#' @param maxspeed the maximum allowed speed in metres per second default 83 m/s
#'   (about 185 mph the max speed of trains on HS1 line)
#' @param routes logical, do one trip per route, faster but may miss some trips
#' @details The function looks a straight line distance between each stop and
#'   detects the fastest segment of the journey. A common cause of errors is
#'   that a stop is in the wrong location so a bus can appear to teleport
#'   across the country in seconds.
#' @export

gtfs_fast_trips <- function(gtfs, maxspeed = 83, routes = TRUE) {

  if(routes){
    gtfs$trips <- gtfs$trips[!duplicated(gtfs$trips$route_id),]
    gtfs$stop_times <- gtfs$stop_times[gtfs$stop_times$trip_id %in% gtfs$trips$trip_id,]
  }

  trips <- gtfs$stop_times
  #times$stop_sequence <- as.integer(times$stop_sequence)
  trips <- dplyr::left_join(trips, gtfs$stops, by = "stop_id")
  trips$distance <- geodist::geodist(as.matrix(trips[,c("stop_lon","stop_lat")]), sequential = TRUE, pad = TRUE)
  trips$distance[trips$stop_sequence == 1] <- NA
  trips$time <- dplyr::if_else(is.na(trips$arrival_time), trips$departure_time, trips$arrival_time)
  if(inherits(trips$time, "character")){
    trips$time <- as.POSIXct(trips$time, format="%H:%M:%S", origin = "1970-01-01")
  }
  if(inherits(trips$time, "Period")){
    trips$time2 <- c(NA, as.numeric(trips$time[2:nrow(trips)] - trips$time[1:(nrow(trips)-1)]))
  } else {
    trips$time <- as.POSIXct(trips$time, format="%H:%M:%S", origin = "1970-01-01")
    trips$time2 <- c(NA, difftime(trips$time[2:nrow(trips)], trips$time[1:(nrow(trips)-1)]))
  }

  trips$speed <- trips$distance / trips$time2
  trips$speed[trips$speed == Inf] <- NA

  times <- dplyr::group_by(trips, trip_id)
  times <- dplyr::summarise(times,
                            max_speed = max(speed, na.rm = TRUE)
  )
  times <- times[times$max_speed > maxspeed,]
  return(times$trip_id)
}

#' Find fast stops
#' @description A varient of gtfs_fast_trips that can detect stops that may be in the wrong location
#' @param gtfs list of gtfs tables
#' @param maxspeed the maximum allowed speed in metres per second default 83 m/s
#'   (about 185 mph the max speed of trains on HS1 line)
#' @details The function looks a straight line distance between each stop and
#'   detects the fastest segment of the journey. A common cause of errors is
#'   that a stop is in the wrong location so a bus can appear to teleport
#'   across the country in seconds.
#' @export

gtfs_fast_stops <- function(gtfs, maxspeed = 83) {

  trips <- gtfs$stop_times
  trips <- dplyr::left_join(trips, gtfs$stops, by = "stop_id")
  trips$distance <- geodist::geodist(as.matrix(trips[,c("stop_lon","stop_lat")]), sequential = TRUE, pad = TRUE)
  trips$distance[trips$stop_sequence == 1] <- NA
  trips$time <- dplyr::if_else(is.na(trips$arrival_time), trips$departure_time, trips$arrival_time)
  if(inherits(trips$time, "character")){
    trips$time <- as.POSIXct(trips$time, format="%H:%M:%S", origin = "1970-01-01")
  }
  if(inherits(trips$time, "Period")){
    trips$time2 <- c(NA, as.numeric(trips$time[2:nrow(trips)] - trips$time[1:(nrow(trips)-1)]))
  } else {
    trips$time <- as.POSIXct(trips$time, format="%H:%M:%S", origin = "1970-01-01")
    trips$time2 <- c(NA, difftime(trips$time[2:nrow(trips)], trips$time[1:(nrow(trips)-1)]))
  }

  trips$speed <- trips$distance / trips$time2
  trips$speed[trips$speed == Inf] <- NA
  trips$speed_after <- c(trips$speed[2:nrow(trips)],NA)
  trips$distance_after <- c(trips$distance[2:nrow(trips)],NA)

  times <- dplyr::group_by(trips, stop_id)
  times <- dplyr::summarise(times,
                            max_speed = round(max(c(speed,speed_after), na.rm = TRUE), 1),
                            min_speed = round(min(c(speed,speed_after), na.rm = TRUE), 1),
                            mean_speed = round(mean(c(speed,speed_after), na.rm = TRUE), 1),
                            max_distance = round(max(c(distance,distance_after), na.rm = TRUE), 0),
                            min_distance = round(min(c(distance,distance_after), na.rm = TRUE), 0),
                            mean_distance = round(mean(c(distance,distance_after), na.rm = TRUE), 0),
                            trips = dplyr::n()

  )
  times <- times[times$max_speed > maxspeed,]


  times <- dplyr::left_join(times, gtfs$stops, by = "stop_id")
  times <- sf::st_as_sf(times, coords = c("stop_lon","stop_lat"), crs = 4326)

  return(times)
}


# gtfs_fast_trips <- function(gtfs, maxspeed = 30) {
#   times <- gtfs$stop_times
#   times$stop_sequence <- as.integer(times$stop_sequence)
#   times <- dplyr::group_by(times, trip_id)
#   times <- dplyr::summarise(times,
#     nstops = dplyr::n(),
#     time_start = arrival_time[stop_sequence == min(stop_sequence)],
#     time_end = if (nstops == 2) {
#       arrival_time[stop_sequence == max(stop_sequence)]
#     } else {
#       arrival_time[stop_sequence == stop_sequence[floor(stats::median(1:nstops))]]
#     },
#     stop_start = stop_id[stop_sequence == min(stop_sequence)],
#     stop_end = if (nstops == 2) {
#       stop_id[stop_sequence == max(stop_sequence)]
#     } else {
#       stop_id[stop_sequence == stop_sequence[floor(stats::median(1:nstops))]]
#     }
#   )
#
#   stops <- gtfs$stops
#   stops <- stops[, c("stop_id", "stop_lon", "stop_lat")]
#   names(stops) <- c("stop_id", "from_lon", "from_lat")
#   times <- dplyr::left_join(times, stops, by = c("stop_start" = "stop_id"))
#   names(stops) <- c("stop_id", "to_lon", "to_lat")
#   times <- dplyr::left_join(times, stops, by = c("stop_end" = "stop_id"))
#   times$time_start <- lubridate::hms(times$time_start)
#   times$time_end <- lubridate::hms(times$time_end)
#   times$duration <- as.numeric(times$time_end - times$time_start)
#   times$distance <- geodist::geodist(
#     x = as.matrix(times[, c("from_lon", "from_lat")]),
#     y = as.matrix(times[, c("to_lon", "to_lat")]),
#     paired = TRUE
#   )
#   times$speed <- times$distance / times$duration
#
#   fast_trips <- times$trip_id[times$speed > maxspeed]
#   return(fast_trips)
# }


#' Clean simple errors from GTFS files
#'
#' @param gtfs gtfs list
#' @export
gtfs_clean <- function(gtfs) {
  # 1 Remove stops with no locations
  gtfs$stop_times <- gtfs$stop_times[gtfs$stop_times$stop_id %in% unique(gtfs$stops$stop_id), ]

  # 2 Remove stops that are never used
  gtfs$stops <- gtfs$stops[gtfs$stops$stop_id %in% unique(gtfs$stop_times$stop_id), ]

  # Replace "" agency_id with dummy name
  gtfs$agency$agency_id[gtfs$agency$agency_id == ""] <- "MISSINGAGENCY"
  gtfs$routes$agency_id[gtfs$routes$agency_id == ""] <- "MISSINGAGENCY"
  gtfs$agency$agency_name[gtfs$agency$agency_name == ""] <- "MISSINGAGENCY"

  return(gtfs)
}
