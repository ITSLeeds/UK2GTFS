#' Compare a GTFS file to Google Directions
#'
#' Takes a random sample of routes and compares them to Google Maps.
#' Google also uses GTFS but doe not seem to publish the GTFS feeds they create.
#'
#' @param gtfs a gtfs object
#' @param n number of routes to check
#' @export

gtfs_compare_google <- function(gtfs, n = 10){
  trips <- gtfs$trips
  routes <- gtfs$routes
  calendar <- gtfs$calendar

  # check GTFS is currently valid
  calendar$start_date <- lubridate::ymd(calendar$start_date)
  calendar$end_date <- lubridate::ymd(calendar$end_date)

  if(all(calendar$end_date < Sys.Date())){
    stop("This GTFS file has expired, we can only check against present day trips")
  }

  calendar <- calendar[calendar$start_date < Sys.Date(), ]
  calendar <- calendar[calendar$end_date > Sys.Date(), ]

  # select n routes
  if(n < nrow(routes)){
    routes <- routes[sample(seq_len(nrow(routes)), n),]
  } else {
    message(paste0("Only ",nrow(routes)," route in GTFS file so checking all"))
  }

  # slect trips for those routes
  # for each route we need a range of trips with differnt characterisitcs
  trips <- trips[trips$route_id %in% routes$route_id,]
  trips <- trips[trips$service_id %in% calendar$service_id, ]
  trip_sub <- trips[,c("route_id","service_id")]
  trip_sub <- unique(trip_sub)
  trip_sub <- split(trip_sub, trip_sub$route_id)
  trip_sub <- lapply(trip_sub, function(x){
    if(nrow(x) == 1){
      return(x)
    } else {
      x <- x[sample(seq_len(nrow(x)), 2),]
    }
  })
  trip_sub <- dplyr::bind_rows(trip_sub)
  trip_sub$keep <- seq_len(nrow(trip_sub))
  trips <- dplyr::left_join(trips, trip_sub, by = c("route_id","service_id"))
  trips <- trips[!is.na(trips$keep),]
  trips <- split(trips, trips$keep)
  trips <- lapply(trips, function(x){
    if(nrow(x) == 1){
      return(x)
    } else {
      x <- x[sample(seq_len(nrow(x)), 2),]
    }
  })
  trips <- dplyr::bind_rows(trips)

  stop_times <- gtfs$stop_times
  stop_times <- stop_times[stop_times$trip_id %in% trips$trip_id,]

  stops <- gtfs$stops
  stops <- stops[stops$stop_id %in% unique(stop_times$stop_id),]

  trip_detail <- dplyr::group_by(stop_times, trip_id)
  trip_detail <- dplyr::summarise(trip_detail,
                                  stop_start = stop_id[stop_sequence == min(stop_sequence)],
                                  time_start = departure_time[stop_sequence == min(stop_sequence)],
                                  stop_end = stop_id[stop_sequence == max(stop_sequence)],
                                  time_end = departure_time[stop_sequence == max(stop_sequence)])

  if(any(trip_detail$stop_start == trip_detail$stop_end)){
    message("Excluding circular trips")
    trip_detail <- trip_detail[trip_detail$stop_start != trip_detail$stop_end,]
  }


  # Generate random valid dates for each trip
  trip_detail <- dplyr::left_join(trip_detail, trips, by ="trip_id")
  trip_detail$date <- as.Date(unlist(lapply(trip_detail$service_id, gennerate_dates, cal = calendar)),
                              origin = "1970-01-01")

  stops <- stops[,c("stop_id","stop_lon","stop_lat")]
  names(stops) <- c("stop_id","start_lon","start_lat")
  trip_detail <- dplyr::left_join(trip_detail, stops, by = c("stop_start" = "stop_id"))
  names(stops) <- c("stop_id","end_lon","end_lat")
  trip_detail <- dplyr::left_join(trip_detail, stops, by = c("stop_end" = "stop_id"))



  # add transit_mode
  # add transit_routing_preference
  google_routes <- list()
  for(i in seq_len(nrow(trip_detail))){
    gr <- googleway::google_directions(origin = trip_detail[i,c("start_lat", "start_lon")],
                                                  destination = trip_detail[i,c("end_lat", "end_lon")],
                                                  mode = "transit",
                                                  transit_mode = "bus",
                                                  transit_routing_preference = "less_walking",
                                                  departure_time = as.POSIXct(paste(trip_detail$date[i],
                                                                                    trip_detail$time_start[i]),
                                                                              format="%Y-%m-%d %H:%M:%S")
    )

    gr_legs <- gr$routes$legs[[1]]
    gr_geom <- gr$routes$overview_polyline[[1]]
    gr_geom <- googlePolylines::decode(gr_geom)
    gr_geom <- gr_geom[[1]]
    gr_geom <- gr_geom[,2:1]
    gr_geom <- sf::st_linestring(as.matrix(gr_geom))
    gr_geom <- sf::st_as_sfc(list(gr_geom), crs = 4326)
    gr_detail <- gr_legs$steps[[1]]$transit_details
    gr_detail <- gr_detail[!is.na(gr_detail$num_stops),]

    if(!is.null(gr_legs$departure_time$text)){
      gr_res <- data.frame(depart = gr_legs$departure_time$text,
                           arrive = gr_legs$arrival_time$text,
                           stops = gr_detail$num_stops[1],
                           geometry = gr_geom)
    } else {
      gr_res <- data.frame(depart = NA,
                           arrive = NA,
                           stops = NA,
                           geometry = gr_geom)
    }




    google_routes[[i]] <- gr_res
    rm(gr, gr_legs,gr_geom, gr_res)
  }

  google_routes <- dplyr::bind_rows(google_routes)
  google_routes <- st_as_sf(google_routes, crs = 4326)

  res <- dplyr::bind_cols(trip_detail, google_routes)




  qtm(gr_routes_poly) + qtm(pts)

}


gennerate_dates <- function(id, cal){
  cal <- cal[cal$service_id == id, ]

  # Next 14 days
  days <- seq(Sys.Date(), Sys.Date() + 14, by = 1)
  days <- days[days >= cal$start_date]
  days <- days[days <= cal$end_date]

  if(length(days) == 0){
    stop("No valid days in the next 14 days")
  }

  days <- data.frame(date = days, stringsAsFactors = FALSE)
  days$day <- tolower(lubridate::wday(days$date, label = TRUE, abbr = FALSE))

  valid_days <- cal[,c("monday","tuesday","wednesday","thursday","friday","saturday","sunday")]
  valid_days <- t(valid_days)
  valid_days <- row.names(valid_days)[valid_days == 1]

  days <- days[days$day %in% valid_days,]

  date_final <- days$date[sample(seq_len(nrow(days)), 1)]
  return(date_final)
}
