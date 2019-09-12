#' Split a gtfs file
#'
#' @param gtfs a gtfs object
#' @export
gtfs_split <- function(gtfs, n_split = 2){
  agency <- gtfs$agency
  stops <- gtfs$stops
  routes <- gtfs$routes
  trips <- gtfs$trips
  stop_times <- gtfs$stop_times
  calendar <- gtfs$calendar
  calendar_dates <- gtfs$calendar_dates

  res <- list()


  agency <- split(agency, rep(seq(1,n_split), each = ceiling(nrow(agency)/n_split))[seq(1,nrow(agency))])

  for(i in seq(1, n_split)){
    res[[i]] <- list()
    res[[i]]$agency <- agency[[i]]
    res[[i]]$routes <- routes[routes$agency_id %in% agency[[i]]$agency_id,]
    res[[i]]$trips  <- trips[trips$route_id %in% unique(res[[i]]$routes$route_id),]
    res[[i]]$calendar        <- calendar[calendar$service_id %in% unique(res[[i]]$trips$service_id),]
    res[[i]]$calendar_dates  <- calendar_dates[calendar_dates$service_id %in% unique(res[[i]]$trips$service_id),]
    res[[i]]$stop_times <- stop_times[stop_times$trip_id %in% unique(res[[i]]$trips$trip_id), ]
    res[[i]]$stops <- stops[stops$stop_id %in% unique(res[[i]]$stop_times$stop_id), ]
  }

  return(res)
}
