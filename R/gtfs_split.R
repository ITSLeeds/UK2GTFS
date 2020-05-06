#' Split a GTFS object
#'
#' For large regions GTFS fiels can get very big. THis fucntion splits a GTFS
#' object into a list of GTFS objects. It tries to balance the sizes of the
#' objects. Splits are made by agency_id so maximumd number of splits equalts
#' the number of unique agency_ids.
#'
#' @param gtfs a gtfs object
#' @param n_split how many to split into default 2
#' @return a list of GTFS objects
#' @export
gtfs_split <- function(gtfs, n_split = 2) {
  agency <- gtfs$agency
  stops <- gtfs$stops
  routes <- gtfs$routes
  trips <- gtfs$trips
  stop_times <- gtfs$stop_times
  calendar <- gtfs$calendar
  calendar_dates <- gtfs$calendar_dates

  # try to balance splits

  routes_table <- routes[,c("route_id","agency_id")]
  routes_table <- dplyr::left_join(routes_table,
                                   as.data.frame(table(trips$route_id),
                                                 stringsAsFactors = FALSE),
                                   by = c("route_id" = "Var1"))
  routes_table <- routes_table[order(routes_table$Freq, decreasing = TRUE),]
  routes_table$cumsum <- cumsum(routes_table$Freq)
  routes_table$bucket <- as.integer(cut(routes_table$cumsum, n_split))
  #aggregate(Freq~bucket,FUN=sum, data=routes_table)
  routes_table <- routes_table[,c("agency_id","bucket")]

  agency <- dplyr::left_join(agency, routes_table, by = "agency_id")

  agency <- split(agency, agency$bucket)
  agency <- lapply(agency, function(x){
    x$bucket <- NULL
    return(x)
  })

  res <- list()

  for (i in seq_len(n_split)) {
    res[[i]] <- list()
    res[[i]]$agency <- agency[[i]]
    res[[i]]$routes <- routes[routes$agency_id %in% agency[[i]]$agency_id, ]
    res[[i]]$trips <- trips[trips$route_id %in% unique(res[[i]]$routes$route_id), ]
    res[[i]]$calendar <- calendar[calendar$service_id %in% unique(res[[i]]$trips$service_id), ]
    res[[i]]$calendar_dates <- calendar_dates[calendar_dates$service_id %in% unique(res[[i]]$trips$service_id), ]
    res[[i]]$stop_times <- stop_times[stop_times$trip_id %in% unique(res[[i]]$trips$trip_id), ]
    res[[i]]$stops <- stops[stops$stop_id %in% unique(res[[i]]$stop_times$stop_id), ]
  }

  return(res)
}
