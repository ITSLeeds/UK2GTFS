#' merge a list of gtfs files
#'
#' @param gtfs_list a list of gtfs objects to be merged
#' @export
gtfs_merge <- function(gtfs_list) {

  # remove any NULLS
  gtfs_list <- gtfs_list[lengths(gtfs_list) != 0]

  # Split out lists
  agency <- sapply(gtfs_list, "[", "agency")
  stops <- sapply(gtfs_list, "[", "stops")
  routes <- sapply(gtfs_list, "[", "routes")
  trips <- sapply(gtfs_list, "[", "trips")
  stop_times <- sapply(gtfs_list, "[", "stop_times")
  calendar <- sapply(gtfs_list, "[", "calendar")
  calendar_dates <- sapply(gtfs_list, "[", "calendar_dates")

  # bind togther
  names(agency) <- seq(1, length(agency))
  suppressWarnings(agency <- dplyr::bind_rows(agency, .id = "file_id"))

  names(stops) <- seq(1, length(stops))
  suppressWarnings(stops <- dplyr::bind_rows(stops, .id = "file_id"))

  names(routes) <- seq(1, length(routes))
  suppressWarnings(routes <- dplyr::bind_rows(routes, .id = "file_id"))

  names(trips) <- seq(1, length(trips))
  suppressWarnings(trips <- dplyr::bind_rows(trips, .id = "file_id"))

  names(stop_times) <- seq(1, length(stop_times))
  suppressWarnings(stop_times <- dplyr::bind_rows(stop_times, .id = "file_id"))

  names(calendar) <- seq(1, length(calendar))
  suppressWarnings(calendar <- dplyr::bind_rows(calendar, .id = "file_id"))

  names(calendar_dates) <- seq(1, length(calendar_dates))
  suppressWarnings(calendar_dates <- dplyr::bind_rows(calendar_dates, .id = "file_id"))

  # fix typo
  agency$agency_name[agency$agency_name == "Dockland Light Railway"] <- "Docklands Light Railway"


  # agency
  agency$file_id <- NULL
  agency <- unique(agency)
  if (any(duplicated(agency$agency_id))) {
    # Check for upppercase problems
    # Sometime same agency with a captial letter in the name
    agency.check <- agency
    agency.check$agency_name <- tolower(agency.check$agency_name)
    agency.check <- unique(agency.check)
    if (any(duplicated(agency.check$agency_id))) {
      stop("Duplicated Agency IDS")
    } else {
      agency <- agency[!duplicated(agency$agency_id), ]
    }
  }

  # stops
  stops$file_id <- NULL
  stops <- unique(stops)
  if (any(duplicated(stops$stop_id))) {
    stop("Duplicated Stop IDS")
  }

  # routes
  if (any(duplicated(routes$route_id))) {
    message("De-duplicating route_id")
    route_id <- routes[, c("file_id", "route_id")]
    if (any(duplicated(route_id))) {
      stop("Duplicated route_id within the same GTFS file")
    }
    route_id$route_id_new <- seq(1, nrow(route_id))
    routes <- dplyr::left_join(routes, route_id, by = c("file_id", "route_id"))
    routes <- routes[, c("route_id_new", "agency_id", "route_short_name", "route_long_name", "route_desc", "route_type")]
    names(routes) <- c("route_id", "agency_id", "route_short_name", "route_long_name", "route_desc", "route_type")
  }


  # calendar
  if (any(duplicated(calendar$service_id))) {
    message("De-duplicating service_id")
    service_id <- calendar[, c("file_id", "service_id")]
    if (any(duplicated(service_id))) {
      stop("Duplicated service_id within the same GTFS file")
    }
    service_id$service_id_new <- seq(1, nrow(service_id))
    calendar <- dplyr::left_join(calendar, service_id, by = c("file_id", "service_id"))
    calendar <- calendar[, c("service_id_new", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday", "start_date", "end_date")]
    names(calendar) <- c("service_id", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday", "start_date", "end_date")

    calendar_dates <- dplyr::left_join(calendar_dates, service_id, by = c("file_id", "service_id"))
    calendar_dates <- calendar_dates[, c("service_id_new", "date", "exception_type")]
    names(calendar_dates) <- c("service_id", "date", "exception_type")
  }


  # Trips
  if (any(duplicated(trips$trip_id))) {
    message("De-duplicating trip_id")
    trip_id <- trips[, c("file_id", "trip_id")]
    if (any(duplicated(trip_id))) {
      stop("Duplicated trip_id within the same GTFS file")
    }
    trip_id$trip_id_new <- seq(1, nrow(trip_id))
    trips <- dplyr::left_join(trips, trip_id, by = c("file_id", "trip_id"))
    trips <- trips[, c("route_id", "service_id", "trip_id_new", "file_id")]
    names(trips) <- c("route_id", "service_id", "trip_id", "file_id")


    stop_times <- dplyr::left_join(stop_times, trip_id, by = c("file_id", "trip_id"))
    stop_times <- stop_times[, c("trip_id_new", "arrival_time", "departure_time", "stop_id", "stop_sequence", "timepoint")]
    names(stop_times) <- c("trip_id", "arrival_time", "departure_time", "stop_id", "stop_sequence", "timepoint")
  }
  if (exists("service_id")) {
    trips <- dplyr::left_join(trips, service_id, by = c("file_id", "service_id"))
    trips <- trips[, c("route_id", "service_id_new", "trip_id", "file_id")]
    names(trips) <- c("route_id", "service_id", "trip_id", "file_id")
  }
  if (exists("route_id")) {
    trips <- dplyr::left_join(trips, route_id, by = c("file_id", "route_id"))
    trips <- trips[, c("route_id_new", "service_id", "trip_id", "file_id")]
    names(trips) <- c("route_id", "service_id", "trip_id", "file_id")
  }

  trips <- trips[, c("route_id", "service_id", "trip_id")]
  names(trips) <- c("route_id", "service_id", "trip_id")

  # Condense Duplicate Service patterns
  message("Condensing duplicated servie patterns")
  calendar_dates_summary <- dplyr::group_by(calendar_dates, service_id)
  calendar_dates_summary <- dplyr::summarise(calendar_dates_summary,
    pattern = paste(c(date, exception_type), collapse = "")
  )
  calendar_summary <- dplyr::left_join(calendar, calendar_dates_summary, by = "service_id")
  calendar_summary <- dplyr::group_by(
    calendar_summary,
    start_date, end_date, monday, tuesday, wednesday,
    thursday, friday, saturday, sunday, pattern
  )
  calendar_summary$service_id_new <- dplyr::group_indices(calendar_summary)
  calendar_summary <- calendar_summary[, c("service_id_new", "service_id")]

  trips <- dplyr::left_join(trips, calendar_summary, by = c("service_id"))
  trips <- trips[, c("route_id", "service_id_new", "trip_id")]
  names(trips) <- c("route_id", "service_id", "trip_id")

  calendar <- dplyr::left_join(calendar, calendar_summary, by = c("service_id"))
  calendar <- calendar[, c("service_id_new", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday", "start_date", "end_date")]
  names(calendar) <- c("service_id", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday", "start_date", "end_date")
  calendar <- calendar[!duplicated(calendar$service_id), ]


  calendar_dates <- dplyr::left_join(calendar_dates, calendar_summary, by = c("service_id"))
  calendar_dates <- calendar_dates[, c("service_id_new", "date", "exception_type")]
  names(calendar_dates) <- c("service_id", "date", "exception_type")
  calendar_dates <- calendar_dates[!duplicated(calendar_dates$service_id), ]

  stop_times$file_id <- NULL
  routes$file_id <- NULL

  res_final <- list(agency, stops, routes, trips, stop_times, calendar, calendar_dates)
  names(res_final) <- c("agency", "stops", "routes", "trips", "stop_times", "calendar", "calendar_dates")
  return(res_final)
}
