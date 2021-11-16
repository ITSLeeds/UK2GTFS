#' merge a list of gtfs files
#'
#' @param gtfs_list a list of gtfs objects to be merged
#' @param force logical, if TRUE duplicated values are merged taking the fist
#'   instance to be the correct instance, in most cases this is ok, but may
#'   cause some errors
#' @export
gtfs_merge <- function(gtfs_list, force = FALSE) {

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
  calendar_dates <- calendar_dates[sapply(calendar_dates, nrow) > 0]
  suppressWarnings(calendar_dates <- dplyr::bind_rows(calendar_dates, .id = "file_id"))

  # fix typo
  agency$agency_name <- as.character(agency$agency_name)
  agency$agency_name[agency$agency_name == "Dockland Light Railway"] <- "Docklands Light Railway"
  agency$agency_name[agency$agency_name == "Edward Bros"] <- "Edwards Bros"
  agency$agency_name[agency$agency_name == "John`s Coaches"] <- "John's Coaches"
  agency$agency_name[agency$agency_name == "Stagecoach in Lancaster."] <- "Stagecoach in Lancashire"
  agency$agency_name[agency$agency_name == "Stagecoach in South Wales"] <- "Stagecoach South Wales"

  # fix duplicated agency_ids - special cases
  #agency$agency_id[agency$agency_name == "Tanat Valley Coaches"] <- "TanVaCo"

  # if agency names are same as IDs but not always
  if (any(agency$agency_name == agency$agency_id)) {
    agency_sub <- agency
    agency_sub$file_id <- NULL
    agency_sub <- unique(agency)
    id_dups <- agency_sub$agency_id[duplicated(agency_sub$agency_id)]
    if (length(id_dups) > 0) {
      agency_sub <- agency_sub[agency_sub$agency_id %in% id_dups, ]
      agency_sub <- agency_sub[agency_sub$agency_id != agency_sub$agency_name, ]
      for (i in seq(1, nrow(agency_sub))) {
        agency$agency_name[agency$agency_name == agency_sub$agency_id[i]] <- agency_sub$agency_name[i]
      }
    } else {
      rm(agency_sub, id_dups)
    }
  }


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
      if(force){
        warning(paste0("Duplicated Agency IDs ",
                       paste(unique(agency.check$agency_id[duplicated(agency.check$agency_id)]), collapse = " "),
                       " will be merged"))
        # Assume 1st Name is correct name
        agency <- dplyr::group_by(agency, agency_id)
        agency <- dplyr::summarise(agency,
                                   agency_name = agency_name[1],
                                   agency_url = agency_url[1],
                                   agency_timezone = agency_timezone[1],
                                   agency_lang = agency_lang[1]
                                   )
      } else {
        stop(paste0("Duplicated Agency IDs ",
                    paste(unique(agency.check$agency_id[duplicated(agency.check$agency_id)]), collapse = " ")))
      }
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
      if(force){
        routes <- routes[!duplicated(route_id), ]
        route_id <- routes[, c("file_id", "route_id")]
      } else {
        stop("Duplicated route_id within the same GTFS file, try using force = TRUE")
      }
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

    if (nrow(calendar_dates) > 0) {
      calendar_dates <- dplyr::left_join(calendar_dates, service_id, by = c("file_id", "service_id"))
      calendar_dates <- calendar_dates[, c("service_id_new", "date", "exception_type")]
      names(calendar_dates) <- c("service_id", "date", "exception_type")
    }
  }


  # Trips
  if (any(duplicated(trips$trip_id))) {
    message("De-duplicating trip_id")
    trip_id <- trips[, c("file_id", "trip_id")]
    if (any(duplicated(trip_id))) {
      if(force){
        trips <- unique(trips)
        stop_times <- unique(stop_times)
        trip_id <- trips[, c("file_id", "trip_id")]
      } else{
        stop("Duplicated trip_id within the same GTFS file")
      }


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
  if (nrow(calendar_dates) > 0) {
    message("Condensing duplicated service patterns")
    calendar_dates_summary <- dplyr::group_by(calendar_dates, service_id)
    if(class(calendar_dates_summary$date) == "Date"){
      calendar_dates_summary <- dplyr::summarise(calendar_dates_summary,
                                                 pattern = paste(c(as.character(date), exception_type), collapse = "")
      )
    } else {
      calendar_dates_summary <- dplyr::summarise(calendar_dates_summary,
                                                 pattern = paste(c(date, exception_type), collapse = "")
      )
    }

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
  }

  stop_times$file_id <- NULL
  routes$file_id <- NULL
  calendar$file_id <- NULL

  res_final <- list(agency, stops, routes, trips, stop_times, calendar, calendar_dates)
  names(res_final) <- c("agency", "stops", "routes", "trips", "stop_times", "calendar", "calendar_dates")
  return(res_final)
}
